---
name: sc-pipeline
description: 单细胞转录组数据处理流水线。把 10x Genomics 的 matrix 三件套（barcodes/features/matrix）处理成可直接出图的 Seurat 对象，覆盖去双细胞、质控、多样本合并、批次校正、两段式聚类、筛群、样本剔除、文献驱动注释全链路，并产出运行报告。全程配置驱动：所有业务参数来自 pipeline.json，脚本里没有任何写死的默认值；需要人决定的地方（质控下限取多少、剔哪些簇、剔哪个样本）做成待确认闸门，没有答案就停下要答案。当用户要求「处理单细胞数据」「从 matrix 跑出结果」「按对方脚本复刻流程」「做质控合并聚类注释」时使用。关键词：单细胞、scRNA-seq、Seurat、matrix、10x、去双、DoubletFinder、质控、合并、Harmony、聚类、注释、pipeline、预处理、配置驱动、工作台、人工确认。 在「个性化分析助手」里由阶段 id 直接调用（pipeline1 / pipeline2 / precheck），此时按 flow_spec 的命令模板执行、跑完必须过 reviewer 审核才算完成。
invocable: user
argument-hint: "--config <pipeline.json> [--init-config] [--resume-from <步骤>] [--indir <覆盖>] [--outdir <覆盖>]"
---

# 单细胞数据处理流水线（sc-pipeline）

把「10x matrix」变成「可直接出图的 Seurat 对象」。

## 在本应用里怎么被调用（个性化分析助手 · 服务器模式）

这一段是给接手的 agent 看的：这个 skill 在本应用里不是"想起来才用"，而是**被阶段 id 点名调用的**。
流程定义写在 `engine/flow_spec.R`，命令模板也以它为准；下面这些命令就是从那里生成的。

| 阶段 id | 该跑什么 | 命令（占位符由应用填好） |
|---|---|---|
| `precheck` | 只算够不够跑，不写结果 | `{rscript} {scripts}/00_run_all.R --config {run_dir}/pipeline.json --indir {indir} --outdir {outdir} --dry-run` |
| `pipeline1` | 跑到一次聚类、出人工判断依据就停住 | 同上，`--stage cluster1 --gates {run_dir}/gates.json` |
| `pipeline2` | 筛群 → 二次聚类 → 剔样本 → 注释 | 同上，`--stage continue --gates {run_dir}/gates.json` |

三条硬约束，违反任一条这一步就不算完成：

1. **不许换脚本。** 就是 `00_run_all.R`（它再调 `01_preprocess.R` / `02_annotate.R`）。另写一份产出"看起来正常的图"，过不了 reviewer 白名单。
2. **不许跳步。** `--stage continue` 之前必须真有一次聚类快照 `results/status/stage_cluster1.rds`；没有就是没到那一步，不是"可以凑合继续"。
3. **不许猜闸门值。** 闸门没答就停下报回，不要自己填一个默认值 —— 这条是这个 skill 的根，历史上因为写死阈值静默删错过 1590 个细胞。

跑完必须过审核，否则下游一步都发不出去：

```
{rscript} {engine}/reviewer.R --engine {engine} --dir {run_dir} --stage pipeline1
```

它的结论要回写 `POST /api/review/import`（带 stage / ok / output / host）。
审核里会核三件事：台账 set_id 是否落在 sc-pipeline、脚本 MD5 是否命中白名单、阶段与产物是否对齐。

**进度要按环节回写**，不然应用只知道"整步在跑"，不知道卡在哪个环节：

| 阶段 | 环节 id（按顺序） |
|---|---|
| pipeline1 | dep → sample → precheck → doublet_qc → merge → cluster1 → basis |
| pipeline2 | filter → cluster2 → dropsample → annot → save |

回写接口：`POST /api/step/import`，body 带 `step`、`sub`、`status`、`started_at`、`ended_at`、`log`。


## 三条铁律（最高优先级）

1. **不做降级。** 依赖缺失、步骤失败、结果异常 → 停下来报告，不许回退、不许跳过、不许自动改参数。
2. **不用隐藏默认值。** 业务参数只能来自「用户显式指定」或「官方口径配置」。两者都没有 → 报错退出，并列出缺哪些。**脚本代码里不出现任何业务默认值。**
3. **不许抽样顶替全量。** 抽样只允许用于流程冒烟测试，必须在运行报告与产物目录显著标注「抽样结果，不得用于交付对照」。

## 配置驱动：值不在代码里

这是本 skill 的核心设计。所有业务参数放在 `pipeline.json`，脚本只读值。

**为什么这样做。** 以前把「剔除 16–22 号簇」「剔除 18m-G761」「质控 300」这类值写在代码里。换一批数据这些值就全错，而且**错得静默**——不报错，只是结果不对。真实事故：照抄对方脚本里写死的簇号过滤，在自己数据上误删了 1590 个细胞（含对方 UC 类的 95%、NEU 类的 98%），全程没有任何报错。

### 四个来源标签（不许混用）

| source | 含义 | 换数据时 |
|---|---|---|
| `official` | 官方口径，照抄对方脚本里的值 | 通常仍适用 |
| `instance` | 本机实例适配，为在这台机器跑通做的改动 | 通常仍适用 |
| `user` | 用户显式指定 | 优先最高 |
| `gate` | **未定，需要人工确认** | **没有答案就不许开始计算** |

阈值/分辨率的判定顺序：`user` > `instance` > `official`。遇到 `gate` 且没有取值 → `cfg_require_resolved()` 直接停下，列出所有待确认项。

### 硬编码禁令（回归测试清单）

以下形态出现即视为缺陷，`cfg_selfcheck()` 会拦：

- 样本名（如 `18m-G761`）出现在 `steps` 之外的地方
- 按簇号过滤而没有配套的 `mapping_file` 与 `reason`
- 质控阈值为 NA 或缺失
- 缺少 `seed`

## 人工确认闸门（gates.json）

需要人决定的事，**在代码里做不出来的**，一律做成闸门，不做兜底：

| 闸门 | 为什么必须人来定 |
|---|---|
| 质控下限 | 对方材料里 300 / 400 / 500 三个数字互相矛盾，只有人能确认以哪个为准 |
| 读入前剔除样本 | 数据质量与实验设计的判断 |
| **筛群（剔除哪些簇）** | 簇号是本次运行的产物，换一次全变；剔谁取决于本次聚类结果 |
| 聚类后剔除样本 | 如「为凑 3 对 3 而剔掉一例」，是实验设计决策，不是数据判据 |
| 低置信门槛 | 多小的簇算证据不足 |

闸门的三态：`pending` 等人答 / `answered` 已答 / `auto` 系统自动判定无需人管。

**闸门设计原则：**
- 必须给出**证据**（表格或键值），不能让人凭空决定
- 必须给**推荐值**（`suggested`），并说明推荐理由
- 答案写回后，同步把 `pipeline.json` 里对应项的 `source` 改成 `user` 并填 `value`
- 未答的闸门拦住整个流程，不许默认通过

## 工作台（可视化 + 人工操作）

`workbench/工作台.html` — 单文件自包含 HTML，双击打开，零外部依赖。

它做四件事：
1. **流程可视化**：18 个步骤的状态、细胞数流转漏斗
2. **待确认卡片**：每条闸门显示证据 + 选项 + 推荐值，人点选即填，可导出
3. **参数表**：值 / 来源标签 / 是否锁定，可直接改，改过自动标 `user`
4. **导出预览**：实时显示将要写回的两个 JSON

需要人工操作的地方都在这里，不在命令行参数里。

## 处理链（18 步）

| 步 | 动作 | 参数来源 |
|---|---|---|
| 0 | 环境检查 + 容量预检 | 内存需求按 barcode 数**实算**，不用经验常数 |
| 1 | 逐样本读入 10x | `min.cells=3, min.features=0`（official） |
| 2 | 算线粒体比例 | Mouse `^mt-` / Human `^MT-` / Rat `^Mt-` |
| 3 | 去双（RunDF） | 见下「去双规格」 |
| 4 | 取 Singlet 重建 | `min.cells=3, min.features=200` |
| 5 | 逐样本质控 | `qc_min_features` / `qc_max_features` / `qc_mt`（**gate，须确认**） |
| 6-10 | 合并 / 标准化 / 周期打分 / 回归 / PCA | `nfeatures` / `dims` |
| 11 | 批次校正 | `RunHarmony('orig.ident')`；**harmony 不可用则停止** |
| 12 | 一次聚类 | `res1` → 落盘簇大小供闸门决策 |
| 13 | **筛群** | `steps.cluster_filter`（**gate**）。默认 `none`；要剔优先按类型名 |
| 14 | 二次聚类 | `res2` |
| 15 | **剔除样本** | `steps.post_cluster_exclusion`（**gate**）。样本名须真实存在于本次结果 |
| 16 | 注释 | marker 打分写入 `annot` 列 |
| 17 | 保存 + 运行报告 | 每步细胞数流转 + 每个参数的来源 |

**顺序不能改**：13 → 14 → 15。

## 去双规格（RunDF）

```
NormalizeData → ScaleData → FindVariableFeatures(vst, nfeatures) → RunPCA
FindNeighbors(dims) → FindClusters(resolution) → RunUMAP(dims)
清空 scale.data
paramSweep_v3(PCs, sct=TRUE)
summarizeSweep(GT=FALSE) → find.pK
pK = BCmetric 最大处的 pK          ← 自动选，不许写死
DoubletRate = ncol × 8e-6
nExp = round(DoubletRate × ncol)
nExp = round(nExp × (1 - homotypic.prop))
doubletFinder_v3(PCs, pN, pK, nExp, sct=TRUE)
```

⚠️ **`sct=TRUE` 必须照抄。** 官方对象用 LogNormalize 而非 SCTransform，这个参数组合本身是官方写法。

## 内存不足时怎么办（重要）

去双的 `paramSweep` 内部要算一张 **(真实细胞 + 人造双细胞)²** 的距离矩阵。单样本约 1.3 万细胞 → 翻倍 2.6 万 → 单张矩阵约 5.3 GB，且要算 6 次。

### 处理原则：不抽样、不降精度、不改参数

只有**一条**替代路径，且必须结果等价：

| 路径 | 触发 | 改了什么 | 结果 |
|---|---|---|---|
| 原生路径 | 内存充足 | 什么都没改 | 与官方一致 |
| 分块路径 | 预检判定不足，或配置 `force_blocked` | **只改一行**：整张距离矩阵改按列分块（每块约 200 MB），算完即释放 | **完全等价** |

等价性实测（`lib/test_rdist_equiv.R`）：

```
RANK_IDENTICAL          : TRUE
MISMATCH_COLS           : 0
MAX_RANK_ELEM_DIFF      : 0
MAX_ABS_DISTANCE_DIFF   : 0e+00
```

同一份 `fields::rdist()`、同一组坐标，只换了计算顺序。

**配置项**：`steps.memory.mode` = `auto`（默认）/ `force_blocked` / `force_native`。
判定结果写入 `status/precheck.json`，运行报告里也有一节。

## 输入

| 项 | 必需 | 说明 |
|---|---|---|
| `io.indir` | 是 | 每样本一个子目录。支持两种结构：① 直接放三件套；② CellRanger 的 `outs/filtered_feature_bc_matrix/` |
| `io.outdir` | 是 | 输出目录 |
| `io.samples_file` | 否 | 两列制表符：样本目录名 + 分组名。**缺省则分组名=样本名，且不能做组间差异** |
| `io.reference_table` | 否 | 带 marker 的文献表。与 `io.lit_dict` 二选一 |
| `io.lit_dict` | 否 | 组装好的 marker 词典 |

## 输出

```
outdir/
├── processed.rds            最终对象（含 annot 列）
├── 01_single/               每样本去双后 rds
├── 02_qc/                   逐样本质控统计
├── run_report.md            ★ 人读运行报告
├── run_manifest.json        机器可读：参数 + 来源 + 版本 + 种子 + 细胞数流转
└── status/
    ├── precheck.json        容量预检的判定过程（含用了哪条内存路径）
    ├── cluster1.json        一次聚类结果（供工作台做筛群决策）
    ├── cluster1_sizes.tsv   簇大小表
    ├── param_sources.tsv    ★ 每个参数的值与来源
    └── run_status.json      流程进度（供工作台读取）
```

**运行报告必须记录：**
- 每个参数的值 + **来源**（由程序生成，不手抄）
- 每步的细胞数减少（进 → 出）
- R 版本 + 各关键包版本
- 随机种子
- **内存路径决策**（走原生还是分块、为什么）
- 任何被跳过、失败或异常的环节

## 依赖

R 包：Seurat、DoubletFinder、harmony、Matrix、dplyr、ggplot2、patchwork、cowplot、stringr、argparser、jsonlite
可选加速：presto（Wilcoxon C++ 实现）、BPCells（磁盘后端）

**缺任何一个必需包 → 停止并给出安装命令，不继续。**

## 边界

- 本 skill 只做数据处理，不出最终报告用图（交给 sc-plot-kit）
- 不做质控阈值的「智能推荐」——阈值由用户或配置给定；未定则走闸门
- 不做细胞类型的主观判定——判定依据来自 reference 表或 sc-lit-dict 产出的词典
- 不修改用户原始数据，所有产物写入 `outdir`
- **按簇号过滤只允许配合映射表使用**，且必须在 `reason` 里写明；换数据必须重做映射表

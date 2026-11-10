---
name: sc-plot-kit
description: 单细胞转录组标准图集出图器。把处理好的 Seurat 对象按交付报告(PPT)的版式一键产出六段图集：质控小提琴网格图、聚类 UMAP(总体/按样本/按分组分面)、marker 热图与 FeaturePlot 与小提琴拼板、细胞注释 UMAP 与 marker 对照点图、细胞数量与比例统计图(带误差线散点与 p 值)、组间差异三色火山图与 GO/KEGG 横向柱状富集图。当用户要求"给这批单细胞数据出图""按 PPT 的图样画图""产出可放进报告的单细胞图表""复现那份分析报告的配图"时使用。关键词：单细胞、scRNA-seq、Seurat、出图、绘图、UMAP、FeaturePlot、热图、火山图、富集图、细胞比例、质控图、DotPlot、报告配图、plot、visualization。 在「个性化分析助手」里由阶段 id plots 调用，出六套图后必须过 reviewer 审核（脚本须命中白名单）。
invocable: user
argument-hint: "<seurat.rds> --outdir <目录> [--celltype-col annot] [--group-col group] [--sample-col orig.ident] [--compare MicroplasticsvsMock] [--stage qc,cluster,markgene,annot,ratio,diff,enrich]"
---

# 单细胞标准图集出图器（sc-plot-kit）

把「已处理好的 Seurat 对象」变成「一套可以放进交付报告的图」。图样与目录结构**逐张对齐客户交付 PPT**。

当前脚本版本为 v2，已按逐张核对原报告得出的「复刻规格」重写，并落实了九条改进项。

## 在本应用里怎么被调用（个性化分析助手 · 服务器模式）

出图是流程里的 `plots` 阶段，命令由 `engine/flow_spec.R` 生成：

```
{rscript} {engine}/sc-plot-kit/scripts/plot_all_v2.R {outdir}/processed.rds \
  --outdir {outdir}/plots --config {run_dir}/pipeline.json \
  --stage qc,cluster,markgene,annot,ratio,diff,enrich
```

前置条件：`{outdir}/processed.rds` 必须已经存在（主流程两段都跑完）。没有它就是还没到出图这一步，不许拿别的东西顶替。

六段参数对应六套图，缺哪段就少哪套：`qc`（质控）、`cluster`（聚类）、`markgene`（marker）、`annot`（注释）、`ratio`（细胞比例）、`diff`（组间差异）、`enrich`（富集）。

跑完必须过审核：`{rscript} {engine}/reviewer.R --engine {engine} --dir {run_dir} --stage plots`，
结论回写 `POST /api/review/import`。审核会核台账 set_id 是否落在 sc-plot-kit、脚本 MD5 是否命中白名单。

环节回写：`POST /api/step/import`，`step=plots`，`sub` 取 `qc / cluster / markgene / annot / ratio / diff / enrich` 之一。


## 什么时候用它

- 手上有一个跑完聚类（并通常已注释）的 Seurat rds，要出报告配图
- 用户说「按 PPT 的图样画」「复现那份报告的图」「要和对方出的图一模一样」
- 需要一次性交付质控、聚类、marker、注释、比例、差异富集六段图

## 依赖环境（重要）

```r
# 库路径按需改
if (dir.exists("D:/soft/R/library")) .libPaths(c("D:/soft/R/library", .libPaths()))
install.packages(c("Seurat", "dplyr", "ggplot2", "patchwork", "ggrepel",
                   "pheatmap", "ggpubr", "harmony"))
BiocManager::install(c("org.Mm.eg.db"))
```

**必须锁 ggplot2 3.5.x，不能用 4.x。** 原因：ggplot2 4.0 改了 `+.gg` 的内部实现（用 deparse 取表达式），
遇到 Seurat 的 S4 绘图对象会直接报 `'S4SXP': should not happen`，
连最基础的 `VlnPlot()` 都会崩。装 3.5.1：

```r
remotes::install_version("ggplot2", version = "3.5.1", upgrade = "never")
```

同时注意：新版 clusterProfiler 的 DESCRIPTION 声明 `ggplot2 >= 4.0`，
所以它和 Seurat 不能共存在同一个库路径。本 skill 的富集段**不依赖 clusterProfiler**，
改用 `org.Mm.eg.db` + 超几何检验自己算，出图风格与原报告一致，也省掉版本冲突。

## 职责边界（重要）

本 skill **只做出图**。它不关心数据怎么来，但要求输入是一个已经跑完的 Seurat rds。

- 上游是 **sc-pipeline**：读 matrix → 去双 → 质控 → 合并 → 聚类 → 筛群 → 剔样本 → 注释
- 本 skill **不做**：读 10x 矩阵、去双细胞、质控过滤、聚类、细胞类型判定
- 原 `scripts/process_data.R` 已迁至 `sc-pipeline/scripts/legacy_reference/`（历史参考，不在本 skill 范围）

**失败即报错**：输入缺列、依赖缺失、路径不存在 → 停下报错，不静默改用替代列、不跳过环节。

## 输入要求

| 项 | 说明 | 必需 |
|---|---|---|
| Seurat 对象 | 已完成归一化、聚类、UMAP 的 rds | 是 |
| 分组列 | 默认 `group`（如 Mock / Microplastics） | 是 |
| 样本列 | 默认 `orig.ident` | 是 |
| 细胞类型列 | 默认 `annot` | 注释段必需 |
| 对比组 | 形如 `MicroplasticsvsMock` | 差异/富集段必需 |

## 输出目录结构

```
outdir/
├── 0.QC/            质控小提琴图（每样本一张小图拼 3×3 网格）+ 统计表
├── 1.cluster/       聚类 UMAP（总体 / 按样本 / 按分组分面 / 按样本分面）
├── 2.markgene/      top10_heatmap + featureplot_cluster{N} + violin_cluster{N} + marker 表
├── 3.cell_ratio/    细胞数量与比例（按组 / 按样本 / 按组带误差线与 p 值）
├── 4.diff_enrich/   diff/火山图 + enrich/GO 与 KEGG 横向柱状图
└── 5.annot/         注释 UMAP + 注释×群 UMAP + DotPlot 点图
```

## 六段图的画法（v2 对齐原报告的关键点）

### 0.QC — 质控小提琴图

- **每个样本单独画一张小图**，再拼成 **3×3 网格**（7 样本时第 8、9 格留空）
- 每张小图自带纵轴刻度、自带图例（样本名）
- **统一单一珊瑚红** `#F8766D` 填充 + 深色描边，不是多色
- 画之前必须 `Idents(o) <- 样本列`，否则按默认 identity 分组会导致配色数不够而报错
- 指标：`nFeature_RNA`、`nCount_RNA`、`percent.mt`

### 1.cluster — 聚类 UMAP

- 轴标签必须写 **大写的 `UMAP_1` / `UMAP_2`**（原报告如此）
- 四张：总体分群（簇上标群号）、按样本着色、按分组分面（ncol=2）、按样本分面（ncol=4）

### 2.markgene — 三类图 + 表

```r
markers <- FindAllMarkers(obj, only.pos = TRUE, min.pct = 0.2, logfc.threshold = 0.25)
markers <- markers[!grepl(markers$gene, pattern = "^mt-|^MT-|^Mt-"), ]
sig <- subset(markers, p_val_adj < 0.05)
```

- 热图：每群 top10，`DoHeatmap()`，配 **蓝-白-红** 渐变填充
- FeaturePlot：每群 top12，排成 **3 行 × 4 列**，配色 `lightblue → lightyellow → orange → red`
- 小提琴：每群 top12，排成 3×4，20 群彩虹填充，纵轴 `Expression Level`
- **可复用已算好的 marker 表**：若 `all_markgene.xls` / `sig_markgene.xls` 已存在就直接读，跳过重算（FindAllMarkers 很慢）

### 3.cell_ratio — 细胞数量与比例

- **纵轴统一写 `Percentage(%)`**，不是 Proportion
- 横轴无标签无刻度
- **按细胞类型分面**，4 列
- 三张图：
  1. 按组：每面 2 柱，图例标题为分组列名，**Mock 红 `#E64B35` / Microplastics 青 `#4DBBD5`**
  2. 按样本：每面 7 柱，图例标题为样本列名
  3. 按组带统计：**黑色误差线 + 黑色个体散点 + p 值标签**（用样本级比例算 mean±se 并做 t 检验），原报告无显著性星号

### 4.diff_enrich — 差异与富集

火山图（每细胞类型一张）：

- **三色**：UP 红 `#E64B35` / DOWN 绿 `#00A087` / NO 蓝 `#3C5488`
- 图例标题 `p_val_adj<0.05`，图例条目**带计数**（如 `UP 249`、`DOWN 1462`、`NO 906`）
- 竖虚线在 `x = 0`，横虚线在 `y = -log10(0.05) ≈ 1.301`
- 标注 |log2FC| 最大的前 40 个基因
- 入组门槛：每个细胞类型在两组的细胞数各 ≥ 3

富集图（**横向柱状图，不是气泡图**）：

- 纵轴写**通路英文全称**（不带 GO/KEGG 编号）
- 横轴 `Count`
- 柱色由 `p.adjust` 映射，`#E64B35 → #C51B7D → #7B3294 → #3C5488`（红→洋红→紫→蓝）
- 浅灰横向网格，无数值标注，取最显著的前 30 条
- GO 用 `org.Mm.eg.db` 的 `GO`+`ONTOLOGY=="BP"` 列，KEGG 用 `PATH` 列（配 `KEGGREST::keggList("mmu")` 把 mmu 编号换成英文全称）

### 5.annot — 注释图

- 注释 UMAP（标题为细胞类型列名）、注释×群 UMAP（标题 `annot_cluster`）
- **DotPlot 点图**：横轴基因、纵轴细胞类型，配色 `blue → lightpink → red`，`RotatedAxis()`

## 配色约定

细胞类型/群统一用一套 33 色板，保证同一类型跨图颜色一致：

```r
color_panel <- c('#53A85F','#58A4C3','#AB3282','#8C549C','#BD956A','#57C3F3','#6778AE',
                 '#F3B1A0','#F1BB72','#DCC1DD','#E95C59','#625D9E','#F7F398','#E63863',
                 '#5F3D69','#C5DEBA','#CCE0F5','#B53E2B','#AA9A59','#E39A35','#91D0BE',
                 '#23452F','#E4C755','#585658','#C1E6F3','#D6E7A3','#712820','#CCC9E6',
                 '#3A6963','#68A180','#61B934','#476D87','#9FA3A8','#968175')
```

## 怎么跑

```bash
Rscript scripts/plot_all_v2.R <seurat.rds> --outdir <目录> \
    --group-col group --sample-col orig.ident --celltype-col annot \
    --compare MicroplasticsvsMock \
    --stage qc,cluster,markgene,annot,ratio,diff,enrich
```

`--stage` 可只跑部分段，便于分批出图与续跑（比如先 `qc,cluster` 确认版式，再补 `markgene,diff,enrich`）。

数据量大时建议按段拆开跑，单段失败不影响其他段。

## 验收自检（每段出完都要过）

1. 文件齐不齐：六段目录都存在
2. 图能不能读：打开 png 确认不是空白（**特别注意白底：原报告 PNG 是透明底，导出必须显式 `bg="white"`，否则换环境会变黑底**）
3. 颜色对不对：同一细胞类型跨图颜色一致；质控图是单一红；火山图是三色
4. 轴标签对不对：UMAP 图必须是 `UMAP_1`/`UMAP_2`；比例图纵轴必须是 `Percentage(%)`
5. 数量对不对：比例表各群 rate 之和应约等于 100
6. 命名对不对：文件名与段号匹配（0.QC / 1.cluster / 2.markgene / 3.cell_ratio / 4.diff_enrich / 5.annot）

## 边界与已知限制

- 本 skill 只负责出图，不做质控阈值决定、不做细胞类型判定
- 数据处理（读矩阵 / 去双 / 质控 / 聚类 / 注释）归 sc-pipeline，本 skill 不重复实现、不内置任何数据处理脚本
- 细胞类型注释需先用文献或 marker 知识确定，本 skill 按传入的 `annot` 列出图
- 去双细胞（DoubletFinder 等）不在本 skill 范围
- 若对象是 Seurat v5 且用到了 `@assays$RNA@data` 这类旧写法，需先确认 assay 类型
- 富集段不做 UP/DOWN 拆分（原报告只出 ALL）；需要拆分可传 `--compare` 后自行筛 avg_log2FC

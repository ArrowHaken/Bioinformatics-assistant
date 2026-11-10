---
name: sc-ppt-kit
description: 单细胞分析报告 PPT 生成器。把六套分析结果（QC / 聚类 / 分群 marker / 注释 / 细胞比例 / 组间差异富集）按客户交付 PPT 的版式与页序，一键生成同款 .pptx。当用户要求「出 PPT」「做个报告 PPT」「按客户那版 PPT 生成」「复现交付 PPT」「把这批结果做成 pptx」「生成分析报告」时使用。关键词：PPT、pptx、报告、幻灯片、交付稿、分析报告、单细胞、scRNA-seq、版式复刻、模板、出图成稿。 在「个性化分析助手」里由阶段 id ppt 调用，按母版出 pptx 后必须过 reviewer 审核。
invocable: user
argument-hint: "--result-dir <六套结果根目录> --template <客户PPT模板.pptx> [--out <输出.pptx>] [--date YYYY-MM-DD] [--title <封面标题>] [--on-missing error|skip] [--dry-run]"
---

# 单细胞分析报告 PPT 生成器（sc-ppt-kit）

把「跑完的六套结果」变成「一份可以直接交付的 .pptx」。

版式、页序、字体、配色、文案句式**逐项对齐客户交付的那份 PPT**（22 页，16:9）。

## 在本应用里怎么被调用（个性化分析助手 · 服务器模式）

出 PPT 是流程里的 `ppt` 阶段，命令由 `engine/flow_spec.R` 生成：

```
{python} {engine}/sc-ppt-kit/scripts/make_ppt.py --result-dir {outdir}/plots \
  --template {engine}/sc-ppt-kit/templates/template.pptx \
  --config {run_dir}/pipeline.json --out {outdir}/analysis_report.pptx
```

前置条件：`{outdir}/plots` 必须已经有图。没图就没东西可装，不许空跑。

跑完必须过审核：`{rscript} {engine}/reviewer.R --engine {engine} --dir {run_dir} --stage ppt`，
结论回写 `POST /api/review/import`。

环节回写：`POST /api/step/import`，`step=ppt`，`sub` 取 `collect / assemble`。


## 什么时候用它

- 分析跑完了，要把结果做成 PPT 交付
- 用户说「按客户那版 PPT 出一份」「复现交付 PPT」「做成 pptx」
- 需要固定口径的封面、目录、章节编号与说明文案，避免每次手写

**不该用它**：只出图（那是 `sc-plot-kit`）；只提取报告文字和数字（那是 `sc-report-extract`）。
本 skill 负责的是最后一步——**把图和文字组装成 .pptx**。

## 依赖（重要）

**只用 python3 标准库**（`zipfile` / `re` / `xml.etree` 等），
**不需要 python-pptx，不需要 R 的 officer**。因此在本机（Linux）和用户的 Windows 上都能直接跑。

原理：把客户 PPT 当作版式母版，读取它的 `slideMasters` / `slideLayouts` / `theme` / `media`，
按页序生成 `slideN.xml` 并重写各自的关系文件，最后用 `zipfile` 重新打包为 `.pptx`。

## 输入

| 参数 | 必需 | 说明 |
|---|---|---|
| `--result-dir` | 是 | 六套结果的根目录（含 `0.QC` / `1.cluster` / `2.markgene` / `5.annot` / `3.cell_ratio` / `4.diff_enrich`） |
| `--template` | 是（生成时） | 客户交付的 PPT 模板 `.pptx`，用作版式母版 |
| `--out` | 否 | 输出路径；默认 `analysis_report_<YYYYMMDD>.pptx` |
| `--date` | 否 | 封面日期，默认取当天 |
| `--title` | 否 | 封面标题，默认用 `slide_spec.json` 里的默认标题 |
| `--spec` | 否 | 页序与文案配置，默认 `scripts/slide_spec.json` |
| `--on-missing` | 否 | `error`（默认，缺图即报错）或 `skip`（跳过并写入报告） |
| `--dry-run` | 否 | 只打印 22 页计划与图片来源，不写文件 |
| `--report` | 否 | 把本次生成情况写成报告文件 |
| `--stats-json` | 否 | 输出统计信息（页数、填充页、跳过页） |

## 模板规格（已从客户 PPT 提取，改动前先核对）

| 项 | 值 |
|---|---|
| 幻灯片尺寸 | `cx=12192000 cy=6858000`（16:9，EMU） |
| 标题字体 | 等线 Light（majorFont） |
| 正文字体 | 等线（minorFont）；页面内正文显式指定 **微软雅黑** |
| 标题字号 / 正文字号 | 20pt（sz=2000）/ 15pt（sz=1500） |
| 标题强调色 | `C86400` |
| 章节编号块 | 左上角 `00`–`05` |

## 页序（22 页，必须按此顺序）

| 页 | 章节 | 内容 |
|---|---|---|
| 1 | 封面 | 标题 + 日期 |
| 2 | 目录 | 01.样本基本信息 / 02.细胞聚类 / 03.分群marker / 04.细胞注释 / 05.细胞比例统计 / 06.组间差异富集 |
| 3 | 00 样本基本信息 | 文字 + 样本统计表 |
| 4–6 | 00 | QC 小提琴图 / 特征图 |
| 7–10 | 01 细胞聚类 | merge_umap / merge_sample_umap / merge_group_umap / group_umap |
| 11–13 | 02 分群marker | top10 热图 / featureplot / violin |
| 14–15 | 03 细胞注释 | 注释 UMAP / marker dotplot |
| 16–18 | 04 细胞比例统计 | 3 张柱状图 |
| 19–21 | 05 组间差异富集 | 火山图 / GO 气泡图 / KEGG 气泡图 |
| 22 | 尾页 | 空 |

每页版式：左上角章节编号 + 章节标题；右侧放图；下方说明文案，句式统一为
「右图为……。该分析结果所在目录：*xxx/」。

## 输出

- `<out>`：生成的 `.pptx`
- 可选 `--report`：本次生成报告（含每页取到的图、跳过页及原因）

## 命名规则

默认输出 `analysis_report_<YYYYMMDD>.pptx`；需要指定路径时用 `--out`。

## 铁律

1. **缺图不许静默留空。** 默认 `--on-missing error` 直接报错退出；
   只有显式加 `--on-missing skip` 才允许跳过，且**必须写入报告，逐页列出跳过原因**。
2. **不改模板版式。** 尺寸、字体、字号、配色一律沿用母版，不做"顺手美化"。
3. **页序固定。** 22 页的顺序与章节编号来自 `slide_spec.json`，改页序要改配置并在报告中记录。
4. **可重复。** 同一批结果连跑两次，产出的页数与每页取图应完全一致。

## 用法示例

```bash
# 先看计划，不写文件
python3 scripts/make_ppt.py \
  --result-dir /path/to/results \
  --template /path/to/analysis_report_20260122.pptx \
  --dry-run

# 正式生成
python3 scripts/make_ppt.py \
  --result-dir /path/to/results \
  --template /path/to/analysis_report_20260122.pptx \
  --out analysis_report_20261011.pptx \
  --date 2026-10-11 \
  --report ppt_report.md
```

## 边界

- 本 skill 只做「结果 → PPT」的组装，**不做分析、不改数据、不改图**。
- 图必须由 `sc-plot-kit` 先产出；本 skill 只负责取图和排版。
- 不联网、不安装依赖、不改用户原始文件。
- 模板文件由用户提供，本 skill 只读不写。

---
name: sc-report-extract
description: 单细胞分析报告的关键信息提取器。从一个分好的 Seurat 对象和一批结果表里，按交付报告(PPT)的口径自动提取要写进报告的关键信息：样本数与细胞数、基因中位数与线粒体中位数、质控阈值与过滤统计、各细胞类型的数量与占比、组间差异与富集要点，并生成可直接放进报告的段落文字与表格。当用户要求"从结果里提取关键信息""生成报告文字""这部分该怎么写进报告""汇总这批单细胞结果"时使用。关键词：单细胞、报告、提取、汇总、关键指标、细胞数、质控统计、比例、报告文字、摘要、extract、summary。
invocable: user
argument-hint: "<seurat.rds> --outdir <目录> [--celltype-col annot] [--group-col group] [--raw-table <去双表>] [--stats-json <文件名>]"
---

# 单细胞报告信息提取器（sc-report-extract）

把「跑完的分析结果」变成「能写进报告的数字与文字」。提取口径对齐客户交付 PPT 的写法。

## 什么时候用它

- 分析跑完了，要把结果汇总成报告
- 用户问「这批数据一共多少细胞」「质控过滤了多少」「各类型占比多少」
- 需要按固定口径生成报告段落，避免每次手写
- **PPT 生成前**：产出 `report_stats.json`，供 sc-ppt-kit 填第 3 页的六行指标

## 输入

| 项 | 说明 |
|---|---|
| Seurat 对象 | 已注释的 rds（含 `orig.ident`、`group`、`annot`） |
| 去双表 | 可选，`1.doublet/sample_raw_cellnumber.xls`（含 sample / raw_number） |
| 差异表 | 可选，各细胞类型的 diffgene 表 |

## 提取什么（六段口径，与 PPT 一致）

### 1. 样本与细胞总览（对应 PPT 样本基本信息段）

必须提取：

| 指标 | 算法 |
|---|---|
| 分析样本数 | `length(unique(obj$orig.ident))` |
| 总细胞数（原始） | 各样本 raw 之和；无 raw 时用对象细胞数 |
| 总细胞数（高质量） | `ncol(obj)` |
| 基因中位数 | `median(obj$nFeature_RNA)`，取整 |
| 线粒体中位数 | `median(obj$percent.mt)`，保留两位小数 |
| 质控阈值 | 从分析记录读取（如 nFeature 300–5000、percent.mt < 10） |

生成文字模板（照 PPT 句式）：

> 本次分析样本数共 N 个，总共获得细胞数 M 个，所有样本基因中位数为 X；线粒体中位数为 Y。细胞过滤阈值：nFeature: A–B & percent.mt < C。

### 2. 质控统计表（对应 PPT 第三页那张表）

列为：样本名 / 原始细胞数 / 高质量细胞数 / 过滤掉细胞数 / 过滤比率(%) / 基因中位数 / 线粒体中位数(%)

过滤比率 = 100 × 过滤掉 / 原始，保留两位小数。

### 3. 细胞类型构成（对应 PPT 细胞注释与比例段）

按细胞类型统计：细胞数、占比(%)、该类型在两组中的占比与倍数变化。

### 4. 分群结果（对应 PPT 细胞聚类段）

群数、每群细胞数、每群对应的细胞类型。

### 5. 差异与富集要点（对应 PPT 组间差异富集段）

每类细胞的显著差异基因数、上调最多的几个基因、富集到的主要通路。

### 6. 图表清单

把产出的图按六段编号列成清单，方便核对报告有没有缺图。

## 输出

- `report_key_info.tsv`：上面各段的关键指标，一行一项
- `report_text.md`：可直接粘进报告的文字段落与表格
- `qc_stat.xls` / `celltype_composition.xls` / `cluster_stat.xls`：明细表
- **`report_stats.json`**：机读版，供 sc-ppt-kit 填 PPT 表格。结构：
  - `n_samples` / `total_cells` / `gene_median` / `mt_median`
  - `table_rows`：宽表，首行 `["sample", 样本1, 样本2, ...]`，之后 `[指标名, 值1, 值2, ...]`
  - 指标名必须与 `slide_spec.json` 的 `table_spec.metric_rows[].keys` 对得上

## 怎么跑

```bash
Rscript scripts/extract.R <seurat.rds> --outdir <目录> \
  --celltype-col annot --group-col group \
  --raw-table <1.doublet/sample_raw_cellnumber.xls> \
  --stats-json report_stats.json
```

## 边界

- 只做提取与汇总，不改数据、不做注释判断
- 若对象里缺某列（如没有 group），对应段落跳过并注明，不编造
- 数字一律来自实际对象或结果表，不估算
- 没有 `--raw-table` 时不写「原始细胞数 / 过滤掉细胞数 / 过滤比率」三行 —— 缺行是少一行，写错数是错一整页
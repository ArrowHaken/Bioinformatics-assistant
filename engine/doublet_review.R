# Verify audit evidence from the unchanged DoubletFinder algorithm. This is a
# provenance/count-consistency check, not a biological appropriateness threshold.
doublet_review <- function(facts) {
  abort <- function(status, detail) {
    stop(structure(list(message=detail,call=NULL,status=status),
                   class=c('doublet_review_check','error','condition')))
  }
  need <- function(ok, detail) if (!isTRUE(ok)) abort('unknown',detail)
  require_ok <- function(ok, detail) if (!isTRUE(ok)) abort('fail',detail)
  scalar_text <- function(x) is.character(x) && length(x)==1L && !is.na(x) && nzchar(x)
  num <- function(x, label, integer=FALSE) {
    need(is.numeric(x) && length(x)==1L && !is.na(x) && is.finite(x),paste0(label,' 缺少有限数值'))
    require_ok(!integer || (x>=0 && x==floor(x)),paste0(label,' 必须是非负整数'))
    as.numeric(x)
  }
  same_num <- function(a,b,label) require_ok(identical(as.numeric(a),as.numeric(b)),paste0(label,' 不一致'))
  close_num <- function(a,b,label) require_ok(abs(a-b)<=1e-12,paste0(label,' 不一致'))
  strings <- function(x,label) {
    v <- unlist(x,use.names=FALSE)
    if (!length(v)) return(character())
    need(is.character(v) && !anyNA(v) && all(nzchar(v)),paste0(label,' 缺少有效字符串'))
    v
  }
  tryCatch({
    ev <- facts$doublet_evidence
    need(is.list(ev), '缺少逐样本双细胞审计证据；不能从总细胞损失推算双细胞数')
    need(identical(ev$schema_version,'doublet-evidence/v1'), '双细胞证据版本缺失或不支持')
    if (identical(ev$status,'error')) abort('fail','执行端读取双细胞证据时发生错误')
    need(identical(ev$status,'complete') && !length(ev$errors), '双细胞审计证据尚未完整')
    need(is.list(ev$identity), '缺少本次任务身份')
    for (key in c('session_id','exec_id','req_id','run_dir')) {
      need(scalar_text(facts[[key]]) && scalar_text(ev$identity[[key]]), paste0('缺少 ',key))
      require_ok(identical(facts[[key]],ev$identity[[key]]), paste0('审计证据跨任务：',key))
    }
    artifacts <- facts$artifacts
    need(is.list(artifacts) && length(artifacts)>0L,'缺少本次产物清单')
    rels <- vapply(artifacts,function(a) if(scalar_text(a$rel)) a$rel else '', '')
    require_ok(all(nzchar(rels)) && !anyDuplicated(rels),'产物路径为空或重复')
    file_check <- function(f, expected) {
      need(is.list(f) && scalar_text(f$rel),'缺少审计文件路径')
      require_ok(identical(f$rel,expected),'审计文件不属于本样本的固定产物路径')
      require_ok(!grepl('(^/|(^|/)\\.\\.(/|$)|\\\\)',f$rel),'审计文件路径越界')
      need(scalar_text(f$md5) && grepl('^[a-fA-F0-9]{32}$',f$md5),'审计文件缺少MD5')
      need(scalar_text(f$sha256) && grepl('^[a-fA-F0-9]{64}$',f$sha256),'审计文件缺少SHA256')
      size <- num(f$bytes,paste0(expected,' bytes'),TRUE)
      require_ok(size>0,'审计文件为空')
      i <- which(rels==expected)
      need(length(i)==1L,paste0('产物清单缺少 ',expected))
      art <- artifacts[[i]]
      need(scalar_text(art$md5),'产物清单缺少MD5')
      require_ok(identical(tolower(f$md5),tolower(art$md5)),paste0(expected,' 指纹与产物清单不一致'))
      same_num(size,num(art$bytes,paste0(expected,' artifact bytes'),TRUE),paste0(expected,' 文件大小'))
      invisible(TRUE)
    }
    file_check(ev$manifest,'1.doublet/audit/manifest.json')
    file_check(ev$summary,'1.doublet/audit/summary.json')
    samples <- strings(facts$declared_samples,'应用声明样本')
    need(length(samples)>0L,'缺少应用声明的样本清单')
    require_ok(!anyDuplicated(samples),'应用样本清单重复')
    need(is.list(ev$samples) && length(ev$samples)>0L,'缺少逐样本证据')
    names_ev <- vapply(ev$samples,function(s) if(scalar_text(s$sample)) s$sample else '', '')
    require_ok(length(names_ev)==length(samples) && !anyDuplicated(names_ev) && setequal(names_ev,samples),
               '双细胞证据样本清单缺失、多余或重复')
    raw_rel <- '1.doublet/sample_raw_cellnumber.xls'
    raw <- facts$tables[[raw_rel]]
    need(is.list(raw) && is.list(raw$data) && length(raw$data)>0,'缺少原始细胞统计表逐样本内容')
    raw_art <- which(rels==raw_rel)
    need(length(raw_art)==1L && scalar_text(raw$md5),'缺少原始细胞统计表指纹')
    require_ok(identical(tolower(raw$md5),tolower(artifacts[[raw_art]]$md5)), '原始细胞统计表指纹不一致')
    raw_samples <- vapply(raw$data,function(r) if(scalar_text(r$sample)) r$sample else '', '')
    require_ok(length(raw_samples)==length(samples) && !anyDuplicated(raw_samples) && setequal(raw_samples,samples),
               '原始统计表与审计样本清单不一致')
    total_input <- 0; total_final <- 0; total_doublet <- 0; total_qc <- 0
    for (sample in samples) {
      s <- ev$samples[[match(sample,names_ev)]]
      d <- s$declared; o <- s$observed
      need(is.list(d) && is.list(o),paste0(sample,' 缺少声明或独立观测'))
      for (k in c('cells','singlet_qc','meta','final_rds','pk_sweep','pre_qc_counts')) {
        path <- switch(k,cells=paste0('1.doublet/audit/',sample,'.cells.tsv'),
                       singlet_qc=paste0('1.doublet/audit/',sample,'.singlet_qc.tsv'),
                       meta=paste0('1.doublet/audit/',sample,'.meta.rds'),
                       final_rds=paste0('1.doublet/',sample,'.rds'),
                       pk_sweep=paste0('1.doublet/audit/',sample,'.pk_sweep.tsv'),
                       pre_qc_counts=paste0('1.doublet/audit/',sample,'.pre_qc_counts.rds'))
        file_check(s$files[[k]],path)
      }
      integer_fields <- c('input_cells','nExp_raw','nExp_adjusted','doublet_called',
                          'singlet_called','after_doublet','after_qc')
      for (k in integer_fields) {
        dv <- num(d[[k]],paste(sample,k,'declared'),TRUE)
        ov <- num(o[[k]],paste(sample,k,'observed'),TRUE)
        same_num(dv,ov,paste(sample,k))
      }
      n <- num(o$input_cells,paste(sample,'input_cells'),TRUE)
      require_ok(n>0,paste0(sample,' 输入细胞数必须大于0'))
      raw_count <- raw$data[[match(sample,raw_samples)]]$raw_number
      if (is.character(raw_count) && length(raw_count)==1L && grepl('^[0-9]+$',raw_count))
        raw_count <- as.numeric(raw_count)
      same_num(num(raw_count,paste(sample,'原始统计表计数'),TRUE),n,paste(sample,'原始统计表输入数量'))
      cc <- o$cluster_counts
      need(is.list(cc) && length(cc)>0,paste0(sample,' 缺少原始聚类频数'))
      cn <- vapply(cc,function(r) if(scalar_text(r$cluster)) r$cluster else '', '')
      require_ok(all(nzchar(cn)) && !anyDuplicated(cn),paste0(sample,' 簇标签为空或重复'))
      counts <- vapply(cc,function(r) num(r$cells,paste(sample,'cluster cells'),TRUE),0)
      require_ok(all(counts>0),paste0(sample,' 簇频数必须大于0'))
      same_num(sum(counts),n,paste(sample,'簇频数总和'))
      require_ok(identical(o$cluster_counts_source,'meta.rds') &&
                 identical(o$classification_counts_source,'meta.rds'),paste0(sample,' 计数来源不是原始元数据'))
      hp <- sum((counts/n)^2)
      close_num(num(d$homotypic_prop,paste(sample,'homotypic declared')),hp,paste(sample,'同型比例声明'))
      close_num(num(o$homotypic_prop,paste(sample,'homotypic observed')),hp,paste(sample,'同型比例观测'))
      rate <- n*8*1e-6
      expected_raw <- round(rate*n)
      expected_adj <- round(expected_raw*(1-hp))
      close_num(num(d$doublet_rate,paste(sample,'doublet_rate')),rate,paste(sample,'原算法预期率'))
      same_num(d$nExp_raw,expected_raw,paste(sample,'原始预期双细胞数'))
      same_num(d$nExp_adjusted,expected_adj,paste(sample,'校正预期双细胞数'))
      used <- num(d$nExp_used,paste(sample,'nExp_used'),TRUE)
      same_num(used,expected_adj,paste(sample,'实际传入nExp'))
      require_ok(0<=hp && hp<=1 && expected_adj<=expected_raw && expected_raw<=n,
                 paste0(sample,' 预期数量或同型比例超出数学有效范围'))
      same_num(o$doublet_called,used,paste(sample,'实际分类双细胞数与nExp'))
      same_num(o$doublet_called+o$singlet_called,n,paste(sample,'双细胞/单细胞守恒'))
      same_num(o$after_doublet,o$singlet_called,paste(sample,'去双后细胞数'))
      require_ok(o$after_qc<=o$after_doublet,paste0(sample,' 质控后细胞数增加'))
      require_ok(all(strings(o$classification_values,'分类值') %in% c('Singlet','Doublet')) &&
                 length(strings(o$classification_values,'分类值'))>0L,paste0(sample,' 非法或空分类'))
      for (key in c('meta_cells_match_tsv','meta_labels_match_tsv','meta_clusters_match_tsv',
                    'meta_pann_match_tsv','meta_labels_match_df','pk_matches_sweep',
                    'qc_retained_matches_threshold','final_cells_match_qc',
                    'final_cells_subset_singlets','final_nfeature_match_qc','barcodes_unique')) {
        need(!is.null(o[[key]]),paste(sample,'缺少独立核对',key))
        require_ok(isTRUE(o[[key]]),paste(sample,'独立核对失败',key))
      }
      barcodes <- strings(o$rds_barcodes,paste(sample,'最终RDS细胞清单'))
      require_ok(!anyDuplicated(barcodes),paste0(sample,' 最终对象细胞名重复'))
      same_num(length(barcodes),o$after_qc,paste(sample,'最终对象细胞清单长度'))
      fixed <- c(pN=.25,PCs=20,dims=20,resolution=.5,nfeatures=2000,min_cells=3,min_features=200)
      for (key in names(fixed)) same_num(num(d[[key]],paste(sample,key)),fixed[[key]],paste(sample,'原参数',key))
      require_ok(isTRUE(d$sct),paste0(sample,' sct不符合原脚本设置'))
      pk <- num(d$pK,paste(sample,'pK'))
      require_ok(pk>0 && pk<1,paste0(sample,' pK超出数学有效范围'))
      need(scalar_text(d$classification_column),paste0(sample,' 缺少原始DF分类列名'))
      total_input <- total_input+n; total_final <- total_final+o$after_qc
      total_doublet <- total_doublet+o$doublet_called
      total_qc <- total_qc+o$after_doublet-o$after_qc
    }
    same_num(num(facts$counts$cells_before,'counts.cells_before',TRUE),total_input,'输入细胞总数')
    same_num(num(facts$counts$cells_after,'counts.cells_after',TRUE),total_final,'最终细胞总数')
    same_num(num(facts$counts$n_samples,'counts.n_samples',TRUE),length(samples),'样本总数')
    list(status='pass',detail=paste0(length(samples),' 个样本分类、预期双细胞数、同型校正和最终对象逐项一致；',
       '双细胞 ',total_doublet,'，后续质控另滤 ',total_qc,'。本检查不评价生物学阈值是否适宜。'),
       counts=list(input=total_input,doublets=total_doublet,qc_removed=total_qc,final=total_final))
  },doublet_review_check=function(e) list(status=e$status,detail=conditionMessage(e)),
    error=function(e) list(status='unknown',detail=paste0('双细胞审计证据无法读取：',conditionMessage(e))))
}

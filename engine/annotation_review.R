# Technical consistency only: does not choose cell types or change score thresholds.
# reference_baseline must come from the application-selected reference, never callback data.
annotation_review <- function(facts, reference_baseline = NULL) {
  reject <- function(status, detail) {
    stop(structure(list(message=detail, call=NULL, status=status),
                   class=c('annotation_check','error','condition')))
  }
  scalar <- function(x, label, required=TRUE) {
    if (is.null(x) || length(x)==0L || (length(x)==1L && is.na(x))) {
      if (required) reject('unknown', paste0('缺少 ', label))
      return('')
    }
    if (length(x)!=1L || is.list(x)) reject('fail', paste0(label,' 必须是单值'))
    v <- trimws(as.character(x))
    if (required && !nzchar(v)) reject('unknown',paste0('缺少 ',label))
    v
  }
  number <- function(x,label,positive=FALSE) {
    s <- scalar(x,label)
    n <- suppressWarnings(as.numeric(s))
    if (!is.finite(n) || n<0 || n!=floor(n) || (positive && n==0))
      reject('fail',paste0(label,' 必须是',if(positive)'正' else '非负','整数'))
    n
  }
  digest <- function(x,label) {
    s <- tolower(scalar(x,label))
    if (!grepl('^[0-9a-f]{32}$',s)) reject('fail',paste0(label,' 不是有效MD5'))
    s
  }
  records <- function(x,label) {
    if (is.null(x) || !length(x)) reject('unknown',paste0('缺少 ',label))
    if (!is.list(x) || is.data.frame(x) || any(!vapply(x,is.list,TRUE)))
      reject('fail',paste0(label,' 必须是记录列表'))
    x
  }
  genes <- function(x,label) {
    if (is.null(x)) reject('unknown',paste0('缺少 ',label))
    if (length(x)!=1L || is.na(x) || is.list(x)) reject('fail',paste0(label,' 必须是逗号分隔字符串'))
    v <- trimws(unlist(strsplit(as.character(x),',',fixed=TRUE)))
    v <- toupper(v[nzchar(v)])
    if (anyDuplicated(v)) reject('fail',paste0(label,' 存在重复基因'))
    v
  }
  tryCatch({
    ec <- number(facts$exit_code,'退出码')
    if (ec!=0) reject('fail','注释执行退出码非0')
    e <- facts$annotation_evidence
    if (!is.list(e)) reject('unknown','没有执行端实读的 annotation_evidence')
    for (key in c('session_id','req_id','run_dir')) {
      want <- scalar(facts[[key]],paste0('本次 ',key))
      got <- scalar(e$binding[[key]],paste0('证据 ',key))
      if (!identical(want,got)) reject('fail',paste0('注释证据 ',key,' 与本次不一致'))
    }
    if (scalar(facts$step_id,'本次step')!='annotate' ||
        scalar(e$binding$step,'证据step')!='annotate') reject('fail','注释证据步骤不一致')
    arts <- records(facts$artifacts,'产物清单')
    artifact <- function(part,rel) {
      if (!is.list(part)) reject('unknown',paste0('缺少 ',rel,' 实读证据'))
      if (scalar(part$rel,paste0(rel,'证据路径'))!=rel) reject('fail',paste0(rel,' 路径不符'))
      matches <- Filter(function(a) identical(as.character(a$rel),rel),arts)
      if (!length(matches)) reject('unknown',paste0(rel,' 不在产物清单'))
      if (length(matches)!=1L) reject('fail',paste0(rel,' 产物清单不唯一'))
      if (digest(part$md5,paste0(rel,'证据MD5'))!=digest(matches[[1]]$md5,paste0(rel,'产物MD5')))
        reject('fail',paste0(rel,' 实读证据与产物MD5不一致'))
    }
    artifact(e$result,'9.annot/annot_result.xls')
    artifact(e$counts,'9.annot/celltype_count.xls')
    artifact(e$object,'9.annot/annotated.rds')
    artifact(e$reference,'9.annot/reference_parse.json')
    if (!is.list(reference_baseline)) reject('unknown','没有应用侧已选参考表的可信基线')
    if (scalar(e$reference$source_path,'实际参考路径')!=scalar(reference_baseline$source_path,'已选参考路径'))
      reject('fail','实际参考表路径与本次选择不一致')
    if (digest(e$reference$source_md5,'实际参考MD5')!=digest(reference_baseline$source_md5,'已选参考MD5'))
      reject('fail','实际参考表内容与应用基线不一致')
    dict <- records(e$reference$dictionary,'逐类型参考词典')
    keys <- vapply(dict,function(x) scalar(x$celltype,'词典类型'),'')
    if (anyDuplicated(keys)) reject('fail','参考词典类型键重复')
    markers <- lapply(dict,function(x) {
      if (is.null(x$markers)) reject('unknown','词典缺少markers字段')
      v <- as.character(unlist(x$markers,use.names=FALSE))
      if (anyNA(v)) reject('fail','词典基因中有NA')
      unique(toupper(trimws(v[nzchar(trimws(v))])))
    }); names(markers) <- keys
    clusters <- records(e$object$clusters,'对象逐簇统计')
    ids <- vapply(clusters,function(x) scalar(x$cluster,'对象簇号'),'')
    if (anyDuplicated(ids)) reject('fail','对象簇号重复')
    ns <- vapply(clusters,function(x) number(x$n_cells,'簇细胞数',TRUE),0)
    total <- number(e$object$cells_total,'对象总细胞数',TRUE)
    if (sum(ns)!=total) reject('fail','逐簇细胞数与对象总数不一致')
    obj_types <- vapply(clusters,function(x) {
      if (is.null(x$celltypes) || length(x$celltypes)==0L) reject('unknown','缺少簇的实际annot取值')
      if (length(x$celltypes)!=1L) reject('fail','同一个簇在对象中有多个annot取值')
      scalar(x$celltypes[[1]],'对象annot',FALSE)
    },'')
    rows <- records(e$result$rows,'注释结果逐行证据')
    for (x in rows) if (!all(c('cluster','celltype','n_hit','hit_genes','hit_genes_all','reason') %in% names(x)))
      reject('unknown','注释结果逐行证据缺少实际输出列')
    rids <- vapply(rows,function(x) scalar(x$cluster,'结果簇号'),'')
    if (anyDuplicated(rids) || !setequal(ids,rids)) reject('fail','注释结果必须覆盖对象每个簇且恰好一行')
    rows <- rows[match(ids,rids)]
    types <- vapply(rows,function(x) scalar(x$celltype,'结果celltype',FALSE),'')
    if (!identical(types,obj_types)) reject('fail','结果表类型与对象实际annot不一致')
    for (i in seq_along(rows)) {
      x <- rows[[i]]; ct <- types[i]; cl <- ids[i]
      n <- number(x$n_hit,paste0('簇',cl,' n_hit'))
      full <- genes(x$hit_genes_all,paste0('簇',cl,'完整命中'))
      shown <- genes(x$hit_genes,paste0('簇',cl,'展示命中'))
      if (length(full)!=n || !identical(shown,head(full,8L)))
        reject('fail',paste0('簇',cl,' 命中计数/完整列表/前8展示不一致'))
      if (!nzchar(ct)) {
        scalar(x$reason,paste0('未定型簇',cl,'原因'))
        next
      }
      if (!ct %in% keys) reject('fail',paste0('簇',cl,' 类型不在参考表中：',ct))
      if (!n || any(!full %in% markers[[ct]]))
        reject('fail',paste0('簇',cl,' 命中基因不属于所报类型 ',ct,' 的阳性词典'))
    }
    counts <- records(e$counts$rows,'细胞类型计数表')
    labels <- vapply(counts,function(x) scalar(x$celltype,'计数表类型'),'')
    if (anyDuplicated(labels)) reject('fail','细胞类型计数表类型重复')
    values <- vapply(counts,function(x) number(x$n_cells,'类型细胞数'),0)
    typed <- vapply(counts,function(x) scalar(x$typed,'typed标记'),'')
    expected_labels <- ifelse(nzchar(types),types,'unassigned')
    expected <- tapply(ns,expected_labels,sum)
    if (!setequal(labels,names(expected)) || sum(values)!=total ||
        any(values!=unname(expected[labels]))) reject('fail','类型计数表与对象逐簇细胞数不守恒')
    if (any(typed!=ifelse(labels=='unassigned','no','yes'))) reject('fail','计数表typed标记与类型不一致')
    list(status='pass',detail=paste0(length(ids),'个簇逐行齐全；',sum(nzchar(types)),
      '个簇有类型，命中按各自词典核对；',total,'个细胞计数守恒（含未定型）'))
  },annotation_check=function(e) list(status=e$status,detail=e$message),
    error=function(e) list(status='unknown',detail=paste0('注释证据结构不可读：',conditionMessage(e))))
}

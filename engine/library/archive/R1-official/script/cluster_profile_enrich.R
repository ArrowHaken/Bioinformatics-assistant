#.libPaths(c('/share/apps/R/R-4.1.2-4/lib64/R/library'))
suppressPackageStartupMessages(library(clusterProfiler))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(org.Hs.eg.db,lib.loc = '/share/apps/R/R-4.1.2-4/lib64/R/library'))
suppressPackageStartupMessages(library(org.Mm.eg.db,lib.loc = '/share/apps/R/R-4.1.2-4/lib64/R/library'))
suppressPackageStartupMessages(library(org.Rn.eg.db,lib.loc = '/share/apps/R/R-4.1.2-4/lib64/R/library'))
#R.utils::setOption("clusterProfiler.download.method","auto")

#targetGain<-read.delim(file="E:/single-cell/du_new/gene_list/clust_0_diff_expres_normal_RSA_gene_symbol",header=T,sep="\t",stringsAsFactors=F)[,1]
#targetKEGG<-read.delim(file="E:/single-cell/du_new/gene_list/clust_0_diff_expres_normal_RSA_KEGG",header=T,sep="\t",stringsAsFactors=F)[,1]

go_enrich_analysis_all<-function(genes,species,outdir,prefix){
  if(species=='Human'){
    #library(org.Hs.eg.db,lib.loc = '/share/apps/R/R-4.2.0/lib64/R/library')
    db='org.Hs.eg.db'
  }else if(species=='Mouse'){
    #library(org.Mm.eg.db,lib.loc = '/share/apps/R/R-4.2.0/lib64/R/library')
    db='org.Mm.eg.db'
  }else if(species=='Rat'){
    #library(org.Rn.eg.db,lib.loc = '/share/apps/R/R-4.2.0/lib64/R/library')
    db='org.Rn.eg.db'
  }
  GainALLGo<-enrichGO(genes, db, keyType ='SYMBOL', ont="ALL", pvalueCutoff=1,qvalueCutoff = 1)
  if(class(GainALLGo)!="NULL"){
  GOenrich <- as.data.frame(GainALLGo)
  #GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
  GOenrich <- data.frame(GOenrich)
  write.table(GOenrich, file=paste0(outdir,"/",prefix,"_GO.ALL.txt"), sep="\t", quote=F, row.names=F)
  all_bar<-barplot(GainALLGo, showCategory=15, split="ONTOLOGY",label_format=100)+ 
               theme(axis.text.x = element_text(size=14,colour="black"),
          axis.text.y = element_text(size=14,colour="black"),
          axis.title.x.bottom = element_text(size=14,colour="black"))+
          facet_grid(ONTOLOGY~., scale='free')
  ggsave(all_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentALL.bar.png"),dpi=400, width = 18, height = 20)
  ggsave(all_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentALL.bar.pdf"),width = 18, height = 20)
}}

go_enrich_analysis_all_dot<-function(genes,species,outdir,prefix){
  if(species=='Human'){
    #library(org.Hs.eg.db,lib.loc = '/share/apps/R/R-4.2.0/lib64/R/library')
    db='org.Hs.eg.db'
  }else if(species=='Mouse'){
    #library(org.Mm.eg.db,lib.loc = '/share/apps/R/R-4.2.0/lib64/R/library')
    db='org.Mm.eg.db'
  }else if(species=='Rat'){
    #library(org.Rn.eg.db,lib.loc = '/share/apps/R/R-4.2.0/lib64/R/library')
    db='org.Rn.eg.db'
  }
  GainALLGo<-enrichGO(genes, db, keyType ='SYMBOL', ont="ALL", pvalueCutoff=1,qvalueCutoff = 1)
  if(class(GainALLGo)!="NULL"){
  GOenrich <- as.data.frame(GainALLGo)
  #GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
  GOenrich <- data.frame(GOenrich)
  write.table(GOenrich, file=paste0(outdir,"/",prefix,"_GO.ALL.txt"), sep="\t", quote=F, row.names=F)
  all_bar<-dotplot(GainALLGo, showCategory=15, split="ONTOLOGY",label_format=100)+
               theme(axis.text.x = element_text(size=14,colour="black"),
          axis.text.y = element_text(size=14,colour="black"),
          axis.title.x.bottom = element_text(size=14,colour="black"))+
          facet_grid(ONTOLOGY~., scale='free')
  ggsave(all_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentALL.dot.png"),dpi=400, width = 18, height = 20)
  ggsave(all_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentALL.dot.pdf"),width = 18, height = 20)
}}

go_enrich_analysis<-function(genes,species,outdir,prefix){
  if(species=='Human'){
    #library(org.Hs.eg.db,lib.loc = '/share/apps/R/R-4.2.0/lib64/R/library')
    db='org.Hs.eg.db'
  }else if(species=='Mouse'){
    #library(org.Mm.eg.db,lib.loc = '/share/apps/R/R-4.2.0/lib64/R/library')
    db='org.Mm.eg.db'
  }else if(species=='Rat'){
    #library(org.Rn.eg.db,lib.loc = '/share/apps/R/R-4.2.0/lib64/R/library')
    db='org.Rn.eg.db'
  }
  #BP
  GainBPGo<-enrichGO(genes, db, keyType ='SYMBOL', ont="BP", pvalueCutoff=1,qvalueCutoff = 1)
  if(class(GainBPGo)!="NULL"){
    GOenrich <- as.data.frame(GainBPGo)
    GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
    write.table(GOenrich, file=paste0(outdir,"/",prefix,"_GO.BP.txt"), sep="\t", quote=F, row.names=F)
    bp_bar<-barplot(GainBPGo, showCategory=30,label_format=100)+
    theme(axis.text.x = element_text(size=14,colour="black"),
          axis.text.y = element_text(size=14,colour="black"),
          axis.title.x.bottom = element_text(size=14,colour="black"))
    ggsave(bp_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentBP.bar.png"),dpi=400, width = 15, height = 15)
    ggsave(bp_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentBP.bar.pdf"),width = 15, height = 15)
    #pdf(paste(outdir,"/",prefix,".GObp_DAG.pdf",sep=""))
    #plotGOgraph(GainBPGo,firstSigNodes = 5)
    #dev.off()
    #png(paste(outdir,"/",prefix,".GObp_DAG.png",sep=""),type="cairo-png",width=480*4,height=480*4,res=72*4)
    #plotGOgraph(GainBPGo,firstSigNodes = 5)
    #dev.off()
  }
  #MF
  GainMFGo<-enrichGO(genes, db, keyType ='SYMBOL', ont="MF", pvalueCutoff=1,qvalueCutoff = 1)
  if(class(GainMFGo)!="NULL"){
    GOenrich <- as.data.frame(GainMFGo)
    GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
    write.table(GOenrich, file=paste0(outdir,"/",prefix,"_GO.MF.txt"), sep="\t", quote=F, row.names=F)
    mf_bar<-barplot(GainMFGo, showCategory=30,label_format=100)+
    theme(axis.text.x = element_text(size=14,colour="black"),
          axis.text.y = element_text(size=14,colour="black"),
          axis.title.x.bottom = element_text(size=14,colour="black"))
    ggsave(mf_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentMF.bar.png"),dpi=400, width = 15, height = 15)
    ggsave(mf_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentMF.bar.pdf"),width = 15, height = 15)
    #pdf(paste(outdir,"/",prefix,".GOmf_DAG.pdf",sep=""))
    #plotGOgraph(GainMFGo,firstSigNodes = 5)
    #dev.off()
    #png(paste(outdir,"/",prefix,".GOmf_DAG.png",sep=""),type="cairo-png",width=480*4,height=480*4,res=72*4)
    #plotGOgraph(GainMFGo,firstSigNodes = 5)
    #dev.off()
  }
  #CC
  GainCCGo<-enrichGO(genes, db, keyType ='SYMBOL', ont="CC", pvalueCutoff=1,qvalueCutoff = 1)
  if(class(GainCCGo)!="NULL"){
    GOenrich <- as.data.frame(GainCCGo)
    GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
    write.table(GOenrich, file=paste0(outdir,"/",prefix,"_GO.CC.txt"), sep="\t", quote=F, row.names=F)
    cc_bar<-barplot(GainCCGo, showCategory=30,label_format=100)+
    theme(axis.text.x = element_text(size=14,colour="black"),
          axis.text.y = element_text(size=14,colour="black"),
          axis.title.x.bottom = element_text(size=14,colour="black"))
    ggsave(cc_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentCC.bar.png"),dpi=400, width = 15, height = 15)
    ggsave(cc_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentCC.bar.pdf"),width = 15, height = 15)
    #pdf(paste(outdir,"/",prefix,".GOcc_DAG.pdf",sep=""))
    #plotGOgraph(GainCCGo,firstSigNodes = 5)
    #dev.off()
    #png(paste(outdir,"/",prefix,".GOcc_DAG.png",sep=""),type="cairo-png",width=480*4,height=480*4,res=72*4)
    #plotGOgraph(GainCCGo,firstSigNodes = 5)
    #dev.off()
  }
}

kegg_enrich_analysis<-function(genes,species,outdir,prefix){
  if(species=='Human'){
    entrezIDs <- unlist(mget(genes, org.Hs.egSYMBOL2EG, ifnotfound=NA))
    enrich_kegg<-enrichKEGG(gene = entrezIDs, organism = "hsa", keyType = "kegg", pvalueCutoff = 1, pAdjustMethod = "BH", universe = keys(org.Hs.eg.db), minGSSize = 10, maxGSSize = 500, qvalueCutoff = 1, use_internal_data = T)
  }else if(species=='Mouse'){
    entrezIDs <- unlist(mget(genes, org.Mm.egSYMBOL2EG, ifnotfound=NA))
    enrich_kegg<-enrichKEGG(gene = entrezIDs, organism = "mmu", keyType = "kegg", pvalueCutoff = 1, pAdjustMethod = "BH", universe = keys(org.Mm.eg.db), minGSSize = 10, maxGSSize = 500, qvalueCutoff = 1, use_internal_data = T)
  }else if(species=='Rat'){
    entrezIDs <- unlist(mget(genes, org.Rn.egSYMBOL2EG, ifnotfound=NA))
    enrich_kegg<-enrichKEGG(gene = entrezIDs, organism = "rno", keyType = "kegg", pvalueCutoff = 1, pAdjustMethod = "BH", universe = keys(org.Rn.eg.db), minGSSize = 10, maxGSSize = 500, qvalueCutoff = 1, use_internal_data = T)
  }
  if(class(enrich_kegg)!="NULL"){
    gene_list <- strsplit(enrich_kegg$geneID,split='/')
    geneName <- unlist(lapply(gene_list,FUN=function(x){paste(names(entrezIDs[match(x,entrezIDs)]),collapse='/')}))
    KEGGenrich <- as.data.frame(enrich_kegg)
    KEGGenrich <- data.frame(KEGGenrich[,1:6],KEGGenrich[8],geneName,KEGGenrich[9])
    write.table(KEGGenrich ,file=paste0(outdir,"/",prefix,"_KEGG.txt"),sep="\t",quote=F,row.names = F)
    kegg_bar<-barplot(enrich_kegg, showCategory=30,label_format=100)+
    theme(axis.text.x = element_text(size=14,colour="black"),
          axis.text.y = element_text(size=14,colour="black"),
          axis.title.x.bottom = element_text(size=14,colour="black"))
    ggsave(kegg_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentKEGG.bar.png"),dpi=400, width = 15, height = 15)
    ggsave(kegg_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentKEGG.bar.pdf"),width = 15, height = 15)}
}

kegg_enrich_analysis_dot<-function(genes,species,outdir,prefix){
  if(species=='Human'){
    entrezIDs <- unlist(mget(genes, org.Hs.egSYMBOL2EG, ifnotfound=NA))
    enrich_kegg<-enrichKEGG(gene = entrezIDs, organism = "hsa", keyType = "kegg", pvalueCutoff = 1, pAdjustMethod = "BH", universe = keys(org.Hs.eg.db), minGSSize = 10, maxGSSize = 500, qvalueCutoff = 1, use_internal_data = T)
  }else if(species=='Mouse'){
    entrezIDs <- unlist(mget(genes, org.Mm.egSYMBOL2EG, ifnotfound=NA))
    enrich_kegg<-enrichKEGG(gene = entrezIDs, organism = "mmu", keyType = "kegg", pvalueCutoff = 1, pAdjustMethod = "BH", universe = keys(org.Mm.eg.db), minGSSize = 10, maxGSSize = 500, qvalueCutoff = 1, use_internal_data = T)
  }else if(species=='Rat'){
    entrezIDs <- unlist(mget(genes, org.Rn.egSYMBOL2EG, ifnotfound=NA))
    enrich_kegg<-enrichKEGG(gene = entrezIDs, organism = "rno", keyType = "kegg", pvalueCutoff = 1, pAdjustMethod = "BH", universe = keys(org.Rn.eg.db), minGSSize = 10, maxGSSize = 500, qvalueCutoff = 1, use_internal_data = T)
  }
  if(class(enrich_kegg)!="NULL"){
    gene_list <- strsplit(enrich_kegg$geneID,split='/')
    geneName <- unlist(lapply(gene_list,FUN=function(x){paste(names(entrezIDs[match(x,entrezIDs)]),collapse='/')}))
    KEGGenrich <- as.data.frame(enrich_kegg)
    KEGGenrich <- data.frame(KEGGenrich[,1:6],KEGGenrich[8],geneName,KEGGenrich[9])
    write.table(KEGGenrich ,file=paste0(outdir,"/",prefix,"_KEGG.txt"),sep="\t",quote=F,row.names = F)
    kegg_bar<-dotplot(enrich_kegg, showCategory=30,label_format=100)+
    theme(axis.text.x = element_text(size=14,colour="black"),
          axis.text.y = element_text(size=14,colour="black"),
          axis.title.x.bottom = element_text(size=14,colour="black"))
    ggsave(kegg_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentKEGG.dot.png"),dpi=400, width = 15, height = 15)
    ggsave(kegg_bar, filename = paste0(outdir,"/",prefix,"_EnrichmentKEGG.dot.pdf"),width = 15, height = 15)}
}


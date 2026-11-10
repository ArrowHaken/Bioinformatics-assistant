#!/usr/bin/Rscript
suppressPackageStartupMessages(library(clusterProfiler))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(org.Hs.eg.db,lib.loc = '/share/apps/R/R-4.1.2-4/lib64/R/library'))
suppressPackageStartupMessages(library(org.Mm.eg.db,lib.loc = '/share/apps/R/R-4.1.2-4/lib64/R/library'))
suppressPackageStartupMessages(library(org.Rn.eg.db,lib.loc = '/share/apps/R/R-4.1.2-4/lib64/R/library'))

file<-read.delim(file="./cluster0_sig_markgene.xls",header=T,sep="\t",stringsAsFactors=F)
genes<-file$gene
GainBPGo<-enrichGO(genes, 'org.Mm.eg.db', keyType ='SYMBOL', ont="BP", pvalueCutoff=1,qvalueCutoff = 1)
GOenrich <- as.data.frame(GainBPGo)
GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
write.table(GOenrich, file="./cluster0_GO.BP.txt", sep="\t", quote=F, row.names=F)
if(class(GainBPGo)!="NULL"){
bp_bar<-barplot(GainBPGo, showCategory=30,label_format=100)+
    theme(axis.text.x = element_text(size=14,colour="black"),
          axis.text.y = element_text(size=14,colour="black"),
          axis.title.x.bottom = element_text(size=14,colour="black"))
ggsave(bp_bar, filename = "./cluster0_EnrichmentBP.bar.png",dpi=400, width = 15, height = 10)
ggsave(bp_bar, filename = "./cluster0_EnrichmentBP.bar.pdf",width = 15, height = 10)
}

GainMFGo<-enrichGO(genes, 'org.Mm.eg.db', keyType ='SYMBOL', ont="MF", pvalueCutoff=1,qvalueCutoff = 1)
GOenrich <- as.data.frame(GainMFGo)
GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
write.table(GOenrich, file="./cluster0_GO.MF.txt", sep="\t", quote=F, row.names=F)
if(class(GainMFGo)!="NULL"){
mf_bar<-barplot(GainMFGo, showCategory=30,label_format=100)+
    theme(axis.text.x = element_text(size=14,colour="black"),
          axis.text.y = element_text(size=14,colour="black"),
          axis.title.x.bottom = element_text(size=14,colour="black"))
ggsave(mf_bar, filename = "./cluster0_EnrichmentMF.bar.png",dpi=400, width = 15, height = 10)
ggsave(mf_bar, filename = "./cluster0_EnrichmentMF.bar.pdf",width = 15, height = 10)
}

GainCCGo<-enrichGO(genes, 'org.Mm.eg.db', keyType ='SYMBOL', ont="CC", pvalueCutoff=1,qvalueCutoff = 1)
GOenrich <- as.data.frame(GainCCGo)
GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
write.table(GOenrich, file="./cluster0_GO.CC.txt", sep="\t", quote=F, row.names=F)
if(class(GainCCGo)!="NULL"){
cc_bar<-barplot(GainCCGo, showCategory=30,label_format=100)+
    theme(axis.text.x = element_text(size=14,colour="black"),
          axis.text.y = element_text(size=14,colour="black"),
          axis.title.x.bottom = element_text(size=14,colour="black"))
ggsave(cc_bar, filename = "./cluster0_EnrichmentCC.bar.png",dpi=400, width = 15, height = 10)
ggsave(cc_bar, filename = "./cluster0_EnrichmentCC.bar.pdf",width = 15, height = 10)
}

#GainALLGo<-enrichGO(genes, 'org.Mm.eg.db', keyType ='SYMBOL', ont="ALL", pvalueCutoff=1,qvalueCutoff = 1)
#if(class(GainALLGo)!="NULL"){
#all_bar<-barplot(GainALLGo, showCategory=10, split="ONTOLOGY")+ facet_grid(ONTOLOGY~., scale='free')
#ggsave(all_bar, filename = "./cluster0_EnrichmentALL.bar.png",dpi=400, width = 15, height = 10)
#ggsave(all_bar, filename = "./cluster0_EnrichmentALL.bar.pdf",width = 15, height = 10)
#}

#entrezIDs <- unlist(mget(genes, org.Mm.egSYMBOL2EG, ifnotfound=NA))
#enrich_kegg<-enrichKEGG(gene = entrezIDs, organism = "mmu", keyType = "kegg", pvalueCutoff = 1, pAdjustMethod = "BH", universe = keys(org.Mm.eg.db), minGSSize = 10, maxGSSize = 500, qvalueCutoff = 1, use_internal_data = T)
#gene_list <- strsplit(enrich_kegg$geneID,split='/')
#geneName <- unlist(lapply(gene_list,FUN=function(x){paste(names(entrezIDs[match(x,entrezIDs)]),collapse='/')}))
#KEGGenrich <- as.data.frame(enrich_kegg)
#KEGGenrich <- data.frame(KEGGenrich[,1:6],KEGGenrich[8],geneName,KEGGenrich[9])
#write.table(KEGGenrich ,file="./cluster0_KEGG.txt",sep="\t",quote=F,row.names = F)
#kegg_bar<-barplot(enrich_kegg, showCategory=30,label_format=100)+
#    theme(axis.text.x = element_text(size=14,colour="black"),
#          axis.text.y = element_text(size=14,colour="black"),
#          axis.title.x.bottom = element_text(size=14,colour="black"))
#ggsave(kegg_bar, filename = "./cluster0_EnrichmentKEGG.bar.png",dpi=400, width = 15, height = 10)
#ggsave(kegg_bar, filename = "./cluster0_EnrichmentKEGG.bar.pdf",width = 15, height = 10)

.libPaths(c('/share/apps/R/R-4.1.2-4/lib64/R/library/'))
suppressMessages({
library(dplyr)
library(cowplot)
library(ggplot2)
library(Seurat)
library(argparser)
library(stringr)})

argv <- arg_parser('')
argv <- add_argument(argv,"--rds", help="the seurat rds file")
argv <- add_argument(argv,"--group", help="the compare of diff,such as AvsB1:AvsB2,AvsB")
argv <- add_argument(argv,"--group_flag", help="the group flag of seurat,such as group,group2")
argv <- add_argument(argv,"--outdir", help="the output dir")
argv <- add_argument(argv,"--species", help="the species of project")
argv <- add_argument(argv,"--volcano_flag", help="the choose volcane genelist flag,1 or 0, 1:all gene,0:partgene")
argv <- add_argument(argv,"--p_val_adj", help="the value of p_val_adj in volcane")
argv <- add_argument(argv,"--avg_log2FC", help="the value of avg_log2FC in volcane")
argv <- parse_args(argv)

source('/share/home/liny/Project/personality/script/enrich/vR-4.1.2/cluster_profile_enrich.R')
source('/share/home/liny/Project/personality/script/01_packages.R')

rds <- argv$rds
group <- argv$group
group_flag <- argv$group_flag
outdir <- argv$outdir
species <- argv$species
volcano_flag <- argv$volcano_flag
p_val_adj <- argv$p_val_adj
avg_log2FC <- argv$avg_log2FC

if(is.na(p_val_adj)){
  p_val_adj = 0.05
}else{p_val_adj = as.numeric(p_val_adj)}
if(is.na(avg_log2FC)){
  avg_log2FC = 0
}else{avg_log2FC = as.numeric(avg_log2FC)}

group<-str_split(group,pattern = ',')[[1]]
group_flag<-str_split(group_flag,pattern = ',')[[1]]
pancreas.integrated<-readRDS(rds)

DefaultAssay(pancreas.integrated)<-'RNA'
Idents(pancreas.integrated)<-pancreas.integrated@meta.data$annot

if (species %in% c('Human','Mouse','Rat')){
  if(species == 'Human'){
    gene_annot_file<-read.delim(file='/share/home/liny/Script/bulkseq/genome/Animal/Homo_sapiens/GRCh38/GRCh38_gene.txt',header=T,sep='\t')
  }
  if(species == 'Mouse'){
    gene_annot_file<-read.delim(file='/share/home/liny/Script/bulkseq/genome/Animal/Mus_musculus/Mm10/Mm10_gene.txt',header=T,sep='\t')
  }
  if(species == 'Rat'){
    gene_annot_file<-read.delim(file='/share/home/liny/Script/bulkseq/genome/Animal/Rattus_norvegicus/Rn6/Rn6_gene.txt',header=T,sep='\t')
  }
  gene_annot_file<-gene_annot_file[,c('gene_name','gene_description')]
  gene_annot_file<-unique(gene_annot_file)
  uniq_gene_list<-c()
  uniq_gene_annot_file<-data.frame()
  i=1
  for(each in as.character(gene_annot_file$gene_name)){
    if(!(each %in% uniq_gene_list)){
      uniq_gene_annot_file<-rbind(uniq_gene_annot_file,gene_annot_file[i,])
      uniq_gene_list<-c(uniq_gene_list,each)}
    i<-i+1
  }
  rownames(uniq_gene_annot_file)<-uniq_gene_annot_file$gene_name
}
pos = 1
for(each_flag in group_flag){
  each_group<-str_split(group[pos],pattern = ':')[[1]]
  for(each in each_group){
    each_name<-gsub(each,pattern='/| ', replacement='-')
    dir.create(paste0(outdir,'/diff/'))
    dir.create(paste0(outdir,'/diff/',each_name))
    dir.create(paste0(outdir,'/diff/',each_name,'/volcano'))
    dir.create(paste0(outdir,'/enrich/'))
    dir.create(paste0(outdir,'/enrich/',each_name))
    dir.create(paste0(outdir,'/enrich/',each_name,'/ALL'))
    dir.create(paste0(outdir,'/enrich/',each_name,'/UP'))
    dir.create(paste0(outdir,'/enrich/',each_name,'/DOWN'))
    diff1 <- str_split(each,pattern = 'vs')[[1]][1]
    diff2 <- str_split(each,pattern = 'vs')[[1]][2]
    for (i in as.vector(levels(pancreas.integrated@meta.data$annot))) {
      i_name<-gsub(i,pattern='/', replacement='-')
      if(table(pancreas.integrated@meta.data[pancreas.integrated@meta.data$annot==i,][,each_flag])[[diff1]]>=3 & table(pancreas.integrated@meta.data[pancreas.integrated@meta.data$annot==i,][,each_flag])[[diff2]]>=3){
      dir.create(paste0(outdir,'/enrich/',each_name,'/ALL/',i_name))
      dir.create(paste0(outdir,'/enrich/',each_name,'/UP/',i_name))
      dir.create(paste0(outdir,'/enrich/',each_name,'/DOWN/',i_name))
      pancreas.integrated.marker <- FindMarkers(pancreas.integrated, ident.1 = diff1,ident.2 = diff2,group.by = each_flag, subset.ident = i,only.pos = FALSE,min.cells.group = 0)
      pancreas.integrated.marker$gene<-rownames(pancreas.integrated.marker)
      pancreas.integrated.marker<-pancreas.integrated.marker[,c(ncol(pancreas.integrated.marker),1:ncol(pancreas.integrated.marker)-1)]
      if (species %in% c('Human','Mouse','Rat')){
        pancreas.integrated.marker$gene_description<-uniq_gene_annot_file[as.character(pancreas.integrated.marker$gene),]$gene_description}
      write.table(pancreas.integrated.marker,paste0(outdir,'/diff/',each_name,'/',i_name,'_',each_name,'_diffgene.xls'),quote = F,row.names = F,sep = '\t')
      sig_mark<-subset(pancreas.integrated.marker,p_val_adj<0.05)
      write.table(sig_mark,paste0(outdir,'/diff/',each_name,'/',i_name,'_',each_name,'_sig_diffgene.xls'),quote = F,row.names = F,sep = '\t')
      if(volcano_flag==1){
        gene_label <- as.character(sig_mark$gene)
        Run_diffgene_volcano(diffgene=pancreas.integrated.marker,label_gene_list=gene_label,cpname=paste0(i_name,'_',each_name),p_val_adj=p_val_adj,logFC=avg_log2FC,outdir=paste0(outdir,'/diff/',each_name,'/volcano'))}
      if(nrow(sig_mark)>=50 & volcano_flag==0){
        sig_mark$avg_log2FC2<-abs(sig_mark$avg_log2FC)
        top50 <- sig_mark %>% top_n(n = 50, wt = avg_log2FC2)
        gene_label <- as.character(top50$gene)
        Run_diffgene_volcano(diffgene=pancreas.integrated.marker,label_gene_list=gene_label,cpname=paste0(i_name,'_',each_name),p_val_adj=p_val_adj,logFC=avg_log2FC,outdir=paste0(outdir,'/diff/',each_name,'/volcano'))}
      if(nrow(sig_mark)<50 & volcano_flag==0){
        gene_label <- as.character(sig_mark$gene)
        Run_diffgene_volcano(diffgene=pancreas.integrated.marker,label_gene_list=gene_label,cpname=paste0(i_name,'_',each_name),p_val_adj=p_val_adj,logFC=avg_log2FC,outdir=paste0(outdir,'/diff/',each_name,'/volcano'))}
      if(!(is.na(species))){
        sig_gene<-sig_mark$gene
        outdir2=paste0(outdir,'/enrich/',each_name,'/ALL/',i_name)
        prefix=paste0(i_name)
        go_enrich_analysis(sig_gene,species,outdir2,prefix)
        kegg_enrich_analysis(sig_gene,species,outdir2,prefix)
        sig_mark_up<-subset(sig_mark,avg_log2FC>0)
        sig_mark_down<-subset(sig_mark,avg_log2FC<0)
        sig_gene_up<-sig_mark_up$gene
        outdir2=paste0(outdir,'/enrich/',each_name,'/UP/',i_name)
        prefix=paste0(i_name,'_up')
        go_enrich_analysis(sig_gene_up,species,outdir2,prefix)
        kegg_enrich_analysis(sig_gene_up,species,outdir2,prefix)
        sig_gene_down<-sig_mark_down$gene
        outdir2=paste0(outdir,'/enrich/',each_name,'/DOWN/',i_name)
        prefix=paste0(i_name,'_down')
        go_enrich_analysis(sig_gene_down,species,outdir2,prefix)
        kegg_enrich_analysis(sig_gene_down,species,outdir2,prefix)
      }}
    }
  }
  pos <-pos+1
}

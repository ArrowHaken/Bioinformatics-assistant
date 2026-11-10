.libPaths('/share/apps/R/R-4.1.2-4/lib64/R/library')
suppressMessages({
library(dplyr)
library(cowplot)
library(ggplot2)
library(Seurat)
library(patchwork)
library(argparser)})
plan("multicore", workers = 8)
options(future.globals.maxSize = 1000000 * 1024^5)

argv <- arg_parser('')
argv <- add_argument(argv,"--rds", help="the seurat rds file")
argv <- add_argument(argv,"--outdir", help="the output dir")
argv <- add_argument(argv,"--reduction", help="the reduction of cluster")
argv <- add_argument(argv,"--species", help="the species of project")
argv <- parse_args(argv)

rds <- argv$rds
outdir <- argv$outdir
reduction <- argv$reduction
species <- argv$species


pancreas.integrated<-readRDS(rds)

DefaultAssay(pancreas.integrated)<-'RNA'
#pancreas.integrated$seurat_clusters<-factor(pancreas.integrated$seurat_clusters,levels=unique(pancreas.integrated$seurat_clusters))
#pancreas.integrated$seurat_clusters<-as.character(pancreas.integrated$seurat_clusters)
pancreas.integrated$seurat_clusters<-factor(pancreas.integrated$seurat_clusters,levels=unique(sort(pancreas.integrated$seurat_clusters)))
Idents(pancreas.integrated)<-pancreas.integrated@meta.data$annot
pancreas.integrated<-NormalizeData(pancreas.integrated,verbose = FALSE)
pancreas.integrated.allmarkers <- FindAllMarkers(pancreas.integrated, only.pos = TRUE, min.pct = 0.2, logfc.threshold = 0.25)
pancreas.integrated.allmarkers<-pancreas.integrated.allmarkers[!grepl(pancreas.integrated.allmarkers$gene,pattern = '^MT-|^mt-|^Mt-'),]
if (!is.na(species)){
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
  pancreas.integrated.allmarkers$gene_description<-uniq_gene_annot_file[as.character(pancreas.integrated.allmarkers$gene),]$gene_description
}
all_sig_marker_gene<-subset(pancreas.integrated.allmarkers,p_val_adj<0.05)
#write.table(pancreas.integrated.allmarkers,paste(outdir,'/all_markgene.xls',sep = ''),quote = F,row.names = F,sep = '\t')
write.table(all_sig_marker_gene,paste(outdir,'/celltype_sig_markgene.xls',sep = ''),quote = F,row.names = F,sep = '\t')
q()
all.genes <- rownames(pancreas.integrated)
pancreas.integrated <- ScaleData(pancreas.integrated, features = all.genes)
top10 <- all_sig_marker_gene %>% group_by(cluster) %>% top_n(n = 10, wt = avg_log2FC)
top12 <- all_sig_marker_gene %>% group_by(cluster) %>% top_n(n = 12, wt = avg_log2FC)
#p1<-DoHeatmap(pancreas.integrated, features = as.character(top10$gene)) + NoLegend()+scale_fill_gradient2(low = "#0000FF",mid = "#FFF8DC",high = "#FF0000")
p2<-DoHeatmap(pancreas.integrated, features = as.character(top10$gene)) +scale_fill_gradient2(low = "#0000FF",mid = "#FFF8DC",high = "#FF0000")

if(file.exists(paste0(outdir,'/heatmap'))){
  unlink(paste0(outdir,'/heatmap'), recursive=TRUE)
}else{dir.create(paste0(outdir,'/heatmap'))}
if(file.exists(paste0(outdir,'/featureplot'))){
  unlink(paste0(outdir,'/featureplot'), recursive=TRUE)
}else{dir.create(paste0(outdir,'/featureplot'))}
if(file.exists(paste0(outdir,'/violin'))){
  unlink(paste0(outdir,'/violin'), recursive=TRUE)
}else{dir.create(paste0(outdir,'/violin'))}

dir.create(paste0(outdir,'/heatmap'))
dir.create(paste0(outdir,'/featureplot'))
dir.create(paste0(outdir,'/violin'))

ggsave(p2,filename = paste0(outdir,'/heatmap/top10_heatmap.pdf'),width = 25,height = 20)

for (i in levels(pancreas.integrated@meta.data$seurat_clusters)) {
    p1<-VlnPlot(pancreas.integrated,features = as.character(top12[top12$cluster==i,]$gene),pt.size = 0)
    ggsave(p1,filename = paste0(outdir,'/violin/violin_cluster',i,'.png'),width = 20,height = 15)
    ggsave(p1,filename = paste0(outdir,'/violin/violin_cluster',i,'.pdf'),width = 20,height = 15)
    p1<-FeaturePlot(pancreas.integrated,features = as.character(top12[top12$cluster==i,]$gene),label = T,reduction = reduction,raster = F) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
    ggsave(p1,filename = paste0(outdir,'/featureplot/featureplot_cluster',i,'.png'),width = 20,height = 15)
    ggsave(p1,filename = paste0(outdir,'/featureplot/featureplot_cluster',i,'.pdf'),width = 20,height = 15)
}
ggsave(p2,filename = paste0(outdir,'/heatmap/top10_heatmap.png'),width = 25,height = 20)

##########王文标 7个微塑料
for (each in c('18m-G489','18m-G490','18m-G491','18m-G756','18m-G757','18m-G761','18m-G760')) {
  seu<-readRDS(file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/samplerds3/',each,'.rds'))
  print(each)
  print(ncol(seu))
  seu<-subset(seu,subset = nFeature_RNA > 300 & nFeature_RNA < 5000 & percent.mt <10)
  print(ncol(seu))
  #saveRDS(seu,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/samplerds3/fiterrds/',each,'.rds'))
  saveRDS(seu,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/samplerds3/fiterrds2/',each,'.rds'))
}
#############merge 7
integrated_data<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
integrated_data<-subset(integrated_data,seurat_clusters %in% c(0:15,23,24))#del 16 17 18 19 20 21 22 25 26 27
ElbowPlot(integrated_data)
integrated_data <- FindNeighbors(integrated_data, reduction = 'harmony',dims = 1:20)
integrated_data <- FindClusters(integrated_data, resolution = 0.6)
integrated_data <- RunUMAP(integrated_data, reduction = 'harmony', dims = 1:20)
integrated_data <- RunTSNE(integrated_data, reduction = 'harmony', dims = 1:20)
#看看内皮能否分出POD
integrated_data_sub<-subset(integrated_data,annot=='Endo')
integrated_data_sub <- integrated_data_sub %>% Seurat::NormalizeData(verbose = FALSE) %>% FindVariableFeatures(selection.method = 'vst', nfeatures = 2000) %>% ScaleData(verbose = FALSE) %>% RunPCA(verbose = FALSE)
integrated_data_sub <- integrated_data_sub %>% RunHarmony('orig.ident')
integrated_data_sub <- FindNeighbors(integrated_data_sub, reduction = 'harmony',dims = 1:20)
integrated_data_sub <- FindClusters(integrated_data_sub, resolution = 0.6)
integrated_data_sub <- RunUMAP(integrated_data_sub, reduction = 'harmony', dims = 1:20)
DimPlot(integrated_data_sub,label = T,cols = color_panel,raster = F)
marker_heatmap<-RunFindAllMarkers(integrated_data_sub,species = 'Mouse',min.pct = 0.3)
DimPlot(integrated_data,label = T,cols = color_panel,raster = F)
#剔除G761
integrated_data<-subset(integrated_data,orig.ident!='18m-G761')
integrated_data$orig.ident<-factor(integrated_data$orig.ident)
plot_cluster_umap(integrated_data,outdir = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/1.cluster',type = 'merge',cols = color_panel,ncol_group = 2,ncol = 3,group = 'group',raster = F,pt.size = 0.4)
saveRDS(integrated_data,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
#test
#QC stat
raw_stat<-read.delim(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/samplerds3/sample_raw_cellnumber.xls',header = T)
total_stat<-data.frame(table(integrated_data$orig.ident),geneMedian=sapply(levels(integrated_data$orig.ident), function(x){ceiling(median(integrated_data@meta.data[integrated_data$orig.ident==x,]$nFeature_RNA))}),
                       MtMedian=sapply(levels(integrated_data$orig.ident), function(x){round(median(integrated_data@meta.data[integrated_data$orig.ident==x,]$percent.mt),digits = 2)}))
colnames(total_stat)<-c('sample','residue','geneMedian','MtMedian')
raw_stat$residue<-mapvalues(raw_stat$sample,rownames(total_stat),total_stat$residue)
raw_stat$residue<-as.numeric(raw_stat$residue)
raw_stat$filter<-raw_stat$raw-raw_stat$residue
raw_stat$ratio<-round(100*raw_stat$filter/raw_stat$raw,digits = 2)
raw_stat$geneMedian<-mapvalues(raw_stat$sample,rownames(total_stat),total_stat$geneMedian)
raw_stat$MtMedian<-mapvalues(raw_stat$sample,rownames(total_stat),total_stat$MtMedian)
write.table(raw_stat,file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/0.QC/total_stat.xls',quote = F,sep = '\t',row.names = F)
###annot
integrated_data<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
#PT-S1:2 PT-S2:0 5 6 PT-S3:3 
p1<-FeaturePlot(integrated_data,features = c('Slc5a12','Slc5a2','Cyp4b1','Cyp2e1','Napsa','Cyp7b1'),ncol=3,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/PT_featureplot.png',width = 15,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/PT_featureplot.pdf',width = 15,height = 10)
#LOH TAL:4 8 TL: DCT:7 11
p1<-FeaturePlot(integrated_data,features = c('Slc12a1','Umod','Slc12a3','Trpm6'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/LOH_DCT_featureplot.png',width = 10,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/LOH_DCT_featureplot.pdf',width = 10,height = 10)
#PC:10 17 IC:14
p1<-FeaturePlot(integrated_data,features = c('Aqp2','Frmpd4','Atp6v0d2','Atp6v1c2'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/PC_IC_featureplot.png',width = 10,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/PC_IC_featureplot.pdf',width = 10,height = 10)
#UC:16 POD:
p1<-FeaturePlot(integrated_data,features = c('Krt19','Upk1b','Nphs2','Wt1'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/UC_POD_featureplot.png',width = 10,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/UC_POD_featureplot.pdf',width = 10,height = 10)
#Endo:1 13
p1<-FeaturePlot(integrated_data,features = c('Pecam1','Ptprb'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/Endo_featureplot.png',width = 10,height = 5)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/Endo_featureplot.pdf',width = 10,height = 5)
#Fib:9 Adi:
p1<-FeaturePlot(integrated_data,features = c('Col1a2','Fbln5'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/Fib_featureplot.png',width = 10,height = 5)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/Fib_featureplot.pdf',width = 10,height = 5)
#VMSC:
#p1<-FeaturePlot(integrated_data,features = c('Notch3','Pdgfrb'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
#ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/VSMC_featureplot.png',width = 10,height = 5)
#ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/VSMC_featureplot.pdf',width = 10,height = 5)
#T:15 B:19 MAC:12 NEU:18
p1<-FeaturePlot(integrated_data,features = c('Il7r','Themis','Bank1','Igkc','Cd86','Mrc1','Csf3r','S100a9'),ncol=3,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/Immune_featureplot.png',width = 15,height = 15)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/Immune_featureplot.pdf',width = 15,height = 15)
#celltype
cell_type = c("0"="PT-S2","1"="Endo","2"="PT-S1","3"="PT-S3","4"="LOH","5"="PT-S2","6"="PT-S2","7"="DCT","8"="LOH","9"="Fib",
              "10"="PC","11"="DCT","12"="MAC","13"="Endo","14"="IC","15"="T","16"="UC","17"="PC","18"="NEU","19"="B")
integrated_data$annot<-cell_type[integrated_data$seurat_clusters]
integrated_data$annot<-factor(integrated_data$annot,levels = c("PT-S1","PT-S2","PT-S3","LOH","DCT","PC","IC","UC","Endo","Fib","T","B","MAC","NEU"))
integrated_data$annot_cluster<-factor(paste0(integrated_data$seurat_clusters,':',integrated_data$annot),levels = paste0(names(cell_type),':',cell_type))
p1<-DimPlot(integrated_data,group.by = 'annot',cols = color_panel,label = T,raster = F,pt.size = 0.5)
p2<-DimPlot(integrated_data,group.by = 'annot_cluster',cols = color_panel,raster = F,label = T,pt.size = 0.5)
p3<-DimPlot(integrated_data,group.by = 'annot',cols = color_panel,split.by = 'group',label = T,raster = F,pt.size = 0.5)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/merge_umap_annot.png',width = 10,height = 8)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/merge_umap_annot.pdf',width = 10,height = 8)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/merge_umap_cluster_annot.png',width = 12,height = 10)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/merge_umap_cluster_annot.pdf',width = 12,height = 10)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/merge_umap_annot_group.png',width = 15,height = 10)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/merge_umap_annot_group.pdf',width = 15,height = 10)
saveRDS(integrated_data,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
#dotplot
genelist<-c('Slc5a12','Slc5a2','Cyp4b1','Cyp2e1','Napsa','Cyp7b1','Slc12a1','Umod','Slc12a3','Trpm6','Aqp2','Frmpd4','Atp6v0d2','Atp6v1c2','Krt19','Upk1b','Pecam1','Ptprb','Col1a2','Fbln5','Il7r','Themis','Bank1','Igkc','Cd86','Mrc1','Csf3r','S100a9')
integrated_data$annot<-factor(integrated_data$annot,levels=rev(levels(integrated_data$annot)))
Idents(integrated_data)<-integrated_data$annot
p2<-DotPlot(integrated_data,features = c(genelist),cols = c('#009ACD','#FF0000'))+#coord_flip()+
  theme(axis.text.x = element_text(size=12,color = "black", hjust=1,vjust=1,angle=45))+labs(x="",y="")
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/genelist_dotplot.png',width = 12,height = 6)
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/6.singleR/annot/genelist_dotplot.pdf',width = 12,height = 6)
#cell ratio加显著性值annot
for (each_group in c('group')) {
  cell_number<-read.table(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/3.cell_ratio/cell_number_sample.xls'),header = T,sep = '\t')
  sample_group_data<-unique(integrated_data@meta.data[,c('orig.ident',each_group)])
  cell_number$group<-mapvalues(cell_number$sample,as.character(sample_group_data$orig.ident),as.character(sample_group_data[,2]))
  cell_number$group<-factor(cell_number$group,levels = levels(integrated_data@meta.data[,each_group]))
  cell_number$cluster<-factor(cell_number$cluster,levels = levels(integrated_data$annot))
  my_comparisons <- combn(levels(cell_number$group), 2, simplify = FALSE)
  p1<-ggplot(cell_number , aes(x = group, y = rate, fill=group)) +facet_wrap(~cluster,scales = 'free')+
    geom_bar(stat = "summary", fun = mean, color = "black", position = position_dodge(),width=0.8) +
    #scale_y_continuous(expand = c(0,0))+
    labs(x='',y="Percentage(%)")+ theme_bw()+#scale_fill_manual(values = c("#90BFF9","#FFA0A0"))+
    stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",width = 0.2,position = position_dodge()) +
    geom_point(aes(x = group, y = rate),color='black',position = position_dodge())+
    stat_compare_means(comparisons = my_comparisons)+
    #stat_compare_means(comparisons = my_comparisons,method = 't.test',symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "ns")))+
    theme(axis.title.y = element_text(size=15,color = "black"),
          axis.text = element_text(size=15,color = "black"),
          axis.text.x = element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          strip.background = element_blank(),
          strip.text = element_text(size = 15,color="black"),
          legend.title = element_text(size = 10,color="black"),
          legend.text = element_text(size = 10,color="black"))
  ggsave(p1,filename = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/3.cell_ratio/all_cluster_cell_number_rate_addpvalue.png'),width = 20,height = 20)
  ggsave(p1,filename = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/3.cell_ratio/all_cluster_cell_number_rate_addpvalue.pdf'),width = 20,height = 20)
  write.table(cell_number,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/3.cell_ratio/all_cluster_cell_number_rate.xls'),quote = F,sep = '\t',row.names = F)
}
###Fib GSEA分析
integrated_data<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
geneset_go_file<-read.delim('~/Project/personality/script/getGoTerm/Mouse_go_gene_sets_symbols.xls',header = T,sep = '\t')
geneset_go<-data.frame(term=geneset_go_file$Pathway,gene=geneset_go_file$Gene)
colnames(geneset_go)<-c('term','gene')
geneset_go2<-data.frame(term=geneset_go_file$Pathway,name=geneset_go_file$DESCRPTION)
geneset_kegg_file<-read.delim('~/Project/personality/script/getGoTerm/Mouse_kegg_gene_sets_symbols.xls',header = T,sep = '\t')
geneset_kegg<-data.frame(term=geneset_kegg_file$Pathway,gene=geneset_kegg_file$Gene)
colnames(geneset_kegg)<-c('term','gene')
geneset_kegg2<-data.frame(term=geneset_kegg_file$Pathway,name=geneset_kegg_file$DESCRPTION)
geneset_kegg2$name<-gsub(geneset_kegg2$name,pattern = ' - Mus musculus \\(house mouse\\)',replacement = '')
for (i in c('Fib')) {
  Idents(integrated_data)<-integrated_data$annot
  for (each_group in c('MicroplasticsvsMock')) {
    each1<-str_split(each_group,pattern = 'vs')[[1]][1]
    each2<-str_split(each_group,pattern = 'vs')[[1]][2]
    diffgene_data<-FindMarkers(integrated_data,ident.1 = each1,ident.2 = each2,logfc.threshold = 0,subset.ident = i,group.by = 'group')
    diffgene_data<-diffgene_data[order(diffgene_data$avg_log2FC,decreasing = T),]
    diffgene<-diffgene_data[,'avg_log2FC']
    names(diffgene)<-rownames(diffgene_data)
    #GO
    egmt <- GSEA(diffgene, TERM2GENE=geneset_go,TERM2NAME = geneset_go2,
                 minGSSize = 30,
                 pvalueCutoff = 0.05,
                 verbose=FALSE)
    gsea_results_df <- egmt@result
    outdir2<-paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/13.GSEA/',i,'/',each_group,'/GO')
    dir.create(outdir2,recursive = T)
    write.table(gsea_results_df,file = paste0(outdir2,'/GO_gsea_results.xls'),quote = F,sep = '\t',row.names = F)
    if(nrow(gsea_results_df)>0){
      for (each in unique(gsea_results_df$ID)) {
        pathway_list<-gsea_results_df[gsea_results_df$type==each,]$ID
        res <- gsea_results_df[each,]
        label <- paste0(res$Description,
          "\np.adjust = ", format(res$p.adjust, scientific = TRUE, digits = 3)
        )
        p1<-gseaplot2(egmt,geneSetID = each,pvalue_table = F)+
          annotate("text",x = Inf,y = Inf,label = label,hjust = 1,vjust = 1)
        ggsave(p1,filename = paste0(outdir2,'/',gsub(each,pattern = ':',replacement = '-'),'_gsea.png'),width = 12,height = 10)
        ggsave(p1,filename = paste0(outdir2,'/',gsub(each,pattern = ':',replacement = '-'),'_gsea.pdf'),width = 12,height = 10)
      }
    }
    #KEGG
    egmt <- GSEA(diffgene, TERM2GENE=geneset_kegg,TERM2NAME = geneset_kegg2,
                 minGSSize = 30,
                 pvalueCutoff = 0.05,
                 verbose=FALSE)
    gsea_results_df <- egmt@result
    outdir2<-paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/13.GSEA/',i,'/',each_group,'/KEGG')
    dir.create(outdir2,recursive = T)
    write.table(gsea_results_df,file = paste0(outdir2,'/KEGG_gsea_results.xls'),quote = F,sep = '\t',row.names = F)
    if(nrow(gsea_results_df)>0){
      for (each in unique(gsea_results_df$ID)) {
        pathway_list<-gsea_results_df[gsea_results_df$type==each,]$ID
        res <- gsea_results_df[each,]
        label <- paste0(res$Description,
                        "\np.adjust = ", format(res$p.adjust, scientific = TRUE, digits = 3)
        )
        p1<-gseaplot2(egmt,geneSetID = each,pvalue_table = F)+
          annotate("text",x = Inf,y = Inf,label = label,hjust = 1,vjust = 1)
        ggsave(p1,filename = paste0(outdir2,'/',each,'_gsea.png'),width = 12,height = 10)
        ggsave(p1,filename = paste0(outdir2,'/',each,'_gsea.pdf'),width = 12,height = 10)
      }
    }
  }
}
###################亚群细分
###髓系细分
integrated_data<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
integrated_data_M<-subset(integrated_data,annot %in% c('MAC','NEU'))
integrated_data_M <- FindNeighbors(integrated_data_M, reduction = 'harmony',dims = 1:25)
integrated_data_M <- FindClusters(integrated_data_M, resolution = 0.4)
integrated_data_M <- RunUMAP(integrated_data_M, reduction = 'harmony', dims = 1:25)
integrated_data_M$seurat_clusters<-as.character(integrated_data_M$seurat_clusters)
integrated_data_M@meta.data[integrated_data_M$seurat_clusters==3,]$seurat_clusters<-0
integrated_data_M@meta.data[integrated_data_M$seurat_clusters==4,]$seurat_clusters<-3
integrated_data_M@meta.data[integrated_data_M$seurat_clusters %in% c(5,7),]$seurat_clusters<-4
integrated_data_M@meta.data[integrated_data_M$seurat_clusters==6,]$seurat_clusters<-5
integrated_data_M$seurat_clusters<-factor(integrated_data_M$seurat_clusters)
Idents(integrated_data_M)<-integrated_data_M$seurat_clusters
DimPlot(integrated_data_M,label = T,cols = color_panel,raster = F)
#0群再多分群
integrated_data_M <- integrated_data_M %>% Seurat::NormalizeData(verbose = FALSE) %>% FindVariableFeatures(selection.method = 'vst', nfeatures = 1000) %>% ScaleData(verbose = FALSE) %>% RunPCA(verbose = FALSE)
integrated_data_M <- integrated_data_M %>% RunHarmony('orig.ident')
integrated_data_M <- FindNeighbors(integrated_data_M, reduction = 'pca',dims = 1:25)
integrated_data_M <- FindClusters(integrated_data_M, resolution = 0.5)
integrated_data_M <- RunUMAP(integrated_data_M, reduction = 'pca', dims = 1:25)
DimPlot(integrated_data_M,label = T,cols = color_panel,raster = F)
marker_heatmap<-RunFindAllMarkers(integrated_data_M,species = 'Mouse',min.pct = 0.25)
integrated_data_M@assays$RNA@scale.data<-matrix()
#大群Mock鉴定到的中性粒添加进来
neu_mock_cell<-rownames(integrated_data@meta.data[integrated_data$annot=='NEU' & integrated_data$group=='Mock',])
integrated_data_M$seurat_clusters<-as.character(integrated_data_M$seurat_clusters)
integrated_data_M@meta.data[neu_mock_cell,]$seurat_clusters<-2
integrated_data_M$seurat_clusters<-factor(integrated_data_M$seurat_clusters)
Idents(integrated_data_M)<-integrated_data_M$seurat_clusters
integrated_data_M@reductions$umap@cell.embeddings[integrated_data_M$seurat_clusters %in% c(2) & integrated_data_M$group=='Mock',1]<-integrated_data_M@reductions$umap@cell.embeddings[integrated_data_M$seurat_clusters %in% c(2) & integrated_data_M$group=='Mock',1] +10
plot_cluster_umap(integrated_data_M,outdir = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/1.cluster',type = 'Myeloid_sub',ncol_group = 2,ncol = 3,group = 'group',raster = F,pt.size = 0.5)
saveRDS(integrated_data_M,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/Myeloid_subcluster.rds'))
#annot
integrated_data_M<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/Myeloid_subcluster.rds'))
#Mmp13+Macro:0 Tbc1d30+Macro:4 Edn1+Macro:1 Il34+Macro:3 Vcan+Macro:6 Neu:2 Btla+DC:7 Ccr2+DC:5
#p1<-FeaturePlot(integrated_data_M,features = c('Mmp13','Itga9','Axl','Tbc1d30','Msr1','Apoe','Il2ra','Ms4a4c','Edn1','Il34','Aspa','Itgb6','Vcan','Fn1','Lyz2','Hdc','Csf3r','S100a9','Btla','Htr7','Bcl11a','Wdfy4','Ciita','Ccr2'),ncol=6,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
p1<-FeaturePlot(integrated_data_M,features = c('Ccl12','Tlr1','Adgre1','Ptprm','Zbp1','Irf7','Vegfa','Cdh1','Clec4e','Lyz2','S100a9','S100a8','Ahr','Il6','Ccr2','Havcr2'),ncol=4,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/Myeloid_featureplot.png',width = 20,height = 15)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/Myeloid_featureplot.pdf',width = 20,height = 15)
p1<-FeaturePlot(integrated_data_M,features = c('Nlrp3','Il1b'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
p2<-VlnPlot(integrated_data_M,features = c('Nlrp3','Il1b'),pt.size = 0.1)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/Nlrp3-Il1b_expression_featureplot.png',width = 10,height = 6)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/Nlrp3-Il1b_expression_featureplot.pdf',width = 10,height = 6)
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/Nlrp3-Il1b_expression_violinplot.png',width = 10,height = 6)
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/Nlrp3-Il1b_expression_violinplot.pdf',width = 10,height = 6)
#celltype
#cell_type = c("0"="Mmp13+Macro","1"="Edn1+Macro","2"="Neu","3"="Il34+Macro","4"="Tbc1d30+Macro","5"="Ccr2+DC","6"="Vcan+Macro","7"="Btla+DC")
cell_type = c("0"="Ccl12+Mac","1"="Zbp1+Mac","2"="Neu","3"="Vegfa+Mac","4"="Adgre1+Mac","5"="Ccr2+DC","6"="Clec4e+Mac","7"="Ahr+DC")
integrated_data_M$annot<-cell_type[integrated_data_M$seurat_clusters]
integrated_data_M$annot<-factor(integrated_data_M$annot,levels = c("Ccl12+Mac","Adgre1+Mac","Zbp1+Mac","Vegfa+Mac","Clec4e+Mac","Neu","Ahr+DC","Ccr2+DC"))
integrated_data_M$annot_cluster<-factor(paste0(integrated_data_M$seurat_clusters,':',integrated_data_M$annot),levels = paste0(names(cell_type),':',cell_type))
p1<-DimPlot(integrated_data_M,group.by = 'annot',label = T,raster = F,pt.size = 0.6)
p2<-DimPlot(integrated_data_M,group.by = 'annot_cluster',raster = F,label = T,pt.size = 0.6)
p3<-DimPlot(integrated_data_M,group.by = 'annot',split.by = 'group',label = T,raster = F,pt.size = 0.6)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/Myeloid_umap_annot.png',width = 8,height = 6)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/Myeloid_umap_annot.pdf',width = 8,height = 6)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/Myeloid_umap_cluster_annot.png',width = 10,height = 8)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/Myeloid_umap_cluster_annot.pdf',width = 10,height = 8)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/Myeloid_umap_annot_group.png',width = 12,height = 10)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/Myeloid_umap_annot_group.pdf',width = 12,height = 10)
saveRDS(integrated_data_M,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/Myeloid_subcluster.rds'))
#dotplot
genelist<-c('Ccl12','Tlr1','Adgre1','Ptprm','Zbp1','Irf7','Vegfa','Cdh1','Clec4e','Lyz2','S100a9','S100a8','Ahr','Il6','Ccr2','Havcr2')
integrated_data_M$annot<-factor(integrated_data_M$annot,levels=rev(levels(integrated_data_M$annot)))
Idents(integrated_data_M)<-integrated_data_M$annot
p2<-DotPlot(integrated_data_M,features = c(genelist),cols = c('#009ACD','#FF0000'))+#coord_flip()+
  theme(axis.text.x = element_text(size=12,color = "black", hjust=1,vjust=1,angle=45))+labs(x="",y="")
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/genelist_dotplot.png',width = 10,height = 5)
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/5.annot/genelist_dotplot.pdf',width = 10,height = 5)
#cell ratio加显著性值annot
for (each_group in c('group')) {
  cell_number<-read.table(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/3.cell_ratio/cell_number_sample.xls'),header = T,sep = '\t')
  sample_group_data<-unique(integrated_data_M@meta.data[,c('orig.ident',each_group)])
  cell_number$group<-mapvalues(cell_number$sample,as.character(sample_group_data$orig.ident),as.character(sample_group_data[,2]))
  cell_number$group<-factor(cell_number$group,levels = levels(integrated_data_M@meta.data[,each_group]))
  cell_number$cluster<-factor(cell_number$cluster,levels = levels(integrated_data_M$annot))
  my_comparisons <- combn(levels(cell_number$group), 2, simplify = FALSE)
  p1<-ggplot(cell_number , aes(x = group, y = rate, fill=group)) +facet_wrap(~cluster,scales = 'free')+
    geom_bar(stat = "summary", fun = mean, color = "black", position = position_dodge(),width=0.8) +
    #scale_y_continuous(expand = c(0,0))+
    labs(x='',y="Percentage(%)")+ theme_bw()+#scale_fill_manual(values = c("#90BFF9","#FFA0A0"))+
    stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",width = 0.2,position = position_dodge()) +
    geom_point(aes(x = group, y = rate),color='black',position = position_dodge())+
    stat_compare_means(comparisons = my_comparisons)+
    #stat_compare_means(comparisons = my_comparisons,method = 't.test',symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "ns")))+
    theme(axis.title.y = element_text(size=15,color = "black"),
          axis.text = element_text(size=15,color = "black"),
          axis.text.x = element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          strip.background = element_blank(),
          strip.text = element_text(size = 15,color="black"),
          legend.title = element_text(size = 10,color="black"),
          legend.text = element_text(size = 10,color="black"))
  ggsave(p1,filename = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/3.cell_ratio/all_cluster_cell_number_rate_addpvalue.png'),width = 10,height = 12)
  ggsave(p1,filename = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/3.cell_ratio/all_cluster_cell_number_rate_addpvalue.pdf'),width = 10,height = 12)
  write.table(cell_number,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/3.cell_ratio/all_cluster_cell_number_rate.xls'),quote = F,sep = '\t',row.names = F)
}
######基因阴阳性表达差异
integrated_data_M<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/Myeloid_subcluster.rds'))
genelist<-c('Nlrp3')
for (each in c('Neu','DC','Mac','Ccl12+Mac_Adgre1+Mac','Adgre1+Mac')) {
  if(each=='DC'){
    integrated_data_M_sub<-subset(integrated_data_M,annot %in% c('Ahr+DC','Ccr2+DC'))
  }else if(each=='Mac'){
    integrated_data_M_sub<-subset(integrated_data_M,annot %in% c('Ccl12+Mac','Adgre1+Mac','Zbp1+Mac','Vegfa+Mac','Clec4e+Mac'))
  }else if(each=='Ccl12+Mac_Adgre1+Mac'){
    integrated_data_M_sub<-subset(integrated_data_M,annot %in% c('Ccl12+Mac','Adgre1+Mac'))
  }else{integrated_data_M_sub<-subset(integrated_data_M,annot %in% each)}
  integrated_data_M_sub@meta.data$flag<-ifelse(integrated_data_M_sub@assays$RNA@data['Nlrp3',]>0,'Pos','Neg')
  Idents(integrated_data_M_sub)<-integrated_data_M_sub$flag
  each_diffgene<-FindMarkers(integrated_data_M_sub,ident.1 = 'Pos',ident.2 = 'Neg',logfc.threshold = 0)
  each_sig_diffgene<-each_diffgene[each_diffgene$p_val<0.05 & abs(each_diffgene$avg_log2FC)>0.25,]
  dir.create(paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/10.Nlrp3_PosvsNeg/',each,'_PosvsNeg/'))
  outdir2<-paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/10.Nlrp3_PosvsNeg/',each,'_PosvsNeg/')
  write.table(each_sig_diffgene,file = paste0(outdir2,each,'_Nlrp3_PosvsNeg_sig_diffgene.xls'),sep = '\t',quote = F,col.names = NA)
  each_diffgene$gene<-rownames(each_diffgene)
  each_sig_diffgene$gene<-rownames(each_sig_diffgene)
  each_sig_diffgene$avg_log2FC2<-abs(each_sig_diffgene$avg_log2FC)
  top50 <- each_sig_diffgene %>% top_n(n = 50, wt = avg_log2FC2)
  gene_label <- as.character(top50$gene)
  Run_diffgene_volcano_p_val(diffgene=each_diffgene,label_gene_list=gene_label,cpname=paste0(each,'_Nlrp3+vsNlrp3-'),p_val=0.05,logFC=0.25,outdir=outdir2)
  #UP
  tryCatch({
    go_enrich_analysis3(rownames(each_sig_diffgene[each_sig_diffgene$avg_log2FC>0,]),'Mouse',paste0(outdir2),paste0(each,'_Nlrp3+vsNlrp3-_UP'),'bar',25)
    kegg_enrich_analysis3(rownames(each_sig_diffgene[each_sig_diffgene$avg_log2FC>0,]),'Mouse',paste0(outdir2),paste0(each,'_Nlrp3+vsNlrp3-_UP'),'bar',25)
    #Down
    go_enrich_analysis3(rownames(each_sig_diffgene[each_sig_diffgene$avg_log2FC<0,]),'Mouse',paste0(outdir2),paste0(each,'_Nlrp3+vsNlrp3-_DOWN'),'bar',25)
    kegg_enrich_analysis3(rownames(each_sig_diffgene[each_sig_diffgene$avg_log2FC<0,]),'Mouse',paste0(outdir2),paste0(each,'_Nlrp3+vsNlrp3-_DOWN'),'bar',25)
  },
  error = function(e) {
    message("捕获错误: ", conditionMessage(e), " (迭代", each, ")")})
}
#enrich pvalue
for (each in c('Neu','Mac','Ccl12+Mac_Adgre1+Mac','Adgre1+Mac')) {
  if(each=='DC'){
    integrated_data_M_sub<-subset(integrated_data_M,annot %in% c('Ahr+DC','Ccr2+DC'))
  }else if(each=='Mac'){
    integrated_data_M_sub<-subset(integrated_data_M,annot %in% c('Ccl12+Mac','Adgre1+Mac','Zbp1+Mac','Vegfa+Mac','Clec4e+Mac'))
  }else if(each=='Ccl12+Mac_Adgre1+Mac'){
    integrated_data_M_sub<-subset(integrated_data_M,annot %in% c('Ccl12+Mac','Adgre1+Mac'))
  }else{integrated_data_M_sub<-subset(integrated_data_M,annot %in% each)}
  integrated_data_M_sub@meta.data$flag<-ifelse(integrated_data_M_sub@assays$RNA@data['Nlrp3',]>0,'Pos','Neg')
  Idents(integrated_data_M_sub)<-integrated_data_M_sub$flag
  each_diffgene<-FindMarkers(integrated_data_M_sub,ident.1 = 'Pos',ident.2 = 'Neg',logfc.threshold = 0)
  each_sig_diffgene<-each_diffgene[each_diffgene$p_val<0.05 & abs(each_diffgene$avg_log2FC)>0.25,]
  dir.create(paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/10.Nlrp3_PosvsNeg/',each,'_PosvsNeg_pvalue/'))
  outdir2<-paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/10.Nlrp3_PosvsNeg/',each,'_PosvsNeg_pvalue/')
  write.table(each_sig_diffgene,file = paste0(outdir2,each,'_Nlrp3_PosvsNeg_sig_diffgene.xls'),sep = '\t',quote = F,col.names = NA)
  each_diffgene$gene<-rownames(each_diffgene)
  each_sig_diffgene$gene<-rownames(each_sig_diffgene)
  each_sig_diffgene$avg_log2FC2<-abs(each_sig_diffgene$avg_log2FC)
  top50 <- each_sig_diffgene %>% top_n(n = 50, wt = avg_log2FC2)
  gene_label <- as.character(top50$gene)
  Run_diffgene_volcano_p_val(diffgene=each_diffgene,label_gene_list=gene_label,cpname=paste0(each,'_Nlrp3+vsNlrp3-'),p_val=0.05,logFC=0.25,outdir=outdir2)
  #UP
  tryCatch({
    go_enrich_analysis3_pvalue(rownames(each_sig_diffgene[each_sig_diffgene$avg_log2FC>0,]),'Mouse',paste0(outdir2),paste0(each,'_Nlrp3+vsNlrp3-_UP'),'bar',25)
    kegg_enrich_analysis3_pvalue(rownames(each_sig_diffgene[each_sig_diffgene$avg_log2FC>0,]),'Mouse',paste0(outdir2),paste0(each,'_Nlrp3+vsNlrp3-_UP'),'bar',25)
    #Down
    go_enrich_analysis3_pvalue(rownames(each_sig_diffgene[each_sig_diffgene$avg_log2FC<0,]),'Mouse',paste0(outdir2),paste0(each,'_Nlrp3+vsNlrp3-_DOWN'),'bar',25)
    kegg_enrich_analysis3_pvalue(rownames(each_sig_diffgene[each_sig_diffgene$avg_log2FC<0,]),'Mouse',paste0(outdir2),paste0(each,'_Nlrp3+vsNlrp3-_DOWN'),'bar',25)
  },
  error = function(e) {
    message("捕获错误: ", conditionMessage(e), " (迭代", each, ")")})
}

###成纤维细分
integrated_data<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
integrated_data_Fib<-subset(integrated_data,annot %in% c('Fib'))
integrated_data_Fib <- FindNeighbors(integrated_data_Fib, reduction = 'harmony',dims = 1:20)
integrated_data_Fib <- FindClusters(integrated_data_Fib, resolution = 0.3)
integrated_data_Fib <- RunUMAP(integrated_data_Fib, reduction = 'harmony', dims = 1:20)
integrated_data_Fib_c012<-subset(integrated_data_Fib,seurat_clusters %in% c(0,1,2))
integrated_data_Fib_c012 <- integrated_data_Fib_c012 %>% Seurat::NormalizeData(verbose = FALSE) %>% FindVariableFeatures(selection.method = 'vst', nfeatures = 2000) %>% ScaleData(verbose = FALSE) %>% RunPCA(verbose = FALSE)
integrated_data_Fib_c012 <- integrated_data_Fib_c012 %>% RunHarmony('orig.ident')
integrated_data_Fib_c012 <- FindNeighbors(integrated_data_Fib_c012, reduction = 'harmony',dims = 1:20)
integrated_data_Fib_c012 <- FindClusters(integrated_data_Fib_c012, resolution = 0.3)
integrated_data_Fib_c012 <- RunUMAP(integrated_data_Fib_c012, reduction = 'harmony', dims = 1:20)
DimPlot(integrated_data_Fib_c012,label = T,cols = color_panel,raster = F)
#映射
integrated_data_Fib$seurat_clusters<-as.character(integrated_data_Fib$seurat_clusters)
integrated_data_Fib@meta.data[integrated_data_Fib$seurat_clusters==1,]$seurat_clusters<-0
integrated_data_Fib@meta.data[rownames(integrated_data_Fib_c012@meta.data[integrated_data_Fib_c012$seurat_clusters %in% c(2,4),]),]$seurat_clusters<-3
integrated_data_Fib@meta.data[integrated_data_Fib$seurat_clusters==3,]$seurat_clusters<-1
integrated_data_Fib@meta.data[integrated_data_Fib$seurat_clusters %in% c(4,5),]$seurat_clusters<-3
integrated_data_Fib$seurat_clusters<-factor(integrated_data_Fib$seurat_clusters)
Idents(integrated_data_Fib)<-integrated_data_Fib$seurat_clusters
DimPlot(integrated_data_Fib,label = T,cols = color_panel,raster = F)
marker_heatmap<-RunFindAllMarkers(integrated_data_Fib,species = 'Mouse',min.pct = 0.25)
integrated_data_Fib@assays$RNA@scale.data<-matrix()
plot_cluster_umap(integrated_data_Fib,outdir = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/1.cluster',type = 'Fibroblast_sub',ncol_group = 2,ncol = 3,group = 'group',raster = F,pt.size = 0.5)
saveRDS(integrated_data_Fib,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
#annot
integrated_data_Fib<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
#Fbln5+Fib:0 Negr1+Fib:2 Lepr+Fib:3 Il6+Fib:1
#p1<-FeaturePlot(integrated_data_Fib,features = c('Fbln5','Slc40a1','Adamts5','Negr1','Hmcn1','Sh3gl2','Lepr','Adgrl3','Tnn','Il6','Cxcl10','Cd44'),ncol=6,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
p1<-FeaturePlot(integrated_data_Fib,features = c('Abca8a','Fhl2','Fbln5','Negr1','Hmcn1','Sh3gl2','Igfbp5','Tnc','Tnn','Il6','Cd44','Csmd1'),ncol=6,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/5.annot/Fibroblast_featureplot.png',width = 30,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/5.annot/Fibroblast_featureplot.pdf',width = 30,height = 10)
#celltype
cell_type = c("0"="Mesenchymal fibroblast","1"="Inflammatory fibroblast","2"="Adherent fibroblast","3"="Pro-resolving fibroblast")
integrated_data_Fib$annot<-cell_type[integrated_data_Fib$seurat_clusters]
integrated_data_Fib$annot<-factor(integrated_data_Fib$annot,levels = c("Mesenchymal fibroblast","Adherent fibroblast","Pro-resolving fibroblast","Inflammatory fibroblast"))
integrated_data_Fib$annot_cluster<-factor(paste0(integrated_data_Fib$seurat_clusters,':',integrated_data_Fib$annot),levels = paste0(names(cell_type),':',cell_type))
p1<-DimPlot(integrated_data_Fib,group.by = 'annot',label = T,raster = F,pt.size = 0.5)
p2<-DimPlot(integrated_data_Fib,group.by = 'annot_cluster',raster = F,label = T,pt.size = 0.5)
p3<-DimPlot(integrated_data_Fib,group.by = 'annot',split.by = 'group',label = T,raster = F,pt.size = 0.5)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/5.annot/Fibroblast_umap_annot.png',width = 8,height = 6)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/5.annot/Fibroblast_umap_annot.pdf',width = 8,height = 6)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/5.annot/Fibroblast_umap_cluster_annot.png',width = 10,height = 8)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/5.annot/Fibroblast_umap_cluster_annot.pdf',width = 10,height = 8)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/5.annot/Fibroblast_umap_annot_group.png',width = 12,height = 10)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/5.annot/Fibroblast_umap_annot_group.pdf',width = 12,height = 10)
saveRDS(integrated_data_Fib,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
#dotplot
genelist<-c('Abca8a','Fhl2','Fbln5','Negr1','Hmcn1','Sh3gl2','Igfbp5','Tnc','Tnn','Il6','Cd44')
integrated_data_Fib$annot<-factor(integrated_data_Fib$annot,levels=rev(levels(integrated_data_Fib$annot)))
Idents(integrated_data_Fib)<-integrated_data_Fib$annot
p2<-DotPlot(integrated_data_Fib,features = c(genelist),cols = c('#009ACD','#FF0000'))+#coord_flip()+
  theme(axis.text.x = element_text(size=12,color = "black", hjust=1,vjust=1,angle=45))+labs(x="",y="")
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/5.annot/genelist_dotplot.png',width = 8,height = 5)
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/5.annot/genelist_dotplot.pdf',width = 8,height = 5)
#heatmap
all.genes <- rownames(integrated_data_Fib)
integrated_data_Fib$annot<-factor(integrated_data_Fib$annot,levels=rev(levels(integrated_data_Fib$annot)))
Idents(integrated_data_Fib)<-integrated_data_Fib$annot
integrated_data_Fib <- ScaleData(integrated_data_Fib, features = all.genes)
p1<-DoHeatmap(integrated_data_Fib, features = genelist,group.colors = colors) +scale_fill_gradient2(low = "#0000FF",mid = "#FFF8DC",high = "#FF0000")
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/5.annot/genelist_heatmap.png',width = 10,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/5.annot/genelist_heatmap.pdf',width = 10,height = 10)


#cell ratio加显著性值annot
for (each_group in c('group')) {
  cell_number<-read.table(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/3.cell_ratio/cell_number_sample.xls'),header = T,sep = '\t')
  sample_group_data<-unique(integrated_data_Fib@meta.data[,c('orig.ident',each_group)])
  cell_number$group<-mapvalues(cell_number$sample,as.character(sample_group_data$orig.ident),as.character(sample_group_data[,2]))
  cell_number$group<-factor(cell_number$group,levels = levels(integrated_data_Fib@meta.data[,each_group]))
  cell_number$cluster<-factor(cell_number$cluster,levels = levels(integrated_data_Fib$annot))
  my_comparisons <- combn(levels(cell_number$group), 2, simplify = FALSE)
  p1<-ggplot(cell_number , aes(x = group, y = rate, fill=group)) +facet_wrap(~cluster,scales = 'free')+
    geom_bar(stat = "summary", fun = mean, color = "black", position = position_dodge(),width=0.8) +
    #scale_y_continuous(expand = c(0,0))+
    labs(x='',y="Percentage(%)")+ theme_bw()+#scale_fill_manual(values = c("#90BFF9","#FFA0A0"))+
    stat_summary(fun.data = 'mean_se', geom = "errorbar", colour = "black",width = 0.2,position = position_dodge()) +
    geom_point(aes(x = group, y = rate),color='black',position = position_dodge())+
    stat_compare_means(comparisons = my_comparisons)+
    #stat_compare_means(comparisons = my_comparisons,method = 't.test',symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "ns")))+
    theme(axis.title.y = element_text(size=15,color = "black"),
          axis.text = element_text(size=15,color = "black"),
          axis.text.x = element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          strip.background = element_blank(),
          strip.text = element_text(size = 15,color="black"),
          legend.title = element_text(size = 10,color="black"),
          legend.text = element_text(size = 10,color="black"))
  ggsave(p1,filename = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/3.cell_ratio/all_cluster_cell_number_rate_addpvalue.png'),width = 10,height = 10)
  ggsave(p1,filename = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/3.cell_ratio/all_cluster_cell_number_rate_addpvalue.pdf'),width = 10,height = 10)
  write.table(cell_number,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/3.cell_ratio/all_cluster_cell_number_rate.xls'),quote = F,sep = '\t',row.names = F)
}
######monocle3
library(monocle3)
library(SeuratWrappers)
integrated_data_Fib<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
cds <- as.cell_data_set(integrated_data_Fib)
cds@clusters$UMAP$clusters <- Idents(integrated_data_Fib)[rownames(colData(cds))]
#parition的数目根据自己的实际情况处理，如果后面的 learn_graph(cds2, use_partition = F)的use_partition为F，则不用partition来做不同pseudotime推测，没有没有必要设置partition；
cds@clusters$UMAP$partitions <- factor(x = rep(1, length(rownames(colData(cds)))), levels = 1)
names(cds@clusters$UMAP$partitions) <- rownames(colData(cds))
cds <- estimate_size_factors(cds)
## Add gene names into CDS
cds@rowRanges@elementMetadata@listData$gene_short_name <- rownames(cds)
rownames(cds@principal_graph_aux[["UMAP"]]$dp_mst) <- NULL
# colnames(cds@reducedDims$UMAP) <- NULL
colnames(cds@int_colData@listData$reducedDims@listData$UMAP) <- NULL
## Step 5: Learn a graph
cds <- learn_graph(cds)
## Step 6: Order cells
cds <- order_cells(cds,reduction_method = "UMAP")
p1<-plot_cells(cds,color_cells_by = "annot",group_label_size = 5,show_trajectory_graph = TRUE,
               trajectory_graph_color = "#2A788E",
               trajectory_graph_segment_size = 1.5,
               label_branch_points = FALSE,
               label_leaves = FALSE)
#p2<-plot_cells(cds,color_cells_by = "pseudotime",label_cell_groups=F,label_leaves=T,label_branch_points=F,graph_label_size=1.5)
p2<-plot_cells(cds,
               color_cells_by = "pseudotime",
               show_trajectory_graph = TRUE,
               trajectory_graph_color = "#2A788E",
               trajectory_graph_segment_size = 1.5,
               label_branch_points = FALSE,
               label_leaves = FALSE) + theme(legend.position = "right")
ggsave(p1,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/8.monocle3/Fibroblast_monocle3_cluster.png'),width = 6,height = 6)
ggsave(p1,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/8.monocle3/Fibroblast_monocle3_cluster.pdf'),width = 6,height = 6)
ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/8.monocle3/Fibroblast_monocle3_pseudotime.png'),width = 6,height = 5)
ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/8.monocle3/Fibroblast_monocle3_pseudotime.pdf'),width = 6,height = 5)
saveRDS(cds,file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/8.monocle3/Fibroblast_monocle3.rds')
###slingshot
library(slingshot)
integrated_data_Fib<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
integrated_data_Fib_count<-integrated_data_Fib@assays$RNA@counts
sim <- SingleCellExperiment(assays = List(counts = integrated_data_Fib_count))
assays(sim)$norm <- integrated_data_Fib@assays$RNA@data
rd1 <- integrated_data_Fib@reductions$pca@cell.embeddings[,1:2]
rd2 <- integrated_data_Fib@reductions$umap@cell.embeddings[,1:2]
reducedDims(sim) <- SimpleList(PCA = rd1, UMAP = rd2)
#reducedDims(sim) <- SimpleList(PCA = rd1)
cl <- integrated_data_Fib$seurat_clusters
colData(sim)$GMM <- cl
#group <- integrated_data_Fib$group
#colData(sim)$group<-group
sim <- slingshot(sim, clusterLabels = 'GMM',reducedDim = 'UMAP',stretch = 0,extend='n',start.clus='0',end.clus=c('1','3'))
sim2<-SlingshotDataSet(sim)
sim2_pseudotime <- slingPseudotime(sim2, na = FALSE)
sim2_cellWeights <- slingCurveWeights(sim2)
sim2_pseudotime1<-sim2_pseudotime[,-2]
sim2_cellWeights1<-sim2_cellWeights[,-2]
plot(rd2, col = brewer.pal(7,"Set1")[cl], pch=16, asp = 1)
colors <- colorRampPalette(brewer.pal(9,'Spectral'))(100)
plotcol <- colors[cut(sim$slingPseudotime_1, breaks=100)]#颜色
plot(reducedDims(sim)$UMAP, col = plotcol, pch=16, asp = 1)
lines(SlingshotDataSet(sim), lwd=2, col='black',lineages = 1)
#
sim$Pseudotime<-ifelse(!(is.na(sim$slingPseudotime_1)),sim$slingPseudotime_1,sim$slingPseudotime_2)#sim$slingPseudotime
colors <- colorRampPalette(brewer.pal(10,'Spectral'))(100)
plotcol <- colors[cut(sim$Pseudotime, breaks=100)]
pdf(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/10.slingshot/Fibroblast_slingshot_Pseudotime2.pdf',width = 8,height = 6)
plot(reducedDims(sim)$UMAP, col = plotcol, pch=16, asp = 1)
lines(SlingshotDataSet(sim), lwd=2, col=brewer.pal(2,"Set1"))
legend("left",
       legend = paste0("lineage",1:2),
       col = unique(brewer.pal(2,"Set1")),
       inset=0.8,
       pch = 16)
dev.off()
#ggplot2 plot
sim_data<-data.frame(reducedDims(sim)$UMAP)
sim_data$Pseudotime<-ifelse(!(is.na(sim$slingPseudotime_1)),sim$slingPseudotime_1,sim$slingPseudotime_2)#sim$slingPseudotime
sim2<-SlingshotDataSet(sim)
p1<-ggplot(sim_data,aes(x= UMAP_1,y = UMAP_2)) +
  geom_point(aes(color=Pseudotime),size = 0.5,alpha =1)+
  geom_path(aes(x= UMAP_1,y = UMAP_2),data = as.data.frame(sim2@curves$Lineage1$s),arrow = arrow(angle = 15, ends = "last", type = "closed"))+
  geom_path(aes(x= UMAP_1,y = UMAP_2),data = as.data.frame(sim2@curves$Lineage2$s),arrow = arrow(angle = 15, ends = "last", type = "closed"))+
  scale_color_gradientn(colours = rev(brewer.pal(7,'YlOrRd')))+theme_cowplot()
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/10.slingshot/Fibroblast_slingshot_Pseudotime.png',width = 8,height = 6)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/10.slingshot/Fibroblast_slingshot_Pseudotime.pdf',width = 8,height = 6)
saveRDS(sim,file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/10.slingshot/Fibroblast_slingshot.rds')
####monocle2
cds<-readRDS(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/9.monocle2/Fibroblast_monocle.rds')
state_celltype_table <- table(pData(cds)$State, pData(cds)$annot)
plot_data <- as.data.frame(state_celltype_table)
colnames(plot_data) <- c("State", "CellType", "Count") # 重命名列
plot_list <- list()
for(state in unique(plot_data$State)) {
  state_data <- plot_data[plot_data$State == state, ]
  p <- ggplot(state_data, aes(x = "", y = Count, fill = CellType)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = paste("State", state)) +
    theme_void() +
    theme(legend.position = "right")
  plot_list[[as.character(state)]] <- p
}
p1<-grid.arrange(grobs = plot_list, ncol = 3, nrow = 1)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/9.monocle2/State_annot_pie.png',width = 18,height = 6)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/9.monocle2/State_annot_pie.pdf',width = 18,height = 6)
#gene plot
p1<-plot_cell_trajectory(cds,markers = c('Il6','Cd44','Il1r1'),use_color_gradient = T,cell_size = 0.2)+theme(legend.position = 'bottom')+scale_color_gradient(low = 'grey',high = 'red')
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/9.monocle2/Branch1/genelist_plot/Fib_sub_monocle2_Il6_Cd44_Il1r1_expression.png',width = 12,height = 5)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/9.monocle2/Branch1/genelist_plot/Fib_sub_monocle2_Il6_Cd44_Il1r1_expression.pdf',width = 12,height = 5)
p1<-plot_genes_branched_pseudotime(cds[c('Il6','Cd44','Il1r1'),],
                                   branch_point = 1,
                                   color_by = "annot",
                                   ncol = 1)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/9.monocle2/Branch1/genelist_plot/Fib_sub_monocle2_Il6_Cd44_Il1r1_expression_jitter.png',width = 8,height = 12)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/9.monocle2/Branch1/genelist_plot/Fib_sub_monocle2_Il6_Cd44_Il1r1_expression_jitter.pdf',width = 8,height = 12)
###ECM score
integrated_data_Fib<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
signature_file<-read.delim(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/14.signature_score/signature_list.xls',header = T,sep = '\t')
signature_file<-signature_file[signature_file$signature!="Secreted Factors",]
signature_file$signature<-'ECM'
for (each_type in unique(signature_file$signature)) {
  each_signature<-unique(signature_file[signature_file$signature==each_type,]$gene)
  each_type<-gsub(each_type,pattern = ' |-',replacement = '_')
  integrated_data_Fib<-AddModuleScore(integrated_data_Fib,features = list(each_signature),name = paste0(each_type,'_score'))
  colnames(integrated_data_Fib@meta.data)[which(colnames(integrated_data_Fib@meta.data)==paste0(each_type,'_score1'))]<-paste0(each_type,'_score')
  geneset_data<-data.frame(cell=rownames(integrated_data_Fib@meta.data),geneset_score=integrated_data_Fib@meta.data[,paste0(each_type,'_score')],group=integrated_data_Fib$group,cluster=integrated_data_Fib$annot)
  geneset_data$group<-factor(geneset_data$group,levels = levels(integrated_data_Fib$group))
  geneset_data$cluster<-factor(geneset_data$cluster,levels = levels(integrated_data_Fib$annot))
  my_comparisons <- combn(levels(geneset_data$group), 2, simplify = FALSE)
  my_comparisons_cluster <- combn(levels(geneset_data$cluster), 2, simplify = FALSE)
  outdir<-'~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/14.signature_score/'
  p1<-ggplot(geneset_data, aes(x = factor(cluster), y = geneset_score, fill = cluster)) +
    geom_violin() +
    geom_boxplot(width = 0.1, fill = "white") +
    theme_minimal() + theme_bw()+
    stat_compare_means(comparisons = my_comparisons_cluster)+
    #scale_fill_manual(values  = color_panel)+
    theme(legend.position = "none")+ylab(paste0(each_type,' score'))+xlab('')
  p2<-ggplot(geneset_data, aes(x=factor(group), y=geneset_score,fill=group)) +
    geom_violin() +geom_boxplot(width=0.1, fill="white")+
    theme(legend.position = "none")+ylab(paste0(each_type,' score'))+xlab('group')+
    stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
    theme(axis.text.x = element_text(hjust = 1,vjust = 1,angle = 45,size = 12))+
    facet_wrap(~cluster)
  ggsave(p1,filename = paste0(outdir,each_type,'_Fib_score.png'),width = 8,height = 8)
  ggsave(p1,filename = paste0(outdir,each_type,'_Fib_score.pdf'),width = 8,height = 8)
  ggsave(p2,filename = paste0(outdir,each_type,'_Fib_score_group.png'),width = 15,height = 15)
  ggsave(p2,filename = paste0(outdir,each_type,'_Fib_score_group.pdf'),width = 15,height = 15)
  write.table(geneset_data,file = paste0(outdir,each_type,'_Fib_score_group.xls'),quote = F,sep = '\t',row.names = F)
}
###映射亚群
integrated_data<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
integrated_data_Fib<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
integrated_data_M<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/Myeloid_subcluster.rds'))
all_flag<-c()
for (each in rownames(integrated_data@meta.data)) {
  if(each %in% rownames(integrated_data_Fib@meta.data)){
    all_flag<-c(all_flag,as.character(integrated_data_Fib@meta.data[each,]$annot))
  }else if(each %in% rownames(integrated_data_M@meta.data)){
    all_flag<-c(all_flag,as.character(integrated_data_M@meta.data[each,]$annot))
  }else{
    all_flag<-c(all_flag,as.character(integrated_data@meta.data[each,]$annot))
  }
}
integrated_data$subtype<-all_flag
integrated_data$subtype<-factor(integrated_data$subtype,levels = c('PT-S1','PT-S2','PT-S3','LOH','DCT','PC','IC','UC','Endo',levels(integrated_data_Fib$annot),'T','B',levels(integrated_data_M$annot)))
saveRDS(integrated_data,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
#互作对添加Il1b-Il1r1
CellChatDB <- CellChatDB.mouse 
new_db<-CellChatDB$interaction
write.table(new_db,file = '/share/home/liny/Project/personality/script/cellchat/cellchat_update_cellchatDB_mouse.xls',quote = F,sep = '\t',col.names = NA)
new_db2<-read.delim(file = '/share/home/liny/Project/personality/script/cellchat/cellchat_update_cellchatDB_mouse.xls',header=T,sep = '\t',row.names=1)
###Il1b-Il1r1在大群和亚群的表达
integrated_data<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
integrated_data_Fib<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
integrated_data_M<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/Myeloid_subcluster.rds'))
p1<-VlnPlot(integrated_data,features = c('Il1b','Il1r1'),pt.size = 0,group.by = 'annot',cols = color_panel)
integrated_data$annot<-factor(integrated_data$annot,levels = rev(levels(integrated_data$annot)))
Idents(integrated_data)<-integrated_data$annot
p2<-DotPlot(integrated_data,features = c('Il1b','Il1r1'))
integrated_data$group<-factor(integrated_data$group,levels = rev(levels(integrated_data$group)))
Idents(integrated_data)<-integrated_data$group
p3<-DotPlot(integrated_data,features = c('Il1b','Il1r1'))
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b-Il1r1_majortype_violin.png',width = 10,height = 6)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b-Il1r1_majortype_violin.pdf',width = 10,height = 6)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b-Il1r1_majortype_dotplot.png',width = 7,height = 6)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b-Il1r1_majortype_dotplot.pdf',width = 7,height = 6)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b-Il1r1_majortype_group_dotplot.png',width = 7,height = 5)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b-Il1r1_majortype_group_dotplot.pdf',width = 7,height = 5)
p1<-VlnPlot(integrated_data_M,features = c('Il1b'),pt.size = 0,group.by = 'annot')
p11<-VlnPlot(integrated_data_M,features = c('Il1b'),pt.size = 0,group.by = 'group')
integrated_data_M$annot<-factor(integrated_data_M$annot,levels = rev(levels(integrated_data_M$annot)))
Idents(integrated_data_M)<-integrated_data_M$annot
p2<-DotPlot(integrated_data_M,features = c('Il1b'))
integrated_data_M$group<-factor(integrated_data_M$group,levels = rev(levels(integrated_data_M$group)))
Idents(integrated_data_M)<-integrated_data_M$group
p3<-DotPlot(integrated_data_M,features = c('Il1b'))
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b_Myeloid_violin.png',width = 7,height = 6)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b_Myeloid_violin.pdf',width = 7,height = 6)
ggsave(p11,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b_Myeloid_group_violin.png',width = 7,height = 6)
ggsave(p11,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b_Myeloid_group_violin.pdf',width = 7,height = 6)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b_Myeloid_dotplot.png',width = 6,height = 5)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b_Myeloid_dotplot.pdf',width = 6,height = 5)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b_Myeloid_group_dotplot.png',width = 6,height = 5)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1b_Myeloid_group_dotplot.pdf',width = 6,height = 5)
p1<-VlnPlot(integrated_data_Fib,features = c('Il1r1'),pt.size = 0,group.by = 'annot')
p11<-VlnPlot(integrated_data_Fib,features = c('Il1r1'),pt.size = 0,group.by = 'group')
integrated_data_Fib$annot<-factor(integrated_data_Fib$annot,levels = rev(levels(integrated_data_Fib$annot)))
Idents(integrated_data_Fib)<-integrated_data_Fib$annot
p2<-DotPlot(integrated_data_Fib,features = c('Il1r1'))
integrated_data_Fib$group<-factor(integrated_data_Fib$group,levels = rev(levels(integrated_data_Fib$group)))
Idents(integrated_data_Fib)<-integrated_data_Fib$group
p3<-DotPlot(integrated_data_Fib,features = c('Il1r1'))
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1r1_Fibroblast_violin.png',width = 10,height = 6)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1r1_Fibroblast_violin.pdf',width = 10,height = 6)
ggsave(p11,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1r1_Fibroblast_group_violin.png',width = 7,height = 6)
ggsave(p11,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1r1_Fibroblast_group_violin.pdf',width = 7,height = 6)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1r1_Fibroblast_dotplot.png',width = 6,height = 5)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1r1_Fibroblast_dotplot.pdf',width = 6,height = 5)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1r1_Fibroblast_group_dotplot.png',width = 6,height = 5)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/11.Il1b-Il1r1_expression_plot/Il1r1_Fibroblast_group_dotplot.pdf',width = 6,height = 5)
#NLRP家族基因在大群的表达
integrated_data<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
integrated_data_Fib<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
integrated_data_M<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/Myeloid_subcluster.rds'))
genelist<-c('Nlrp1','Nlrp2','Nlrp3','Nlrp4','Nlrp5','Nlrp6','Nlrp7','Nlrp8','Nlrp9','Nlrp10','Nlrp11','Nlrp12','Nlrp13','Nlrp14')
genelist<-genelist[genelist %in% rownames(integrated_data)]
p1<-VlnPlot(integrated_data,features = genelist,pt.size = 0,group.by = 'annot',cols = color_panel)
integrated_data$annot<-factor(integrated_data$annot,levels = rev(levels(integrated_data$annot)))
Idents(integrated_data)<-integrated_data$annot
p2<-DotPlot(integrated_data,features = genelist)
integrated_data$group<-factor(integrated_data$group,levels = rev(levels(integrated_data$group)))
Idents(integrated_data)<-integrated_data$group
p3<-DotPlot(integrated_data,features = genelist)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp_majortype_violin.png',width = 12,height = 6)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp_majortype_violin.pdf',width = 12,height = 6)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp_majortype_dotplot.png',width = 10,height = 6)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp_majortype_dotplot.pdf',width = 10,height = 6)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp_majortype_group_dotplot.png',width = 10,height = 5)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp_majortype_group_dotplot.pdf',width = 10,height = 5)
p1<-VlnPlot(integrated_data_M,features = c('Nlrp3'),pt.size = 0,group.by = 'annot')
p11<-VlnPlot(integrated_data_M,features = c('Nlrp3'),pt.size = 0,group.by = 'group')
integrated_data_M$annot<-factor(integrated_data_M$annot,levels = rev(levels(integrated_data_M$annot)))
Idents(integrated_data_M)<-integrated_data_M$annot
p2<-DotPlot(integrated_data_M,features = c('Nlrp3'))
integrated_data_M$group<-factor(integrated_data_M$group,levels = rev(levels(integrated_data_M$group)))
Idents(integrated_data_M)<-integrated_data_M$group
p3<-DotPlot(integrated_data_M,features = c('Nlrp3'))
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp3_Myeloid_violin.png',width = 7,height = 6)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp3_Myeloid_violin.pdf',width = 7,height = 6)
ggsave(p11,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp3_Myeloid_group_violin.png',width = 7,height = 6)
ggsave(p11,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp3_Myeloid_group_violin.pdf',width = 7,height = 6)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp3_Myeloid_dotplot.png',width = 6,height = 5)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp3_Myeloid_dotplot.pdf',width = 6,height = 5)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp3_Myeloid_group_dotplot.png',width = 6,height = 5)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/12.Nlrp_expression_plot/Nlrp3_Myeloid_group_dotplot.pdf',width = 6,height = 5)


###########两例空转分析
integrated_data_sp<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
integrated_data_sp <- FindClusters(integrated_data_sp, resolution = 0.8)
#Fib+Mac细分，看能否分出Neu
integrated_data_sp_sub<-subset(integrated_data_sp,annot %in% c('Fib','MAC'))
integrated_data_sp_sub <- integrated_data_sp_sub %>% Seurat::NormalizeData(verbose = FALSE) %>% FindVariableFeatures(selection.method = 'vst', nfeatures = 2000) %>% ScaleData(verbose = FALSE) %>% RunPCA(verbose = FALSE)
integrated_data_sp_sub <- integrated_data_sp_sub %>% RunHarmony('orig.ident')
integrated_data_sp_sub <- FindNeighbors(integrated_data_sp_sub, reduction = 'harmony',dims = 1:20)
integrated_data_sp_sub <- FindClusters(integrated_data_sp_sub, resolution = 0.6)
integrated_data_sp_sub <- RunUMAP(integrated_data_sp_sub, reduction = 'harmony', dims = 1:20)
DimPlot(integrated_data_sp_sub,label = T,cols = color_panel,raster = F)
integrated_data_sp_sub@assays$RNA@scale.data<-matrix()
saveRDS(integrated_data_sp_sub,file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/Fib-Mac_sub.rds')
#Endo-Pod细分出来
integrated_data_sp_c10<-subset(integrated_data_sp,seurat_clusters %in% c(10))
integrated_data_sp_c10 <- integrated_data_sp_c10 %>% Seurat::NormalizeData(verbose = FALSE) %>% FindVariableFeatures(selection.method = 'vst', nfeatures = 2000) %>% ScaleData(verbose = FALSE) %>% RunPCA(verbose = FALSE)
integrated_data_sp_c10 <- integrated_data_sp_c10 %>% RunHarmony('orig.ident')
integrated_data_sp_c10 <- FindNeighbors(integrated_data_sp_c10, reduction = 'harmony',dims = 1:25)
integrated_data_sp_c10 <- FindClusters(integrated_data_sp_c10, resolution = 0.8)
integrated_data_sp_c10 <- RunUMAP(integrated_data_sp_c10, reduction = 'harmony', dims = 1:25)
DimPlot(integrated_data_sp_c10,label = T,cols = color_panel,raster = F)
#映射
integrated_data_sp$seurat_clusters<-as.character(integrated_data_sp$seurat_clusters)
integrated_data_sp@meta.data[rownames(integrated_data_sp_c10@meta.data[integrated_data_sp_c10$seurat_clusters %in% c(1,3,4,7,12),]),]$seurat_clusters<-14
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$seurat_clusters %in% c(13,14),]),]$seurat_clusters<-19
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$seurat_clusters %in% c(6,7,8,12),]),]$seurat_clusters<-17
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$seurat_clusters %in% c(10),]),]$seurat_clusters<-20
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$seurat_clusters %in% c(1),]),]$seurat_clusters<-1
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$seurat_clusters %in% c(0),]),]$seurat_clusters<-3
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$seurat_clusters %in% c(2),]),]$seurat_clusters<-5
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$seurat_clusters %in% c(3,4,5,9,11),]),]$seurat_clusters<-2
integrated_data_sp$seurat_clusters<-factor(integrated_data_sp$seurat_clusters,levels = c(0:20))
Idents(integrated_data_sp)<-integrated_data_sp$seurat_clusters
DimPlot(integrated_data_sp,label = T,cols = color_panel,raster = F)
saveRDS(integrated_data_sp,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
plot_cluster_umap(integrated_data_sp,outdir = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/1.cluster',type = 'merge',cols = color_panel,ncol_group = 2,ncol = 2,group = 'group',raster = F,pt.size = 0.4)
###annot
#PT-S1:1 4 PT-S2:3 9 12 15 PT-S3:7 11
p1<-FeaturePlot(integrated_data_sp,features = c('Slc5a12','Slc5a2','Cyp4b1','Cyp2e1','Napsa','Cyp7b1'),ncol=3,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/PT_featureplot.png',width = 15,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/PT_featureplot.pdf',width = 15,height = 10)
#LOH TAL:0 6 TL: DCT:5
p1<-FeaturePlot(integrated_data_sp,features = c('Slc12a1','Umod','Slc12a3','Trpm6'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/LOH_DCT_featureplot.png',width = 10,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/LOH_DCT_featureplot.pdf',width = 10,height = 10)
#PC:13 IC:8
p1<-FeaturePlot(integrated_data_sp,features = c('Aqp2','Aqp3','Atp6v0d2','Atp6v1c2'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/PC_IC_featureplot.png',width = 10,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/PC_IC_featureplot.pdf',width = 10,height = 10)
#UC:16 POD:14
p1<-FeaturePlot(integrated_data_sp,features = c('Krt19','Upk1b','Nphs2','Wt1'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/UC_POD_featureplot.png',width = 10,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/UC_POD_featureplot.pdf',width = 10,height = 10)
#Endo:10
p1<-FeaturePlot(integrated_data_sp,features = c('Pecam1','Ptprb'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/Endo_featureplot.png',width = 10,height = 5)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/Endo_featureplot.pdf',width = 10,height = 5)
#Fib:2 14 Adi:18
p1<-FeaturePlot(integrated_data_sp,features = c('Col1a2','Fbln5','Cfd','Fabp4'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/Fib-Adi_featureplot.png',width = 10,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/Fib-Adi_featureplot.pdf',width = 10,height = 10)
#VMSC:
#p1<-FeaturePlot(integrated_data_sp,features = c('Notch3','Pdgfrb'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
#ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/VSMC_featureplot.png',width = 10,height = 5)
#ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/VSMC_featureplot.pdf',width = 10,height = 5)
#T: B:19 MAC:17 NEU:20
p1<-FeaturePlot(integrated_data_sp,features = c('Il7r','Themis','Ighm','Igkc','Cd86','C1qa','Csf3r','S100a9'),ncol=3,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/Immune_featureplot.png',width = 15,height = 15)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/Immune_featureplot.pdf',width = 15,height = 15)
#celltype
cell_type = c("0"="LOH","1"="PT-S1","2"="Fib","3"="PT-S2","4"="PT-S1","5"="DCT","6"="LOH","7"="PT-S3","8"="IC","9"="PT-S2",
              "10"="POD","11"="PT-S3","12"="PT-S2","13"="PC","14"="Endo","15"="PT-S2","16"="UC","17"="MAC","18"="Adi","19"="B","20"="NEU")
integrated_data_sp$annot_cluster_old<-integrated_data_sp$annot_cluster
integrated_data_sp$annot<-cell_type[integrated_data_sp$seurat_clusters]
integrated_data_sp$annot<-factor(integrated_data_sp$annot,levels = c("PT-S1","PT-S2","PT-S3","LOH","DCT","PC","IC","UC","POD","Endo","Fib","MAC","NEU","B","Adi"))
integrated_data_sp$annot_cluster<-factor(paste0(integrated_data_sp$seurat_clusters,':',integrated_data_sp$annot),levels = paste0(names(cell_type),':',cell_type))
p1<-DimPlot(integrated_data_sp,group.by = 'annot',cols = color_panel,label = T,raster = F,pt.size = 0.5)
p2<-DimPlot(integrated_data_sp,group.by = 'annot_cluster',cols = color_panel,raster = F,label = T,pt.size = 0.5)
p3<-DimPlot(integrated_data_sp,group.by = 'annot',cols = color_panel,split.by = 'group',label = T,raster = F,pt.size = 0.5)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/merge_umap_annot.png',width = 10,height = 8)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/merge_umap_annot.pdf',width = 10,height = 8)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/merge_umap_cluster_annot.png',width = 12,height = 10)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/merge_umap_cluster_annot.pdf',width = 12,height = 10)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/merge_umap_annot_group.png',width = 15,height = 10)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/merge_umap_annot_group.pdf',width = 15,height = 10)
saveRDS(integrated_data_sp,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
#dotplot
genelist<-c('Slc5a12','Slc5a2','Cyp4b1','Cyp2e1','Napsa','Cyp7b1','Slc12a1','Umod','Slc12a3','Trpm6','Aqp2','Aqp3','Atp6v0d2','Atp6v1c2','Krt19','Upk1b','Nphs2','Wt1','Pecam1','Ptprb','Myh11','Col1a2','Irf8','C1qa','Csf3r','S100a9','Igkc','Ighm','Cfd','Fabp4')
integrated_data_sp$annot<-factor(integrated_data_sp$annot,levels=rev(levels(integrated_data_sp$annot)))
Idents(integrated_data_sp)<-integrated_data_sp$annot
p2<-DotPlot(integrated_data_sp,features = c(genelist),cols = c('#009ACD','#FF0000'))+#coord_flip()+
  theme(axis.text.x = element_text(size=12,color = "black", hjust=1,vjust=1,angle=45),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA))+labs(x="",y="")
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/genelist_dotplot.png',width = 15,height = 7)
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/6.singleR/annot/genelist_dotplot.pdf',width = 15,height = 7)
###Loupe
for(each in unique(integrated_data_sp$orig.ident)){
  sp_rds_sub<-subset(integrated_data_sp,orig.ident==each)
  #1.cluster
  cluster_data<-data.frame(Barcode=gsub(rownames(sp_rds_sub@meta.data),pattern = paste0(each,'_'),replacement = ''),celltype=sp_rds_sub$annot)
  write.table(cluster_data,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/7.Loupe/',each,'_annot.csv'),quote = F,sep = ',',row.names = F)
  cluster_data<-data.frame(Barcode=gsub(rownames(sp_rds_sub@meta.data),pattern = paste0(each,'_'),replacement = ''),cluster=sp_rds_sub$seurat_clusters)
  write.table(cluster_data,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/7.Loupe/',each,'_cluster.csv'),quote = F,sep = ',',row.names = F)
}
#######Fib-MAC细分
integrated_data_sp_sub<-readRDS(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/Fib-Mac_sub.rds')
plot_cluster_umap(integrated_data_sp_sub,outdir = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac/1.cluster',type = 'Fib-Mac_sub',ncol_group = 2,ncol = 2,group = 'group',raster = F,pt.size = 0.4)
###RCTD反卷积 loupe
integrated_data_sp<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
integrated_data<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
rctd_file<-read.delim(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/9.RCTD/subtype/RCTD_deconvolution_classification.xls',header = T,sep = '\t',row.names = 1)
rctd_file$spot_class<-NULL
integrated_data_sp<-AddMetaData(integrated_data_sp,rctd_file)
integrated_data_sp@meta.data$subtype_first[integrated_data_sp$annot=='POD']<-'POD'
integrated_data_sp@meta.data$subtype_first[integrated_data_sp$annot=='Adi']<-'Adi'
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$annot=='NEU',]),]$subtype_first<-'Neu'
integrated_data_sp$subtype_first<-factor(integrated_data_sp$subtype_first,levels = c(levels(integrated_data_sp$annot)[1:10],levels(integrated_data$subtype)[10:23],'Adi'))
saveRDS(integrated_data_sp,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
for(each in unique(integrated_data_sp$orig.ident)){
  sp_rds_sub<-subset(integrated_data_sp,orig.ident==each)
  #1.cluster
  cluster_data<-data.frame(Barcode=gsub(rownames(sp_rds_sub@meta.data),pattern = paste0(each,'_'),replacement = ''),subtype=sp_rds_sub$subtype_first)
  write.table(cluster_data,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/7.Loupe/',each,'_subtype_RCTD.csv'),quote = F,sep = ',',row.names = F)
}
#######Fib-MAC周围细胞细分
integrated_data_sp<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
integrated_data_sp_sub<-subset(integrated_data_sp,seurat_clusters %in% c(2,4,6,11,12,15,17,18,19,20))
integrated_data_sp_sub <- integrated_data_sp_sub %>% Seurat::NormalizeData(verbose = FALSE) %>% FindVariableFeatures(selection.method = 'vst', nfeatures = 2000) %>% ScaleData(verbose = FALSE) %>% RunPCA(verbose = FALSE)
integrated_data_sp_sub <- integrated_data_sp_sub %>% RunHarmony('orig.ident')
integrated_data_sp_sub <- FindNeighbors(integrated_data_sp_sub, reduction = 'harmony',dims = 1:20)
integrated_data_sp_sub <- FindClusters(integrated_data_sp_sub, resolution = 1)
integrated_data_sp_sub <- RunUMAP(integrated_data_sp_sub, reduction = 'harmony', dims = 1:20)
#分出Neu
integrated_data_sp_sub_c6<-subset(integrated_data_sp_sub,seurat_clusters==6)
integrated_data_sp_sub_c6 <- FindNeighbors(integrated_data_sp_sub_c6, reduction = 'harmony',dims = 1:20)
integrated_data_sp_sub_c6 <- FindClusters(integrated_data_sp_sub_c6, resolution = 0.5)
integrated_data_sp_sub_c6 <- RunUMAP(integrated_data_sp_sub_c6, reduction = 'harmony', dims = 1:20)
DimPlot(integrated_data_sp_sub_c6,label = T,cols = color_panel,raster = F)
#映射
integrated_data_sp_sub$seurat_clusters<-as.character(integrated_data_sp_sub$seurat_clusters)
integrated_data_sp_sub@meta.data[rownames(integrated_data_sp_sub_c6@meta.data[integrated_data_sp_sub_c6$seurat_clusters==4,]),]$seurat_clusters<-22
integrated_data_sp_sub$seurat_clusters<-factor(integrated_data_sp_sub$seurat_clusters,levels = c(0:22))
Idents(integrated_data_sp_sub)<-integrated_data_sp_sub$seurat_clusters
DimPlot(integrated_data_sp_sub,label = T,cols = color_panel,raster = F)
integrated_data_sp_sub@assays$RNA@scale.data<-matrix()
saveRDS(integrated_data_sp_sub,file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/Fib-Mac_nearby_sub.rds')
plot_cluster_umap(integrated_data_sp_sub,outdir = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/1.cluster',type = 'merge',cols = color_panel,ncol_group = 2,ncol = 2,group = 'group',raster = F,pt.size = 0.4)
###annot
integrated_data_sp_sub<-readRDS(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/Fib-Mac_nearby_sub.rds')
#PT-S1:2 8 PT-S2:0 1 17 12 PT-S3:4 7 15 13
p1<-FeaturePlot(integrated_data_sp_sub,features = c('Slc5a12','Slc5a2','Cyp4b1','Cyp2e1','Napsa','Cyp7b1'),ncol=3,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/PT_featureplot.png',width = 15,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/PT_featureplot.pdf',width = 15,height = 10)
#LOH TAL:3 5 9 10 TL: DCT:
p1<-FeaturePlot(integrated_data_sp_sub,features = c('Slc12a1','Umod'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/LOH_featureplot.png',width = 10,height = 5)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/LOH_featureplot.pdf',width = 10,height = 5)
#Fib:11 16 19 20 21 Adi:18
p1<-FeaturePlot(integrated_data_sp_sub,features = c('Col1a2','Fbln5','Cfd','Fabp4'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/Fib-Adi_featureplot.png',width = 10,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/Fib-Adi_featureplot.pdf',width = 10,height = 10)
#VMSC:
#p1<-FeaturePlot(integrated_data_sp_sub,features = c('Notch3','Pdgfrb'),ncol=2,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
#ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/VSMC_featureplot.png',width = 10,height = 5)
#ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/VSMC_featureplot.pdf',width = 10,height = 5)
#T: B:14 MAC:6 NEU:22
p1<-FeaturePlot(integrated_data_sp_sub,features = c('Il7r','Themis','Ighm','Igkc','Cd86','C1qa','Csf3r','S100a9'),ncol=3,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/Immune_featureplot.png',width = 15,height = 15)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/Immune_featureplot.pdf',width = 15,height = 15)
#celltype
cell_type = c("0"="PT-S2","1"="PT-S2","2"="PT-S1","3"="LOH","4"="PT-S3","5"="LOH","6"="MAC","7"="PT-S3","8"="PT-S1","9"="LOH",
              "10"="LOH","11"="Fib","12"="PT-S2","13"="PT-S3","14"="B","15"="PT-S3","16"="Fib","17"="PT-S2","18"="Adi","19"="Fib","20"="Fib","21"="Fib","22"="NEU")
integrated_data_sp_sub$annot<-cell_type[integrated_data_sp_sub$seurat_clusters]
integrated_data_sp_sub$annot<-factor(integrated_data_sp_sub$annot,levels = c("PT-S1","PT-S2","PT-S3","LOH","Fib","MAC","NEU","B","Adi"))
integrated_data_sp_sub$annot_cluster<-factor(paste0(integrated_data_sp_sub$seurat_clusters,':',integrated_data_sp_sub$annot),levels = paste0(names(cell_type),':',cell_type))
p1<-DimPlot(integrated_data_sp_sub,group.by = 'annot',cols = color_panel,label = T,raster = F,pt.size = 0.5)
p2<-DimPlot(integrated_data_sp_sub,group.by = 'annot_cluster',cols = color_panel,raster = F,label = T,pt.size = 0.5)
p3<-DimPlot(integrated_data_sp_sub,group.by = 'annot',cols = color_panel,split.by = 'group',label = T,raster = F,pt.size = 0.5)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/merge_umap_annot.png',width = 10,height = 8)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/merge_umap_annot.pdf',width = 10,height = 8)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/merge_umap_cluster_annot.png',width = 12,height = 10)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/merge_umap_cluster_annot.pdf',width = 12,height = 10)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/merge_umap_annot_group.png',width = 15,height = 10)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/merge_umap_annot_group.pdf',width = 15,height = 10)
saveRDS(integrated_data_sp_sub,file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/Fib-Mac_nearby_sub.rds')
#dotplot
genelist<-c('Slc5a12','Slc5a2','Cyp4b1','Cyp2e1','Napsa','Cyp7b1','Slc12a1','Umod','Myh11','Col1a2','Irf8','C1qa','Csf3r','S100a9','Igkc','Ighm','Cfd','Fabp4')
integrated_data_sp_sub$annot<-factor(integrated_data_sp_sub$annot,levels=rev(levels(integrated_data_sp_sub$annot)))
Idents(integrated_data_sp_sub)<-integrated_data_sp_sub$annot
p2<-DotPlot(integrated_data_sp_sub,features = c(genelist),cols = c('#009ACD','#FF0000'))+#coord_flip()+
  theme(axis.text.x = element_text(size=12,color = "black", hjust=1,vjust=1,angle=45),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA))+labs(x="",y="")
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/genelist_dotplot.png',width = 12,height = 6)
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/5.annot/genelist_dotplot.pdf',width = 12,height = 6)
###Loupe
for(each in unique(integrated_data_sp_sub$orig.ident)){
  sp_rds_sub<-subset(integrated_data_sp_sub,orig.ident==each)
  #1.cluster
  cluster_data<-data.frame(Barcode=gsub(rownames(sp_rds_sub@meta.data),pattern = paste0(each,'_'),replacement = ''),celltype=sp_rds_sub$annot)
  write.table(cluster_data,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/6.Loupe/',each,'_Fib-Mac_nearby_annot.csv'),quote = F,sep = ',',row.names = F)
  cluster_data<-data.frame(Barcode=gsub(rownames(sp_rds_sub@meta.data),pattern = paste0(each,'_'),replacement = ''),cluster=sp_rds_sub$annot_cluster)
  write.table(cluster_data,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/6.Loupe/',each,'_Fib-Mac_nearby_cluster.csv'),quote = F,sep = ',',row.names = F)
}
###Fib-MAC周围细胞细分亚群映射回大群
integrated_data_sp_sub<-readRDS(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fib-Mac_nearby/Fib-Mac_nearby_sub.rds')
integrated_data_sp<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
integrated_data_sp$seurat_clusters<-as.character(integrated_data_sp$seurat_clusters)
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$annot %in% c('LOH'),]),]$seurat_clusters<-6
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$annot %in% c('PT-S1'),]),]$seurat_clusters<-4
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$annot %in% c('PT-S2'),]),]$seurat_clusters<-12
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$annot %in% c('PT-S3'),]),]$seurat_clusters<-11
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$annot %in% c('Fib'),]),]$seurat_clusters<-2
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$annot %in% c('MAC'),]),]$seurat_clusters<-17
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$annot %in% c('NEU'),]),]$seurat_clusters<-20
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$annot %in% c('B'),]),]$seurat_clusters<-19
integrated_data_sp@meta.data[rownames(integrated_data_sp_sub@meta.data[integrated_data_sp_sub$annot %in% c('Adi'),]),]$seurat_clusters<-18
c15_cell<-rownames(integrated_data_sp@meta.data[integrated_data_sp$annot_cluster_old %in% c('15:PT-S2'),])[rownames(integrated_data_sp@meta.data[integrated_data_sp$annot_cluster_old %in% c('15:PT-S2'),]) %in% rownames(integrated_data_sp@meta.data[integrated_data_sp$seurat_clusters %in% c(12),])]
integrated_data_sp@meta.data[c15_cell,]$seurat_clusters<-15
integrated_data_sp$seurat_clusters<-factor(integrated_data_sp$seurat_clusters,levels = c(0:20))
#挪动POD位置
integrated_data_sp@reductions$umap@cell.embeddings[integrated_data_sp$seurat_clusters %in% c(10),2]<-integrated_data_sp@reductions$umap@cell.embeddings[integrated_data_sp$seurat_clusters %in% c(10),2] +1.5
Idents(integrated_data_sp)<-integrated_data_sp$seurat_clusters
DimPlot(integrated_data_sp,label = T,cols = color_panel,raster = F)
#Mac1 3群Neu映射回大群
integrated_data_sp$seurat_clusters<-as.character(integrated_data_sp$seurat_clusters)
integrated_data_sp@meta.data[rownames(integrated_data_sp_M@meta.data[integrated_data_sp_M$seurat_clusters %in% c(1,3),]),]$seurat_clusters<-20
integrated_data_sp@meta.data[rownames(integrated_data_sp_M@meta.data[integrated_data_sp_M$seurat_clusters %in% c(0,2,4,5,6),]),]$seurat_clusters<-17
integrated_data_sp$seurat_clusters<-factor(integrated_data_sp$seurat_clusters,levels = c(0:20))
Idents(integrated_data_sp)<-integrated_data_sp$seurat_clusters
DimPlot(integrated_data_sp,label = T,cols = color_panel,raster = F)
saveRDS(integrated_data_sp,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
plot_cluster_umap(integrated_data_sp,outdir = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/1.cluster',type = 'merge',cols = color_panel,ncol_group = 2,ncol = 2,group = 'group',raster = F,pt.size = 0.4)
#########空转Fib 细分
integrated_data_sp<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
integrated_data_sp_Fib<-subset(integrated_data_sp,annot %in% c('Fib'))
integrated_data_sp_Fib <- integrated_data_sp_Fib %>% Seurat::NormalizeData(verbose = FALSE) %>% FindVariableFeatures(selection.method = 'vst', nfeatures = 2000) %>% ScaleData(verbose = FALSE) %>% RunPCA(verbose = FALSE)
integrated_data_sp_Fib <- integrated_data_sp_Fib %>% RunHarmony('orig.ident')
integrated_data_sp_Fib <- FindNeighbors(integrated_data_sp_Fib, reduction = 'harmony',dims = 1:20)
integrated_data_sp_Fib <- FindClusters(integrated_data_sp_Fib, resolution = 0.3)
integrated_data_sp_Fib <- RunUMAP(integrated_data_sp_Fib, reduction = 'harmony', dims = 1:20)
DimPlot(integrated_data_sp_Fib,label = T,cols = color_panel,raster = F)
marker_heatmap<-RunFindAllMarkers(integrated_data_sp_Fib,species = 'Mouse',min.pct = 0.25)
integrated_data_sp_Fib@assays$RNA@scale.data<-matrix()
plot_cluster_umap(integrated_data_sp_Fib,outdir = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/1.cluster',type = 'Fibroblast_sub',ncol_group = 2,ncol = 2,group = 'group',raster = F,pt.size = 0.5)
saveRDS(integrated_data_sp_Fib,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
#单细胞marker和空转marker互相验证
integrated_data_sp_Fib<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
integrated_data_Fib<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
sig_marker_file<-read.delim(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/2.markgene/sig_markgene.xls',header = T,sep = '\t')
top <- sig_marker_file %>% group_by(cluster) %>% top_n(n = 25, wt = avg_log2FC)
Idents(integrated_data_sp_Fib)<-integrated_data_sp_Fib$seurat_clusters
for(each in levels(integrated_data_Fib$annot_cluster)){
  each1<-str_split(each,pattern = ':')[[1]][1]
  each2<-str_split(each,pattern = ':')[[1]][2]
  p2<-DotPlot(integrated_data_sp_Fib,features = top[top$cluster==each1,]$gene,cols = c('#009ACD','#FF0000'))+#coord_flip()+
    theme(axis.text.x = element_text(size=12,color = "black", hjust=1,vjust=1,angle=45))+labs(x="",y="")+ggtitle(label = paste0('Sc','-',each2))
  ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/6.sc-sp_marker_match_plot/Sc-',each2,'_marker_spatial_dotplot.png'),width = 10,height = 5)
  ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/6.sc-sp_marker_match_plot/Sc-',each2,'_marker_spatial_dotplot.pdf'),width = 10,height = 5)
}
sig_marker_file<-read.delim(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/2.markgene/sig_markgene.xls',header = T,sep = '\t')
top <- sig_marker_file %>% group_by(cluster) %>% top_n(n = 25, wt = avg_log2FC)
Idents(integrated_data_Fib)<-integrated_data_Fib$annot
for(each in levels(integrated_data_sp_Fib$seurat_clusters)){
  p2<-DotPlot(integrated_data_Fib,features = top[top$cluster==each,]$gene,cols = c('#009ACD','#FF0000'))+#coord_flip()+
    theme(axis.text.x = element_text(size=12,color = "black", hjust=1,vjust=1,angle=45))+labs(x="",y="")+ggtitle(label = paste0('Sp','-c',each))
  ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/6.sc-sp_marker_match_plot/Sp-c',each,'_marker_sc_dotplot.png'),width = 12,height = 5)
  ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/6.sc-sp_marker_match_plot/Sp-c',each,'_marker_sc_dotplot.pdf'),width = 12,height = 5)
}
#annot
integrated_data_sp_Fib<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
#Fbln5+Fib:0 Negr1+Fib:2 Lepr+Fib:3 Il6+Fib:1
#p1<-FeaturePlot(integrated_data_sp_Fib,features = c('Fbln5','Slc40a1','Adamts5','Negr1','Hmcn1','Sh3gl2','Lepr','Adgrl3','Tnn','Il6','Cxcl10','Cd44'),ncol=6,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
p1<-FeaturePlot(integrated_data_sp_Fib,features = c('Abca8a','Fhl2','Fbln5','Negr1','Hmcn1','Sh3gl2','Igfbp5','Tnc','Tnn','Il6','Cd44','Csmd1'),ncol=6,raster=FALSE,label = T) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/5.annot/Fibroblast_featureplot.png',width = 30,height = 10)
ggsave(p1,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/5.annot/Fibroblast_featureplot.pdf',width = 30,height = 10)
#celltype
cell_type = c("0"="Adherent fibroblast","1"="Inflammatory fibroblast","2"="Pro-resolving fibroblast","3"="Pro-resolving fibroblast","4"="Mesenchymal fibroblast","5"="Pro-resolving fibroblast","6"="Inflammatory fibroblast")
integrated_data_sp_Fib$annot<-cell_type[integrated_data_sp_Fib$seurat_clusters]
integrated_data_sp_Fib$annot<-factor(integrated_data_sp_Fib$annot,levels = c("Mesenchymal fibroblast","Adherent fibroblast","Pro-resolving fibroblast","Inflammatory fibroblast"))
integrated_data_sp_Fib$annot_cluster<-factor(paste0(integrated_data_sp_Fib$seurat_clusters,':',integrated_data_sp_Fib$annot),levels = paste0(names(cell_type),':',cell_type))
p1<-DimPlot(integrated_data_sp_Fib,group.by = 'annot',label = T,raster = F,pt.size = 0.5)
p2<-DimPlot(integrated_data_sp_Fib,group.by = 'annot_cluster',raster = F,label = T,pt.size = 0.5)
p3<-DimPlot(integrated_data_sp_Fib,group.by = 'annot',split.by = 'group',label = T,raster = F,pt.size = 0.5)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/5.annot/Fibroblast_umap_annot.png',width = 8,height = 6)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/5.annot/Fibroblast_umap_annot.pdf',width = 8,height = 6)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/5.annot/Fibroblast_umap_cluster_annot.png',width = 10,height = 8)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/5.annot/Fibroblast_umap_cluster_annot.pdf',width = 10,height = 8)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/5.annot/Fibroblast_umap_annot_group.png',width = 12,height = 10)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/5.annot/Fibroblast_umap_annot_group.pdf',width = 12,height = 10)
saveRDS(integrated_data_sp_Fib,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
#dotplot
genelist<-c('Abca8a','Fhl2','Fbln5','Negr1','Hmcn1','Sh3gl2','Igfbp5','Tnc','Tnn','Il6','Cd44','Csmd1')
integrated_data_sp_Fib$annot<-factor(integrated_data_sp_Fib$annot,levels=rev(levels(integrated_data_sp_Fib$annot)))
Idents(integrated_data_sp_Fib)<-integrated_data_sp_Fib$annot
p2<-DotPlot(integrated_data_sp_Fib,features = c(genelist),cols = c('#009ACD','#FF0000'))+#coord_flip()+
  theme(axis.text.x = element_text(size=12,color = "black", hjust=1,vjust=1,angle=45))+labs(x="",y="")
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/5.annot/genelist_dotplot.png',width = 8,height = 5)
ggsave(p2,filename = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/5.annot/genelist_dotplot.pdf',width = 8,height = 5)
###Loupe
for(each in unique(integrated_data_sp_Fib$orig.ident)){
  sp_rds_sub<-subset(integrated_data_sp_Fib,orig.ident==each)
  #1.cluster
  cluster_data<-data.frame(Barcode=gsub(rownames(sp_rds_sub@meta.data),pattern = paste0(each,'_'),replacement = ''),cluster=sp_rds_sub$seurat_clusters)
  write.table(cluster_data,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/7.Loupe/',each,'_Fib_subcluster.csv'),quote = F,sep = ',',row.names = F)
  cluster_data<-data.frame(Barcode=gsub(rownames(sp_rds_sub@meta.data),pattern = paste0(each,'_'),replacement = ''),cluster=sp_rds_sub$annot)
  write.table(cluster_data,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/7.Loupe/',each,'_Fib_celltype.csv'),quote = F,sep = ',',row.names = F)
}

#########空转巨噬中性粒细分
integrated_data_sp<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
integrated_data_sp_M<-subset(integrated_data_sp,annot %in% c('MAC','NEU'))
#integrated_data_sp_M <- integrated_data_sp_M %>% Seurat::NormalizeData(verbose = FALSE) %>% FindVariableFeatures(selection.method = 'vst', nfeatures = 2000) %>% ScaleData(verbose = FALSE) %>% RunPCA(verbose = FALSE)
#integrated_data_sp_M <- integrated_data_sp_M %>% RunHarmony('orig.ident')
integrated_data_sp_M <- FindNeighbors(integrated_data_sp_M, reduction = 'harmony',dims = 1:20)
integrated_data_sp_M <- FindClusters(integrated_data_sp_M, resolution = 0.4)
integrated_data_sp_M <- RunUMAP(integrated_data_sp_M, reduction = 'harmony', dims = 1:20)
DimPlot(integrated_data_sp_M,label = T,cols = color_panel,raster = F)
#c0细分
integrated_data_sp_M_c0<-subset(integrated_data_sp_M,seurat_clusters==0)
integrated_data_sp_M_c0 <- integrated_data_sp_M_c0 %>% Seurat::NormalizeData(verbose = FALSE) %>% FindVariableFeatures(selection.method = 'vst', nfeatures = 1000) %>% ScaleData(verbose = FALSE) %>% RunPCA(verbose = FALSE)
integrated_data_sp_M_c0 <- integrated_data_sp_M_c0 %>% RunHarmony('orig.ident')
integrated_data_sp_M_c0 <- FindNeighbors(integrated_data_sp_M_c0, reduction = 'pca',dims = 1:20)
integrated_data_sp_M_c0 <- FindClusters(integrated_data_sp_M_c0, resolution = 0.3)
integrated_data_sp_M_c0 <- RunUMAP(integrated_data_sp_M_c0, reduction = 'pca', dims = 1:20)
DimPlot(integrated_data_sp_M_c0,label = T,raster = F)
marker_heatmap<-RunFindAllMarkers(integrated_data_sp_M_c0,species = 'Mouse',min.pct = 0.25)
#映射
integrated_data_sp_M$seurat_clusters<-as.character(integrated_data_sp_M$seurat_clusters)
integrated_data_sp_M@meta.data[rownames(integrated_data_sp_M_c0@meta.data[integrated_data_sp_M_c0$seurat_clusters==1,]),]$seurat_clusters<-6
integrated_data_sp_M$seurat_clusters<-factor(integrated_data_sp_M$seurat_clusters)
integrated_data_sp_M@reductions$umap@cell.embeddings[integrated_data_sp_M$seurat_clusters %in% c(0),2]<-integrated_data_sp_M@reductions$umap@cell.embeddings[integrated_data_sp_M$seurat_clusters %in% c(0),2] -1
integrated_data_sp_M@reductions$umap@cell.embeddings[integrated_data_sp_M$seurat_clusters %in% c(6),2]<-integrated_data_sp_M@reductions$umap@cell.embeddings[integrated_data_sp_M$seurat_clusters %in% c(6),2] +1.5
Idents(integrated_data_sp_M)<-integrated_data_sp_M$seurat_clusters
DimPlot(integrated_data_sp_M,label = T,raster = F)
marker_heatmap<-RunFindAllMarkers(integrated_data_sp_M,species = 'Mouse',min.pct = 0.25)
integrated_data_sp_M@assays$RNA@scale.data<-matrix()
plot_cluster_umap(integrated_data_sp_M,outdir = '/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/1.cluster',type = 'MAC-NEU_sub',ncol_group = 2,ncol = 2,group = 'group',raster = F,pt.size = 0.5)
saveRDS(integrated_data_sp_M,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/MAC-NEU_subcluster.rds'))
#单细胞marker和空转marker互相验证
integrated_data_sp_M<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/MAC-NEU_subcluster.rds'))
integrated_data_M<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/Myeloid_subcluster.rds'))
sig_marker_file<-read.delim(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Myeloid/2.markgene/sig_markgene.xls',header = T,sep = '\t')
top <- sig_marker_file %>% group_by(cluster) %>% top_n(n = 25, wt = avg_log2FC)
Idents(integrated_data_sp_M)<-integrated_data_sp_M$seurat_clusters
for(each in levels(integrated_data_M$annot_cluster)){
  each1<-str_split(each,pattern = ':')[[1]][1]
  each2<-str_split(each,pattern = ':')[[1]][2]
  p2<-DotPlot(integrated_data_sp_M,features = top[top$cluster==each1,]$gene,cols = c('#009ACD','#FF0000'))+#coord_flip()+
    theme(axis.text.x = element_text(size=12,color = "black", hjust=1,vjust=1,angle=45))+labs(x="",y="")+ggtitle(label = paste0('Sc','-',each2))
  ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/6.sc-sp_marker_match_plot/Sc-',each2,'_marker_spatial_dotplot.png'),width = 10,height = 5)
  ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/6.sc-sp_marker_match_plot/Sc-',each2,'_marker_spatial_dotplot.pdf'),width = 10,height = 5)
}
sig_marker_file<-read.delim(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/2.markgene/sig_markgene.xls',header = T,sep = '\t')
top <- sig_marker_file %>% group_by(cluster) %>% top_n(n = 25, wt = avg_log2FC)
Idents(integrated_data_M)<-integrated_data_M$annot
for(each in levels(integrated_data_sp_M$seurat_clusters)){
  p2<-DotPlot(integrated_data_M,features = top[top$cluster==each,]$gene,cols = c('#009ACD','#FF0000'))+#coord_flip()+
    theme(axis.text.x = element_text(size=12,color = "black", hjust=1,vjust=1,angle=45))+labs(x="",y="")+ggtitle(label = paste0('Sp','-c',each))
  ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/6.sc-sp_marker_match_plot/Sp-c',each,'_marker_sc_dotplot.png'),width = 10,height = 6)
  ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/6.sc-sp_marker_match_plot/Sp-c',each,'_marker_sc_dotplot.pdf'),width = 10,height = 6)
}
#celltype
integrated_data_sp_M<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/MAC-NEU_subcluster.rds'))
cell_type = c("0"="Ccl12+Mac","1"="Neu","2"="Vegfa+Mac","3"="Neu","4"="Adgre1+Mac","5"="Clec4e+Mac","6"="Ccr2+DC")
integrated_data_sp_M$annot<-cell_type[integrated_data_sp_M$seurat_clusters]
integrated_data_sp_M$annot<-factor(integrated_data_sp_M$annot,levels = c("Ccl12+Mac","Adgre1+Mac","Vegfa+Mac","Clec4e+Mac","Neu","Ccr2+DC"))
integrated_data_sp_M$annot_cluster<-factor(paste0(integrated_data_sp_M$seurat_clusters,':',integrated_data_sp_M$annot),levels = paste0(names(cell_type),':',cell_type))
p1<-DimPlot(integrated_data_sp_M,group.by = 'annot',label = T,raster = F,pt.size = 0.5)
p2<-DimPlot(integrated_data_sp_M,group.by = 'annot_cluster',raster = F,label = T,pt.size = 0.5)
p3<-DimPlot(integrated_data_sp_M,group.by = 'annot',split.by = 'group',label = T,raster = F,pt.size = 0.5)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/5.annot/MAC-NEU_umap_annot.png',width = 8,height = 6)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/5.annot/MAC-NEU_umap_annot.pdf',width = 8,height = 6)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/5.annot/MAC-NEU_umap_cluster_annot.png',width = 10,height = 8)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/5.annot/MAC-NEU_umap_cluster_annot.pdf',width = 10,height = 8)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/5.annot/MAC-NEU_umap_annot_group.png',width = 12,height = 10)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/5.annot/MAC-NEU_umap_annot_group.pdf',width = 12,height = 10)
saveRDS(integrated_data_sp_M,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/MAC-NEU_subcluster.rds'))
###Loupe
for(each in unique(integrated_data_sp_M$orig.ident)){
  sp_rds_sub<-subset(integrated_data_sp_M,orig.ident==each)
  #1.cluster
  cluster_data<-data.frame(Barcode=gsub(rownames(sp_rds_sub@meta.data),pattern = paste0(each,'_'),replacement = ''),cluster=sp_rds_sub$seurat_clusters)
  write.table(cluster_data,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/7.Loupe/',each,'_Mac-Neu_subcluster.csv'),quote = F,sep = ',',row.names = F)
  cluster_data<-data.frame(Barcode=gsub(rownames(sp_rds_sub@meta.data),pattern = paste0(each,'_'),replacement = ''),cluster=sp_rds_sub$annot)
  write.table(cluster_data,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/7.Loupe/',each,'_Mac-Neu_celltype.csv'),quote = F,sep = ',',row.names = F)
}
########映射回大群
integrated_data_sp<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
integrated_data_sp_Fib<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
integrated_data_sp_M<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/8.subcluster/MAC-NEU/MAC-NEU_subcluster.rds'))
integrated_data_sp$subtype<-as.character(integrated_data_sp$annot)
integrated_data_sp@meta.data[rownames(integrated_data_sp_Fib@meta.data[integrated_data_sp_Fib$annot=='Mesenchymal fibroblast',]),]$subtype<-'Mesenchymal fibroblast'
integrated_data_sp@meta.data[rownames(integrated_data_sp_Fib@meta.data[integrated_data_sp_Fib$annot=='Adherent fibroblast',]),]$subtype<-'Adherent fibroblast'
integrated_data_sp@meta.data[rownames(integrated_data_sp_Fib@meta.data[integrated_data_sp_Fib$annot=='Pro-resolving fibroblast',]),]$subtype<-'Pro-resolving fibroblast'
integrated_data_sp@meta.data[rownames(integrated_data_sp_Fib@meta.data[integrated_data_sp_Fib$annot=='Inflammatory fibroblast',]),]$subtype<-'Inflammatory fibroblast'
integrated_data_sp@meta.data[rownames(integrated_data_sp_M@meta.data[integrated_data_sp_M$annot=='Ccl12+Mac',]),]$subtype<-'Ccl12+Mac'
integrated_data_sp@meta.data[rownames(integrated_data_sp_M@meta.data[integrated_data_sp_M$annot=='Adgre1+Mac',]),]$subtype<-'Adgre1+Mac'
integrated_data_sp@meta.data[rownames(integrated_data_sp_M@meta.data[integrated_data_sp_M$annot=='Vegfa+Mac',]),]$subtype<-'Vegfa+Mac'
integrated_data_sp@meta.data[rownames(integrated_data_sp_M@meta.data[integrated_data_sp_M$annot=='Clec4e+Mac',]),]$subtype<-'Clec4e+Mac'
integrated_data_sp@meta.data[rownames(integrated_data_sp_M@meta.data[integrated_data_sp_M$annot=='Neu',]),]$subtype<-'Neu'
integrated_data_sp@meta.data[rownames(integrated_data_sp_M@meta.data[integrated_data_sp_M$annot=='Ccr2+DC',]),]$subtype<-'Ccr2+DC'
integrated_data_sp$subtype<-factor(integrated_data_sp$subtype,levels = c('PT-S1','PT-S2','PT-S3','LOH','DCT','PC','IC','UC','POD','Endo',levels(integrated_data_sp_Fib$annot),levels(integrated_data_sp_M$annot),'B','Adi'))
saveRDS(integrated_data_sp,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
write.csv(integrated_data_sp@meta.data[,c('orig.ident','group','annot','subtype')],file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/allcell_meta.csv',quote = F)
#loupe
for(each in unique(integrated_data_sp$orig.ident)){
  sp_rds_sub<-subset(integrated_data_sp,orig.ident==each)
  #1.cluster
  cluster_data<-data.frame(Barcode=gsub(rownames(sp_rds_sub@meta.data),pattern = paste0(each,'_'),replacement = ''),subtype=sp_rds_sub$subtype)
  write.table(cluster_data,file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/7.Loupe/',each,'_subtype.csv'),quote = F,sep = ',',row.names = F,col.names=F)
}
#nearb Inflammatory fibroblast 
integrated_data_sp<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
for (each in c('18m-F953','18m-G760')) {
  celltype_data <- integrated_data_sp@meta.data[integrated_data_sp$orig.ident==each,c('subtype','group')]
  celltype_data$cellid<-gsub(rownames(celltype_data),pattern = paste0(each,'_'),replacement = '')
  celltype_data$group<-NULL
  sample_nearby <- readRDS(file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/12.cell_nearby/',each,'_cell_nearby.rds'))
  cell_segmentation <- st_read(paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Spatial_data3/',each,'/outs/segmented_outputs/cell_segmentations.geojson'))
  cell_segmentation$num <- row.names(cell_segmentation)
  celltype_data$ID <- gsub(x = celltype_data$cellid,pattern = '^cellid_[0]+',replacement = '')
  celltype_data$ID <- gsub(x = celltype_data$ID,pattern = '-1',replacement = '')
  each_celltype_data <- celltype_data[celltype_data$subtype == 'Inflammatory fibroblast',]
  each_cell_segmentation <- cell_segmentation[cell_segmentation$cell_id %in% each_celltype_data$ID,]
  whole = c()
  for (key1 in each_cell_segmentation$num){
    cells1 <- sample_nearby[[as.integer(key1)]]
    self1 <- cell_segmentation[key1,]
    nighbor <- cell_segmentation[cells1,]
    whole <- c(whole,nighbor$cell_id,self1$cell_id)
  }
  whole <- unique(whole)
  all_celltype_data <- celltype_data[celltype_data$ID %in% whole,]
  write.table(all_celltype_data[,c(2,1)],file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/12.cell_nearby/nearby_Inflammatory fibroblast_',each,'_subtype.csv'),quote = F,sep = ',',row.names = F,col.names=T)
}
#plot Inflammatory fibroblast
integrated_data_sp<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
colors <- color_panel[1:length(levels(integrated_data_sp$annot))]
names(colors)<-levels(integrated_data_sp$annot)
colors<-c(colors,'Mesenchymal fibroblast'='#712820','Inflammatory fibroblast'='#9400D3','Adherent fibroblast'='#E76A29','Pro-resolving fibroblast'='#BC8F8F',"Ccl12+Mac"="#C71585","Adgre1+Mac"="#0000CD","Vegfa+Mac"="#40E0D0","Clec4e+Mac"="#3A6963","Neu"="#F7F398","Ccr2+DC"="#CB7AA5")
all_ratio_inceltype<-data.frame()
all_ratio_insample<-data.frame()
for (each in c('18m-F953','18m-G760')) {
  celltype_data = integrated_data_sp@meta.data[integrated_data_sp$orig.ident==each,c('subtype','group')]
  celltype_data$cellid<-gsub(rownames(celltype_data),pattern = paste0(each,'_'),replacement = '')
  nighbor_celltype_data<-read.csv(file = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/12.cell_nearby/nearby_Inflammatory fibroblast_',each,'_subtype.csv'),header = T)
  celltype_data$subtype <-factor(celltype_data$subtype,levels = levels(integrated_data_sp$subtype))
  celltype_data$subtype <-factor(celltype_data$subtype,levels = levels(celltype_data$subtype)[-c(which(levels(celltype_data$subtype) %in% c('Inflammatory fibroblast','UC','Vegfa+Mac','Ccr2+DC','Adi','PC')))])
  nighbor_celltype_data<-nighbor_celltype_data[!(nighbor_celltype_data$subtype %in% c('Inflammatory fibroblast','UC','Vegfa+Mac','Ccr2+DC','Adi','PC')),]
  celltype_data<-celltype_data[!(celltype_data$subtype %in% c('Inflammatory fibroblast','UC','Vegfa+Mac','Ccr2+DC','Adi','PC')),]
  nighbor_celltype_data$subtype<-factor(nighbor_celltype_data$subtype,levels = levels(celltype_data$subtype ))
  each_ratio_inceltype<-data.frame(table(nighbor_celltype_data$subtype)/table(celltype_data$subtype))
  colnames(each_ratio_inceltype)<-c('celltype','rate')
  each_ratio_inceltype$cell_number<-table(nighbor_celltype_data$subtype)
  each_ratio_inceltype$sample<-each
  all_ratio_inceltype<-rbind(all_ratio_inceltype,each_ratio_inceltype)
  each_ratio_insample<-data.frame(table(nighbor_celltype_data$subtype)/nrow(nighbor_celltype_data))
  colnames(each_ratio_insample)<-c('celltype','rate')
  each_ratio_insample$cell_number<-table(nighbor_celltype_data$subtype)
  each_ratio_insample$sample<-each
  all_ratio_insample<-rbind(all_ratio_insample,each_ratio_insample)
}
all_ratio_inceltype$group<-ifelse(all_ratio_inceltype$sample=='18m-F953','Mock','Microplastics')
all_ratio_inceltype$sample<-factor(all_ratio_inceltype$sample,levels = unique(all_ratio_inceltype$sample))
all_ratio_inceltype$group<-factor(all_ratio_inceltype$group,levels = levels(integrated_data_sp$group))
all_ratio_insample$group<-ifelse(all_ratio_insample$sample=='18m-F953','Mock','Microplastics')
all_ratio_insample$sample<-factor(all_ratio_insample$sample,levels = unique(all_ratio_insample$sample))
all_ratio_insample$group<-factor(all_ratio_insample$group,levels = levels(integrated_data_sp$group))
all_ratio_inceltype$Proportion<-100*all_ratio_inceltype$rate
all_ratio_inceltype$Proportion<-round(all_ratio_inceltype$Proportion,digits = 2)
p1<-ggplot(all_ratio_inceltype, aes(x = group, y = Proportion,fill = celltype)) +
  geom_col(position = "dodge") +scale_fill_manual(values = colors)+
  geom_text(aes(label = cell_number), 
            vjust = -0.5, size = 3) +   # 在柱子上方标注数量
  theme_classic() +#theme(axis.text.x = element_text(hjust = 1,vjust = 1,angle = 45,size=12),
  #     axis.text.y = element_text(size=12,color = "black"))+
  labs(y = "neighbour Inflammatory fibroblast proportion in total celltype (%)", x = "")+facet_wrap(~celltype)
all_ratio_inceltype$celltype<-factor(all_ratio_inceltype$celltype,levels = names(sort(tapply(all_ratio_inceltype$Proportion,list(all_ratio_inceltype$celltype), mean),decreasing = T)))
p2<-ggplot(all_ratio_inceltype, aes(x = group, y = Proportion,fill = celltype)) +
  geom_col(position = "dodge") +scale_fill_manual(values = colors)+
  geom_text(aes(label = Proportion), 
            vjust = -0.5, size = 3) +   # 在柱子上方标注数量
  theme_classic() +#theme(axis.text.x = element_text(hjust = 1,vjust = 1,angle = 45,size=12),
  #      axis.text.y = element_text(size=12,color = "black"))+
  labs(y = "neighbour Inflammatory fibroblast proportion in total celltype (%)", x = "")+facet_wrap(~celltype)
all_ratio_inceltype$celltype<-factor(all_ratio_inceltype$celltype,levels = names(sort(tapply(all_ratio_inceltype$cell_number,list(all_ratio_inceltype$celltype), mean),decreasing = T)))
p3<-ggplot(all_ratio_inceltype, aes(x = group, y = cell_number ,fill = celltype)) +
  geom_col(position = "dodge") +scale_fill_manual(values = colors)+
  geom_text(aes(label = cell_number ), 
            vjust = -0.5, size = 3) +   # 在柱子上方标注数量
  theme_classic() +#theme(axis.text.x = element_text(hjust = 1,vjust = 1,angle = 45,size=12),
  #      axis.text.y = element_text(size=12,color = "black"))+
  labs(y = "neighbour Inflammatory fibroblast cellnumber in total celltype", x = "")+facet_wrap(~celltype)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/12.cell_nearby/neighbour_Inflammatory fibroblast_in_total_subtype.png',width = 15,height = 18)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/12.cell_nearby/neighbour_Inflammatory fibroblast_in_total_subtype.pdf',width = 15,height = 18)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/12.cell_nearby/Inflammatory fibroblast_in_total_subtype_rate.png',width = 15,height = 18)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/12.cell_nearby/Inflammatory fibroblast_in_total_subtype_rate.pdf',width = 15,height = 18)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/12.cell_nearby/Inflammatory fibroblast_in_total_subtype_number.png',width = 15,height = 18)
ggsave(p3,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/12.cell_nearby/Inflammatory fibroblast_in_total_subtype_number.pdf',width = 15,height = 18)
write.table(all_ratio_inceltype,file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/12.cell_nearby/Inflammatory fibroblast_in_total_subtype.xls',sep = '\t',quote = F,row.names = F)
all_ratio_insample$rate<-100*all_ratio_insample$rate
p2<-ggplot(all_ratio_insample, aes(x =group, y= rate, fill = celltype)) + 
  #geom_bar(stat = 'identity',width = 0.5,position = "fill", col='black')+
  geom_col(width = 0.5,col='black') +scale_y_continuous(limits = c(0, 100))+
  theme_classic() +#theme(axis.text.x = element_text(hjust = 1,vjust = 1,angle = 45,size=12),
  #      axis.text.y = element_text(size=12,color = "black"))+
  labs(y='neighbour Inflammatory fibroblast proportion(%)',x='',fill="celltype")+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = colors)
ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/12.cell_nearby/neighbour_Inflammatory fibroblast_subtype_rate.png'),width = 8,height = 6)
ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/12.cell_nearby/neighbour_Inflammatory fibroblast_subtype_rate.pdf'),width = 8,height = 6)
write.table(all_ratio_insample,file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/12.cell_nearby/neighbour_Inflammatory fibroblast_subtype.xls',sep = '\t',quote = F,row.names = F)


###########Paper 作图
####Fig1
integrated_data<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
Idents(integrated_data)<-integrated_data$orig.ident
p1<-VlnPlot(integrated_data,features = c('nFeature_RNA','nCount_RNA','percent.mt'),ncol = 1,pt.size = 0,raster = F)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/scRNA-gene_umi_percent.mt_violin.png',width = 10,height = 15)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/scRNA-gene_umi_percent.mt_violin.pdf',width = 10,height = 15)
Idents(integrated_data)<-integrated_data$annot
color_panel <-c('#53A85F','#58A4C3','#AB3282','#8C549C','#BD956A','#57C3F3','#6778AE','#F3B1A0','#F1BB72',
                '#DCC1DD','#E95C59','#625D9E','#F7F398','#E63863','#5F3D69','#C5DEBA','#CCE0F5','#B53E2B',
                '#AA9A59','#E39A35','#91D0BE','#23452F','#E4C755','#585658','#C1E6F3','#D6E7A3','#712820',
                '#CCC9E6','#3A6963','#68A180','#61B934','#476D87','#9FA3A8','#968175','#BC8F8F','#D8BFD8')
#colors <- color_panel[1:length(levels(integrated_data$annot))]
#names(colors)<-levels(integrated_data$annot)
#colors<-c(colors,'POD'='#23452F','Adi'='#CCE0F5')
colors = c('PT-S1'= '#045DFC','PT-S2'= '#81F7A6','PT-S3'= '#CE4A7F','LOH'= '#FC8814','DCT'= '#7DFBFB',
  'PC'= '#90683C','IC'= '#915D95','UC'= '#9CC4C2','Endo'= '#9B23D7','Fib'= '#FFFF00','T'='#FE48FF','B'= '#907079','MAC'= '#6D9E62',
  'NEU'= '#C84036','POD'= '#25FB44','Adi'= '#8FA759')
p1<-DimPlot(integrated_data,cols = colors,label = T,raster = F,pt.size = 0.4)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sc_merge_umap_annot.pdf',width = 10,height = 8)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sc_merge_umap_annot.png',width = 10,height = 8)
#dotplot
genelist<-c('Slc5a2','Cyp2e1','Cyp7b1','Umod','Slc12a3','Aqp2','Atp6v0d2','Ppl','Ptprb','Col1a2','Il7r','Bank1','Mrc1','S100a9')
integrated_data$annot<-factor(integrated_data$annot,levels=rev(levels(integrated_data$annot)))
Idents(integrated_data)<-integrated_data$annot
p2<-DotPlot(integrated_data,features = c(genelist),cols = c('#D3D3D3','#FF0000'))+#coord_flip()+#'#009ACD','#FF0000'
  theme(axis.text.x = element_text(size=12,color = "black", hjust=1,vjust=1,angle=45))+labs(x="",y="")
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sc_genelist_dotplot.pdf',width = 10,height = 6)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sc_genelist_dotplot.png',width = 10,height = 6)
#violin
integrated_data$annot<-factor(integrated_data$annot,levels=rev(levels(integrated_data$annot)))
Idents(integrated_data)<-integrated_data$annot
p2<-VlnPlot(integrated_data,features = genelist,pt.size =0,stack =TRUE,flip =TRUE,cols = as.character(colors[levels(integrated_data$annot)]))+theme(legend.position = "none")+labs(x='')
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sc_genelist_violinplot.pdf',width = 10,height = 8)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sc_genelist_violinplot.png',width = 10,height = 8)
#heatmap
all.genes <- rownames(integrated_data)
Idents(integrated_data)<-integrated_data$annot
integrated_data<-subset(integrated_data,downsample=5000)
integrated_data <- ScaleData(integrated_data, features = all.genes)
p1<-DoHeatmap(integrated_data, features = genelist,group.colors = colors) +scale_fill_gradient2(low = "#0000FF",mid = "#FFF8DC",high = "#FF0000")
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sc_genelist_heatmap.pdf',width = 12,height = 7)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sc_genelist_heatmap.png',width = 12,height = 7)
###空转
integrated_data_sp<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
Idents(integrated_data_sp)<-integrated_data_sp$orig.ident
p1<-VlnPlot(integrated_data_sp,features = c('nFeature_RNA','nCount_RNA','percent.mt'),ncol = 1,pt.size = 0,raster = F)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/HD-gene_umi_percent.mt_violin.png',width = 8,height = 15)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/HD-gene_umi_percent.mt_violin.pdf',width = 8,height = 15)
p1<-FeaturePlot(integrated_data,features = c('nFeature_RNA','nCount_RNA','percent.mt'),raster=FALSE,label = F,reduction = 'umap') & scale_colour_gradientn(colours = c("lightblue","orange","red"))
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/HD-gene_umi_percent.mt_featureplot.png',width = 12,height = 10)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/HD-gene_umi_percent.mt_featureplot.pdf',width = 12,height = 10)
integrated_data_sp$annot<-factor(integrated_data_sp$annot,levels = c('PT-S1','PT-S2','PT-S3','LOH','DCT','PC','IC','UC','Endo','Fib','B','MAC','NEU','POD','Adi'))
saveRDS(integrated_data_sp,file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
Idents(integrated_data_sp)<-integrated_data_sp$annot
p1<-DimPlot(integrated_data_sp,cols = colors,label = T,raster = F,pt.size = 0.4)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sp_merge_umap_annot.pdf',width = 10,height = 8)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sp_merge_umap_annot.png',width = 10,height = 8)
#dotplot
genelist<-c('Slc5a2','Cyp2e1','Cyp7b1','Umod','Slc12a3','Aqp2','Atp6v0d2','Ppl','Ptprb','Col1a2','Igkc','C1qa','S100a9','Wt1','Cfd')
integrated_data_sp$annot<-factor(integrated_data_sp$annot,levels=rev(levels(integrated_data_sp$annot)))
Idents(integrated_data_sp)<-integrated_data_sp$annot
p2<-DotPlot(integrated_data_sp,features = c(genelist),cols = c('#D3D3D3','#FF0000'))+#coord_flip()+
  theme(axis.text.x = element_text(size=12,color = "black", hjust=1,vjust=1,angle=45))+labs(x="",y="")
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sp_genelist_dotplot.pdf',width = 10,height = 6)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sp_genelist_dotplot.png',width = 10,height = 6)
#violin
integrated_data_sp$annot<-factor(integrated_data_sp$annot,levels=rev(levels(integrated_data_sp$annot)))
Idents(integrated_data_sp)<-integrated_data_sp$annot
p2<-VlnPlot(integrated_data_sp,features = genelist,pt.size =0,stack =TRUE,flip =TRUE,cols = as.character(colors[levels(integrated_data_sp$annot)]))+theme(legend.position = "none")+labs(x='')
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sp_genelist_violinplot.pdf',width = 10,height = 8)
ggsave(p2,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sp_genelist_violinplot.png',width = 10,height = 8)
#heatmap
all.genes <- rownames(integrated_data_sp)
Idents(integrated_data_sp)<-integrated_data_sp$annot
integrated_data_sp<-subset(integrated_data_sp,downsample=5000)
integrated_data_sp <- ScaleData(integrated_data_sp, features = all.genes)
p1<-DoHeatmap(integrated_data_sp, features = genelist,group.colors = colors) +scale_fill_gradient2(low = "#0000FF",mid = "#FFF8DC",high = "#FF0000")
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sp_genelist_heatmap.pdf',width = 12,height = 7)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sp_genelist_heatmap.png',width = 12,height = 7)
#cellratio heatmap
sc_cellratio<-read.delim(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/3.cell_ratio/cell_number_group.xls',header = T,sep = '\t')
sp_cellratio<-read.delim(file = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/3.cell_ratio/cell_number_group.xls',header = T,sep = '\t')
sc_cellratio<-sc_cellratio[sc_cellratio$cluster %in% sp_cellratio$cluster & sc_cellratio$group == 'Mock',]
sp_cellratio<-sp_cellratio[sp_cellratio$cluster %in% sc_cellratio$cluster & sp_cellratio$group == 'Mock',]
sc_cellratio$rate<-round(100*sc_cellratio$cell_number/sum(sc_cellratio$cell_number),digits = 2)
cellratio<-cbind(sc_cellratio,sp_cellratio)
cellratio<-cellratio[,c(1,2,4,8)]
colnames(cellratio)<-c('celltype','group','scRNA-seq','HD-seq')
cellratio$celltype<-factor(cellratio$celltype,levels = c(unique(cellratio$celltype)))
cellratio<-cellratio[order(cellratio$celltype),]
cellratio$group<-NULL
rownames(cellratio)<-cellratio$celltype
cellratio$celltype<-NULL
color <- colorRampPalette(c("#2161A0", "white", "firebrick3"))(100)
p1<-pheatmap::pheatmap(mat=cellratio,cluster_rows=F,cluster_cols=F,scale='none',legend=TRUE,display_numbers = T,#border_color = NA,color=color,annotation_colors = ann_colors,
                       show_rownames=T,show_colnames = T,fontsize_row=8,fontsize_col=10,angle_col = 0)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sc_sp_celltype_ratio_heatmap.pdf',width = 6,height = 5)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig1/sc_sp_celltype_ratio_heatmap.png',width = 6,height = 5)
###Fig2
#ratio 单细胞 柱状图趋势图
integrated_data<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/merge_rds/merge_data.rds'))
color_panel <-c('#53A85F','#58A4C3','#AB3282','#8C549C','#BD956A','#57C3F3','#6778AE','#F3B1A0','#F1BB72',
                '#DCC1DD','#E95C59','#625D9E','#F7F398','#E63863','#5F3D69','#C5DEBA','#CCE0F5','#B53E2B',
                '#AA9A59','#E39A35','#91D0BE','#23452F','#E4C755','#585658','#C1E6F3','#D6E7A3','#712820',
                '#CCC9E6','#3A6963','#68A180','#61B934','#476D87','#9FA3A8','#968175','#BC8F8F','#D8BFD8')
#colors <- color_panel[1:length(levels(integrated_data$annot))]
#names(colors)<-levels(integrated_data$annot)
#colors<-c(colors,'POD'='#23452F','Adi'='#CCE0F5')
colors = c('PT-S1'= '#045DFC','PT-S2'= '#81F7A6','PT-S3'= '#CE4A7F','LOH'= '#FC8814','DCT'= '#7DFBFB',
           'PC'= '#90683C','IC'= '#915D95','UC'= '#9CC4C2','Endo'= '#9B23D7','Fib'= '#FFFF00','T'='#FE48FF','B'= '#907079','MAC'= '#6D9E62',
           'NEU'= '#C84036','POD'= '#25FB44','Adi'= '#8FA759')
all_cluster_data<-read.delim('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/3.cell_ratio/cell_number_group.xls',header = T,sep = '\t')
trend_df <- all_cluster_data %>%
  dplyr::filter(group %in% c('Mock','Microplastics')) %>%
  dplyr::select(group, cluster, rate) %>%
  tidyr::pivot_wider(names_from = group, values_from = rate) %>%
  mutate(trend = `Microplastics` / `Mock`)
trend_order <- trend_df %>%
  arrange(trend) %>%          # 从负到正
  pull(cluster)
all_cluster_data$cluster<-factor(all_cluster_data$cluster,levels = trend_order)
library(ggalluvial)
p2<-ggplot(all_cluster_data, aes(x =group, y= rate, fill = cluster,
                                 stratum=cluster, alluvium=cluster)) + 
  geom_col(width = 0.5, color='black')+
  geom_flow(width=0.5,alpha=0.4, knot.pos=0)+
  theme_classic() +coord_flip()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size=12,color = "black"),
        axis.text = element_text(size=12,color = "black"))+
  labs(y='ratio(%)',x='',fill="celltype")+
  scale_fill_manual(values = colors)+scale_y_reverse()
ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig2/scRNA_group_celltype_rate_barplot.png'),width = 10,height = 6)
ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig2/scRNA_group_celltype_rate_barplot.pdf'),width = 10,height = 6)
#ratio HD 柱状图趋势图
integrated_data_sp<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/merge_rds/merge_data.rds'))
all_cluster_data<-read.delim('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/Microplastic_2Seg_Spatial/3.cell_ratio/cell_number_group.xls',header = T,sep = '\t')
trend_df <- all_cluster_data %>%
  dplyr::filter(group %in% c('Mock','Microplastics')) %>%
  dplyr::select(group, cluster, rate) %>%
  tidyr::pivot_wider(names_from = group, values_from = rate) %>%
  mutate(trend = `Microplastics` / `Mock`)
trend_order <- trend_df %>%
  arrange(trend) %>%          # 从负到正
  pull(cluster)
all_cluster_data$cluster<-factor(all_cluster_data$cluster,levels = trend_order)
library(ggalluvial)
p2<-ggplot(all_cluster_data, aes(x =group, y= rate, fill = cluster,
                                 stratum=cluster, alluvium=cluster)) + 
  geom_col(width = 0.5, color='black')+
  geom_flow(width=0.5,alpha=0.4, knot.pos=0)+
  theme_classic() +coord_flip()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size=12,color = "black"),
        axis.text = element_text(size=12,color = "black"))+
  labs(y='ratio(%)',x='',fill="celltype")+
  scale_fill_manual(values = colors)+scale_y_reverse()
ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig2/HD_group_celltype_rate_barplot.png'),width = 10,height = 6)
ggsave(p2,filename = paste0('~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig2/HD_group_celltype_rate_barplot.pdf'),width = 10,height = 6)
###Fig4
integrated_data_Fib<-readRDS(file = paste0('/share/home/liny/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/8.subcluster/Fibroblast/Fibroblast_subcluster.rds'))
#wgcna module3 gene
integrated_data_Fib_sub<-subset(integrated_data_Fib,annot %in% 'Inflammatory fibroblast')
module_gene<-c('Il6','Osmr','Nfkb1','Irf8','Cd44','Il1r1')
p1<-DotPlot(integrated_data_Fib_sub,features = module_gene,cols = c('#009ACD','#FF0000'),group.by = 'group')+
  theme(axis.text.x = element_text(size=12,color = "black", hjust=1,vjust=1,angle=45))+labs(x="",y="")
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig4/Module3_geneselect_dotplot.png',width = 7,height = 4)
ggsave(p1,filename = '~/Project/personality/SY_wangwenbiao_spleen_mouse_20250521/Seurat/7Harmony/Paper_Fig/Fig4/Module3_geneselect_dotplot.pdf',width = 7,height = 4)








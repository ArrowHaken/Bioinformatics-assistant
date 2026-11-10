######################################################################
############################### Seurat ###############################
######################################################################
#.libPaths(c('/home/honghh/R/x86_64-redhat-linux-gnu-library/3.6','/share/nas2/public/R/library/3.6/'))
.libPaths(c('/share/apps/R/R-4.1.2-4/lib64/R/library'))
suppressMessages({
library(plyr)
library(dplyr)
library(cowplot)
library(ggplot2)
library(gridExtra)
library(colorRamps)
library(patchwork)
library(argparser)
library(pheatmap)
library(DoubletFinder)
library(harmony)
library(Seurat)
library(monocle)
library(ggrepel)
library(enrichplot)
library(clusterProfiler)
library(SingleR)})

#my_comparisons <- combn(levels(integrated_data_TNK$group),2,simplify = F)
#detach("package:dplyr", unload = TRUE)
#result <- do.call(rbind, strsplit(rownames(integrated_data@meta.data), "_"))
#plot_cell_trajectory(cds,markers = genelist,use_color_gradient = T,cell_size = 0.2)+theme(legend.position = 'bottom')+scale_color_gradient(low = 'grey',high = 'red')
#as.character(sapply(levels(integrated_data_B$annot),FUN = function(x){paste0(x,'-',levels(integrated_data_B$group))}))
#sapply(levels(integrated_data$orig.ident), function(x){ceiling(median(integrated_data@meta.data[integrated_data$orig.ident==x,]$nFeature_RNA))})
#sapply(levels(integrated_data$orig.ident), function(x){round(median(integrated_data@meta.data[integrated_data$orig.ident==x,]$percent.mt),digits = 2)})
#ls *rds|awk -F '.' '{print $1"<-readRDS(file = \"~/Project/personality/ZZ_zhaohongzhe_pancreas_xiaoshu_20230913/samplerds/"$0"\")"}'
#ls *rds|awk -F '.' '{print $1"<-subset("$1",subset = nFeature_RNA > 500 & nFeature_RNA < 5000 & percent.mt <10)"}'
#ls *rds|awk -F '.' '{print "saveRDS("$1",file=\"~/Project/personality/ZZ_zhaohongzhe_pancreas_xiaoshu_20230913/samplerds/fiterrds/"$0"\")"}'
#cat 7.Loupe/info.txt |sed 1d|cut -d, -f1|awk '{printf "\""$0"\"=\"-"NR"\","}'
#cat 7.Loupe/info.txt |sed 1d|cut -d, -f1|awk '{printf $0"_|"}'
s.genes <- cc.genes$s.genes
g2m.genes <- cc.genes$g2m.genes
color_panel = c('#FFC1C1','#FFA500','#FFF68F','#7FFFD4','#B0E2FF','#FF83FA','#F08080',
                '#54FF9F','#C0FF3E','#6495ED','#8470FF','#FF69B4','#FF1493','#FFDEAD',
                '#0000FF','#8B1C62','#FF0000','#FFFF00','#00EE00','#B22222','#00BFFF',
                '#006400','#000080','#CD5C5C','#9400D3','#836FFF','#008B8B','#CD5B45',
                '#2E8B57','#473C8B','#BCD2EE','#363636','#F5F5DC','#53868B','#D8BFD8',
                '#8B5A00','#8B658B','#C1CDC1','#EED5D2','#000000','#828282','#BC8F8F',
                '#FF4040','#98F5FF','#90EE90')
color_panel <-c('#53A85F','#58A4C3','#AB3282','#8C549C','#BD956A','#57C3F3','#6778AE','#F3B1A0','#F1BB72',
             '#DCC1DD','#E95C59','#625D9E','#F7F398','#E63863','#5F3D69','#C5DEBA','#CCE0F5','#B53E2B',
             '#AA9A59','#E39A35','#91D0BE','#23452F','#E4C755','#585658','#C1E6F3','#D6E7A3','#712820',
             '#CCC9E6','#3A6963','#68A180','#61B934','#476D87','#9FA3A8','#968175','#BC8F8F','#D8BFD8')
color_code<-c("#0000CD","#FF3333","#7CFC00","#9370DB","#4682B4","#FFFFE0","#9400D3","#708090","#008000","#A52A2A",
              "#DA70D6","#2E8B57","#800000","#FF7F00","#6495ED","#FF6666","#00BFFF","#FF8C00","#00FF00","#90EE90",
              "#FF7F50","#B22222","#FFFF00","#C71585","#4169E1","#000080","#3CB371","#1E90FF","#006400","#ADFF2F",
              "#FF0000","#8B0000","#EE82EE","#A0522D","#FF1493","#FFD700","#FF00FF","#7FFFD4","#000000","#6B8E23",
              "#98FB98","#FF9999","#ADD8E6","#D3D3D3","#F0FFF0","#DDA0DD","#DC143C","#00FFFF","#F5FFFA","#FAEBD7",
              "#DB7093","#800080","#A9A9A9","#9932CC","#B0C4DE","#CD5C5C","#191970","#C0C0C0","#66CDAA","#D8BFD8",
              "#008B8B","#FFFFE0","#2F4F4F","#FFA07A","#87CEEB","#696969","#0000FF","#FFA500","#FAFAD2","#FF4500","#FFC125")
mycolour<-c("#DC143C","#0000FF","#20B2AA","#FFA500","#9370DB","#98FB98","#F08080","#1E90FF","#FFFF00",
           "#808000","#FF00FF","#FA8072","#7B68EE","#9400D3","#800080","#A0522D","#D2B48C","#D2691E",
           "#87CEEB","#40E0D0","#5F9EA0","#FF1493","#0000CD","#008B8B","#FFE4B5","#8A2BE2","#228B22",
           "#E9967A","#4682B4","#32CD32","#F0E68C","#FFFFE0","#EE82EE",
           "#FF6347","#6A5ACD","#9932CC","#8B008B","#8B4513","#DEB887")
color_teacher<-c('#D73027','#3C8DBC','#7EBC59','#E69F00','#9C6BBD','#56B4E9','#F0E442','#009E73','#F15C80','#4DBBD5','#B3DE69','#F98E72','#FF9933','#C77CFF','#A6CEE3','#FB9A99','#8DD3C7','#BC80BD','#A6B83F','#9FA3A8')
my36colors <-c('#E5D2DD', '#53A85F', '#F1BB72', '#F3B1A0', '#D6E7A3', '#57C3F3', '#476D87',
               '#E95C59', '#E59CC4', '#AB3282', '#23452F', '#BD956A', '#8C549C', '#585658',
               '#9FA3A8', '#E0D4CA', '#5F3D69', '#C5DEBA', '#58A4C3', '#E4C755', '#F7F398',
               '#AA9A59', '#E63863', '#E39A35', '#C1E6F3', '#6778AE', '#91D0BE', '#B53E2B',
               '#712820', '#DCC1DD', '#CCE0F5',  '#CCC9E6', '#625D9E', '#68A180', '#3A6963',
               '#968175',"#9370DB","#9400D3",'#D73027')
zzm60colors<-c('#4b6aa8','#3ca0cf','#c376a7','#ad98c3','#cea5c7',
               '#53738c','#a5a9b0','#a78982','#696a6c','#92699e',
               '#d69971','#df5734','#6c408e','#ac6894','#d4c2db',
               '#537eb7','#83ab8e','#ece399','#405993','#cc7f73',
               '#b95055','#d5bb72','#bc9a7f','#e0cfda','#d8a0c0',
               '#e6b884','#b05545','#d69a55','#64a776','#cbdaa9',
               '#efd2c9','#da6f6d','#ebb1a4','#a44e89','#a9c2cb',
               '#b85292','#6d6fa0','#8d689d','#c8c7e1','#d25774',
               '#c49abc','#927c9a','#3674a2','#9f8d89','#72567a',
               '#63a3b8','#c4daec','#61bada','#b7deea','#e29eaf',
               '#4490c4','#e6e2a3','#de8b36','#c4612f','#9a70a8',
               '#76a2be','#408444','#c6adb0','#9d3b62','#2d3462')
zzm13colors<-c('#d4de9c','#94c58f','#86c7b4','#9cd2ed','#a992c0',
               '#ea9994','#f2c396','#bb82b1','#df5734','#d25774','#3ca0cf','#2d3462','#d5bb72',"#4b6aa8","#d69971","#53738c","#a5a9b0")
subcluster_color<-c('#855399','#77C7EB','#3694B3','#F3A30D','#C0A365','#E76A29','#EE9866','#57A497','#61B934','#A5D08F')
subcluster_color2<-c('#CD853F','#BFA16C','#EBB300','#EA6839','#FFB6C1','#7490D0','#B1D1E5','#7CCBDC','#378CAF','#229B32','#65B732','#7F5893','#D8BFD8','#BC8F8F')
color_palette10<-c("#B5C3D7","#6E8FB2","#B6C9C0","#7DA494","#F5D8B7","#EAB67A","#F1D0C6","#E5A79A","#DDAEAB","#C16E71")
color_palette13<-c('#F7DD56','#B8DBAD','#F7E1D3','#EE9678','#DD4838','#962221','#EE8432','#FB70CF','#BADD77','#E1EBF8','#A6C9E1','#5690C2','#3A64A1')
cols1 <- c('#699ECA', '#FF8C00', '#F898CB', '#4DAF4A', '#D65190', '#731A73', '#FFCB5B', '#E87B1E', '#0076B9',
          '#3D505A', '#0098B2')
cols2 <- c("#1B9E77", "#D95F02", "#7570B3", "#E95C59", "#E42320", "#66A61E", "#E7298A", "#58A4C3", "#E6AB02", "#AB3282",
          "#8b0000","#59b56f","#C20088","#F56105","#4C005C","#F56105","#ffa500","#0042b0")
colors_10<-c('#AE0E7C','#F28ABE','#F04A52','#3B6B8B','#29C0ED','#D2E7A3','#F9AE97','#FDBA5D','#1EB05A','#E8CBD8')
group_color<-c('#955CBB','#2CA42E','#F37C17','#4971A7')
color_panel2 = c("lightblue","orange","red")
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
my_theme <- theme(
    axis.title.y = element_text(size = 18, color = "black"),
    axis.text    = element_text(size = 18, color = "black"),
    axis.text.x  = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_blank(),
    axis.line        = element_line(colour = "black"),
    strip.background = element_blank(),
    strip.text       = element_text(size = 18, color = "black"),
    legend.title     = element_text(size = 18, color = "black"),
    legend.text      = element_text(size = 18, color = "black"))
my_theme2<-theme(axis.text= element_text(size = 18, color = "black"),
    axis.title = element_text(size = 18, color = "black"),
    strip.text       = element_text(size = 18, color = "black"),
    legend.title     = element_text(size = 18, color = "black"),
    legend.text= element_text(size = 18, color = "black"))
#df<-AverageExpression(integrated_data, assays = 'RNA', group.by ='seurat_clusters')
#vln.dat=FetchData(integrated_data,c(genelist,"annot"))
#stack violin
modify_vlnplot<- function(obj,feature,pt.size = 0,# split.by = 'orig.ident',
                          split.plot = FALSE,plot.margin = unit(c(-0.75, 0, -0.75, 0), "cm"),
                          ...) {
  p<- VlnPlot(obj, features = feature, pt.size = pt.size, adjust = 1,... )  +xlab("") + ylab(feature) + ggtitle("") +
    theme(legend.position = "none",axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(),axis.title.y = element_text(size = rel(1), angle = 0, vjust = 0.5),plot.margin = plot.margin)
  return(p)
}
StackedVlnPlot<- function(obj, features,pt.size = 0,# split.by='orig.ident',
                          split.plot=FALSE,plot.margin = unit(c(-0.75, 0, -0.75, 0), "cm"),
                          ...) {
  plot_list<- purrr::map(features, function(x) modify_vlnplot(obj = obj,feature = x,pt.size= pt.size,split.plot=split.plot,...))
  plot_list[[length(plot_list)]]<- plot_list[[length(plot_list)]] +theme(axis.text.x=element_text(angle = 45), axis.ticks.x = element_line())
  p<- patchwork::wrap_plots(plotlist = plot_list, ncol = 1)
  return(p)
}
DimPlot_custom <- function(
    object,
    sample_col = "orig.ident",
    group_col = "celltype",
    reduction = "umap",
    layout = c(4, 6, 6),
    point_size = 0.5,
    label = FALSE
) {
  # 获取样本
  samples <- unique(object@meta.data[[sample_col]])
  samples <- samples[!is.na(samples)]
  # 检查布局
  if (sum(layout) != length(samples)) {
    stop(
      paste0(
        "布局数量 ", sum(layout),
        " 与样本数量 ", length(samples),
        " 不一致！\n",
        "当前样本数：", length(samples),
        "；layout = c(", paste(layout, collapse = ", "), ")"
      )
    )
  }
  # 每个样本生成一个DimPlot
  plot_list <- lapply(samples, function(x) {
    cells_use <- rownames(object@meta.data)[
      object@meta.data[[sample_col]] == x
    ] 
    p <- DimPlot(
      object,
      cells = cells_use,
      reduction = reduction,
      group.by = group_col,
      pt.size = point_size,
      label = label
    ) +
      ggtitle(x) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          size = 12
        )
      )
    return(p)
  })
  # 按 layout 分行
  rows <- list()
  start <- 1
  for (n in layout) {
    end <- start + n - 1
    rows[[length(rows) + 1]] <- wrap_plots(
      plot_list[start:end],
      ncol = n
    ) 
    start <- end + 1
  }
  # 拼接所有行
  final_plot <- wrap_plots(
    rows,
    ncol = 1) 
  return(final_plot)
}
#Soupx
library(SoupX)
RunSoupX<-function(path, name, species, min.cells = 3, min.features = 200, rho=NULL, names.delim = "-"){
  sc = load10X(path)
  #sc = autoEstCont(sc,forceAccept=TRUE)
  if (is.null(rho)) {
  sc<-tryCatch(
    {sc = autoEstCont(sc)}, 
    error=function(e) {
    #因为自动计算经常会报错，所以如果报错则设置rho为0.2去除99%的环境RNA污染
    print("autoEstCont Error !")
    sc = setContaminationFraction(sc, 0.2)
    return(sc)}
    )
  }else{
    #自行设置污染比例系数
    sc = setContaminationFraction(sc, rho)
  }
  out = adjustCounts(sc)
  seu = CreateSeuratObject(counts =out, project = name, min.cells = min.cells, min.features = min.features, names.delim = names.delim)
  if(species == 'Human'){
    seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^MT-")
  }else if(species == 'Mouse'){
    seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^mt-")
  }else if(species == 'Rat'){
    seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^Mt-")
  }
  return(seu)
}
#Doublefinder
RunDF<-function(before_rds,nfeatures=2000,dims=20,resolution=0.5,PCs=20,pN=0.25){
  before_rds <- NormalizeData(before_rds)
  before_rds <- ScaleData(before_rds)
  before_rds <- FindVariableFeatures(before_rds, selection.method = "vst", nfeatures = nfeatures)
  before_rds <- RunPCA(before_rds)
  #ElbowPlot(before_rds)
  before_rds <- FindNeighbors(before_rds,dims = 1:dims)
  before_rds <- FindClusters(before_rds, resolution = resolution)
  before_rds <- RunUMAP(before_rds, dims = 1:dims)
  before_rds@assays$RNA@scale.data <- matrix()
  sweep.res.list_kidney <- paramSweep_v3(before_rds, PCs = 1:PCs, sct = T)
  sweep.stats_kidney <- summarizeSweep(sweep.res.list_kidney, GT = FALSE)
  bcmvn_kidney <- find.pK(sweep.stats_kidney)
  mpK<-as.numeric(as.vector(bcmvn_kidney$pK[which.max(bcmvn_kidney$BCmetric)]))

  # 找最佳 nExp
  annotations <- before_rds@meta.data$seurat_clusters
  homotypic.prop <- modelHomotypic(annotations)
  DoubletRate = ncol(before_rds@assays$RNA@data)*8*1e-6
  nExp_poi <- round(DoubletRate*ncol(before_rds@assays$RNA@data))
  #nExp_poi <- round(ratio*ncol(before_rds@assays$RNA@data))
  nExp_poi <- round(nExp_poi*(1-homotypic.prop))

  seurat_filterDouble <- doubletFinder_v3(before_rds, PCs = 1:PCs, pN = pN, pK = mpK, nExp = nExp_poi, reuse.pANN = FALSE, sct = T)
  seurat_filterDouble@meta.data$Doublet<-seurat_filterDouble@meta.data[,ncol(seurat_filterDouble@meta.data)]
  return(seurat_filterDouble)
}
RunDF2<-function(before_rds,nfeatures=2000,dims=20,resolution=0.5,ratio=0.07,PCs=20,pN=0.25){
  before_rds <- NormalizeData(before_rds)
  before_rds <- ScaleData(before_rds)
  before_rds <- FindVariableFeatures(before_rds, selection.method = "vst", nfeatures = nfeatures)
  before_rds <- RunPCA(before_rds)
  #ElbowPlot(before_rds)
  before_rds <- FindNeighbors(before_rds,dims = 1:dims)
  before_rds <- FindClusters(before_rds, resolution = resolution)
  before_rds <- RunUMAP(before_rds, dims = 1:dims)
  before_rds@assays$RNA@scale.data <- matrix()
  sweep.res.list_kidney <- paramSweep_v3(before_rds, PCs = 1:PCs, sct = T)
  sweep.stats_kidney <- summarizeSweep(sweep.res.list_kidney, GT = FALSE)
  bcmvn_kidney <- find.pK(sweep.stats_kidney)
  mpK<-as.numeric(as.vector(bcmvn_kidney$pK[which.max(bcmvn_kidney$BCmetric)]))

  # 找最佳 nExp
  annotations <- before_rds@meta.data$seurat_clusters
  homotypic.prop <- modelHomotypic(annotations)
  nExp_poi <- round(ratio*ncol(before_rds@assays$RNA@data))
  nExp_poi.adj <- round(nExp_poi*(1-homotypic.prop))

  seurat_filterDouble <- doubletFinder_v3(before_rds, PCs = 1:PCs, pN = pN, pK = mpK, nExp = nExp_poi, reuse.pANN = FALSE, sct = T)
  seurat_filterDouble@meta.data$Doublet<-seurat_filterDouble@meta.data[,ncol(seurat_filterDouble@meta.data)]
  return(seurat_filterDouble)
}
#Seurat
RunCreateSeurat<-function(path, name, species=NULL,min.cells = 3, min.features = 200, names.delim = "-"){
  seu.data<-Read10X(data.dir = path)
  seu<-CreateSeuratObject(counts =seu.data,project = name, min.cells = min.cells, min.features = min.features)
  if(!is.null(species)){
    if(species == 'Human'){
      seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^MT-")
    }
    if(species == 'Mouse'){
      seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^mt-")
    }
    if(species == 'Rat'){
      seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^Mt-")
    }}
  return(seu)
}
RunCreateSeurat_MGI<-function(path, name, species=NULL,min.cells = 3, min.features = 200, names.delim = "-"){
  seu.data<-Read10X(data.dir = path,gene.column = 1)
  seu<-CreateSeuratObject(counts =seu.data,project = name, min.cells = min.cells, min.features = min.features)
  if(!is.null(species)){
    if(species == 'Human'){
      seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^MT-")
    }
    if(species == 'Mouse'){
      seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^mt-")
    }
    if(species == 'Rat'){
      seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^Mt-")
    }}
  return(seu)
}
RunCreateSeurat_protein<-function(path, name, species=NULL,min.cells = 3, min.features = 200, names.delim = "-"){
  seu.data<-Read10X(data.dir = path)
  seu<-CreateSeuratObject(counts =seu.data$`Gene Expression`,project = name, min.cells = min.cells, min.features = min.features)
  seu[["ADT"]] <- CreateAssayObject(counts = seu.data$`Antibody Capture`[, colnames(x = seu)])
  if(!is.null(species)){
    if(species == 'Human'){
      seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^MT-")
    }
    if(species == 'Mouse'){
      seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^mt-")
    }
    if(species == 'Rat'){
      seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^Mt-")
    }}
  return(seu)
}
RunCreateSeurat_Spatial<-function(path, name, species=NULL,min.cells = 3, min.features = 200, names.delim = "-"){
  seu<-Load10X_Spatial(data.dir = path)
  seu$orig.ident<-name
  Idents(seu)<-seu$orig.ident
  if(!is.null(species)){
    if(species == 'Human'){
      seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^MT-")
    }
    if(species == 'Mouse'){
      seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^mt-")
    }
    if(species == 'Rat'){
      seu[["percent.mt"]]<-PercentageFeatureSet(seu, pattern="^Mt-")
    }}
  return(seu)
}
#plot cluster umap
plot_cluster_umap<-function(integrated_data,outdir,type,reduction='umap',ncol =ncol,ncol_group=ncol_group,group=group,pt.size =NULL,cols=NULL,raster=NULL,shuffle=NULL,label=NULL,repel = FALSE){
  p6<-VlnPlot(integrated_data,features = c('nFeature_RNA','nCount_RNA','percent.mt'),cols = cols,pt.size = 0,ncol = 1)
  ggsave(p6,filename = paste0(outdir,'/nFeature_nCount_mt_violin.png'),width = 10,height = 12)
  ggsave(p6,filename = paste0(outdir,'/nFeature_nCount_mt_violin.pdf'),width = 10,height = 12)
  if(is.null(label)){label='T'}
  p1<-DimPlot(integrated_data,label = label,cols = cols,pt.size = pt.size,reduction = reduction,raster=raster,shuffle = shuffle,repel = repel)
  p2<-DimPlot(integrated_data,label = label,split.by = group,cols = cols,ncol = ncol_group,pt.size = pt.size,reduction = reduction,raster=raster,shuffle = shuffle,repel = repel)
  p3<-DimPlot(integrated_data,label = label,split.by = 'orig.ident',pt.size = pt.size,cols = cols,ncol = ncol,reduction = reduction,raster=raster,shuffle = shuffle,repel = repel)
  Idents(integrated_data)<-integrated_data@meta.data$orig.ident
  p4<-DimPlot(integrated_data,pt.size = pt.size,reduction = reduction,raster=raster,shuffle = T)
  Idents(integrated_data)<-integrated_data@meta.data[,group]
  p44<-DimPlot(integrated_data,pt.size = pt.size,reduction = reduction,raster=raster,shuffle = T)
  p5<-FeaturePlot(integrated_data,features = c('nFeature_RNA','nCount_RNA','percent.mt'),reduction = reduction,raster=raster) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
  ggsave(p1,filename = paste0(outdir,'/',type,'_',reduction,'.png'),width = 8,height = 6)
  ggsave(p1,filename = paste0(outdir,'/',type,'_',reduction,'.pdf'),width = 8,height = 6)
  ggsave(p2,filename = paste0(outdir,'/',type,'_group_',reduction,'.png'),width = 12,height = 10)
  ggsave(p2,filename = paste0(outdir,'/',type,'_group_',reduction,'.pdf'),width = 12,height = 10)
  ggsave(p3,filename = paste0(outdir,'/',type,'_sample_',reduction,'.png'),width = 15,height = 10)
  ggsave(p3,filename = paste0(outdir,'/',type,'_sample_',reduction,'.pdf'),width = 15,height = 10)
  ggsave(p4,filename = paste0(outdir,'/sample_',reduction,'.png'),width = 8,height = 6)
  ggsave(p4,filename = paste0(outdir,'/sample_',reduction,'.pdf'),width = 8,height = 6)
  ggsave(p44,filename = paste0(outdir,'/group_',reduction,'.png'),width = 8,height = 6)
  ggsave(p44,filename = paste0(outdir,'/group_',reduction,'.pdf'),width = 8,height = 6)
  ggsave(p5,filename = paste0(outdir,'/nFeature_nCount_mt_featureplot.png'),width = 10,height = 8)
  ggsave(p5,filename = paste0(outdir,'/nFeature_nCount_mt_featureplot.pdf'),width = 10,height = 8)
}
#plot cluster umap Spatial
plot_cluster_umap_spatial<-function(integrated_data,outdir,type,reduction='umap',ncol =ncol,ncol_group=ncol_group,group=group,pt.size =NULL,pt.size.factor=NULL,cols=NULL,raster=NULL,shuffle=NULL,label=NULL){
  if(!(is.null(cols))){
      colors <- cols[1:length(levels(Idents(integrated_data)))]
      names(colors)<-levels(Idents(integrated_data))
  }
  p6<-VlnPlot(integrated_data,features = c('nFeature_Spatial','nCount_Spatial','percent.mt'),cols = cols,pt.size = 0,ncol = 1)
  ggsave(p6,filename = paste0(outdir,'/nFeature_nCount_mt_violin.png'),width = 10,height = 12)
  ggsave(p6,filename = paste0(outdir,'/nFeature_nCount_mt_violin.pdf'),width = 10,height = 12)
  if(is.null(label)){label='T'}
  p1<-DimPlot(integrated_data,label = label,cols = cols,pt.size = pt.size,reduction = reduction,raster=raster,shuffle = shuffle)
  p2<-DimPlot(integrated_data,label = label,split.by = group,cols = cols,ncol=ncol_group,pt.size = pt.size,reduction = reduction,raster=raster,shuffle = shuffle)
  p3<-DimPlot(integrated_data,label = label,split.by = 'orig.ident',pt.size = pt.size,cols = cols,ncol = ncol,reduction = reduction,raster=raster,shuffle = shuffle)
  if(!(is.null(cols))){
      if(!(is.null(pt.size.factor))){
          p7<-SpatialDimPlot(integrated_data,ncol = ncol,label = label,label.size=3,cols = colors,pt.size.factor=pt.size.factor)
      }else{
          p7<-SpatialDimPlot(integrated_data,ncol = ncol,label = label,label.size=3,cols = colors)}
  }else{
      if(!(is.null(pt.size.factor))){
          p7<-SpatialDimPlot(integrated_data,ncol = ncol,label = label,label.size=3,pt.size.factor=pt.size.factor)
      }else{p7<-SpatialDimPlot(integrated_data,ncol = ncol,label = label,label.size=3)}
  }
  Idents(integrated_data)<-integrated_data@meta.data$orig.ident
  p4<-DimPlot(integrated_data,pt.size = pt.size,reduction = reduction,raster=raster)
  Idents(integrated_data)<-integrated_data@meta.data[,group]
  p44<-DimPlot(integrated_data,pt.size = pt.size,reduction = reduction,raster=raster)
  p5<-FeaturePlot(integrated_data,features = c('nFeature_Spatial','nCount_Spatial','percent.mt'),reduction = reduction,raster=raster) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
  ggsave(p1,filename = paste0(outdir,'/',type,'_',reduction,'.png'),width = 8,height = 6)
  ggsave(p1,filename = paste0(outdir,'/',type,'_',reduction,'.pdf'),width = 8,height = 6)
  ggsave(p2,filename = paste0(outdir,'/',type,'_group_',reduction,'.png'),width = 12,height = 10)
  ggsave(p2,filename = paste0(outdir,'/',type,'_group_',reduction,'.pdf'),width = 12,height = 10)
  ggsave(p3,filename = paste0(outdir,'/',type,'_sample_',reduction,'.png'),width = 15,height = 10)
  ggsave(p3,filename = paste0(outdir,'/',type,'_sample_',reduction,'.pdf'),width = 15,height = 10)
  ggsave(p4,filename = paste0(outdir,'/sample_',reduction,'.png'),width = 8,height = 6)
  ggsave(p4,filename = paste0(outdir,'/sample_',reduction,'.pdf'),width = 8,height = 6)
  ggsave(p44,filename = paste0(outdir,'/group_',reduction,'.png'),width = 8,height = 6)
  ggsave(p44,filename = paste0(outdir,'/group_',reduction,'.pdf'),width = 8,height = 6)
  ggsave(p5,filename = paste0(outdir,'/nFeature_nCount_mt_featureplot.png'),width = 10,height = 8)
  ggsave(p5,filename = paste0(outdir,'/nFeature_nCount_mt_featureplot.pdf'),width = 10,height = 8)
  ggsave(p7,filename = paste0(outdir,'/',type,'_',reduction,'_slice.png'),width = 35,height = 25)
  ggsave(p7,filename = paste0(outdir,'/',type,'_',reduction,'_slice.pdf'),width = 35,height = 25)
}
#plot_cluster_umap_hd
plot_cluster_umap_hd<-function(integrated_data,outdir,type,reduction='umap',ncol =ncol,ncol_group=ncol_group,group=group,pt.size =NULL,pt.size.factor=NULL,cols=NULL,raster=NULL,shuffle=NULL,label=NULL){
  if(!(is.null(cols))){
      colors <- cols[1:length(levels(Idents(integrated_data)))]
      names(colors)<-levels(Idents(integrated_data))
  }
  p6<-VlnPlot(integrated_data,features = c('nFeature_Spatial.Polygons','nCount_Spatial.Polygons','percent.mt.Polygons'),cols = cols,pt.size = 0,ncol = 1)
  ggsave(p6,filename = paste0(outdir,'/nFeature_nCount_mt_violin.png'),width = 10,height = 12)
  ggsave(p6,filename = paste0(outdir,'/nFeature_nCount_mt_violin.pdf'),width = 10,height = 12)
  if(is.null(label)){label='T'}
  p1<-DimPlot(integrated_data,label = label,cols = cols,pt.size = pt.size,reduction = reduction,raster=raster,shuffle = shuffle)
  p2<-DimPlot(integrated_data,label = label,split.by = group,cols = cols,ncol=ncol_group,pt.size = pt.size,reduction = reduction,raster=raster,shuffle = shuffle)
  p3<-DimPlot(integrated_data,label = label,split.by = 'orig.ident',pt.size = pt.size,cols = cols,ncol = ncol,reduction = reduction,raster=raster,shuffle = shuffle)
  if(!(is.null(cols))){
      if(!(is.null(pt.size.factor))){
          p7<-SpatialDimPlot(integrated_data,ncol = ncol,label = label,label.size=3,cols = colors,pt.size.factor=pt.size.factor)
      }else{
          p7<-SpatialDimPlot(integrated_data,ncol = ncol,label = label,label.size=3,cols = colors)}
  }else{
      if(!(is.null(pt.size.factor))){
          p7<-SpatialDimPlot(integrated_data,ncol = ncol,label = label,label.size=3,pt.size.factor=pt.size.factor)
      }else{p7<-SpatialDimPlot(integrated_data,ncol = ncol,label = label,label.size=3)}
  }
  Idents(integrated_data)<-integrated_data@meta.data$orig.ident
  p4<-DimPlot(integrated_data,pt.size = pt.size,reduction = reduction,raster=raster)
  Idents(integrated_data)<-integrated_data@meta.data[,group]
  p44<-DimPlot(integrated_data,pt.size = pt.size,reduction = reduction,raster=raster)
  #p5<-FeaturePlot(integrated_data,features = c('nFeature_Spatial.Polygons','nCount_Spatial.Polygons','percent.mt.Polygons'),reduction = reduction,raster=raster) & scale_colour_gradientn(colours = c("lightblue","orange","red"))
  ggsave(p1,filename = paste0(outdir,'/',type,'_',reduction,'.png'),width = 8,height = 6)
  ggsave(p1,filename = paste0(outdir,'/',type,'_',reduction,'.pdf'),width = 8,height = 6)
  ggsave(p2,filename = paste0(outdir,'/',type,'_group_',reduction,'.png'),width = 12,height = 10)
  ggsave(p2,filename = paste0(outdir,'/',type,'_group_',reduction,'.pdf'),width = 12,height = 10)
  ggsave(p3,filename = paste0(outdir,'/',type,'_sample_',reduction,'.png'),width = 15,height = 10)
  ggsave(p3,filename = paste0(outdir,'/',type,'_sample_',reduction,'.pdf'),width = 15,height = 10)
  ggsave(p4,filename = paste0(outdir,'/sample_',reduction,'.png'),width = 8,height = 6)
  ggsave(p4,filename = paste0(outdir,'/sample_',reduction,'.pdf'),width = 8,height = 6)
  ggsave(p44,filename = paste0(outdir,'/group_',reduction,'.png'),width = 8,height = 6)
  ggsave(p44,filename = paste0(outdir,'/group_',reduction,'.pdf'),width = 8,height = 6)
  #ggsave(p5,filename = paste0(outdir,'/nFeature_nCount_mt_featureplot.png'),width = 10,height = 8)
  #ggsave(p5,filename = paste0(outdir,'/nFeature_nCount_mt_featureplot.pdf'),width = 10,height = 8)
  ggsave(p7,filename = paste0(outdir,'/',type,'_',reduction,'_slice.png'),width = 35,height = 25)
  ggsave(p7,filename = paste0(outdir,'/',type,'_',reduction,'_slice.pdf'),width = 35,height = 25)
}
#heatmap
RunFindAllMarkers<-function(seu,min.pct = 0.1, species=NULL,logfc.threshold = 0.25,top_gene=10){
  DefaultAssay(seu)<-'RNA'
  all.genes <- rownames(seu)
  seu <- ScaleData(seu, features = all.genes)
  allmarkers <- FindAllMarkers(seu, only.pos = TRUE, min.pct = min.pct, logfc.threshold = logfc.threshold)
  if(!is.null(species)){
    if(species == 'Human'){
      allmarkers<-allmarkers[!grepl(allmarkers$gene,pattern = '^MT-'),]
    }else if(species == 'Mouse'){
      allmarkers<-allmarkers[!grepl(allmarkers$gene,pattern = '^mt-'),]
    }else if(species == 'Rat'){
      allmarkers<-allmarkers[!grepl(allmarkers$gene,pattern = '^Mt-'),]
    }}
  sig_marker<-subset(allmarkers,p_val_adj<0.05)
  top <- sig_marker %>% group_by(cluster) %>% top_n(n = top_gene, wt = avg_log2FC)
  p1<-DoHeatmap(seu, features = as.character(top$gene)) +scale_fill_gradient2(low = "#0000FF",mid = "#FFF8DC",high = "#FF0000")
  return(list(sig_marker,p1))
}
#heatmap sptial
RunFindAllMarkers_Spatial<-function(seu,min.pct = 0.1, species=NULL,logfc.threshold = 0.25,top_gene=10){
  DefaultAssay(seu)<-'Spatial'
  all.genes <- rownames(seu)
  seu <- ScaleData(seu, features = all.genes)
  allmarkers <- FindAllMarkers(seu, only.pos = TRUE, min.pct = min.pct, logfc.threshold = logfc.threshold)
  if(!is.null(species)){
    if(species == 'Human'){
      allmarkers<-allmarkers[!grepl(allmarkers$gene,pattern = '^MT-'),]
    }else if(species == 'Mouse'){
      allmarkers<-allmarkers[!grepl(allmarkers$gene,pattern = '^mt-'),]
    }else if(species == 'Rat'){
      allmarkers<-allmarkers[!grepl(allmarkers$gene,pattern = '^Mt-'),]
    }}
  sig_marker<-subset(allmarkers,p_val_adj<0.05)
  top <- sig_marker %>% group_by(cluster) %>% top_n(n = top_gene, wt = avg_log2FC)
  p1<-DoHeatmap(seu, features = as.character(top$gene)) +scale_fill_gradient2(low = "#0000FF",mid = "#FFF8DC",high = "#FF0000")
  return(list(sig_marker,p1))
}
#cellchat
runcellchat<-function(seurat_rds,group_flag=NULL,flag,species,outdir,lable_level=NULL){
  if(!(is.null(group_flag))){
  print(group_flag)
  seurat_rds<-subset(seurat_rds,group==group_flag)}
  print(seurat_rds)
  if(flag=='seurat_clusters'){
    #identity = data.frame(group = paste0('C',as.character(seurat_rds@meta.data[,flag])), row.names = names(seurat_rds@meta.data[,flag]))}else{ # create a dataframe consisting of the cell labels
    identity = data.frame(group = paste0('C',as.character(seurat_rds@meta.data[,flag])), row.names = rownames(seurat_rds@meta.data))}else{
    identity = data.frame(group = as.character(seurat_rds@meta.data[,flag]), row.names = rownames(seurat_rds@meta.data))}
    #identity = data.frame(group = as.character(seurat_rds@meta.data[,flag]), row.names = names(seurat_rds@meta.data[,flag]))}
  print(unique(identity$group)) 
  cellchat <- createCellChat(object = seurat_rds@assays$RNA@data)# normalized data matrix
  cellchat <- addMeta(cellchat, meta = identity, meta.name = "labels")
  if(!(is.null(lable_level))){
    cellchat@meta$labels<-factor(cellchat@meta$labels,levels=lable_level[lable_level %in% names(table(cellchat@meta$labels))])}
  cellchat <- setIdent(cellchat, ident.use = "labels") # set "labels" as default cell identity
  levels(cellchat@idents) # show factor levels of the cell labels
  groupSize <- as.numeric(table(cellchat@idents)) # number of cells in each cell celltype
  if(species %in% c('human','Human','HUMAN')){
    CellChatDB <- CellChatDB.human}     # use CellChatDB.mouse if running on mouse data
  if(species %in% c('mouse','Mouse','MOUSE')){
    CellChatDB <- CellChatDB.mouse
  }
  showDatabaseCategory(CellChatDB)
  dplyr::glimpse(CellChatDB$interaction)    # Show the structure of the database
  CellChatDB <- subsetDB(CellChatDB, search = "Secreted Signaling")    # use Secreted Signaling for cell-cell communication analysis
  cellchat@DB <- CellChatDB
  #if(species %in% c('mouse','Mouse','MOUSE')){
  #  new_db<-readRDS('/share/home/liny/Project/personality/script/cellchat/cellchat_update_cellchatDB_mouse.rds')
  #  cellchat@DB$interaction<-new_db}
  cellchat <- subsetData(cellchat) # subset the expression data of signaling genes for saving computation cost
  #future::plan("multiprocess", workers = 4) # do parallel
  cellchat <- identifyOverExpressedGenes(cellchat)
  cellchat <- identifyOverExpressedInteractions(cellchat)
  if(species %in% c('human','Human','HUMAN')){
    cellchat <- smoothData(cellchat, adj = PPI.human)}
  if(species %in% c('mouse','Mouse','MOUSE')){
    cellchat <- smoothData(cellchat, adj = PPI.mouse)}
  cellchat <- computeCommunProb(cellchat)
  cellchat <- filterCommunication(cellchat, min.cells = 10)
  cellchat <- computeCommunProbPathway(cellchat)
  cellchat <- aggregateNet(cellchat)
  cellchat <- netAnalysis_computeCentrality(cellchat, slot.name = "netP")
  if(!(is.null(group_flag))){
    saveRDS(cellchat, file = paste0(outdir,"/cellchat_",flag,"_",group_flag,".rds"))}else{
  saveRDS(cellchat, file = paste0(outdir,"/cellchat_",flag,".rds"))
  }
}
runcellchat2<-function(seurat_rds,group_flag=NULL,flag,species,outdir,lable_level=NULL){
  if(!(is.null(group_flag))){
  print(group_flag)
  seurat_rds<-subset(seurat_rds,group==group_flag)}
  print(seurat_rds)
  if(flag=='seurat_clusters'){
    #identity = data.frame(group = paste0('C',as.character(seurat_rds@meta.data[,flag])), row.names = names(seurat_rds@meta.data[,flag]))}else{ # create a dataframe consisting of the cell labels
    identity = data.frame(group = paste0('C',as.character(seurat_rds@meta.data[,flag])), row.names = rownames(seurat_rds@meta.data))}else{
    identity = data.frame(group = as.character(seurat_rds@meta.data[,flag]), row.names = rownames(seurat_rds@meta.data))}
    #identity = data.frame(group = as.character(seurat_rds@meta.data[,flag]), row.names = names(seurat_rds@meta.data[,flag]))}
  print(unique(identity$group))
  cellchat <- createCellChat(object = seurat_rds@assays$RNA@data)# normalized data matrix
  cellchat <- addMeta(cellchat, meta = identity, meta.name = "labels")
  if(!(is.null(lable_level))){
    cellchat@meta$labels<-factor(cellchat@meta$labels,levels=lable_level[lable_level %in% names(table(cellchat@meta$labels))])}
  cellchat <- setIdent(cellchat, ident.use = "labels") # set "labels" as default cell identity
  levels(cellchat@idents) # show factor levels of the cell labels
  groupSize <- as.numeric(table(cellchat@idents)) # number of cells in each cell celltype
  if(species %in% c('human','Human','HUMAN')){
    CellChatDB <- CellChatDB.human}     # use CellChatDB.mouse if running on mouse data
  if(species %in% c('mouse','Mouse','MOUSE')){
    CellChatDB <- CellChatDB.mouse
  }
  showDatabaseCategory(CellChatDB)
  dplyr::glimpse(CellChatDB$interaction)    # Show the structure of the database
  #CellChatDB <- subsetDB(CellChatDB, search = "Secreted Signaling")    # use Secreted Signaling for cell-cell communication analysis
  cellchat@DB <- CellChatDB
  #if(species %in% c('mouse','Mouse','MOUSE')){
  #  new_db<-readRDS('/share/home/liny/Project/personality/script/cellchat/cellchat_update_cellchatDB_mouse.rds')
  #  cellchat@DB$interaction<-new_db}
  cellchat <- subsetData(cellchat) # subset the expression data of signaling genes for saving computation cost
  #future::plan("multiprocess", workers = 4) # do parallel
  cellchat <- identifyOverExpressedGenes(cellchat)
  cellchat <- identifyOverExpressedInteractions(cellchat)
  if(species %in% c('human','Human','HUMAN')){
    cellchat <- smoothData(cellchat, adj = PPI.human)}
  if(species %in% c('mouse','Mouse','MOUSE')){
    cellchat <- smoothData(cellchat, adj = PPI.mouse)}
  cellchat <- computeCommunProb(cellchat)
  cellchat <- filterCommunication(cellchat, min.cells = 10)
  cellchat <- computeCommunProbPathway(cellchat)
  cellchat <- aggregateNet(cellchat)
  cellchat <- netAnalysis_computeCentrality(cellchat, slot.name = "netP")
  if(!(is.null(group_flag))){
    saveRDS(cellchat, file = paste0(outdir,"/cellchat_",flag,"_",group_flag,".rds"))}else{
  saveRDS(cellchat, file = paste0(outdir,"/cellchat_",flag,".rds"))
  }
}

#cell type
RunFeaturePlot<-function(Integrated, wd, reduction){
  DefaultAssay(Integrated)<-'RNA'
  setwd(wd)
  MSC = c('H19','WIF1','SFRP2','SFRP1','WIF1','THY1','POSTN','COL3A1')
  Chondrocytes<-c('COL1A1','COL1A2','SPARC','BGN','COL3A1','MYL9','THY1','CCN2','ANXA2','SFRP2','PMEPA1','SERPINH1','SELENOM','SULF1')
  NK = c('KLRF1')
  Mast_cell<-c('CPA3')
  T_cells<-c('CD3D','CD3E','CD3G','TRAC')
  Osteoclast<-c('SLC9B2','SIGLEC15')
  Macrophage<-c('C1QC')
  B_cell<-c('CD79A','MS4A1')
  Endothelial_cells<-c('CDH5','CLDN5','FLT1','PLVAP')
  Neutrophils<-c('FCGR3B','CXCR2')
  Fibroblast<-c('DCN','COL3A1','ACTA2')
  Myeloid<-c('TPSAB1','LILRA4','CD1C','BATF3','CD14','CD16','NLRP3','PLTP','IL1B','FCN1','C1QC','SPP1')
  Monocyte<-c('FCN1')
  megakaryocytic<-c('ITGA2B') #CD41
  erythroid<-c('HBA2')
  platelet<-c('PPBP', 'PF4')
  ILC3<-c('SLC4A10')
  smooth_muscle_cells<-c('RGS5')
  Plasma<-c('JCHAIN')
  DC<-c('FCER1A')
  Langerhans<-c('CD207')
  interested<-c('AFP', 'GLUL', 'GPT', 'FOXP3', 'EPCAM', 'ALB')
  ##
  Idents(Integrated)<-Integrated$seurat_clusters
  p<-FeaturePlot(Integrated, features=NK, label=T, reduction=reduction)
  ggsave(p, filename='NK.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=Mast_cell, label=T, reduction=reduction)
  ggsave(p, filename='Mast_cell.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=Osteoclast, label=T, reduction=reduction)
  ggsave(p, filename='Osteoclast.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=platelet, label=T, reduction=reduction)
  ggsave(p, filename='platelet.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=smooth_muscle_cells, label=T, reduction=reduction)
  ggsave(p, filename='smooth_muscle_cells.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=DC, label=T, reduction=reduction)
  ggsave(p, filename='DC.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=Plasma, label=T, reduction=reduction)
  ggsave(p, filename='Plasma.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=ILC3, label=T, reduction=reduction)
  ggsave(p, filename='ILC3.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=T_cells, label=T, reduction=reduction)
  ggsave(p, filename='T_cells.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=Macrophage, label=T, reduction=reduction)
  ggsave(p, filename='Macrophage.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=B_cell, label=T, reduction=reduction)
  ggsave(p, filename='B_cell.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=Endothelial_cells, label=T, reduction=reduction)
  ggsave(p, filename='Endothelial_cells.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=Neutrophils, label=T, reduction=reduction)
  ggsave(p, filename='Neutrophils.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=Fibroblast, label=T, reduction=reduction)
  ggsave(p, filename='Fibroblast.png',width=10, height=10)
  # p<-FeaturePlot(Integrated, features=Myeloid, label=T, reduction=reduction)
  # ggsave(p, filename='Myeloid.png',width=10, height=10)
  # p<-FeaturePlot(Integrated, features=megakaryocytic, label=T, reduction=reduction)
  # ggsave(p, filename='megakaryocytic.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=erythroid, label=T, reduction=reduction)
  ggsave(p, filename='erythrocytes.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=Langerhans, label=T, reduction=reduction)
  ggsave(p, filename='Langerhans.png',width=10, height=10)
  # p<-FeaturePlot(Integrated, features=interested, label=T, reduction=reduction)
  # ggsave(p, filename='interested.png',width=10, height=10)
  p<-FeaturePlot(Integrated, features=Monocyte, label=T, reduction=reduction)
  ggsave(p, filename='Monocyte.png',width=10, height=10)
  # p<-FeaturePlot(Integrated, features=MSC, label=T, reduction=reduction)
  # ggsave(p, filename='MSC.png',width=10, height=15)
}

###############volcano
library(ggrepel)
Run_diffgene_volcano<-function(diffgene, label_gene_list, cpname, p_val_adj, logFC, outdir){
    volcano <- subset(diffgene,select=c(gene,avg_log2FC,p_val_adj))
    min_p_val_adj<-ceiling(-log10(min(volcano[volcano$p_val_adj!=0,]$p_val_adj)))
    volcano$log_p_val_adj<--log10(volcano$p_val_adj)
    if('Inf' %in% volcano$log_p_val_adj){
      volcano[volcano$log_p_val_adj=='Inf',]$log_p_val_adj<-min_p_val_adj+100}
    p_val_adj<-as.numeric(p_val_adj)
    logFC<-as.numeric(logFC)
    mark_label<-c()
    for(each in volcano$gene){
      if(each %in% label_gene_list){
       mark_label<-c(mark_label,each)
      }else{
      mark_label<-c(mark_label,"")
      }
    }
    volcano$label<-mark_label
    volcano["group"] <- "NO"
    avg_log2FC=logFC
    avg_log2FC2=-logFC
    volcano[which(volcano["p_val_adj"] <= p_val_adj & volcano["avg_log2FC"] >= avg_log2FC),"group"] <- "UP"
    volcano[which(volcano["p_val_adj"] <= p_val_adj & volcano["avg_log2FC"] <= avg_log2FC2),"group"] <- "DOWN"
    NO_number <- nrow(volcano[which(volcano["group"]=='NO'),])
    UP_number <- nrow(volcano[which(volcano["group"]=='UP'),])
    DOWN_number <- nrow(volcano[which(volcano["group"]=='DOWN'),])
    volcano[which(volcano["group"]=='NO'),"group"] <- paste('NO',NO_number,sep=' ')
    volcano[which(volcano["group"]=='UP'),"group"] <- paste('UP',UP_number,sep=' ')
    volcano[which(volcano["group"]=='DOWN'),"group"] <- paste('DOWN',DOWN_number,sep=' ')
    volcano$group <- factor(volcano$group,levels=c(paste('UP',UP_number,sep=' '),paste('DOWN',DOWN_number,sep=' '),paste('NO',NO_number,sep=' ')))
    fc_min <- min(volcano$avg_log2FC)
    fc_max <- max(volcano$avg_log2FC)
    x_breaks <- c(-log10(p_val_adj),pretty(-log10(volcano$p_val_adj),n=3))
    if (length(unique(round(pretty(-log10(volcano$p_val_adj),n=3))))==length(round(pretty(-log10(volcano$p_val_adj),n=3)))){
    x_labels <- c(round(-log10(p_val_adj),3),round(pretty(-log10(volcano$p_val_adj),n=3)))}else{
    x_labels <- c(round(-log10(p_val_adj),3),pretty(-log10(volcano$p_val_adj),n=3))
    }
    P <- ggplot(volcano,aes(x=avg_log2FC,y=log_p_val_adj,group=group,color=group)) +
       geom_point(size=4) +ylab('-log10(p_val_adj)')+
       labs(title=cpname,color=paste('p_val_adj<',p_val_adj,sep='')) +
       #geom_text(aes(label = label), size = 3,vjust=-0.5, alpha=0.8)+
       theme(plot.title=element_text(hjust=0.5),text = element_text(size = 20)) +
       scale_x_continuous(breaks=c(-logFC,logFC,round(seq(fc_min,fc_max,by=(fc_max-fc_min)/4)))) +
       scale_y_continuous(breaks=x_breaks[x_breaks!=0],labels=x_labels[x_labels!=0]) +
       geom_vline(xintercept=c(-logFC,logFC),linetype='dotdash',size=0.8,color='grey') +
       geom_hline(yintercept=-log10(p_val_adj),linetype='dotdash',size=0.8,color='grey')+
       geom_text_repel(data = volcano, aes(x = avg_log2FC, y = log_p_val_adj,label = label),size = 3,max.overlaps=160,box.padding = unit(0.35, "lines"),point.padding = unit(0.3, "lines"))
       #geom_text_repel(data = volcano, aes(x = avg_log2FC, y = log_p_val_adj,label = label),size = 4,box.padding = unit(0.5, "lines"),point.padding = unit(0, "lines"),segment.color = "black",show.legend = F,nudge_y = 0)
       #geom_text_repel(data = volcano, aes(x = avg_log2FC, y = log_p_val_adj,label = label),size = 2.5,box.padding = unit(0.5, "lines"),point.padding = unit(0.5, "lines"),segment.color = "black",show.legend = F,nudge_y = 0.1)
    P <- P + theme(panel.background=element_rect(fill="transparent"),axis.line=element_line())
    plot(P)
    ggsave(P,file=paste(outdir,'/',cpname,'_volcano.png',sep=''),width = 10,height = 10)
    ggsave(P,file=paste(outdir,'/',cpname,'_volcano.pdf',sep=''),width = 10,height = 10)
    return(volcano)
}

Run_diffgene_volcano_p_val<-function(diffgene, label_gene_list, cpname, p_val, logFC, outdir){
    volcano <- subset(diffgene,select=c(gene,avg_log2FC,p_val))
    min_p_val<-ceiling(-log10(min(volcano[volcano$p_val!=0,]$p_val)))
    volcano$log_p_val<--log10(volcano$p_val)
    if('Inf' %in% volcano$log_p_val){
      volcano[volcano$log_p_val=='Inf',]$log_p_val<-min_p_val+100}
    p_val<-as.numeric(p_val)
    logFC<-as.numeric(logFC)
    mark_label<-c()
    for(each in volcano$gene){
      if(each %in% label_gene_list){
       mark_label<-c(mark_label,each)
      }else{
      mark_label<-c(mark_label,"")
      }
    }
    volcano$label<-mark_label
    volcano["group"] <- "NO"
    avg_log2FC=logFC
    avg_log2FC2=-logFC
    volcano[which(volcano["p_val"] <= p_val & volcano["avg_log2FC"] >= avg_log2FC),"group"] <- "UP"
    volcano[which(volcano["p_val"] <= p_val & volcano["avg_log2FC"] <= avg_log2FC2),"group"] <- "DOWN"
    NO_number <- nrow(volcano[which(volcano["group"]=='NO'),])
    UP_number <- nrow(volcano[which(volcano["group"]=='UP'),])
    DOWN_number <- nrow(volcano[which(volcano["group"]=='DOWN'),])
    volcano[which(volcano["group"]=='NO'),"group"] <- paste('NO',NO_number,sep=' ')
    volcano[which(volcano["group"]=='UP'),"group"] <- paste('UP',UP_number,sep=' ')
    volcano[which(volcano["group"]=='DOWN'),"group"] <- paste('DOWN',DOWN_number,sep=' ')
    volcano$group <- factor(volcano$group,levels=c(paste('UP',UP_number,sep=' '),paste('DOWN',DOWN_number,sep=' '),paste('NO',NO_number,sep=' ')))
    fc_min <- min(volcano$avg_log2FC)
    fc_max <- max(volcano$avg_log2FC)
    x_breaks <- c(-log10(p_val),pretty(-log10(volcano$p_val),n=3))
    if (length(unique(round(pretty(-log10(volcano$p_val),n=3))))==length(round(pretty(-log10(volcano$p_val),n=3)))){
    x_labels <- c(round(-log10(p_val),3),round(pretty(-log10(volcano$p_val),n=3)))}else{
    x_labels <- c(round(-log10(p_val),3),pretty(-log10(volcano$p_val),n=3))
    }
    P <- ggplot(volcano,aes(x=avg_log2FC,y=log_p_val,group=group,color=group)) +
       geom_point(size=4) +ylab('-log10(p_val)')+
       labs(title=cpname,color=paste('p_val<',p_val,sep='')) +
       #geom_text(aes(label = label), size = 3,vjust=-0.5, alpha=0.8)+
       theme(plot.title=element_text(hjust=0.5),text = element_text(size = 20)) +
       scale_x_continuous(breaks=c(-logFC,logFC,round(seq(fc_min,fc_max,by=(fc_max-fc_min)/4)))) +
       scale_y_continuous(breaks=x_breaks[x_breaks!=0],labels=x_labels[x_labels!=0]) +
       geom_vline(xintercept=c(-logFC,logFC),linetype='dotdash',size=0.8,color='grey') +
       geom_hline(yintercept=-log10(p_val),linetype='dotdash',size=0.8,color='grey')+
       geom_text_repel(data = volcano, aes(x = avg_log2FC, y = log_p_val,label = label),size = 3,max.overlaps=160,box.padding = unit(0.35, "lines"),point.padding = unit(0.3, "lines"))
       #geom_text_repel(data = volcano, aes(x = avg_log2FC, y = log_p_val,label = label),size = 4,box.padding = unit(0.5, "lines"),point.padding = unit(0, "lines"),segment.color = "black",show.legend = F,nudge_y = 0)
       #geom_text_repel(data = volcano, aes(x = avg_log2FC, y = log_p_val,label = label),size = 2.5,box.padding = unit(0.5, "lines"),point.padding = unit(0.5, "lines"),segment.color = "black",show.legend = F,nudge_y = 0.1)
    P <- P + theme(panel.background=element_rect(fill="transparent"),axis.line=element_line())
    plot(P)
    ggsave(P,file=paste(outdir,'/',cpname,'_volcano.png',sep=''),width = 10,height = 10)
    ggsave(P,file=paste(outdir,'/',cpname,'_volcano.pdf',sep=''),width = 10,height = 10)
    return(volcano)
}

Run_diffgene_volcano_limma<-function(diffgene, label_gene_list, cpname, padj, logFC, outdir){
    volcano <- diffgene[,c('logFC','adj.P.Val')]
    min_padj<-ceiling(-log10(min(volcano[volcano$adj.P.Val!=0,]$adj.P.Val)))
    volcano$log_padj<--log10(volcano$adj.P.Val)
    if('Inf' %in% volcano$log_padj){
      volcano[volcano$log_padj=='Inf',]$log_padj<-min_padj+100}
    padj<-as.numeric(padj)
    logFC<-as.numeric(logFC)
    mark_label<-c()
    for(each in rownames(volcano)){
      if(each %in% label_gene_list){
       mark_label<-c(mark_label,each)
      }else{
      mark_label<-c(mark_label,"")
      }
    }
    volcano$label<-mark_label
    volcano["group"] <- "NO"
    logFC=logFC
    logFC2=-logFC
    volcano[which(volcano["adj.P.Val"] <= padj & volcano["logFC"] >= logFC),"group"] <- "UP"
    volcano[which(volcano["adj.P.Val"] <= padj & volcano["logFC"] <= logFC2),"group"] <- "DOWN"
    NO_number <- nrow(volcano[which(volcano["group"]=='NO'),])
    UP_number <- nrow(volcano[which(volcano["group"]=='UP'),])
    DOWN_number <- nrow(volcano[which(volcano["group"]=='DOWN'),])
    volcano[which(volcano["group"]=='NO'),"group"] <- paste('NO',NO_number,sep=' ')
    volcano[which(volcano["group"]=='UP'),"group"] <- paste('UP',UP_number,sep=' ')
    volcano[which(volcano["group"]=='DOWN'),"group"] <- paste('DOWN',DOWN_number,sep=' ')
    volcano$group <- factor(volcano$group,levels=c(paste('UP',UP_number,sep=' '),paste('DOWN',DOWN_number,sep=' '),paste('NO',NO_number,sep=' ')))
    fc_min <- min(volcano$logFC)
    fc_max <- max(volcano$logFC)
    x_breaks <- c(-log10(padj),pretty(-log10(volcano$adj.P.Val),n=3))
    if (length(unique(round(pretty(-log10(volcano$adj.P.Val),n=3))))==length(round(pretty(-log10(volcano$adj.P.Val),n=3)))){
    x_labels <- c(round(-log10(padj),3),round(pretty(-log10(volcano$adj.P.Val),n=3)))}else{
    x_labels <- c(round(-log10(padj),3),pretty(-log10(volcano$adj.P.Val),n=3))
    }
    P <- ggplot(volcano,aes(x=logFC,y=log_padj,group=group,color=group)) +
       geom_point() +ylab('-log10(padj)')+
       labs(title=cpname,color=paste('padj<',padj,sep='')) +
       theme(plot.title=element_text(hjust=0.5)) +
       scale_x_continuous(breaks=c(-logFC,logFC,round(seq(fc_min,fc_max,by=(fc_max-fc_min)/4)))) +
       scale_y_continuous(breaks=x_breaks[x_breaks!=0],labels=x_labels[x_labels!=0]) +
       geom_vline(xintercept=c(-logFC,logFC),linetype='dotdash',size=0.8,color='grey') +
       geom_hline(yintercept=-log10(padj),linetype='dotdash',size=0.8,color='grey')+
       geom_text_repel(data = volcano, aes(x = logFC, y = log_padj,label = label),size = 2.5,box.padding = unit(0.5, "lines"),point.padding = unit(0, "lines"),segment.color = "black",show.legend = F,nudge_y = 0)
       #geom_text_repel(data = volcano, aes(x = logFC, y = log_padj,label = label),size = 2.5,box.padding = unit(0.5, "lines"),point.padding = unit(0.5, "lines"),segment.color = "black",show.legend = F,nudge_y = 0.1)
    P <- P + theme(panel.background=element_rect(fill="transparent"),axis.line=element_line())
    plot(P)
    ggsave(P,file=paste(outdir,'/',cpname,'_volcano.png',sep=''),width = 15,height = 12)
    ggsave(P,file=paste(outdir,'/',cpname,'_volcano.pdf',sep=''),width = 15,height = 12)
    return(volcano)
}

#GSVA
flag_gsva_analysis<-function(seurat_rds,GSVA_data,flag,ourdir){
  all_Hallmark_Epithelial_GSVA_seurat_data<-data.frame()
  for (each in levels(seurat_rds@meta.data[,flag])) {
    Hallmark_Epithelial_GSVA_data<-GSVA_data[,rownames(seurat_rds@meta.data[seurat_rds@meta.data[,flag]==each,])]
    Hallmark_Epithelial_GSVA_seurat_data<-data.frame(data=rowMeans(Hallmark_Epithelial_GSVA_data))
    colnames(Hallmark_Epithelial_GSVA_seurat_data)<-each
    if (nrow(all_Hallmark_Epithelial_GSVA_seurat_data)==0) {
      all_Hallmark_Epithelial_GSVA_seurat_data<-Hallmark_Epithelial_GSVA_seurat_data
    }else{all_Hallmark_Epithelial_GSVA_seurat_data<-cbind(all_Hallmark_Epithelial_GSVA_seurat_data,Hallmark_Epithelial_GSVA_seurat_data)}
  }
  rownames(all_Hallmark_Epithelial_GSVA_seurat_data)<-gsub(pattern = 'HALLMARK_',replacement = '',rownames(all_Hallmark_Epithelial_GSVA_seurat_data))
  p1<-pheatmap(as.matrix(all_Hallmark_Epithelial_GSVA_seurat_data),
         scale="row", cluster_row = F, cluster_cols = F,
         color = gplots::bluered(75), #
         show_rownames = T, show_colnames = T, fontsize_col = 6,
         clustering_method = "ward.D2", clustering_distance_rows = "euclidean")
  if (flag=='orig.ident') {
    flag='sample'
  }
  ggsave(plot = p1,filename = paste0(ourdir,'/',flag,'_GSVA.png'),width = 15,height = 10)
  ggsave(plot = p1,filename = paste0(ourdir,'/',flag,'_GSVA.pdf'),width = 15,height = 10)
}
flag_gsva_analysis_group<-function(seurat_rds,GSVA_data,flag,ourdir,prefix){
  all_Hallmark_Epithelial_GSVA_seurat_data<-data.frame()
  for (each in levels(seurat_rds@meta.data[,flag])) {
    for (each_group in levels(seurat_rds@meta.data$group)) {
      Hallmark_Epithelial_GSVA_data<-as.data.frame(GSVA_data[,rownames(seurat_rds@meta.data[seurat_rds@meta.data[,flag]==each & seurat_rds$group==each_group,])])
      rownames(Hallmark_Epithelial_GSVA_data)<-rownames(GSVA_data)
      Hallmark_Epithelial_GSVA_seurat_data<-data.frame(data=rowMeans(Hallmark_Epithelial_GSVA_data))
      colnames(Hallmark_Epithelial_GSVA_seurat_data)<-paste0(each,'_',each_group)
      if (nrow(all_Hallmark_Epithelial_GSVA_seurat_data)==0) {
        all_Hallmark_Epithelial_GSVA_seurat_data<-Hallmark_Epithelial_GSVA_seurat_data
      }else{all_Hallmark_Epithelial_GSVA_seurat_data<-cbind(all_Hallmark_Epithelial_GSVA_seurat_data,Hallmark_Epithelial_GSVA_seurat_data)}
    }}
  rownames(all_Hallmark_Epithelial_GSVA_seurat_data)<-gsub(pattern = 'HALLMARK_|GO_|KEGG_',replacement = '',rownames(all_Hallmark_Epithelial_GSVA_seurat_data))
  color <- colorRampPalette(c("#2161A0", "white", "firebrick3"))(100)
  p1<-pheatmap::pheatmap(as.matrix(all_Hallmark_Epithelial_GSVA_seurat_data),
                         scale="row", cluster_row = T, cluster_cols = F,
                         color=color,breaks=seq(-1.5, 1.5, length.out = 100),#color = gplots::bluered(75),
                         show_rownames = T, show_colnames = T, fontsize=8,angle_col = 45,#cellwidth = 35,cellheight = 35,
                         clustering_method = "ward.D2", clustering_distance_rows = "euclidean")
  if (flag=='orig.ident') {
    flag='sample'
  }
  ggsave(p1,filename = paste0(ourdir,'/',prefix,'_group_GSVA.png'),width = 15,height = 10)
  ggsave(p1,filename = paste0(ourdir,'/',prefix,'_group_GSVA.pdf'),width = 15,height = 10)
  write.table(all_Hallmark_Epithelial_GSVA_seurat_data,file = paste0(ourdir,'/',prefix,'_group_GSVA.xls'),quote = F,sep = '\t',col.names = NA)
}
#Cellphone
cellphone_analysis_file<-function(seurat_rds,outflag=NULL,groupflag=NULL,annotflag=NULL,ourdir){
  if(!is.null(outflag)){
    if(outflag=='annot'){
      if(!is.null(groupflag)){
        if(groupflag=='seurat_clusters'){
          annot_data<-data.frame(rownames(seurat_rds@meta.data),paste0('cluster',seurat_rds@meta.data[,groupflag],'_',seurat_rds@meta.data[,annotflag]))
          write.table(annot_data,file=paste0(ourdir,"/cell_celltype_annot_group.txt"),quote=F,sep="\t",col.names=F,row.names=F)}else{
          annot_data<-data.frame(rownames(seurat_rds@meta.data),paste0(seurat_rds@meta.data[,groupflag],'_',seurat_rds@meta.data[,annotflag]))
          write.table(annot_data,file=paste0(ourdir,"/cell_celltype_annot_group.txt"),quote=F,sep="\t",col.names=F,row.names=F)}
      }else{
        annot_data<-data.frame(rownames(seurat_rds@meta.data),paste0(seurat_rds@meta.data[,annotflag]))
        write.table(annot_data,file=paste0(ourdir,"/cell_celltype_annot.txt"),quote=F,sep="\t",col.names=F,row.names=F)
      }
    }
    if(outflag=='count'){
      count_norm <- apply(seurat_rds@assays$RNA@counts, 2, function(x) (x/sum(x))*10000)#cellphone推荐的标准化方法
      write.table(count_norm, file=paste0(ourdir,"/normalized_counts.txt"), sep='\t', quote=F)
    }}
  if(is.null(outflag)){
    if(!is.null(groupflag)){
      if(groupflag=='seurat_clusters'){
        annot_data<-data.frame(rownames(seurat_rds@meta.data),paste0('cluster',seurat_rds@meta.data[,groupflag],'_',seurat_rds@meta.data[,annotflag]))
        write.table(annot_data,file=paste0(ourdir,"/cell_celltype_annot_group.txt"),quote=F,sep="\t",col.names=F,row.names=F)}else{
        annot_data<-data.frame(rownames(seurat_rds@meta.data),paste0(seurat_rds@meta.data[,groupflag],'_',seurat_rds@meta.data[,annotflag]))
        write.table(annot_data,file=paste0(ourdir,"/cell_celltype_annot_group.txt"),quote=F,sep="\t",col.names=F,row.names=F)}
    }else{
      annot_data<-data.frame(rownames(seurat_rds@meta.data),paste0(seurat_rds@meta.data[,annotflag]))                                       
      write.table(annot_data,file=paste0(ourdir,"/cell_celltype_annot.txt"),quote=F,sep="\t",col.names=F,row.names=F)
    }
    count_norm <- apply(seurat_rds@assays$RNA@counts, 2, function(x) (x/sum(x))*10000)#cellphone推荐的标准化方法
    write.table(count_norm, file=paste0(ourdir,"/normalized_counts.txt"), sep='\t', quote=F)
  }
}

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}
#monocle plot_genes_in_pseudotime data
plot_genes_in_pseudotime2<-function(cds_subset, min_expr = NULL, cell_size = 0.75, nrow = NULL, 
          ncol = 1, panel_order = NULL, color_by = "State", trend_formula = "~ sm.ns(Pseudotime, df=3)", 
          label_by_short_name = TRUE, relative_expr = TRUE, vertical_jitter = NULL, 
          horizontal_jitter = NULL) 
{
  f_id <- NA
  Cell <- NA
  if (cds_subset@expressionFamily@vfamily %in% c("negbinomial", 
                                                 "negbinomial.size")) {
    integer_expression <- TRUE
  }
  else {
    integer_expression <- FALSE
    relative_expr <- TRUE
  }
  if (integer_expression) {
    cds_exprs <- exprs(cds_subset)
    if (relative_expr) {
      if (is.null(sizeFactors(cds_subset))) {
        stop("Error: to call this function with relative_expr=TRUE, you must call estimateSizeFactors() first")
      }
      cds_exprs <- Matrix::t(Matrix::t(cds_exprs)/sizeFactors(cds_subset))
    }
    cds_exprs <- reshape2::melt(round(as.matrix(cds_exprs)))
  }
  else {
    cds_exprs <- reshape2::melt(as.matrix(exprs(cds_subset)))
  }
  if (is.null(min_expr)) {
    min_expr <- cds_subset@lowerDetectionLimit
  }
  colnames(cds_exprs) <- c("f_id", "Cell", "expression")
  cds_pData <- pData(cds_subset)
  cds_fData <- fData(cds_subset)
  cds_exprs <- merge(cds_exprs, cds_fData, by.x = "f_id", by.y = "row.names")
  cds_exprs <- merge(cds_exprs, cds_pData, by.x = "Cell", by.y = "row.names")
  if (integer_expression) {
    cds_exprs$adjusted_expression <- cds_exprs$expression
  }
  else {
    cds_exprs$adjusted_expression <- log10(cds_exprs$expression)
  }
  if (label_by_short_name == TRUE) {
    if (is.null(cds_exprs$gene_short_name) == FALSE) {
      cds_exprs$feature_label <- as.character(cds_exprs$gene_short_name)
      cds_exprs$feature_label[is.na(cds_exprs$feature_label)] <- cds_exprs$f_id
    }
    else {
      cds_exprs$feature_label <- cds_exprs$f_id
    }
  }
  else {
    cds_exprs$feature_label <- cds_exprs$f_id
  }
  cds_exprs$f_id <- as.character(cds_exprs$f_id)
  cds_exprs$feature_label <- factor(cds_exprs$feature_label)
  new_data <- data.frame(Pseudotime = pData(cds_subset)$Pseudotime)
  model_expectation <- genSmoothCurves(cds_subset, cores = 1, 
                                       trend_formula = trend_formula, relative_expr = T, new_data = new_data)
  colnames(model_expectation) <- colnames(cds_subset)
  expectation <- ddply(cds_exprs, .(f_id, Cell), function(x) data.frame(expectation = model_expectation[x$f_id, 
                                                                                                        x$Cell]))
  cds_exprs <- merge(cds_exprs, expectation)
  cds_exprs$expression[cds_exprs$expression < min_expr] <- min_expr
  cds_exprs$expectation[cds_exprs$expectation < min_expr] <- min_expr
  if (is.null(panel_order) == FALSE) {
    cds_exprs$feature_label <- factor(cds_exprs$feature_label, 
                                      levels = panel_order)
  }
  q <- ggplot(aes(Pseudotime, expression,group=group,color=group), data = cds_exprs)
  if (is.null(color_by) == FALSE) {
    q <- q + geom_point(aes_string(color = color_by), size = I(cell_size), 
                        position = position_jitter(horizontal_jitter, vertical_jitter))
  }
  else {
    q <- q + geom_point(size = I(cell_size), position = position_jitter(horizontal_jitter, 
                                                                        vertical_jitter))
  }
  q <- q + geom_line(aes(x = Pseudotime, y = expectation), 
                     data = cds_exprs)
  q <- q + scale_y_log10() + facet_wrap(~feature_label, nrow = nrow, 
                                        ncol = ncol, scales = "free_y")
  if (min_expr < 1) {
    q <- q + expand_limits(y = c(min_expr, 1))
  }
  if (relative_expr) {
    q <- q + ylab("Relative Expression")
  }
  else {
    q <- q + ylab("Absolute Expression")
  }
  q <- q + xlab("Pseudo-time")
  #q <- q + monocle_theme_opts()
  #q
  return(cds_exprs)
  #p1<-ggplot(aes(Pseudotime, expression,group=group,color=group), data = cds_exprs)+geom_point()+ geom_smooth(method = 'loess',level=0)+
  #labs(x="Pseudo-time",y="Relative Expression")+ scale_y_log10() + facet_wrap(~f_id, ncol = 2, scales = "free_y")+
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),plot.title = element_text(hjust = 0.5))
  #return(list(cds_exprs,p1))
}
#plot_genes_branched_pseudotime2
plot_genes_branched_pseudotime2<-function(cds, branch_states = NULL, branch_point = 1, branch_labels = NULL,
                                           method = "fitting", min_expr = NULL, cell_size = 0.75, nrow = NULL,
                                           ncol = 1, panel_order = NULL, color_by = "State", expression_curve_linetype_by = "Branch",
                                           trend_formula = "~ sm.ns(Pseudotime, df=3) * Branch", reducedModelFormulaStr = NULL,
                                           label_by_short_name = TRUE, relative_expr = TRUE, ...)
{
  Branch <- NA
  if (is.null(reducedModelFormulaStr) == FALSE) {
    pval_df <- branchTest(cds, branch_states = branch_states,
                          branch_point = branch_point, fullModelFormulaStr = trend_formula,
                          reducedModelFormulaStr = "~ sm.ns(Pseudotime, df=3)",
                          ...)
    fData(cds)[, "pval"] <- pval_df[row.names(cds), "pval"]
  }
  if ("Branch" %in% all.vars(terms(as.formula(trend_formula)))) {
    cds_subset <- buildBranchCellDataSet(cds = cds, branch_states = branch_states,
                                         branch_point = branch_point, branch_labels = branch_labels,
                                         progenitor_method = "duplicate", ...)
  }
  else {
    cds_subset <- cds
    pData(cds_subset)$Branch <- pData(cds_subset)$State
  }
  if (cds_subset@expressionFamily@vfamily %in% c("negbinomial",
                                                 "negbinomial.size")) {
    integer_expression <- TRUE
  }
  else {
    integer_expression <- FALSE
  }
  if (integer_expression) {
    CM <- exprs(cds_subset)
    if (relative_expr) {
      if (is.null(sizeFactors(cds_subset))) {
        stop("Error: to call this function with relative_expr=TRUE, you must call estimateSizeFactors() first")
      }
      CM <- Matrix::t(Matrix::t(CM)/sizeFactors(cds_subset))
    }
    cds_exprs <- reshape2::melt(round(as.matrix(CM)))
  }
  else {
    cds_exprs <- reshape2::melt(exprs(cds_subset))
  }
  if (is.null(min_expr)) {
    min_expr <- cds_subset@lowerDetectionLimit
  }
  colnames(cds_exprs) <- c("f_id", "Cell", "expression")
  cds_pData <- pData(cds_subset)
  cds_fData <- fData(cds_subset)
  cds_exprs <- merge(cds_exprs, cds_fData, by.x = "f_id", by.y = "row.names")
  cds_exprs <- merge(cds_exprs, cds_pData, by.x = "Cell", by.y = "row.names")
  if (integer_expression) {
    cds_exprs$adjusted_expression <- round(cds_exprs$expression)
  }
  else {
    cds_exprs$adjusted_expression <- log10(cds_exprs$expression)
  }
  if (label_by_short_name == TRUE) {
    if (is.null(cds_exprs$gene_short_name) == FALSE) {
      cds_exprs$feature_label <- as.character(cds_exprs$gene_short_name)
      cds_exprs$feature_label[is.na(cds_exprs$feature_label)] <- cds_exprs$f_id
    }
    else {
      cds_exprs$feature_label <- cds_exprs$f_id
    }
  }
  else {
    cds_exprs$feature_label <- cds_exprs$f_id
  }
  cds_exprs$feature_label <- as.factor(cds_exprs$feature_label)
  cds_exprs$Branch <- as.factor(cds_exprs$Branch)
  new_data <- data.frame(Pseudotime = pData(cds_subset)$Pseudotime,
                         Branch = pData(cds_subset)$Branch)
  full_model_expectation <- genSmoothCurves(cds_subset, cores = 1,
                                            trend_formula = trend_formula, relative_expr = T, new_data = new_data)
  colnames(full_model_expectation) <- colnames(cds_subset)
  cds_exprs$full_model_expectation <- apply(cds_exprs, 1, function(x) full_model_expectation[x[2],
                                                                                             x[1]])
  if (!is.null(reducedModelFormulaStr)) {
    reduced_model_expectation <- genSmoothCurves(cds_subset,
                                                 cores = 1, trend_formula = reducedModelFormulaStr,
                                                 relative_expr = T, new_data = new_data)
    colnames(reduced_model_expectation) <- colnames(cds_subset)
    cds_exprs$reduced_model_expectation <- apply(cds_exprs,
                                                 1, function(x) reduced_model_expectation[x[2], x[1]])
  }
  if (method == "loess")
    cds_exprs$expression <- cds_exprs$expression + cds@lowerDetectionLimit
  if (label_by_short_name == TRUE) {
    if (is.null(cds_exprs$gene_short_name) == FALSE) {
      cds_exprs$feature_label <- as.character(cds_exprs$gene_short_name)
      cds_exprs$feature_label[is.na(cds_exprs$feature_label)] <- cds_exprs$f_id
    }
    else {
      cds_exprs$feature_label <- cds_exprs$f_id
    }
  }
  else {
    cds_exprs$feature_label <- cds_exprs$f_id
  }
  cds_exprs$feature_label <- factor(cds_exprs$feature_label)
  if (is.null(panel_order) == FALSE) {
    cds_exprs$feature_label <- factor(cds_exprs$feature_label,
                                      levels = panel_order)
  }
  cds_exprs$expression[is.na(cds_exprs$expression)] <- min_expr
  cds_exprs$expression[cds_exprs$expression < min_expr] <- min_expr
  cds_exprs$full_model_expectation[is.na(cds_exprs$full_model_expectation)] <- min_expr
  cds_exprs$full_model_expectation[cds_exprs$full_model_expectation <
                                     min_expr] <- min_expr
  if (!is.null(reducedModelFormulaStr)) {
    cds_exprs$reduced_model_expectation[is.na(cds_exprs$reduced_model_expectation)] <- min_expr
    cds_exprs$reduced_model_expectation[cds_exprs$reduced_model_expectation <
                                          min_expr] <- min_expr
  }
  cds_exprs$State <- as.factor(cds_exprs$State)
  cds_exprs$Branch <- as.factor(cds_exprs$Branch)
  cds_exprs$Pseudotime<-100-as.numeric(cds_exprs$Pseudotime)
  q <- ggplot(aes(Pseudotime, expression), data = cds_exprs)
  if (is.null(color_by) == FALSE) {
    q <- q + geom_point(aes_string(color = color_by), size = I(cell_size))
  }
  if (is.null(reducedModelFormulaStr) == FALSE)
    q <- q + scale_y_log10() + facet_wrap(~feature_label +
                                            pval, nrow = nrow, ncol = ncol, scales = "free_y")
  else q <- q + scale_y_log10() + facet_wrap(~feature_label,
                                             nrow = nrow, ncol = ncol, scales = "free_y")
  if (method == "loess")
    q <- q + stat_smooth(aes(fill = Branch, color = Branch),
                         method = "loess")
  else if (method == "fitting") {
    q <- q + geom_line(aes_string(x = "Pseudotime", y = "full_model_expectation",
                                  linetype = "Branch"), data = cds_exprs)
  }
  if (!is.null(reducedModelFormulaStr)) {
    q <- q + geom_line(aes_string(x = "Pseudotime", y = "reduced_model_expectation"),
                       color = "black", linetype = 2, data = cds_exprs)
  }
  q <- q + ylab("Expression") + xlab("Pseudotime (stretched)")
  #q <- q + monocle_theme_opts()
  q <- q +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"))
  q + expand_limits(y = min_expr)
  #return(cds_exprs)
}


#monocle plot_pseudotime_heatmap data
get_pseudotime_heatmap_matrix<-function(cds_subset,norm_method = c("log", "vstExprs"), scale_max = 3,
                                        scale_min = -3, trend_formula = "~sm.ns(Pseudotime, df=3)", cores = 1){
  pseudocount <- 1
  newdata <- data.frame(Pseudotime = seq(min(pData(cds_subset)$Pseudotime),
                                         max(pData(cds_subset)$Pseudotime), length.out = 100))
  m <- genSmoothCurves(cds_subset, cores = cores, trend_formula = trend_formula,
                       relative_expr = T, new_data = newdata)
  m = m[!apply(m, 1, sum) == 0, ]
  norm_method <- match.arg(norm_method)
  if (norm_method == "vstExprs" && is.null(cds_subset@dispFitInfo[["blind"]]$disp_func) == FALSE) {
    m = vstExprs(cds_subset, expr_matrix = m)
  }else if (norm_method == "log") {
    m = log10(m + pseudocount)
  }
  m = m[!apply(m, 1, sd) == 0, ]
  m = Matrix::t(scale(Matrix::t(m), center = TRUE))
  m = m[is.na(row.names(m)) == FALSE, ]
  m[is.nan(m)] = 0
  m[m > scale_max] = scale_max
  m[m < scale_min] = scale_min
  return(m)
}

get_plot_cell_trajectory_data<-function (cds, x = 1, y = 2, color_by = "State", show_tree = TRUE, 
    show_backbone = TRUE, backbone_color = "black", markers = NULL, 
    use_color_gradient = FALSE, markers_linear = FALSE, show_cell_names = FALSE, 
    show_state_number = FALSE, cell_size = 1.5, cell_link_size = 0.75, 
    cell_name_size = 2, state_number_size = 2.9, show_branch_points = TRUE, 
    theta = 0, ...) 
{
    requireNamespace("igraph")
    gene_short_name <- NA
    sample_name <- NA
    sample_state <- pData(cds)$State
    data_dim_1 <- NA
    data_dim_2 <- NA
    lib_info_with_pseudo <- pData(cds)
    if (is.null(cds@dim_reduce_type)) {
        stop("Error: dimensionality not yet reduced. Please call reduceDimension() before calling this function.")
    }
    if (cds@dim_reduce_type == "ICA") {
        reduced_dim_coords <- reducedDimS(cds)
    }
    else if (cds@dim_reduce_type %in% c("simplePPT", "DDRTree")) {
        reduced_dim_coords <- reducedDimK(cds)
    }
    else {
        stop("Error: unrecognized dimensionality reduction method.")
    }
    ica_space_df <- Matrix::t(reduced_dim_coords) %>% as.data.frame() %>% 
        select_(prin_graph_dim_1 = x, prin_graph_dim_2 = y) %>% 
        mutate(sample_name = rownames(.), sample_state = rownames(.))
    dp_mst <- minSpanningTree(cds)
    if (is.null(dp_mst)) {
        stop("You must first call orderCells() before using this function")
    }
    edge_df <- dp_mst %>% igraph::as_data_frame() %>% select_(source = "from", 
        target = "to") %>% left_join(ica_space_df %>% select_(source = "sample_name", 
        source_prin_graph_dim_1 = "prin_graph_dim_1", source_prin_graph_dim_2 = "prin_graph_dim_2"), 
        by = "source") %>% left_join(ica_space_df %>% select_(target = "sample_name", 
        target_prin_graph_dim_1 = "prin_graph_dim_1", target_prin_graph_dim_2 = "prin_graph_dim_2"), 
        by = "target")
    data_df <- t(monocle::reducedDimS(cds)) %>% as.data.frame() %>% 
        select_(data_dim_1 = x, data_dim_2 = y) %>% rownames_to_column("sample_name") %>% 
        mutate(sample_state) %>% left_join(lib_info_with_pseudo %>% 
        rownames_to_column("sample_name"), by = "sample_name")
    return_rotation_mat <- function(theta) {
        theta <- theta/180 * pi
        matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 
            nrow = 2)
    }
    rot_mat <- return_rotation_mat(theta)
    cn1 <- c("data_dim_1", "data_dim_2")
    cn2 <- c("source_prin_graph_dim_1", "source_prin_graph_dim_2")
    cn3 <- c("target_prin_graph_dim_1", "target_prin_graph_dim_2")
    data_df[, cn1] <- as.matrix(data_df[, cn1]) %*% t(rot_mat)
    edge_df[, cn2] <- as.matrix(edge_df[, cn2]) %*% t(rot_mat)
    edge_df[, cn3] <- as.matrix(edge_df[, cn3]) %*% t(rot_mat)
    markers_exprs <- NULL
    if (is.null(markers) == FALSE) {
        markers_fData <- subset(fData(cds), gene_short_name %in% 
            markers)
        if (nrow(markers_fData) >= 1) {
            markers_exprs <- reshape2::melt(as.matrix(exprs(cds[row.names(markers_fData), 
                ])))
            colnames(markers_exprs)[1:2] <- c("feature_id", "cell_id")
            markers_exprs <- merge(markers_exprs, markers_fData, 
                by.x = "feature_id", by.y = "row.names")
            markers_exprs$feature_label <- as.character(markers_exprs$gene_short_name)
            markers_exprs$feature_label[is.na(markers_exprs$feature_label)] <- markers_exprs$Var1
        }
    }
    if (is.null(markers_exprs) == FALSE && nrow(markers_exprs) > 
        0) {
        data_df <- merge(data_df, markers_exprs, by.x = "sample_name", 
            by.y = "cell_id")
        #if (use_color_gradient) {
        #    if (markers_linear) {
        #        g <- ggplot(data = data_df, aes(x = data_dim_1, 
        #          y = data_dim_2)) + geom_point(aes(color = value), 
        #          size = I(cell_size), na.rm = TRUE) + scale_color_viridis(name = paste0("value"), 
        #          ...) + facet_wrap(~feature_label)
        #    }
        #    else {
        #        g <- ggplot(data = data_df, aes(x = data_dim_1, 
        #          y = data_dim_2)) + geom_point(aes(color = log10(value + 
        #          0.1)), size = I(cell_size), na.rm = TRUE) + 
        #          scale_color_viridis(name = paste0("log10(value + 0.1)"), 
        #            ...) + facet_wrap(~feature_label)
        #    }
        #}
        #else {
        #    if (markers_linear) {
        #        g <- ggplot(data = data_df, aes(x = data_dim_1, 
        #          y = data_dim_2, size = (value * 0.1))) + facet_wrap(~feature_label)
        #    }
        #    else {
        #        g <- ggplot(data = data_df, aes(x = data_dim_1, 
        #          y = data_dim_2, size = log10(value + 0.1))) + 
        #          facet_wrap(~feature_label)
        #    }
        #}
    }
    #else {
    #    g <- ggplot(data = data_df, aes(x = data_dim_1, y = data_dim_2))
    #}
    #if (show_tree) {
    #    g <- g + geom_segment(aes_string(x = "source_prin_graph_dim_1", 
    #        y = "source_prin_graph_dim_2", xend = "target_prin_graph_dim_1", 
    #        yend = "target_prin_graph_dim_2"), size = cell_link_size, 
    #        linetype = "solid", na.rm = TRUE, data = edge_df)
    #}
    #if (is.null(markers_exprs) == FALSE && nrow(markers_exprs) > 
    #    0) {
    #    if (use_color_gradient) {
    #    }
    #    else {
    #        g <- g + geom_point(aes_string(color = color_by), 
    #            na.rm = TRUE)
    #    }
    #}
    #else {
    #    if (use_color_gradient) {
    #    }
    #    else {
    #        g <- g + geom_point(aes_string(color = color_by), 
    #            size = I(cell_size), na.rm = TRUE)
    #    }
    #}
    #if (show_branch_points && cds@dim_reduce_type == "DDRTree") {
    #    mst_branch_nodes <- cds@auxOrderingData[[cds@dim_reduce_type]]$branch_points
    #    branch_point_df <- ica_space_df %>% slice(match(mst_branch_nodes, 
    #        sample_name)) %>% mutate(branch_point_idx = seq_len(n()))
    #    g <- g + geom_point(aes_string(x = "prin_graph_dim_1", 
    #        y = "prin_graph_dim_2"), size = 5, na.rm = TRUE, 
    #        branch_point_df) + geom_text(aes_string(x = "prin_graph_dim_1", 
    #        y = "prin_graph_dim_2", label = "branch_point_idx"), 
    #        size = 4, color = "white", na.rm = TRUE, branch_point_df)
    #}
    #if (show_cell_names) {
    #    g <- g + geom_text(aes(label = sample_name), size = cell_name_size)
    #}
    #if (show_state_number) {
    #    g <- g + geom_text(aes(label = sample_state), size = state_number_size)
    #}
    #g <- g + monocle_theme_opts() + xlab(paste("Component", x)) + 
    #    ylab(paste("Component", y)) + theme(legend.position = "top", 
    #    legend.key.height = grid::unit(0.35, "in")) + theme(legend.key = element_blank()) + 
    #    theme(panel.background = element_rect(fill = "white"))
    #g
    return(data_df)
}
#monocle3
plot_monocle3_pseudotime_heatmap<-function (cds_subset, seu_subset, sig_diff_gene, cluster_rows = TRUE, hclust_method = "ward.D2",
                                       num_clusters = 6, hmcols = NULL, add_annotation_row = NULL,
                                       add_annotation_col = NULL, show_rownames = FALSE, use_gene_short_name = TRUE,
                                       scale_max = 3, scale_min = -3,
                                       return_heatmap = FALSE,
                                       cores = 1){
  heatmap_matrix <- GetAssayData(seu_subset,slot='data', assay='RNA')[match(sig_diff_gene,rownames(seu_subset)),order(pseudotime(cds_subset))]
  heatmap_matrix <- t(apply(heatmap_matrix,1,function(x){smooth.spline(x,df=3)$y}))
  heatmap_matrix = heatmap_matrix[!apply(heatmap_matrix, 1, sd) == 0, ]
  heatmap_matrix = Matrix::t(scale(Matrix::t(heatmap_matrix), center = TRUE))
  heatmap_matrix = heatmap_matrix[is.na(row.names(heatmap_matrix)) == FALSE, ]
  heatmap_matrix[is.nan(heatmap_matrix)] = 0
  heatmap_matrix[heatmap_matrix > scale_max] = scale_max
  heatmap_matrix[heatmap_matrix < scale_min] = scale_min
  row_dist <- as.dist((1 - cor(Matrix::t(heatmap_matrix)))/2)
  row_dist[is.na(row_dist)] <- 1
  if (is.null(hmcols)) {
    bks <- seq(-3.1, 3.1, by = 0.1)
    hmcols <- blue2green2red(length(bks) - 1)
  }else {
    bks <- seq(-3.1, 3.1, length.out = length(hmcols))
  }
  ph <- pheatmap::pheatmap(heatmap_matrix, useRaster = T, cluster_cols = FALSE,
                 cluster_rows = cluster_rows, show_rownames = F, show_colnames = F,
                 clustering_distance_rows = row_dist, clustering_method = hclust_method,
                 cutree_rows = num_clusters, silent = TRUE, filename = NA,
                 breaks = bks, border_color = NA, color = hmcols)
  if (cluster_rows) {
    annotation_row <- data.frame(Cluster = factor(cutree(ph$tree_row,
                                                         num_clusters)))
  }
  else {
    annotation_row <- NULL
  }
  if (!is.null(add_annotation_row)) {
    old_colnames_length <- ncol(annotation_row)
    annotation_row <- cbind(annotation_row, add_annotation_row[row.names(annotation_row),
    ])
    colnames(annotation_row)[(old_colnames_length + 1):ncol(annotation_row)] <- colnames(add_annotation_row)
  }
  if (!is.null(add_annotation_col)) {
    if (nrow(add_annotation_col) != 100) {
      stop("add_annotation_col should have only 100 rows (check genSmoothCurves before you supply the annotation data)!")
    }
    annotation_col <- add_annotation_col
  }
  else {
    annotation_col <- NA
  }
  if (use_gene_short_name == TRUE) {
    if (is.null(fData(cds_subset)$gene_short_name) == FALSE) {
      feature_label <- as.character(fData(cds_subset)[row.names(heatmap_matrix),
                                                      "gene_short_name"])
      feature_label[is.na(feature_label)] <- row.names(heatmap_matrix)
      row_ann_labels <- as.character(fData(cds_subset)[row.names(annotation_row),
                                                       "gene_short_name"])
      row_ann_labels[is.na(row_ann_labels)] <- row.names(annotation_row)
    }
    else {
      feature_label <- row.names(heatmap_matrix)
      row_ann_labels <- row.names(annotation_row)
    }
  }
  else {
    feature_label <- row.names(heatmap_matrix)
    if (!is.null(annotation_row)){
      row_ann_labels <- row.names(annotation_row)}
  }
  row.names(heatmap_matrix) <- feature_label
  if (!is.null(annotation_row)){
    row.names(annotation_row) <- row_ann_labels}
  colnames(heatmap_matrix) <- c(1:ncol(heatmap_matrix))
  ph_res <- pheatmap::pheatmap(heatmap_matrix[, ], useRaster = T, cluster_cols = FALSE,
                     cluster_rows = cluster_rows, show_rownames = show_rownames,
                     show_colnames = F, clustering_distance_rows = row_dist,
                     clustering_method = hclust_method, cutree_rows = num_clusters,
                     annotation_row = annotation_row, annotation_col = annotation_col,
                     treeheight_row = 20, breaks = bks, fontsize = 6, color = hmcols,
                     border_color = NA, silent = TRUE, filename = NA)
  grid::grid.rect(gp = grid::gpar("fill", col = NA))
  grid::grid.draw(ph_res$gtable)
  if (return_heatmap) {
    return(ph_res)
  }
}
#Spatial plot
SpatialFeaturePlot_custom <- function(seurat_obj, 
                                      feature = NULL,
                                      group.by = NULL,
                                      pt.size = 1.5,
                                      pt.alpha = 0.8,
                                      cols = NULL,
                                      theme = "classic",
                                      label = FALSE,
                                      label.size = 3.5,
                                      label.color = "black",
                                      label.repel = TRUE,
                                      label.alpha = 0.8,
                                      crop = FALSE,
                                      axes = FALSE,
                                      border = TRUE,
                                      border.size = 0.5,
                                      border.color = "gray30",
                                      ncol = NULL,
                                      combine = TRUE,
                                      plot.title.size = 14,
                                      plot.title.face = "bold",
                                      legend.position = "right") {
  
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(ggrepel)
  
  # 1. 数据准备函数
  prepare_plot_data <- function(obj, feat = NULL, grp = NULL) {
    plot_data <- data.frame(
      x = obj@meta.data$x,
      y = obj@meta.data$y,
      cell_id = colnames(obj)
    )
    
    if (!is.null(feat)) {
      if (feat %in% rownames(obj)) {
        plot_data$value <- GetAssayData(obj, slot = "data")[feat, ]
      } else if (feat %in% colnames(obj@meta.data)) {
        plot_data$value <- obj@meta.data[[feat]]
      } else {
        stop(paste("Feature", feat, "not found in expression matrix or metadata"))
      }
    } else if (!is.null(grp)) {
      plot_data$group <- obj@meta.data[[grp]]
    } else {
      plot_data$group <- Idents(obj)
    }
    
    return(plot_data)
  }
  
  # 2. 主题选择函数
  get_theme <- function(theme_name, border = TRUE, border.color = "gray30", border.size = 0.5) {
    base_theme <- switch(theme_name,
                         "classic" = theme_classic(),
                         "bw" = theme_bw(),
                         "minimal" = theme_minimal(),
                         "light" = theme_light(),
                         "void" = theme_void(),
                         theme_classic()  # 默认
    )
    
    # 添加边框
    border_element <- if (border) {
      element_rect(color = border.color, fill = NA, size = border.size)
    } else {
      element_blank()
    }
    
    # 完整主题
    full_theme <- base_theme +
      theme(
        panel.border = border_element,
        plot.title = element_text(size = plot.title.size, 
                                  face = plot.title.face, 
                                  hjust = 0.5,
                                  margin = margin(b = 10)),
        legend.position = legend.position,
        legend.background = element_rect(fill = "white", color = "gray80"),
        legend.key = element_rect(fill = "white"),
        axis.text = if (axes) element_text() else element_blank(),
        axis.ticks = if (axes) element_line() else element_blank(),
        axis.title = element_blank(),
        axis.line = if (!axes) element_blank() else element_line()
      )
    
    return(full_theme)
  }
  
  # 3. 创建单个图的函数
  create_single_plot <- function(data, title, is_discrete = TRUE) {
    
    # 基础绘图
    if (is_discrete) {
      p <- ggplot(data, aes(x = x, y = y, color = group))
    } else {
      p <- ggplot(data, aes(x = x, y = y, color = value))
    }
    
    # 添加点
    p <- p + 
      geom_point(size = pt.size, alpha = pt.alpha, stroke = 0)
    
    # 应用主题
    p <- p + get_theme(theme, border, border.color, border.size)
    
    # 颜色标度
    if (is_discrete) {
      if (is.null(cols)) {
        n_groups <- length(unique(data$group))
        
        # 优化调色板 - 更美观的颜色组合
        if (n_groups <= 8) {
          # Set2调色板 - 美观的离散颜色
          set2_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", 
                           "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
          cols <- set2_colors[1:n_groups]
        } else if (n_groups <= 12) {
          # Paired调色板
          paired_colors <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
                             "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
                             "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
          cols <- paired_colors[1:n_groups]
        } else {
          # Viridis调色板用于大量分组
          cols <- viridis::viridis(n_groups)
        }
      }
      
      p <- p + 
        scale_color_manual(
          values = cols, 
          name = title,
          guide = guide_legend(
            override.aes = list(size = 4, alpha = 1),
            #  ncol = ifelse(n_groups > 10, 2, 1)
          )
        )
    } else {
      # 连续变量使用viridis调色板
      p <- p + 
        scale_color_viridis_c(
          option = "turbo",#"C"  # plasma调色板，对比度好
          name = title,
          guide = guide_colorbar(
            barwidth = 1,
            barheight = 15,
            frame.colour = "gray30",
            ticks.colour = "gray30",
            title.position = "top",
            title.hjust = 0.5
          )
        )
    }
    
    # 添加标签（仅离散变量）
    if (label && is_discrete) {
      label_data <- data %>%
        group_by(group) %>%
        summarize(
          center_x = median(x, na.rm = TRUE),
          center_y = median(y, na.rm = TRUE),
          .groups = "drop"
        )
      
      # 标签背景颜色
      label_bg <- ifelse(label.alpha > 0, 
                         alpha("white", label.alpha), 
                         NA)
      
      if (label.repel) {
        p <- p + 
          geom_label_repel(
            data = label_data,
            aes(x = center_x, y = center_y, label = group),
            size = label.size,
            color = label.color,
            fill = label_bg,
            fontface = "bold",
            box.padding = 0.5,
            segment.size = 0.3,
            segment.color = "gray50",
            show.legend = FALSE,
            max.overlaps = Inf,
            min.segment.length = 0.2
          )
      } else {
        p <- p + 
          geom_label(
            data = label_data,
            aes(x = center_x, y = center_y, label = group),
            size = label.size,
            color = label.color,
            fill = label_bg,
            fontface = "bold",
            show.legend = FALSE
          )
      }
    }
    
    # 标题和坐标
    p <- p + 
      ggtitle(title) #+
      #coord_fixed(ratio = 1)
    
    return(p)
  }
  
  # 4. 主逻辑：处理多个特征或多个分组
  if (!is.null(feature) && length(feature) > 1) {
    plots <- list()
    for (i in seq_along(feature)) {
      feat <- feature[i]
      plot_data <- prepare_plot_data(seurat_obj, feat = feat)
      
      if (feat %in% rownames(seurat_obj)) {
        # 基因表达 - 连续
        plots[[i]] <- create_single_plot(plot_data, feat, is_discrete = FALSE)
      } else if (feat %in% colnames(seurat_obj@meta.data)) {
        # 检查元数据列是否是因子/字符（离散）还是数值（连续）
        if (is.factor(seurat_obj@meta.data[[feat]]) || 
            is.character(seurat_obj@meta.data[[feat]])) {
          plot_data$group <- plot_data$value
          plot_data$value <- NULL
          plots[[i]] <- create_single_plot(plot_data, feat, is_discrete = TRUE)
        } else {
          plots[[i]] <- create_single_plot(plot_data, feat, is_discrete = FALSE)
        }
      }
    }
    
    if (combine) {
      if (is.null(ncol)) {
        ncol <- ceiling(sqrt(length(plots)))
      }
      return(wrap_plots(plots, ncol = ncol))
    } else {
      return(plots)
    }
    
  } else if (!is.null(group.by) && length(group.by) > 1) {
    plots <- list()
    for (i in seq_along(group.by)) {
      grp <- group.by[i]
      plot_data <- prepare_plot_data(seurat_obj, grp = grp)
      plots[[i]] <- create_single_plot(plot_data, grp, is_discrete = TRUE)
    }
    
    if (combine) {
      if (is.null(ncol)) {
        ncol <- ceiling(sqrt(length(plots)))
      }
      return(wrap_plots(plots, ncol = ncol))
    } else {
      return(plots)
    }
    
  } else {
    # 单个特征或分组
    if (!is.null(feature)) {
      feat <- feature[1]
      plot_data <- prepare_plot_data(seurat_obj, feat = feat)
      
      if (feat %in% rownames(seurat_obj)) {
        # 基因表达 - 连续
        return(create_single_plot(plot_data, feat, is_discrete = FALSE))
      } else if (feat %in% colnames(seurat_obj@meta.data)) {
        if (is.factor(seurat_obj@meta.data[[feat]]) || 
            is.character(seurat_obj@meta.data[[feat]])) {
          plot_data$group <- plot_data$value
          plot_data$value <- NULL
          return(create_single_plot(plot_data, feat, is_discrete = TRUE))
        } else {
          return(create_single_plot(plot_data, feat, is_discrete = FALSE))
        }
      }
    } else if (!is.null(group.by)) {
      grp <- group.by[1]
      plot_data <- prepare_plot_data(seurat_obj, grp = grp)
      return(create_single_plot(plot_data, grp, is_discrete = TRUE))
    } else {
      # 默认使用聚类结果
      plot_data <- prepare_plot_data(seurat_obj)
      return(create_single_plot(plot_data, "Identity", is_discrete = TRUE))
    }
  }
}
#METABOLISM
DotPlot.metabolism2 <- function(obj, pathway, phenotype, annot_factor,norm = "y"){
  input.norm = norm
  input.pathway <- pathway
  input.parameter<-phenotype
  
  metadata<-obj@meta.data
  metabolism.matrix <- obj@assays$METABOLISM$score
  metadata[,input.parameter]<-as.character(metadata[,input.parameter])
  metabolism.matrix_sub<-t(metabolism.matrix[input.pathway,])
  
  #arrange large table
  gg_table<-c()
  for (i in 1:length(input.pathway)){
    gg_table<-rbind(gg_table, cbind(metadata[,input.parameter], input.pathway[i], metabolism.matrix_sub[,i]))
  }
  gg_table<-data.frame(gg_table)

  #get median value
  gg_table_median<-c()
  input.group.x<-unique(as.character(gg_table[,1]))
  input.group.y<-unique(as.character(gg_table[,2]))
  
  for (x in 1:length(input.group.x)){
    for (y in 1:length(input.group.y)){
      gg_table_sub<-subset(gg_table, gg_table[,1] == input.group.x[x] & gg_table[,2] == input.group.y[y])
      gg_table_median<-rbind(gg_table_median, cbind(input.group.x[x], input.group.y[y], median(as.numeric(as.character(gg_table_sub[,3])))))
      
    }
  }
  gg_table_median<-data.frame(gg_table_median)
  if(!is.null(annot_factor)){
    gg_table_median$X1<-factor(gg_table_median$X1,levels = annot_factor)
  }
  gg_table_median[,3]<-as.numeric(as.character(gg_table_median[,3]))
  gg_table_median$X2<-factor(gg_table_median$X2,levels = input.pathway)
  #normalize
  gg_table_median_norm<-c()
  input.group.x<-unique(as.character(gg_table[,1]))
  input.group.y<-unique(as.character(gg_table[,2]))
  
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  
  if (input.norm == "y")
    for (y in 1:length(input.group.y)){
      gg_table_median_sub<-subset(gg_table_median, gg_table_median[,2] == input.group.y[y])
      norm_value<- range01(as.numeric(as.character(gg_table_median_sub[,3])))
      gg_table_median_sub[,3]<-norm_value
      gg_table_median_norm<-rbind(gg_table_median_norm, gg_table_median_sub)
    }
  
  if (input.norm == "x")
    for (x in 1:length(input.group.x)){
      gg_table_median_sub<-subset(gg_table_median, gg_table_median[,1] == input.group.x[x])
      norm_value<- range01(as.numeric(as.character(gg_table_median_sub[,3])))
      gg_table_median_sub[,3]<-norm_value
      gg_table_median_norm<-rbind(gg_table_median_norm, gg_table_median_sub)
    }
  
  if (input.norm == "na") gg_table_median_norm<-gg_table_median
  
  gg_table_median_norm<-data.frame(gg_table_median_norm)
  gg_table_median_norm[,3]<-as.numeric(as.character(gg_table_median_norm[,3]))
  
  library(wesanderson)
  pal <- wes_palette("Zissou1", 100, type = "continuous")
  
  ggplot(data=gg_table_median_norm, aes(x=gg_table_median_norm[,1], y=gg_table_median_norm[,2], color = gg_table_median_norm[,3])) +
    geom_point(data=gg_table_median_norm, aes(size = gg_table_median_norm[,3])) + #geom_line() +
    #theme_bw()+theme(aspect.ratio=0.5, axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab("Metabolic Pathway")+ xlab('')+
    theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1), #aspect.ratio=1,
                     panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
    scale_color_gradientn(colours = pal) +
    labs(color = "Value", size = "Value") +
    #facet_wrap(~tissueunique, ncol = 1) +
    #theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    NULL
}
###go kegg
go_enrich_analysis2<-function(genes,species,outdir,prefix,type,number){
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
    if(type=='bar'){
      bp_plot<-barplot(GainBPGo, showCategory=as.numeric(number),label_format=100)}else{
        bp_plot<-dotplot(GainBPGo, showCategory=as.numeric(number),label_format=100)
      }
    ggsave(bp_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentBP.",type,".png"),dpi=400, width = 20, height = 10)
    ggsave(bp_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentBP.",type,".pdf"),width = 20, height = 10)
  }
  #MF
  GainMFGo<-enrichGO(genes, db, keyType ='SYMBOL', ont="MF", pvalueCutoff=1,qvalueCutoff = 1)
  if(class(GainMFGo)!="NULL"){
    GOenrich <- as.data.frame(GainMFGo)
    GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
    write.table(GOenrich, file=paste0(outdir,"/",prefix,"_GO.MF.txt"), sep="\t", quote=F, row.names=F)
    if(type=='bar'){
      mf_plot<-barplot(GainMFGo, showCategory=as.numeric(number),label_format=100)}else{
        mf_plot<-dotplot(GainMFGo, showCategory=as.numeric(number),label_format=100)
      }
    ggsave(mf_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentMF.",type,".png"),dpi=400, width = 20, height = 10)
    ggsave(mf_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentMF.",type,".pdf"),width = 20, height = 10)
  }
  #CC
  GainCCGo<-enrichGO(genes, db, keyType ='SYMBOL', ont="CC", pvalueCutoff=1,qvalueCutoff = 1)
  if(class(GainCCGo)!="NULL"){
    GOenrich <- as.data.frame(GainCCGo)
    GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
    write.table(GOenrich, file=paste0(outdir,"/",prefix,"_GO.CC.txt"), sep="\t", quote=F, row.names=F)
    if(type=='bar'){
      cc_plot<-barplot(GainCCGo, showCategory=as.numeric(number),label_format=100)}else{
        cc_plot<-dotplot(GainCCGo, showCategory=as.numeric(number),label_format=100)
      }
    ggsave(cc_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentCC.",type,".png"),dpi=400, width = 20, height = 10)
    ggsave(cc_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentCC.",type,".pdf"),width = 20, height = 10)
  }
}
go_enrich_analysis3<-function(genes,species,outdir,prefix,type,number){
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
    GOenrich<-GOenrich[GOenrich$p.adjust<0.05,]
    if(nrow(GOenrich)>0){
    if(nrow(GOenrich)>number){
    GOenrich<-GOenrich[1:number,]}
    GOenrich$Description <- factor(GOenrich$Description, levels = rev(GOenrich$Description[order(GOenrich$p.adjust)]))
    if(type=='bar'){
      bp_plot<-ggplot(GOenrich, aes(x = Count, y = Description, fill = p.adjust)) +
      geom_bar(stat = "identity") +
      scale_fill_gradientn(
      colours = rev(c("#e50000", "#ff0066", "#9900ff", "#0000ff")),trans = "reverse") +
      theme_bw() +
      theme(
      axis.text.y = element_text(size = 12,color = "black"),
      axis.text.x = element_text(size = 12,color = "black"),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
      ) +
      labs(x = "Count",y = "",fill = "p.adjust")
    ggsave(bp_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentBP.",type,".png"),dpi=400, width = 20, height = 10)
    ggsave(bp_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentBP.",type,".pdf"),width = 20, height = 10)}
    }}
  #MF
  GainMFGo<-enrichGO(genes, db, keyType ='SYMBOL', ont="MF", pvalueCutoff=1,qvalueCutoff = 1)
  if(class(GainMFGo)!="NULL"){
    GOenrich <- as.data.frame(GainMFGo)
    GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
    write.table(GOenrich, file=paste0(outdir,"/",prefix,"_GO.MF.txt"), sep="\t", quote=F, row.names=F)
    GOenrich<-GOenrich[GOenrich$p.adjust<0.05,]
    if(nrow(GOenrich)>0){
    if(nrow(GOenrich)>number){
    GOenrich<-GOenrich[1:number,]}
    GOenrich$Description <- factor(GOenrich$Description, levels = rev(GOenrich$Description[order(GOenrich$p.adjust)]))
    if(type=='bar'){
      mf_plot<-ggplot(GOenrich, aes(x = Count, y = Description, fill = p.adjust)) +
      geom_bar(stat = "identity") +
      scale_fill_gradientn(
      colours = rev(c("#e50000", "#ff0066", "#9900ff", "#0000ff")),trans = "reverse") +
      theme_bw() +
      theme(
      axis.text.y = element_text(size = 12,color = "black"),
      axis.text.x = element_text(size = 12,color = "black"),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
      ) +
      labs(x = "Count",y = "",fill = "p.adjust")
    ggsave(mf_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentMF.",type,".png"),dpi=400, width = 20, height = 10)
    ggsave(mf_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentMF.",type,".pdf"),width = 20, height = 10)}
    }}
  #CC
  GainCCGo<-enrichGO(genes, db, keyType ='SYMBOL', ont="CC", pvalueCutoff=1,qvalueCutoff = 1)
  if(class(GainCCGo)!="NULL"){
    GOenrich <- as.data.frame(GainCCGo)
    GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
    write.table(GOenrich, file=paste0(outdir,"/",prefix,"_GO.CC.txt"), sep="\t", quote=F, row.names=F)
    GOenrich<-GOenrich[GOenrich$p.adjust<0.05,]
    if(nrow(GOenrich)>0){
    if(nrow(GOenrich)>number){
    GOenrich<-GOenrich[1:number,]}
    GOenrich$Description <- factor(GOenrich$Description, levels = rev(GOenrich$Description[order(GOenrich$p.adjust)]))
    if(type=='bar'){
      cc_plot<-ggplot(GOenrich, aes(x = Count, y = Description, fill = p.adjust)) +
      geom_bar(stat = "identity") +
      scale_fill_gradientn(
      colours = rev(c("#e50000", "#ff0066", "#9900ff", "#0000ff")),trans = "reverse") +
      theme_bw() +
      theme(
      axis.text.y = element_text(size = 12,color = "black"),
      axis.text.x = element_text(size = 12,color = "black"),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
      ) +
      labs(x = "Count",y = "",fill = "p.adjust")
    ggsave(cc_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentCC.",type,".png"),dpi=400, width = 20, height = 10)
    ggsave(cc_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentCC.",type,".pdf"),width = 20, height = 10)}
  }
}}
go_enrich_analysis3_pvalue<-function(genes,species,outdir,prefix,type,number){
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
    GOenrich<-GOenrich[GOenrich$pvalue<0.05,]
    if(nrow(GOenrich)>0){
      if(nrow(GOenrich)>number){
        GOenrich<-GOenrich[1:number,]}
      GOenrich$Description <- factor(GOenrich$Description, levels = rev(GOenrich$Description[order(GOenrich$pvalue)]))
      if(type=='bar'){
        bp_plot<-ggplot(GOenrich, aes(x = Count, y = Description, fill = pvalue)) +
          geom_bar(stat = "identity") +
          scale_fill_gradientn(
            colours = rev(c("#e50000", "#ff0066", "#9900ff", "#0000ff")),trans = "reverse") +
          theme_bw() +
          theme(
            axis.text.y = element_text(size = 12,color = "black"),
            axis.text.x = element_text(size = 12,color = "black"),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
          ) +
          labs(x = "Count",y = "",fill = "pvalue")
        ggsave(bp_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentBP.",type,".png"),dpi=400, width = 20, height = 10)
        ggsave(bp_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentBP.",type,".pdf"),width = 20, height = 10)}
    }}
  #MF
  GainMFGo<-enrichGO(genes, db, keyType ='SYMBOL', ont="MF", pvalueCutoff=1,qvalueCutoff = 1)
  if(class(GainMFGo)!="NULL"){
    GOenrich <- as.data.frame(GainMFGo)
    GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
    write.table(GOenrich, file=paste0(outdir,"/",prefix,"_GO.MF.txt"), sep="\t", quote=F, row.names=F)
    GOenrich<-GOenrich[GOenrich$pvalue<0.05,]
    if(nrow(GOenrich)>0){
      if(nrow(GOenrich)>number){
        GOenrich<-GOenrich[1:number,]}
      GOenrich$Description <- factor(GOenrich$Description, levels = rev(GOenrich$Description[order(GOenrich$pvalue)]))
      if(type=='bar'){
        mf_plot<-ggplot(GOenrich, aes(x = Count, y = Description, fill = pvalue)) +
          geom_bar(stat = "identity") +
          scale_fill_gradientn(
            colours = rev(c("#e50000", "#ff0066", "#9900ff", "#0000ff")),trans = "reverse") +
          theme_bw() +
          theme(
            axis.text.y = element_text(size = 12,color = "black"),
            axis.text.x = element_text(size = 12,color = "black"),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
          ) +
          labs(x = "Count",y = "",fill = "pvalue")
        ggsave(mf_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentMF.",type,".png"),dpi=400, width = 20, height = 10)
        ggsave(mf_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentMF.",type,".pdf"),width = 20, height = 10)}
    }}
  #CC
  GainCCGo<-enrichGO(genes, db, keyType ='SYMBOL', ont="CC", pvalueCutoff=1,qvalueCutoff = 1)
  if(class(GainCCGo)!="NULL"){
    GOenrich <- as.data.frame(GainCCGo)
    GOenrich <- data.frame(GOenrich[,1:6],GOenrich[8],GOenrich[9])
    write.table(GOenrich, file=paste0(outdir,"/",prefix,"_GO.CC.txt"), sep="\t", quote=F, row.names=F)
    GOenrich<-GOenrich[GOenrich$pvalue<0.05,]
    if(nrow(GOenrich)>0){
      if(nrow(GOenrich)>number){
        GOenrich<-GOenrich[1:number,]}
      GOenrich$Description <- factor(GOenrich$Description, levels = rev(GOenrich$Description[order(GOenrich$pvalue)]))
      if(type=='bar'){
        cc_plot<-ggplot(GOenrich, aes(x = Count, y = Description, fill = pvalue)) +
          geom_bar(stat = "identity") +
          scale_fill_gradientn(
            colours = rev(c("#e50000", "#ff0066", "#9900ff", "#0000ff")),trans = "reverse") +
          theme_bw() +
          theme(
            axis.text.y = element_text(size = 12,color = "black"),
            axis.text.x = element_text(size = 12,color = "black"),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
          ) +
          labs(x = "Count",y = "",fill = "pvalue")
        ggsave(cc_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentCC.",type,".png"),dpi=400, width = 20, height = 10)
        ggsave(cc_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentCC.",type,".pdf"),width = 20, height = 10)}
    }
  }}
kegg_enrich_analysis2<-function(genes,species,outdir,prefix,type,number){
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
    if(type=='bar'){
      kegg_plot<-barplot(enrich_kegg, showCategory=as.numeric(number),label_format=100)}else{
        kegg_plot<-dotplot(enrich_kegg, showCategory=as.numeric(number),label_format=100)
      }
    ggsave(kegg_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentKEGG.",type,".png"),dpi=400, width = 20, height = 10)
    ggsave(kegg_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentKEGG.",type,".pdf"),width = 20, height = 10)}
}

kegg_enrich_analysis3<-function(genes,species,outdir,prefix,type,number){
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
    KEGGenrich<-KEGGenrich[KEGGenrich$p.adjust<0.05,]
   if(nrow(KEGGenrich)>0){
    if(nrow(KEGGenrich)>number){
    KEGGenrich<-KEGGenrich[1:number,]}
    KEGGenrich$Description <- factor(KEGGenrich$Description, levels = rev(KEGGenrich$Description[order(KEGGenrich$p.adjust)]))
    if(type=='bar'){
      kegg_plot<-ggplot(KEGGenrich, aes(x = Count, y = Description, fill = p.adjust)) +
      geom_bar(stat = "identity") +
      scale_fill_gradientn(
      colours = rev(c("#e50000", "#ff0066", "#9900ff", "#0000ff")),trans = "reverse") +
      theme_bw() +
      theme(
      axis.text.y = element_text(size = 12,color = "black"),
      axis.text.x = element_text(size = 12,color = "black"),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
      ) +
      labs(x = "Count",y = "",fill = "p.adjust")
    ggsave(kegg_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentKEGG.",type,".png"),dpi=400, width = 20, height = 10)
    ggsave(kegg_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentKEGG.",type,".pdf"),width = 20, height = 10)}
  }
}}
kegg_enrich_analysis3_pvalue<-function(genes,species,outdir,prefix,type,number){
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
    KEGGenrich<-KEGGenrich[KEGGenrich$pvalue<0.05,]
   if(nrow(KEGGenrich)>0){
    if(nrow(KEGGenrich)>number){
    KEGGenrich<-KEGGenrich[1:number,]}
    KEGGenrich$Description <- factor(KEGGenrich$Description, levels = rev(KEGGenrich$Description[order(KEGGenrich$pvalue)]))
    if(type=='bar'){
      kegg_plot<-ggplot(KEGGenrich, aes(x = Count, y = Description, fill = pvalue)) +
      geom_bar(stat = "identity") +
      scale_fill_gradientn(
      colours = rev(c("#e50000", "#ff0066", "#9900ff", "#0000ff")),trans = "reverse") +
      theme_bw() +
      theme(
      axis.text.y = element_text(size = 12,color = "black"),
      axis.text.x = element_text(size = 12,color = "black"),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
      ) +
      labs(x = "Count",y = "",fill = "pvalue")
    ggsave(kegg_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentKEGG.",type,".png"),dpi=400, width = 20, height = 10)
    ggsave(kegg_plot, filename = paste0(outdir,"/",prefix,"_EnrichmentKEGG.",type,".pdf"),width = 20, height = 10)}
  }
}}

##
library(scRNAtoolVis)
jjVolcano_pvalue<-function (diffData = NULL, myMarkers = NULL, order.by = c("avg_log2FC"), 
          log2FC.cutoff = 0.25, pvalue.cutoff = 0.05, adjustP.cutoff = 0.01, 
          topGeneN = 5, col.type = "updown", back.col = "grey93", pSize = 0.75, 
          aesCol = c("#0099CC", "#CC3333"), legend.position = c(0.7, 0.9), base_size = 14, 
          tile.col = jjAnno::useMyCol("paired",n = 9),cluster.order = NULL,polar = FALSE,expand = c(-1, 1), flip = FALSE, celltypeSize = 3, ...) 
{
  if(col.type=='pvalue'){
    diff.marker <- diffData %>% dplyr::filter(abs(avg_log2FC) >=
                                              log2FC.cutoff & p_val < pvalue.cutoff)
    diff.marker <- diff.marker %>% dplyr::mutate(type = ifelse(avg_log2FC >= log2FC.cutoff, "sigUp", "sigDown")) %>% dplyr::mutate(type2 = ifelse(p_val < pvalue.cutoff, paste("Pvalue < ", pvalue.cutoff, sep = ""), paste("Pvalue >= ", pvalue.cutoff,sep = "")))
  }else{
    diff.marker <- diffData %>% dplyr::filter(abs(avg_log2FC) >=log2FC.cutoff & p_val_adj < pvalue.cutoff)
    diff.marker <- diff.marker %>% dplyr::mutate(type = ifelse(avg_log2FC >= log2FC.cutoff, "sigUp", "sigDown")) %>% dplyr::mutate(type2 = ifelse(p_val_adj < adjustP.cutoff, paste("adjust Pvalue < ", adjustP.cutoff, sep = ""), paste("adjust Pvalue >= ", adjustP.cutoff, sep = "")))                                                                                                 
  }
                                                                                                                                        
  if (!is.null(cluster.order)) {
    diff.marker$cluster <- factor(diff.marker$cluster, levels = cluster.order)
  }
  back.data <- purrr::map_df(unique(diff.marker$cluster), function(x) {
    tmp <- diff.marker %>% dplyr::filter(cluster == x)
    new.tmp <- data.frame(cluster = x, min = min(tmp$avg_log2FC) - 
                            0.2, max = max(tmp$avg_log2FC) + 0.2)
    return(new.tmp)
  })
  top.marker.tmp <- diff.marker %>% dplyr::group_by(cluster)
  top.marker.max <- top.marker.tmp %>% dplyr::slice_max(n = topGeneN, 
                                                        order_by = get(order.by))
  top.marker.min <- top.marker.tmp %>% dplyr::slice_min(n = topGeneN, 
                                                        order_by = get(order.by))
  top.marker <- rbind(top.marker.max, top.marker.min)
  if (!is.null(myMarkers)) {
    top.marker2 <- diff.marker %>% dplyr::filter(gene %in% 
                                                  myMarkers)
    top.marker<-unique(rbind(top.marker2,top.marker))
  }
  else {
    top.marker <- top.marker
  }
  p1 <- ggplot2::ggplot(diff.marker, ggplot2::aes(x = cluster, 
                                                  y = avg_log2FC)) + ggplot2::geom_col(data = back.data, 
                                                                                       ggplot2::aes(x = cluster, y = min), fill = back.col) + 
    ggplot2::geom_col(data = back.data, ggplot2::aes(x = cluster, 
                                                     y = max), fill = back.col)
  if (col.type == "updown") {
    p2 <- p1 + ggplot2::geom_jitter(ggplot2::aes(color = type), 
                                    size = pSize) + ggplot2::scale_color_manual(values = c(sigDown = aesCol[1], 
                                                                                           sigUp = aesCol[2]))
  }
  else if (col.type == "adjustP") {
    p2 <- p1 + ggplot2::geom_jitter(ggplot2::aes(color = type2), 
                                    size = pSize) + ggplot2::scale_color_manual(values = c(aesCol[2], 
                                                                                           aesCol[1]))
  }
  else if (col.type == "pvalue") {
    p2 <- p1 + ggplot2::geom_jitter(ggplot2::aes(color = type2), 
                                    size = pSize) + ggplot2::scale_color_manual(values = c(aesCol[2], 
                                                                                           aesCol[1]))
  }
  p3 <- p2 + ggplot2::scale_y_continuous(n.breaks = 6) + ggplot2::theme_classic(base_size = base_size) + 
    ggplot2::theme(panel.grid = ggplot2::element_blank(), 
                   legend.position = legend.position, legend.title = ggplot2::element_blank(), 
                   legend.background = ggplot2::element_blank()) + ggplot2::xlab("Clusters") + 
    ggplot2::ylab("Average log2FoldChange") + ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5)))
  p4 <- p3 + ggplot2::geom_tile(ggplot2::aes(x = cluster, y = 0, 
                                             fill = cluster), color = "black", height = log2FC.cutoff * 
                                  2, alpha = 0.3, show.legend = FALSE) + ggplot2::scale_fill_manual(values = tile.col) + 
    ggrepel::geom_text_repel(data = top.marker, ggplot2::aes(x = cluster, 
                                                             y = avg_log2FC, label = gene), max.overlaps = 50, 
                             ...)
  if (polar == TRUE) {
    p5 <- p4 + geomtextpath::geom_textpath(ggplot2::aes(x = cluster, y = 0, label = cluster)) + 
          ggplot2::scale_y_continuous(n.breaks = 6, expand = ggplot2::expansion(mult = expand)) + ggplot2::theme_void(base_size = base_size) + 
          ggplot2::theme(legend.position = legend.position, legend.title = ggplot2::element_blank()) + ggplot2::coord_polar(clip = "off", theta = "x")
  }
  else {
    if (flip == TRUE) {
      p5 <- p4 + ggplot2::scale_y_continuous(n.breaks = 6) + 
        ggplot2::geom_label(ggplot2::aes(x = cluster, 
                                         y = 0, label = cluster), size = celltypeSize) + 
        ggplot2::theme(axis.line.y = ggplot2::element_blank(), 
                       axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank()) + 
        ggplot2::coord_flip()
    }
    else {
      p5 <- p4 + ggplot2::scale_y_continuous(n.breaks = 6) + 
        ggplot2::geom_text(ggplot2::aes(x = cluster, 
                                        y = 0, label = cluster), size = celltypeSize) + 
        ggplot2::theme(axis.line.x = ggplot2::element_blank(), 
                       axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
    }
  }
  return(p5)
}
jjVolcano_pvalue <- function (diffData = NULL, myMarkers = NULL, order.by = c("avg_log2FC"),
          log2FC.cutoff = 0.25, pvalue.cutoff = 0.05, adjustP.cutoff = 0.01,
          topGeneN = 5, col.type = "updown", back.col = "grey93", pSize = 0.75,
          nonsig.col = "grey75",
          aesCol = c("#0099CC", "#CC3333"), legend.position = c(0.7, 0.9),
          base_size = 14, tile.col = jjAnno::useMyCol("paired", n = 9),
          cluster.order = NULL, polar = FALSE, expand = c(-1, 1),
          flip = FALSE, celltypeSize = 3, ...)
{
  # ★ 新增0：按 cluster+gene 去重，保留 p_val_adj 最小的一行
  diffData <- diffData %>%
    dplyr::group_by(cluster, gene) %>%
    dplyr::slice_min(n = 1, order_by = p_val_adj, with_ties = FALSE) %>%
    dplyr::ungroup()

  fc.marker <- diffData %>% dplyr::filter(abs(avg_log2FC) >= log2FC.cutoff)

  if (col.type == 'pvalue') {
    fc.marker <- fc.marker %>%
      dplyr::mutate(sig = p_val < pvalue.cutoff) %>%
      dplyr::mutate(type = dplyr::case_when(
        !sig ~ "nonsig",
        avg_log2FC >= log2FC.cutoff ~ "sigUp",
        TRUE ~ "sigDown"))
  } else {
    fc.marker <- fc.marker %>%
      dplyr::mutate(sig = p_val_adj < adjustP.cutoff) %>%
      dplyr::mutate(type = dplyr::case_when(
        !sig ~ "nonsig",
        avg_log2FC >= log2FC.cutoff ~ "sigUp",
        TRUE ~ "sigDown"))
  }

  diff.marker <- fc.marker %>% dplyr::filter(sig)

  if (!is.null(cluster.order)) {
    fc.marker$cluster   <- factor(fc.marker$cluster,   levels = cluster.order)
    diff.marker$cluster <- factor(diff.marker$cluster, levels = cluster.order)
  }

  back.data <- purrr::map_df(unique(fc.marker$cluster), function(x) {
    tmp <- fc.marker %>% dplyr::filter(cluster == x)
    new.tmp <- data.frame(cluster = x,
                          min = min(tmp$avg_log2FC) - 0.2,
                          max = max(tmp$avg_log2FC) + 0.2)
    return(new.tmp)
  })

  top.marker.tmp <- diff.marker %>% dplyr::group_by(cluster)
  # ★ 修改：with_ties = FALSE，避免并列值多取
  top.marker.max <- top.marker.tmp %>% dplyr::slice_max(n = topGeneN, order_by = get(order.by), with_ties = FALSE)
  top.marker.min <- top.marker.tmp %>% dplyr::slice_min(n = topGeneN, order_by = get(order.by), with_ties = FALSE)
  top.marker <- rbind(top.marker.max, top.marker.min)
  if (!is.null(myMarkers)) {
    top.marker2 <- diff.marker %>% dplyr::filter(gene %in% myMarkers)
    top.marker <- rbind(top.marker2, top.marker)
  }

  # ★ 新增：按 cluster+gene 去重 + 只保留显著的，杜绝重复标注
  top.marker <- top.marker %>%
    dplyr::filter(sig) %>%
    dplyr::distinct(cluster, gene, .keep_all = TRUE)

  p1 <- ggplot2::ggplot(fc.marker, ggplot2::aes(x = cluster, y = avg_log2FC)) +
    ggplot2::geom_col(data = back.data, ggplot2::aes(x = cluster, y = min), fill = back.col) +
    ggplot2::geom_col(data = back.data, ggplot2::aes(x = cluster, y = max), fill = back.col)

  p2 <- p1 + ggplot2::geom_jitter(ggplot2::aes(color = type), size = pSize) +
    ggplot2::scale_color_manual(values = c(sigDown = aesCol[1],
                                           sigUp   = aesCol[2],
                                           nonsig  = nonsig.col))

  p3 <- p2 + ggplot2::scale_y_continuous(n.breaks = 6) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   #legend.position = legend.position,
                   legend.title = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank()) +
    ggplot2::xlab("Clusters") + ggplot2::ylab("Average log2FoldChange") +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5)))

  p4 <- p3 + ggplot2::geom_tile(ggplot2::aes(x = cluster, y = 0, fill = cluster),
                                color = "black", height = log2FC.cutoff * 2,
                                alpha = 0.3, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = tile.col) +
    ggrepel::geom_text_repel(data = top.marker,
                             ggplot2::aes(x = cluster, y = avg_log2FC, label = gene),
                             max.overlaps = 50, ...)

  if (polar == TRUE) {
    p5 <- p4 + geomtextpath::geom_textpath(ggplot2::aes(x = cluster, y = 0, label = cluster)) +
      ggplot2::scale_y_continuous(n.breaks = 6, expand = ggplot2::expansion(mult = expand)) +
      ggplot2::theme_void(base_size = base_size) +
      ggplot2::theme(#legend.position = legend.position,
                     legend.title = ggplot2::element_blank()) +
      ggplot2::coord_polar(clip = "off", theta = "x")
  } else {
    if (flip == TRUE) {
      p5 <- p4 + ggplot2::scale_y_continuous(n.breaks = 6) +
        ggplot2::geom_label(ggplot2::aes(x = cluster, y = 0, label = cluster), size = celltypeSize) +
        ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank()) +
        ggplot2::coord_flip()
    } else {
      p5 <- p4 + ggplot2::scale_y_continuous(n.breaks = 6) +
        ggplot2::geom_text(ggplot2::aes(x = cluster, y = 0, label = cluster), size = celltypeSize) +
        ggplot2::theme(axis.line.x = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank())
    }
  }
  return(p5)
}

singlecell_gene_test <- function(SerautObj, 
                           genes.use, 
                           group.by=NULL, 
                           assay = "RNA", 
                           comp = NULL, 
                           alpha_start = .05, 
                           Bonferroni = T,
                           only_postive =F) {
  p_val.out <- c()
  stat.out <- c()
  condition.out <- c()
  gene.out <- c()
  if (only_postive == F){
    for (gene in genes.use){
      group1_cellname = rownames(SerautObj@meta.data[SerautObj@meta.data[[group.by]] == comp[1],])
      group1_exp = SerautObj@assays[[assay]]@data[gene, group1_cellname] 
      group2_cellname = rownames(SerautObj@meta.data[SerautObj@meta.data[[group.by]] == comp[2],])
      group2_exp = SerautObj@assays[[assay]]@data[gene, group2_cellname]
      t_out = wilcox.test(group1_exp, group2_exp)
      cond = paste(comp[1], comp[2], sep = "_")
      condition.out <- c(condition.out, cond)
      stat.out <- c(stat.out, t_out[["statistic"]])
      p_val.out <- c(p_val.out, t_out[["p.value"]])
      gene.out <- c(gene.out, gene)
    }
  }
    else{
      for (gene in genes.use){
        group1_cellname = rownames(SerautObj@meta.data[SerautObj@meta.data[[group.by]] == comp[1],])
        group1_exp = SerautObj@assays[[assay]]@data[gene, group1_cellname]
        group1_exp <- group1_exp[which(group1_exp>0)] 
        group2_cellname = rownames(SerautObj@meta.data[SerautObj@meta.data[[group.by]] == comp[2],])
        group2_exp = SerautObj@assays[[assay]]@data[gene, group2_cellname]
        group2_exp <- group2_exp[which(group2_exp>0)] 
        t_out = wilcox.test(group1_exp, group2_exp)
        cond = paste(comp[1], comp[2], sep = "_")
        condition.out <- c(condition.out, cond)
        stat.out <- c(stat.out, t_out[["statistic"]])
        p_val.out <- c(p_val.out, t_out[["p.value"]])
        gene.out <- c(gene.out, gene)
      }
  }
  if (Bonferroni == T){
    new_alpha = alpha_start/(2*length(genes.use))
    cat(paste("\n", "P-value for significance: p <", new_alpha, "\n"))
    sig_out = p_val.out < new_alpha
    dfOUT<- data.frame(gene=gene.out, condition = condition.out, p_val = p_val.out, statistic = stat.out, significant = sig_out)

    dfOUT$sig = ifelse(dfOUT$p_val > 0.05, "ns",
                       ifelse(dfOUT$p_val > 0.01, '*',
                              ifelse(dfOUT$p_val > 0.001, "**", "****")))

    }
  else{
    dfOUT<- data.frame(gene=gene.out, condition = condition.out, p_val = p_val.out, statistic = stat.out)
    dfOUT$sig = ifelse(dfOUT$p_val > 0.05, "ns",
                       ifelse(dfOUT$p_val > 0.01, '*',
                              ifelse(dfOUT$p_val > 0.001, "**", "****")))
  }
  return(dfOUT)
}

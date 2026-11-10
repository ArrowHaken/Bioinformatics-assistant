.libPaths(c('/share/apps/R/R-4.1.2-4/lib64/R/library/'))
suppressMessages({
  library(dplyr)
  library(cowplot)
  library(ggplot2)
  library(Seurat)
  library(argparser)
  library(stringr)})

argv <- arg_parser('')
argv <- add_argument(argv,"--datadir", help="the sample cellranger outs dir")
argv <- add_argument(argv,"--sample", help="the sample name")
argv <- add_argument(argv,"--outdir", help="the output dir")
argv <- add_argument(argv,"--soupx", help="whether run soupx step,such as Y or N")
argv <- add_argument(argv,"--doublefinder",  help="whether run doublefinder,such as Y or N")
argv <- add_argument(argv,"--species", help="the species of project,such as Human,Mouse,Rat")
argv <- parse_args(argv)

sample <- argv$sample
datadir <- argv$datadir
outdir <- argv$outdir
soupx <- argv$soupx
doublefinder <- argv$doublefinder
species <- argv$species

source('/share/home/liny/Project/personality/script/01_packages.R')

each_sample <- unlist(strsplit(sample,split = ','))
raw_number_data <- data.frame()
nFeature_combine<-list()
nCount_combine<-list()
mt_combine<-list()
#
i=1
for(each in each_sample){
  if(soupx=='Y'){
    print('111:soupx')
    sample_seu<-RunSoupX(path = paste0(datadir,'/',each),name = each,species = species)
  }else{
    print('000')
    sample.data <- Read10X(data.dir = paste0(datadir,'/',each,'/outs/filtered_feature_bc_matrix'))
    sample_seu <- CreateSeuratObject(counts = sample.data, project = each, min.cells = 3, min.features = 0)
    if(species == 'Human'){
      sample_seu[["percent.mt"]]<-PercentageFeatureSet(sample_seu, pattern="^MT-")
    }else if(species == 'Mouse'){
      sample_seu[["percent.mt"]]<-PercentageFeatureSet(sample_seu, pattern="^mt-")
    }else if(species == 'Rat'){
      sample_seu[["percent.mt"]]<-PercentageFeatureSet(sample_seu, pattern="^Mt-")
    }
  }
  each_sample_number_data <- data.frame(sample=each,raw_number=length(sample_seu@meta.data$orig.ident))
  raw_number_data <- rbind(raw_number_data,each_sample_number_data)
  if(doublefinder=='Y'){
    print('222:doublefinder')
    sample_df<-RunDF(before_rds=sample_seu,nfeatures=2000,dims=20,resolution=0.5,PCs=20,pN=0.25)
  }else{
    print('0000')
    sample_df<-sample_seu
  }
  Idents(sample_df)<-sample_df@meta.data$orig.ident
  p1<-VlnPlot(sample_df, features = c("nFeature_RNA"),pt.size = 0)
  p11<-VlnPlot(sample_df, features = c("nCount_RNA"),pt.size = 0)
  p111<-VlnPlot(sample_df, features = c("percent.mt"),pt.size = 0)
  nFeature_combine[[i]]<-p1
  nCount_combine[[i]]<-p11
  mt_combine[[i]]<-p111
  #
  sample_df<-subset(sample_df,Doublet=='Singlet')
  sample_df.data<-sample_df@assays$RNA@counts
  sample_df <- CreateSeuratObject(counts = sample_df.data, project = each, min.cells = 3, min.features = 200)

  if(species == 'Human'){
      sample_df[["percent.mt"]] <- PercentageFeatureSet(sample_df, pattern = "^MT-")
  }else if(species == 'Mouse'){
      sample_df[["percent.mt"]] <- PercentageFeatureSet(sample_df, pattern = "^mt-")
  }else if(species == 'Rat'){
      sample_df[["percent.mt"]] <- PercentageFeatureSet(sample_df, pattern = "^Mt-")
  }
  saveRDS(sample_df,file=paste0(outdir,'/',each,'.rds'))
  i=i+1
}
write.table(raw_number_data,file=paste0(outdir,'/sample_raw_cellnumber.xls'),quote = F,sep = '\t',row.names = F)
if(length(each_sample)<=4){
  nFeature_plot<-CombinePlots(plots = nFeature_combine,ncol=2)
  nCount_plot<-CombinePlots(plots = nCount_combine,ncol=2)
  mt_plot<-CombinePlots(plots = mt_combine,ncol=2)
  ggsave(nFeature_plot,file=paste0(outdir,'/nfeature_violin.png'),width = 8,height=6)
  ggsave(nFeature_plot,file=paste0(outdir,'/nfeature_violin.pdf'),width = 8,height=6)
  ggsave(nCount_plot,file=paste0(outdir,'/nCount_violin.png'),width = 8,height=6)
  ggsave(nCount_plot,file=paste0(outdir,'/nCount_violin.pdf'),width = 8,height=6)
  ggsave(mt_plot,file=paste0(outdir,'/mt_violin.png'),width = 8,height=6)
  ggsave(mt_plot,file=paste0(outdir,'/mt_violin.pdf'),width = 8,height=6)
}else if(length(each_sample)<=12){
  nFeature_plot<-CombinePlots(plots = nFeature_combine,ncol=3)
  nCount_plot<-CombinePlots(plots = nCount_combine,ncol=3)
  mt_plot<-CombinePlots(plots = mt_combine,ncol=3)
  ggsave(nFeature_plot,file=paste0(outdir,'/nfeature_violin.png'),width = 10,height=8)
  ggsave(nFeature_plot,file=paste0(outdir,'/nfeature_violin.pdf'),width = 10,height=8)
  ggsave(nCount_plot,file=paste0(outdir,'/nCount_violin.png'),width = 10,height=8)
  ggsave(nCount_plot,file=paste0(outdir,'/nCount_violin.pdf'),width = 10,height=8)
  ggsave(mt_plot,file=paste0(outdir,'/mt_violin.png'),width = 10,height=8)
  ggsave(mt_plot,file=paste0(outdir,'/mt_violin.pdf'),width = 10,height=8)
}else{
  nFeature_plot<-CombinePlots(plots = nFeature_combine,ncol=6)
  nCount_plot<-CombinePlots(plots = nCount_combine,ncol=6)
  mt_plot<-CombinePlots(plots = mt_combine,ncol=6)
  ggsave(nFeature_plot,file=paste0(outdir,'/nfeature_violin.png'),width = 30,height=25)
  ggsave(nFeature_plot,file=paste0(outdir,'/nfeature_violin.pdf'),width = 30,height=25)
  ggsave(nCount_plot,file=paste0(outdir,'/nCount_violin.png'),width = 30,height=25)
  ggsave(nCount_plot,file=paste0(outdir,'/nCount_violin.pdf'),width = 30,height=25)
  ggsave(mt_plot,file=paste0(outdir,'/mt_violin.png'),width = 30,height=25)
  ggsave(mt_plot,file=paste0(outdir,'/mt_violin.pdf'),width = 30,height=25)
}

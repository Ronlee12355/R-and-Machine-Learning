## the library is open ----
library(Seurat) ## your main single cell workflow toolkit
library(ggplot2) ## ubiquitous - better plotting functions than default, seurat plots are ggplot objects
library(tidyverse) ## ubiquitous - data manipulatation toolkit

library(reticulate) ## allows python functions to be called in R
library(Matrix) ## I need to read in a .mtx file

## read in dataset ----
# usually convienient function ## change the names to "barcodes,features,matrix" if required, as in this case
dat.sham<-Read10X(
  data.dir = "/path/to/your/dataset",gene.column = 1,
  unique.features = TRUE,strip.suffix = FALSE
) 

##  create seurat object ----
dat <- CreateSeuratObject(
  dat.sham,project = "exampleset",
  assay = "RNA",min.cells = 0,
  min.features = 0,names.field = 1,
  names.delim = "_",meta.data = NULL
)

## your best friend : Cheat sheet----
# https://satijalab.org/seurat/essential_commands.html

## QC

## now this is probably already done but nice to check anyway
## following the offical vignettes is the key from here
## https://satijalab.org/seurat/v3.2/pbmc3k_tutorial.html
dat[["percent.mt"]] <- PercentageFeatureSet(dat, pattern = "^Mt") ## if human, "MT"
range(dat[["percent.mt"]])## !
VlnPlot(dat, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)
dat <- subset(dat, subset = nFeature_RNA > 200 & nFeature_RNA < 2500 & percent.mt < 5)
VlnPlot(dat, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3) # if Mt genes were present

## normalise ----
dat <- NormalizeData(dat)
## Variable gene selection ----
dat <- FindVariableFeatures(dat, selection.method = "vst", nfeatures = 2000)
## what are our variable genes ?
top10 <- head(VariableFeatures(dat), 20)
# plot variable features with and without labels for the craic / sanity test
plot1 <- VariableFeaturePlot(dat)
plot1 <- LabelPoints(plot = plot1, points = top10, repel = T)
plot1

## scale----
dat <- ScaleData(dat, features = rownames(dat))
## dim reduction
dat <- RunPCA(dat, features = VariableFeatures(object = dat))
ElbowPlot(dat) # fastest, easiest - can dive in deeper with jackstraw, looking at the individiual loadings etc

## cluster ----
dat <- FindNeighbors(dat, dims = 1:20) ## iterativly decide the dims tbh
dat <- FindClusters(dat, resolution = 0.8) ## semi - supervise resolution, experiment! youll return through this cycle alot
dat <- RunUMAP(dat, dims = 1:20, n.neighbors = 20,min.dist = .01,spread = 6) ## twiddle the dials to get umaps of face validity
#dat <- RunUMAP(dat, dims = 1:15, n.neighbors = 4,min.dist = .001,spread = 5) ## twiddle the dials to get umaps of face validity

DimPlot(dat, reduction = "umap",pt.size = 2)


## marker genes ----
dat.markers <- FindAllMarkers(dat, only.pos = F, min.pct = 0.2, logfc.threshold = 0.2)
dat.markers.0v7 <- FindMarkers(dat, ident.1= "0", ident.2="7",only.pos = F, min.pct = 0.2, logfc.threshold = 0.2)
#?FindAllMarkers#tweak
top10.dat.markers <- dat.markers %>% group_by(cluster) %>% top_n(n = 10, wt = avg_logFC)

## rename ----
## This step requires biological background knowledge
dat <- RenameIdents(object = dat, `0` = "PT1",`1` = "PT2",`2` = "PT3",`3` = "Loh/DCT",`4` = "collecting duct",
                     `5` = "Endothelial",`6` = "Macrophage",`7` = "Mesenchymal")
























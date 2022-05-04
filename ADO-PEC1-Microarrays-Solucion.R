## ----class.source = 'fold-hide', setup, include=FALSE--------------------------
library(knitr)
library(rmdformats)

## Global options
options(max.print="75")
opts_chunk$set(echo=TRUE,
	             cache=FALSE,
               prompt=FALSE,
               tidy=TRUE,
               comment=NA,
               message=FALSE,
               warning=FALSE)
opts_knit$set(width=75)


## ---- fig.align='center'-------------------------------------------------------
knitr::include_graphics("images/analysisProcess.png")


## ---- librerias, echo=TRUE-----------------------------------------------------
installifnot <- function (pckgName){
if(!(require(pckgName, character.only=TRUE))){
  BiocManager::install(pckgName)
  }
}
installifnot("Biobase")
installifnot("oligo")
installifnot("pd.clariom.s.human")
installifnot("clariomshumantranscriptcluster.db")
installifnot("arrayQualityMetrics")
installifnot("genefilter")
installifnot("limma")
installifnot("annotate")
installifnot("hwriter")
installifnot("gplots")
installifnot("GOstats")

## ----class.source = 'fold-hide'------------------------------------------------
listaArchivos <- list.files("datos", pattern="*.CEL")
print(listaArchivos)
archivosCel <- listaArchivos[1:47]
cols <- c(rep("red", 11), rep("green", 18), rep("blue", 18))
grup <- c(rep("SINT", 11), rep("ASINT", 18), rep("SANO",18))
shortNames <-paste(c(substr(archivosCel[1:9],12,13), substr(archivosCel[10:47],12,14)), grup, sep=".")

targets <- data.frame(fileNames = archivosCel, 
                      shortName = shortNames, 
                      grupo = grup,
                      colores = cols, row.names=shortNames)
write.csv(targets, "targetsAll.csv")


## ----class.source = 'fold-hide', echo=TRUE-------------------------------------
selectSamples<- function (myID){
  set.seed(myID)
  selected <- c(sample(1:11, 5),
                sample(12:29, 5), 
                sample(30:47, 5))
  return(sort(selected))
}


## ----eval=FALSE----------------------------------------------------------------
## mySelected <- selectSamples(1234567)
## selectedTargets <- targets[mySelected,]
## rownames(selectedTargets) <- selectedTargets$shortName
## table(selectedTargets$grupo)


## ------------------------------------------------------------------------------
selectedTargets <- targets


## ---- phenoData1---------------------------------------------------------------
require(Biobase)
sampleInfo <-AnnotatedDataFrame(selectedTargets) 
show(pData(sampleInfo))


## ------------------------------------------------------------------------------
library(oligo)
CELfiles <- selectedTargets$fileNames
rawData <- read.celfiles(file.path("datos", CELfiles),
                         phenoData = sampleInfo)
colnames(exprs(rawData)) <- rownames(pData(rawData))
show(rawData)


## ---- etiquetas----------------------------------------------------------------
colores <- pData(rawData)$colores
grupos <- as.factor(pData(rawData)$grupo)
numSamples <- nrow(pData(rawData))
sampleLabels <-pData(rawData)$shortName


## ---- explora1-----------------------------------------------------------------
hist(rawData, main="Distribución de valores", col=colores, lty=1:numSamples)
legend (x="topright", legend=sampleLabels , col=colores, lty=1:numSamples, cex=0.5)

## ----boxplot
boxplot(rawData, cex.axis=0.6, col=colores, las=2, names=sampleLabels, main="Distribución de la señal")


## ---- plotPCA------------------------------------------------------------------
library(ggplot2)
library(ggrepel)
plotPCA3 <- function (datos, labels, factor, title, scale,colores, size = 1.5, glineas = 0.25) {
  data <- prcomp(t(datos),scale=scale)
  # plot adjustments
  dataDf <- data.frame(data$x)
  Group <- factor
  loads <- round(data$sdev^2/sum(data$sdev^2)*100,1)
  # main plot
  p1 <- ggplot(dataDf,aes(x=PC1, y=PC2)) +
    theme_classic() +
    geom_hline(yintercept = 0, color = "gray70") +
    geom_vline(xintercept = 0, color = "gray70") +
    geom_point(aes(color = Group), alpha = 0.55, size = 3) +
    coord_cartesian(xlim = c(min(data$x[,1])-5,max(data$x[,1])+5)) +
    scale_fill_discrete(name = "Group")
  # avoiding labels superposition
  p1 + geom_text_repel(aes(y = PC2 + 0.25, label = labels),segment.size = 0.25, size = size) + 
    labs(x = c(paste("PC1",loads[1],"%")),y=c(paste("PC2",loads[2],"%"))) +  
    ggtitle(title)+ 
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values=colores)
  }


## ---- explora2-----------------------------------------------------------------
plotPCA3(exprs(rawData), labels=sampleLabels, size=2, 
         factor =grupos, colores = unique(colores), title="PCA de las muestras", scale=TRUE)
# dev.off()


## ---- explora3-----------------------------------------------------------------
clust.euclid.average <- hclust(dist(t(exprs(rawData))),method="average")
plot(clust.euclid.average, labels=sampleLabels, main="Hierarchical clustering of samples",  hang=-1, cex=0.6)


## ---- arrayQM, cache=TRUE, eval=FALSE------------------------------------------
## library(arrayQualityMetrics)
## arrayQualityMetrics(rawData, outdir = "arrayQuality",intgroup= "grupo", force=TRUE)


## ---- eval=FALSE---------------------------------------------------------------
## library(GEOquery)
## GEOdata <- getGEO("GSE177477")
## library(Biobase)
## class(GEOdata)
## class(GEOdata[[1]])
## eset<- GEOdata[[1]]
## GEOtargets <- pData(eset)
## save(GEOtargets, file="GEOtargets.Rda")
## apply(GEOtargets, 2, function(s) length(unique(s)))
## table(GEOtargets$"symptons:ch1")
## table(GEOtargets$"disease state:ch1")


## ------------------------------------------------------------------------------
require(oligo)
dias <- substr(runDate(rawData), 1, 10)
table(dias)
table(dias, targets$grupo)


## ----class.source = 'fold-hide', normalizacion---------------------------------
eset_rma <- rma(rawData)    
eset_rma


## ---- exploraNorm--------------------------------------------------------------
plotPCA3(exprs(eset_rma), labels=sampleLabels, size=2, factor =grupos, colores = unique(colores), title="selected samples", scale=TRUE)


## ---- echo=TRUE----------------------------------------------------------------
show(annotation(eset_rma))


## ------------------------------------------------------------------------------
annotation(eset_rma) <-"clariomshumantranscriptcluster.db" 


## ----class.source = 'fold-hide', filtraje--------------------------------------
library(genefilter)
filtered <- nsFilter(eset_rma, require.entrez=TRUE,
         remove.dupEntrez=TRUE, var.func=IQR,
         var.cutoff=0.5, var.filter=TRUE,
         filterByQuantile=TRUE, feature.exclude="^AFFX")


## ---- echo=TRUE----------------------------------------------------------------
names(filtered)
class(filtered$eset)
print(filtered$filter.log)
eset_filtered <-filtered$eset
eset_filtered 


## ------------------------------------------------------------------------------
head(exprs(eset_filtered)[1:10, c(1,2,15,16,30,31)])


## ------------------------------------------------------------------------------
probeIds <- rownames(exprs(eset_filtered))
columns(clariomshumantranscriptcluster.db)
geneIds <- AnnotationDbi::select(clariomshumantranscriptcluster.db, 
                  keys=probeIds, 
                  columns = c("ENTREZID", "SYMBOL"))
head(geneIds)


## ----class.source = 'fold-hide', saveData--------------------------------------
save(eset_rma, eset_filtered, geneIds, file="datos.normalizados.Rda")


## ----class.source = 'fold-hide', matDesign1b-----------------------------------
fgrupos <- factor(grupos, levels=c("SINT", "ASINT", "SANO"))
design <- model.matrix(~ 0+fgrupos)
rownames(design) <-  sampleLabels 
colnames(design) <- c("SINT", "ASINT", "SANO")
print(design)


## ----class.source = 'fold-hide', contrastes, echo=TRUE-------------------------
library(limma)
cont.matrix <- makeContrasts (
      AsintVSSano = ASINT - SANO, 
      SintVSAsint = SINT - ASINT,
      SintVSSano = SINT - SANO,
      levels=design)
print(cont.matrix)


## ----class.source = 'fold-hide', linearmodelfit--------------------------------
library(limma)
fit<-lmFit(eset_filtered, design)
fit.main<-contrasts.fit(fit, cont.matrix)
fit.main<-eBayes(fit.main)


## ----class.source = 'fold-hide', topTabs1--------------------------------------
topTab_AsintVSSano <- topTable (fit.main, number=nrow(fit.main), coef="AsintVSSano", adjust="fdr"); head(topTab_AsintVSSano)
topTab_SintVSAsint <- topTable (fit.main, number=nrow(fit.main), coef="SintVSAsint", adjust="fdr"); head(topTab_SintVSAsint)
topTab_SintVSSano  <- topTable (fit.main, number=nrow(fit.main) , coef="SintVSSano", adjust="fdr"); head(topTab_SintVSSano)


## ------------------------------------------------------------------------------
library(magrittr)
anotaTopTab <- function(topTab){
  topTab_Anot  <- topTab %>% 
    dplyr::mutate (PROBEID=rownames(topTab)) %>%
    merge(geneIds, "PROBEID") %>% 
    dplyr::arrange(adj.P.Val)
  return(topTab_Anot)
}
topTab_AsintVSSano_Anot <- anotaTopTab (topTab_AsintVSSano)
topTab_SintVSAsint_Anot <- anotaTopTab (topTab_SintVSAsint)
topTab_SintVSSano_Anot <- anotaTopTab (topTab_SintVSSano)


## ----class.source = 'fold-hide', decideTests-----------------------------------
res<-decideTests(fit.main, method="separate", adjust.method="fdr", p.value=0.05, lfc=0)


## ----class.source = 'fold-hide', resumeDecideTests, eval=TRUE------------------
sum.res.rows<-apply(abs(res),1,sum)
res.selected<-res[sum.res.rows!=0,] 
print(summary(res))


## ----class.source = 'fold-hide',   fig.cap="Número de genes seleccionado en cada comparacion"----
vennDiagram (res.selected[,1:3], main="Genes in common #1", cex=0.9)


## ------------------------------------------------------------------------------
xlimits <- c(-3,+3)
# ylimits <- c(0,8)
genenames<- topTab_AsintVSSano_Anot$SYMBOL
coef <- 1
volcanoplot(fit.main, highlight=10,
            names=genenames, coef=coef, 
           main = paste("DEGS en comparación ",
                    colnames(cont.matrix)[coef],
                    sep="\n"),cex.main=0.8,
            xlim=xlimits, ylim=ylimits)
            
abline(v = c(-1, 1))

genenames<- topTab_SintVSAsint_Anot$SYMBOL
coef <- 2
volcanoplot(fit.main, highlight=10,
            names=genenames, coef=coef, 
            main = paste("DEGS en comparación ",
                    colnames(cont.matrix)[coef],
                    sep="\n"),cex.main=0.8,
            xlim=xlimits, ylim=ylimits)
abline(v = c(-1, 1))

genenames<- topTab_SintVSSano_Anot$SYMBOL
coef <- 3
volcanoplot(fit.main, highlight=10,
            names=genenames, coef=coef, 
            main = paste("DEGS en comparación ",
                    colnames(cont.matrix)[coef],
                         sep="\n"),cex.main=0.8,
            xlim=xlimits, ylim=ylimits)
abline(v = c(-1, 1))


## ----class.source = 'fold-hide', prepareData-----------------------------------
probeNames<-rownames(res)
probeNames.selected<-probeNames[sum.res.rows!=0]
exprs2cluster <-exprs(eset_filtered)[probeNames.selected,]
symbols<- AnnotationDbi::select(clariomshumantranscriptcluster.db, probeNames.selected, columns="SYMBOL")[,2]
colnames(exprs2cluster)<-sampleLabels
rownames(exprs2cluster)<- symbols
color.map <- function(grupo) { 
  if (grupo=="SINT"){
    c<- "red" 
  }else{ 
    if (grupo=="ASINT"){
      c<- "green"
    }else{
     if (grupo=="SANO"){
      c<- "blue"
     }else{
       c<- "green"
     }
    }
   }
return(c)
}


## ----class.source = 'fold-hide', plotHeatMap1, fig.cap="Mapa de colores basado en los genes seleccionados por estar diferencialmente expresados."----

grupColors <- unlist(lapply(pData(eset_filtered)$grupo, color.map))
heatmap(exprs2cluster, col=rainbow(100), ColSideColors=grupColors, cexCol=0.9)


## ----selGenes------------------------------------------------------------------
selGenes_AsintVSSano <-(topTab_AsintVSSano_Anot %>%
    subset(adj.P.Val < 0.05) %>%
    dplyr::select (ENTREZID) %>%
    unique)[,1]

selGenes_SintVSAsint <-(topTab_SintVSAsint_Anot %>%
    subset(adj.P.Val < 0.05) %>%
    dplyr::select (ENTREZID) %>%
    unique)[,1]

selGenes_SintVSSano <-(topTab_SintVSSano_Anot %>%
    subset(adj.P.Val < 0.05) %>%
    dplyr::select (ENTREZID) %>%
    unique)[,1]

geneUniverse <- (topTab_SintVSAsint_Anot %>%
    dplyr::select (ENTREZID) %>%
    unique)[,1]

# La notación anterior pretende ser más clara y acorde con las tendencias actuales en R
# EL mismo resultado se habría podido obtener con un enfoque más clásico aunque algo más críptico : 
# selGenes_AsintVSSano<-  unique(subset(topTab_AsintVSSano_Anot, adj.P.Val < 0.05)$ENTREZID)
# etc
# geneUniverse <- unique(topTab_SintVSAsint$ENTREZID)


## ----class.source = 'fold-hide', ORA, eval=TRUE--------------------------------
GOparamsAsintVSSano <- new("GOHyperGParams",
    geneIds=selGenes_AsintVSSano, 
    universeGeneIds=geneUniverse,
    annotation="org.Hs.eg.db", 
    ontology="BP",
    pvalueCutoff=0.001, 
    conditional=FALSE,
    testDirection="over")

GOparamsSintVSAsint = new("GOHyperGParams",
    geneIds=selGenes_SintVSAsint, 
    universeGeneIds=geneUniverse,
    annotation="org.Hs.eg.db", 
    ontology="BP",
    pvalueCutoff=0.001, 
    conditional=FALSE,
    testDirection="over")

GOparamsSintVSSano = new("GOHyperGParams",
    geneIds=selGenes_SintVSSano, 
    universeGeneIds=geneUniverse,
    annotation="org.Hs.eg.db", 
    ontology="BP",
    pvalueCutoff=0.001, 
    conditional=FALSE,
    testDirection="over")

KEGGparamsAsintVSSano <- new("KEGGHyperGParams",
    geneIds=selGenes_AsintVSSano, 
    universeGeneIds=geneUniverse,
    annotation="org.Hs.eg.db", 
    pvalueCutoff=0.05)

KEGGparamsSintVSAsint = new("KEGGHyperGParams",
    geneIds=selGenes_SintVSAsint, 
    universeGeneIds=geneUniverse,
    annotation="org.Hs.eg.db", 
    pvalueCutoff=0.05)

KEGGparamsSintVSSano = new("KEGGHyperGParams",
    geneIds=selGenes_SintVSSano, 
    universeGeneIds=geneUniverse,
    annotation="org.Hs.eg.db", 
    pvalueCutoff=0.05)



## ----class.source = 'fold-hide', ORAAnalysis, eval=TRUE------------------------
# Ejecutamos los análisis
GOHyperAsintVSSano <- hyperGTest(GOparamsAsintVSSano)
GOHyperSintVSAsint <- hyperGTest(GOparamsSintVSAsint)
GOHyperSintVSSano <- hyperGTest(GOparamsSintVSSano)
KEGGHyperAsintVSSano <- hyperGTest(KEGGparamsAsintVSSano)
KEGGHyperSintVSAsint <- hyperGTest(KEGGparamsSintVSAsint)
KEGGHyperSintVSSano <- hyperGTest(KEGGparamsSintVSSano)



## ------------------------------------------------------------------------------
summary(GOHyperAsintVSSano)
summary(KEGGHyperAsintVSSano)


## ----class.source = 'fold-hide', ORASaveResults, eval=TRUE---------------------
# Creamos un informe html con los resultados
comparison <-"AsintVSSano"
GOfilename <- paste0("GOResults.", comparison,".html")
htmlReport(GOHyperAsintVSSano, file = GOfilename, summary.args=list("htmlLinks"=TRUE))

KEGGfilename <- paste0("KEGGResults.", comparison,".html")
htmlReport(KEGGHyperSintVSAsint, file = KEGGfilename, summary.args=list("htmlLinks"=TRUE))


comparison <-"SintVSAsint"
GOfilename <- paste0("GOResults.", comparison,".html")
htmlReport(GOHyperSintVSAsint, file = GOfilename, summary.args=list("htmlLinks"=TRUE))

KEGGfilename <- paste0("KEGGResults.", comparison,".html")
htmlReport(KEGGHyperAsintVSSano, file = KEGGfilename, summary.args=list("htmlLinks"=TRUE))

comparison <-"SintVSSano"
GOfilename <- paste0("GOResults.", comparison,".html")
htmlReport(GOHyperSintVSAsint, file = GOfilename, summary.args=list("htmlLinks"=TRUE))

KEGGfilename <- paste0("KEGGResults.", comparison,".html")
htmlReport(KEGGHyperAsintVSSano, file = KEGGfilename, summary.args=list("htmlLinks"=TRUE))


## ----insertaCodigo, echo=TRUE, eval=FALSE, highlight=TRUE----------------------
## 
## 


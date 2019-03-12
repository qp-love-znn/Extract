
argv <- commandArgs(T)
library(limma)
library(locfit)
library(statmod,lib.loc="/home/huangfeifei/R/x86_64-unknown-linux-gnu-library/3.2")
library(edgeR)
mat<-read.table(argv[1], header=T, row.names=1,check.name=0)
x <- mat[, 1:ncol(mat)]
a<- round(x)
y<-DGEList(counts=a, group=colnames(a))
o <- order(rowSums(y$counts), decreasing=TRUE)
yy<-y[o,]
yy$samples$lib.size <- colSums(yy$counts)
plotMDS(yy)
#n<-length(uniq(clonames(mat))) / CK-vs-T
#batch <- factor(c(1,1,1,1,1,1,2,2,2,2,2,2))
batch <- factor(c(rep(1,args[3]),rep(2,args[4])))
design <- model.matrix(~batch)
rownames(design) <- colnames(yy)
yy <- estimateDisp(yy, design, robust=TRUE,trend.method="locfit")
fit <- glmFit(yy, design)
lrt <- glmLRT(fit)
filename <- paste0(argv[2],".","edgeR.DE_results")
write.table(topTags(lrt,n=NULL),file = filename, sep = "\t",quote=F, row.names=T)

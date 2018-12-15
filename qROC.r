#!/usr/bin/Rscript

args<-commandArgs(T)
if(length(args) != 3){
	cat("Rscript xxx.R <exp> <group><outfile>\n")
	q()
}

library(pROC,lib = 'R/x86_64-unknown-linux-gnu-library/3.2')
library(ROCR, lib = 'R/x86_64-unknown-linux-gnu-library/3.2')
mat <- read.table(args[1], header = T,sep="\t",check.name=0,row = 1,comment.char = "")
trdata<-t(mat[,1:13])
meta_names<-colnames(trdata)
group_data <- read.table(args[2], header = T,sep="\t",check.name=0,comment.char = "")
print(group_data$group)
g<-group_data[,2]
names(g)<-as.character(group_data$sample)
#trdata$sample<-as.character(rownames(trdata))
group<-g[rownames(trdata)]

auc_value = c()
for (i in 1:ncol(trdata)) {
	auc = roc(group, trdata[, i])
	auc_value <- c(auc_value, auc$auc[1])
}
#tmp<-cbind(auc_value,mat)
tmp<-cbind(mat,auc_value)
result<-cbind(meta_names,tmp)
write.table(result,file = paste(args[3], '.auc.xls', sep = ''),quote = FALSE, sep = "\t",row = F,)

##cor
library("corrplot")
mat<- read.table(args[1], header = T,sep="\t",check.name=0,comment.char = "")
aa<-as.matrix(mat[,2:ncol(mat)])
row.names(aa)<-mat[,1]
bb=colorRampPalette(c("blue", "white", "red"))
corrplot(aa,method = "ellipse",tl.cex = 0.4,col = bb(100),tl.col = "black")

##tree
library(ape)
g<-read.table(args[1], header = T,sep="\t",check.name=0,comment.char = "")
color<-hsv(runif(length(unique(g$V2)),0.65,0.95),1,1,0.7)
names(color)<-unique(g$V2)
g2<-g
g2$V2<-color[g$V2]
color_new<-g2$V2
names(color_new)<-g$V1
color_new2<-color_new[tree$tip.label]
tree <- read.tree(args[2])
plot(tree,type = "unrooted",cex=0.2,edge.lty=1)
plot(tree,type = "unrooted",tip.color = color_new2)
?plot.phylo

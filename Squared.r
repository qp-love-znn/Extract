args<-commandArgs(T)
infile = args[1]
outpfx = args[2]
mrna_cut = log2(as.numeric(args[3]))
pep_cut = log2(as.numeric(args[4]))
xlab = args[5]
ylab = args[6]
cexlab = as.numeric(args[7])
cexaxis = as.numeric(args[8])

dat<-read.table(infile,header=T,sep="\t",check.name=0,comment.char = "")
dat = dat[,c(1,3,4,2,5)]  ## reorder  id mrna pep part color

cor = cor.test(dat[,2],dat[,3])
pvalue = round(cor$p.value,4)
corr = round(as.numeric(cor$estimate),4)
title<-c(paste("pearson correlation: ",corr,sep=""),paste("p-value:",pvalue,sep=""))

#dat[dat[,2]>6,2] <- 6
#dat[dat[,3]>3,3] <- 3
#dat[dat[,2]< -6,2] <- -6
#dat[dat[,3]< -3,3] <- -3
dat[,5] = as.character(dat[,5])
head(dat)
width = 6
#par(mar=c(5,6,4,2)+0.1,cex.lab=3,cex.axis=1.5)
png(file=paste(outpfx,".png",sep=""),width=width,height=width,units = "in",res=300)
opar= par(no.readonly=TRUE)
par(mar=c(5,6,4,2)+0.1,cex.lab=cexlab,cex.axis=cexaxis)
plot(dat[,3],dat[,2],pch=16,col=dat[,5],xlab=xlab,ylab=ylab,main=title,xlim=c(-2,2),ylim=c(-9,9))
#    cex.axis=1.5,cex.lab=2)
abline(v=c(-pep_cut,pep_cut),lty=2,lwd=1.5)
abline(h=c(-mrna_cut,mrna_cut),lty=2,lwd=1.5)
dev.off()

pdf(file=paste(outpfx,".pdf",sep=""),width=width,height=width)
par(mar=c(5,6,4,2)+0.1,cex.lab=cexlab,cex.axis=cexaxis)
plot(dat[,3],dat[,2],pch=16,col=dat[,5],xlab=xlab,ylab=ylab,main=title,xlim=c(-2,2),ylim=c(-9,9))
abline(v=c(-pep_cut,pep_cut),lty=2,lwd=1.5)
abline(h=c(-mrna_cut,mrna_cut),lty=2,lwd=1.5)
dev.off()


运行：Rscript Squared_Up.R test.xls test 2 1.2 "log2(ratio of protein)" "log2(ratio of transcript)" 1.5 1.5
##test.xls##
GeneID  Quadrant        mrna_log2fc     pep_log2fc      color
ENST00000440909 8       -1.50014725574423       0.0806219068124998      #00ff00
ENST00000396859 6       0.945370910534403       0.505247706451519       #0000ff
ENST00000268182 5       -0.0103888587701686     0.117523441736923       #000000
ENST00000319562 6       -0.702657543390914      0.881002565309756       #0000ff

#!/usr/bin/Rscript
args <- commandArgs(T)

##function read matrix
if(length(args) != 2) {
	cat("Rscript get_filer_allZero.r <depthcovarage><outprefile>\n")
	q()
}


filer_allZero <- function(x) {
	dat <- read.table(x, sep = "\t", header = T,check.name=0,quote = '')
	new_dat = dat[1:(nrow(dat) - 1),2:(ncol(dat)-4)]
	colsum_vector = colSums(new_dat)
	number = seq(1,100)
	result_dat = data.frame(colsum_vector,number)
	return(result_dat)
}

aa<-filer_allZero(args[1])
png(paste(args[2], '.png', sep = ''))
plot(aa$number, aa$colsum_vector, type="s", xlab="percentile of gene body(5'->3')", ylab="reads number", ylim=c(0, max(aa$colsum_vector)))
dev.off()


dat<-read.table(args[1],header=T,sep="\t",check.name=0)
name1<-"A_FPKM"
name2<-"B_FPKM"
fc<-4
pq<-5
fdr <- dat[,pq]
fcV <- dat[,fc]
#upLines<-intersect(which(fcV > 0.58), which(fdr < 0.05))
upLines<-which(dat$colorss=="red")
downLines<-which(dat$colorss=="green")
target_gene<-which(dat$colorss=="yellow")
#downLines<-intersect(which(fcV < -0.58), which(fdr < 0.05))
collist<-rep(rgb(0,0,0,max=255,alpha=50),length(fdr))
collist[upLines] <- rep(rgb(0,0,255,max=255),length(upLines))
collist[downLines] <- rep(rgb(0,255,0,max=255),length(downLines))
collist[target_gene] <- rep(rgb(255,0,0,max=255),length(target_gene))
#fcV[fcV>5] = 5
#fcV[fcV< -5] = -5
logfdr = -1*log10(fdr)
logfdr[logfdr>30] = 30
#png(file=id,units="in",res=300,height=6,width=6)
#plot(fcV, logfdr, col=collist, xlab="logFC", ylab="-1*log10(FDR)", main="Volcano plot", pch=20, xlim=c(-5, 5), ylim=c(0, 30), cex = 0.5)
#abline(v = 1, col = "gray60", lty = 2)
#abline(v = -1, col = "gray60", lty = 2)
#abline(h = -1*log10(0.05), col = "gray60", lty = 2)
#dev.off()
id<-paste("/Bio/User/qipeng/project/GDR2105/", name1,"-vs-",name2,".DE.volcano.pdf",sep="")
pdf(file=id,height=6,width=6)
plot(fcV, logfdr, col=collist, xlab="logFC", ylab="-1*log10(FDR)", main="Volcano plot", pch=20, xlim=c(-5, 5), ylim=c(0, 30), cex = 0.5)
#plot(fcV, logfdr, col=collist, xlab="logFC", ylab="-FDR", main="Volcano plot", pch=20,cex = 0.5)
abline(v = 0.58, col = "gray60", lty = 2)
abline(v = -0.58, col = "gray60", lty = 2)
abline(h = -1*log10(0.05), col = "gray60", lty = 2)
dev.off()

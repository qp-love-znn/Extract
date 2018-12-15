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

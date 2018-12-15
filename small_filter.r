#!/usr/bin/Rscript
args <- commandArgs(T)

##function read matrix
if(length(args) != 3) {
	cat("Rscript get_filer_allZero.r <gene.fpkm> <type><outpre>\n")
	cat("use:Rscript get_filer_allZero.r gene.fpkm filer_allZero outpre\n");
	q()
}


filer_allZero <- function(x) {
	dat <- read.table(x, sep = "\t", header = T, check = F, row = 1,quote = '')
	dat<-dat[rowSums(dat) >0,]
#	dat<-dat[dat$score!="-",]
#	dat<-dat[dat$Pvalue<0.05,]
#	dat<-dat[dat$vip>1,]
	return(dat)
}

get_other_value <- function(x) {
	dat <- read.table(x, sep = "\t", header = T, check = F, row = 1,quote = '')
	for (i in 1:nrow(dat)){
		x = dat[i,]
		y = ifelse(x==0,0.01,x)
		dat[i,] = y
	}
	return(dat)
}

get_targte_file<-function(x,y){
	ss=data.frame()
	for (i in 1:nrow(x)){
		if(test[i,1] %in% y){
			ss = rbind(ss,x[i,])
		}
	}
	return(ss)
}

if(args[2] == "filer_allZero"){
	last_filter=filer_allZero(args[1])
} else{
	last_filter=get_other_value(args[1])
}

name = rownames(last_filter)
last_filter$id<-name
print(colnames(last_filter))
aa<-cbind(last_filter[,ncol(last_filter)],last_filter[,1:(ncol(last_filter)-1)])
colnames(aa)=c("id",colnames(last_filter)[-c(ncol(last_filter))])
write.table(aa,file = paste(args[3], '.filer.xls', sep = ''),quote = FALSE, sep = "\t",row = F)

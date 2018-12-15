library(pheatmap)

args <- commandArgs(T)

all <- read.table(file      = args[1],
				  header    = T,
				  row.names = 1,
				  sep = "\t",
				  check.names = F)

clean_data <- as.matrix(all[which(rowSums(all)>0),])
	min=apply(clean_data, 1, min)
	max=apply(clean_data, 1, max)
	clean_data = (clean_data-min)/(max-min)
#	head(clean_data)

#parameter <- list(mat           = clean_data,
#				  main          = "Significant Differentail Gene Heatmap",
#				  color         = colorRampPalette(c("steelblue", "white", "red"))(256),
#				  cluster_cols  = T,
#				  show_rownames = T,
#				  fontsize_row  = 3,
#				  cellheight    <- 3
#				  )

main          <- "All Diff Metabolite Heatmap"
#color         <- colorRampPalette(c("steelblue", "white", "red"))(256)
color = colorRampPalette(c("green","black","red"))(100)
cluster_cols  <- FALSE
show_rownames <- T
fontsize_row  <- 4
cellheight    <- 4


gene_num <- nrow(clean_data)
if(gene_num <= 200){
	fontsize_row  <- 4
	cellheight    <- 4
} else if(gene_num > 200 && gene_num <= 500){
	fontsize_row  <- 3
	cellheight    <- 3
} else if(gene_num > 500 && gene_num <= 2000){
	fontsize_row  <- 2
	cellheight    <- 2
} else {
	show_rownames <- F
	cellheight    <- 4000 / gene_num
}

if(length(args) == 3){
	group_info <- read.table(file = args[3], 
							 row.names = 2,
							 check.names = F)
	colnames(group_info)     <- c("Group")

#	unique <- unique(group_info)
#	table <- table(group_info)
#	group <- vector(mode = "integer", length = length(unique$Group))
#	for(i in 1:length(unique$Group)){
#		group[i] <- sum(table[unique[1:i,1]])
#	}

#	gaps_col       <- group
	cluster_cols   <- F
	annotation_col <- as.data.frame(group_info)

	scale  <- "none";
	filename <- paste(args[2], ".heatmap.noscale.png", sep = "")
	pheatmap(clean_data,
			 filename = filename,
			 main = main,
			 color = color,
			 scale = scale,
			 cluster_cols = cluster_cols,
			 cluster_rows = F,
			 show_rownames = show_rownames,
			 fontsize_row = fontsize_row, cellheight = cellheight,
			 annotation_col = annotation_col)
	filename <- paste(args[2], ".heatmap.noscale.pdf", sep = "")
	pheatmap(clean_data,
			 filename = filename,
			 main = main,
			 color = color,
			 scale = scale,
			 cluster_rows = F,
			 cluster_cols = cluster_cols,
			 show_rownames = show_rownames,
			 fontsize_row = fontsize_row, cellheight = cellheight,
			 annotation_col = annotation_col)
	
	scale  <- "row";
	filename <- paste(args[2], ".heatmap.png", sep = "")
	pheatmap(clean_data,
			 filename = filename,
			 main = main,
			 color = color,
			 scale = scale,
			 cluster_cols = cluster_cols,
			 cluster_rows = F,
			 show_rownames = show_rownames,
			 fontsize_row = fontsize_row, cellheight = cellheight,
			 annotation_col = annotation_col)
	filename <- paste(args[2], ".heatmap.pdf", sep = "")
	pheatmap(clean_data,
			 filename = filename,
			 main = main,
			 color = color,
			 scale = scale,
			 cluster_rows = F,
			 cluster_cols = cluster_cols,
			 show_rownames = show_rownames,
			 fontsize_row = fontsize_row, cellheight = cellheight,
			 annotation_col = annotation_col)
} else {
if(1>2){
	scale  <- "none";
	filename <- paste(args[2], ".heatmap.noscale.png", sep = "")
	pheatmap(clean_data,
			 filename = filename,
			 main = main,
			 color = color,
			 scale = scale,
			 cluster_rows = F,
			 cluster_cols = cluster_cols,
			 show_rownames = show_rownames,
			 border_color = 'NA',)
#			 fontsize_row = fontsize_row, cellheight = cellheight)
	filename <- paste(args[2], ".heatmap.noscale.pdf", sep = "")
	pheatmap(clean_data,
			 filename = filename,
			 main = main,
			 color = color,
			 scale = scale,
			 cluster_rows = F,
			 cluster_cols = cluster_cols,
			 show_rownames = show_rownames,
			 border_color = 'NA',)
#			 fontsize_row = fontsize_row, cellheight = cellheight)
}
	

	scale  <- "none";
	filename <- paste(args[2], ".heatmap.png", sep = "")
	pheatmap(clean_data,
			 filename = filename,
			 main = main,
			 color = color,
			 scale = scale,
			 cluster_rows = F,
			 cluster_cols = cluster_cols,
			 show_rownames = show_rownames,
			 border_color = 'NA'
#			 fontsize_row = fontsize_row, cellheight = cellheight)
			 )
	filename <- paste(args[2], ".heatmap.pdf", sep = "")
	pheatmap(clean_data,
			 filename = filename,
			 main = main,
			 color = color,
			 scale = scale,
			 cluster_rows = F,
			 cluster_cols = cluster_cols,
			 show_rownames = show_rownames,
			 border_color = 'NA'
#			 fontsize_row = fontsize_row, cellheight = cellheight)
			 )
	
}
file.remove("Rplots.pdf")

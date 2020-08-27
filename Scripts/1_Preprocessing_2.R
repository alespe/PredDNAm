
library(sva)

args = commandArgs(trailingOnly=TRUE)
impute <- args[1] # TRUE if Filtering and imputation requested (e.g. Illumina normalization method generates missing values)
batch_adj <- args[2] # TRUE if batch correction requested
input_file <- args[3] #Full path input matrix
batch <- args[4] # Full path metadata file with matched ids and batch id
outputDir <- args[5] # Output directory



# Importing files
batch <- read.table(batch,sep="\t",header=TRUE)

if(grepl(".rds",input_file) == TRUE){
	mat <- readRDS(input_file)
}
if(grepl(".txt",input_file) == TRUE){
	mat <- read.table(input_file,row.names=1,sep="\t",header=TRUE)
}

	if(!identical(as.character(batch[,1]),colnames(mat))){
		cat(paste("ERROR: matrix id names and batch matrix id names don't match \n"))
		quit()
	}


# Filtering and imputation if requested 
if(impute == "TRUE"){
	xy=dim(mat)
	rownas=rowSums(is.na(mat))
	rrow=which(rownas>=0.8*xy[2])
	if (length(rrow)>0) {  					
		mat=mat[-mat,]
	}
	library(impute)
	xx=impute.knn(as.matrix(mat))
	mat=xx$data
}

# ComBat
if(batch_adj == "TRUE"){
	mat_combat <- ComBat(as.matrix(mat), batch[,2], mod=NULL, par.prior = TRUE)
	write.table(mat_combat, file.path(outputDir,"Preprocessed_ComBat_M.txt"),sep="\t",row.names=TRUE,col.names=NA)
	saveRDS(mat_combat, file = file.path(outputDir,"Preprocessed_ComBat_M.rds"))

	# Converting M values to beta values
	Beta = 2^mat_combat/(2^mat_combat + 1)
	write.table(Beta, file.path(outputDir,"Preprocessed_ComBat_Beta.txt"),sep="\t",row.names=TRUE,col.names=NA)
	saveRDS(Beta, file = file.path(outputDir,"Preprocessed_ComBat_Beta.rds"))
}


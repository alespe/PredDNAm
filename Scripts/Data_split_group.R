	

Data_split_group <- function(covar,split_N_train,features_input){	

	features_matrix <- read.table(file.path(features_input),sep="\t",row.names=1,header=TRUE)
	features_matrix <- t(features_matrix)
	clinical <- read.table(file.path(covar),sep="\t",header=TRUE)
	clinical <- clinical[order(match(as.character(clinical[,1]),rownames(features_matrix))),]
	if(!identical(as.character(clinical[,1]),rownames(features_matrix))){
		cat(paste("ERROR: features and labels don't match \n"))
		quit()
	}	
	features_matrix <- data.frame(classes=clinical[,2],features_matrix)	
	x_train <- features_matrix[rownames(features_matrix) %in% split_N_train,]
	x_test  <- features_matrix[!rownames(features_matrix) %in% split_N_train,]
	
	return(list(x_train=x_train,x_test=x_test))
}


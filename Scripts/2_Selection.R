
library(plyr)
library(caret)
library(glmnet)
library(limma)

args = commandArgs(trailingOnly=TRUE)
cutoff = as.numeric(args[1])
norm = as.character(args[2])
method = as.character(args[3])
Regularization = as.numeric(args[4]) 
seed_N = as.numeric(args[5]) 
scaling <- args[6]
filtering <- args[7]
input_file <- as.character(args[8]) 
input_file_metadata <- as.character(args[9])
input_file_chr <- as.character(args[10]) 
input_file_snps <- as.character(args[11]) 
outputDir <- as.character(args[12]) 

dir.create(file.path(outputDir,norm,method,Regularization),showWarnings = FALSE,recursive = TRUE)
outputDir <- file.path(outputDir,norm,method,Regularization)
set.seed(seed_N)


# Importing files
 
if(grepl(".rds",input_file) == TRUE){
	mat <- readRDS(input_file)
}

if(grepl(".txt",input_file) == TRUE){
	mat <- read.table(input_file,sep="\t",row.names=1,header=TRUE)
}
mat <- as.data.frame(mat)

# Removing SNPs and sex chromosomes
if(filtering == TRUE){
	chr <- read.table(input_file_chr,sep="\t",header=TRUE)
	snps <- read.table(input_file_snps,sep="\t",header=TRUE)
	rownames(snps) <- chr$CpGs

	chr_filt <- chr[-which(chr[,2] == "chrX" | chr[,2] == "chrY"),]
	snps_filt <- snps[-which(snps$SNPS.CpG_maf > 0.1 | snps$SNPS.SBE_maf > 0.1),]

	mat <- mat[rownames(mat) %in% chr_filt[,1],]
	print(paste0("Features after filtering out those from sex chromosomes:",nrow(mat)))
	mat <- mat[rownames(mat) %in% rownames(snps_filt),]
	print(paste0("Features  after filtering out those from SNPs:",nrow(mat)))

}


metadata <- read.table(input_file_metadata,header=TRUE,sep="\t")
metadata <- metadata[metadata[,1] %in% colnames(mat),]
mat <- mat[,colnames(mat) %in% metadata[,1]] 
metadata <- metadata[order(match(metadata[,1],colnames(mat))),]
metadata[,2] <- relevel(metadata[,2],ref="CO")

	if(!identical(as.character(metadata[,1]),colnames(mat))){
		cat(paste("ERROR: matrix id names and batch matrix id names don't match \n"))
		quit()
	}
	cat(paste("Importing data from",nrow(metadata),"samples and",nrow(mat),"features"))

# Standarizing data
if(scaling == TRUE){
	dir.create(file.path(outputDir,"Scaled"),showWarnings = FALSE,recursive = TRUE)
	outputDir <- file.path(outputDir,"Scaled")
	sds_c=apply(mat,1,sd,na.rm=T)
	mns_c=apply(mat,1,mean,na.rm=T)
	mat = (mat-as.numeric(mns_c))/as.numeric(sds_c)	
}

################################## Training set for selection step

index <- createDataPartition(metadata[,2], p = 0.7, list = FALSE)
metadata_70 <- metadata[index,]
mat_70 <- mat[,index]
print(identical(colnames(mat_70),as.character(metadata_70[,1])))
write.table(colnames(mat_70),file.path(outputDir,paste0(seed_N,"seed_SamplesTraining.txt")),sep="\t",row.names=TRUE)


##################################  Univariate 
if(!file.exists(file.path(outputDir,paste0(seed_N,"_Results_Limma_H.txt")))){


	# Limma Models
	group <- factor(metadata_70$Label,levels=c("CO","CA_H","CA_L"))
	
	age <- as.numeric(metadata_70$Age)
	sex <- as.factor(metadata_70$Sex)

	design <- model.matrix(~ group + age + sex) # i reverse the order from the missMethyl documentation to llma vignette (https://bioconductor.org/packages/release/workflows/vignettes/RNAseq123/inst/doc/designmatrices.html). Section 7.3. Also https://support.bioconductor.org/p/114418/ 

	fit.reduced <- lmFit(mat_70,design)
	fit.reduced <- eBayes(fit.reduced, robust=TRUE)
	
	DE <- decideTests(fit.reduced)
	summary(DE)
	up_H <- rownames(DE[which(DE[,2] ==1),])
	down_H <- rownames(DE[which(DE[,2] ==-1),])
	up_L <- rownames(DE[which(DE[,3] ==1),])
	down_L <- rownames(DE[which(DE[,3] ==-1),])
	
	top_H <- topTable(fit.reduced,coef="groupCA_H",number=(length(up_H)+length(down_H)))
	top_L <- topTable(fit.reduced,coef="groupCA_L",number=(length(up_L)+length(down_L)))

	write.table(top_H,file.path(outputDir,paste0(seed_N,"_Results_Limma_H.txt")),sep="\t",row.names=TRUE,col.names=NA)
	write.table(top_L,file.path(outputDir,paste0(seed_N,"_Results_Limma_L.txt")),sep="\t",row.names=TRUE,col.names=NA)

	
}else{
	top_H <- read.table(file.path(outputDir,paste0(seed_N,"_Results_Limma_H.txt")),sep="\t",row.names=1,header=TRUE)
	top_L <- read.table(file.path(outputDir,paste0(seed_N,"_Results_Limma_L.txt")),sep="\t",row.names=1,header=TRUE)
}

##################################  Selection Univariate  

# Limma results

results_GA_H_sig <- top_H[which(top_H$adj.P.Val < cutoff),]
print(paste0("Significant Features Limma Model H:",nrow(results_GA_H_sig)))
results_GA_L_sig <- top_L[which(top_L$adj.P.Val < cutoff),]
print(paste0("Significant Features Limma Model L:",nrow(results_GA_L_sig)))

mat_filt_GA <- mat_70[rownames(mat_70) %in% unique(c(rownames(results_GA_H_sig),rownames(results_GA_L_sig))),] 

results_GA_H_sig2 <- top_H[which(top_H$P.Value < 0.00000024),]
print(paste0("Significant Features Limma Model H cutoff 2.4x10-7:",nrow(results_GA_H_sig2)))
results_GA_L_sig2 <- top_L[which(top_L$P.Value < 0.00000024),]
print(paste0("Significant Features Limma Model L:",nrow(results_GA_L_sig2)))
mat_filt_GA2 <- mat_70[rownames(mat_70) %in% unique(c(rownames(results_GA_H_sig2),rownames(results_GA_L_sig2))),] 


################################## Selection Multivariate 

if(nrow(mat_filt_GA) > ncol(mat)){
	print("Conducting Additional Multivariate Feature Selection Gaussian")
	x <- t(mat_filt_GA)
	y <- metadata_70[,2]
	print(identical(as.character(metadata_70[,1]),colnames(mat_filt_GA)))

	if(method == "ElasticNet"){
		#alpha = Regularization
		lambda_seq <- 10^seq(2, -2, by = -.1)

		# Lasso 
		cv_output <- cv.glmnet(x, y, alpha = Regularization, lambda = lambda_seq, family="multinomial")
		best_lam <- cv_output$lambda.min
		lasso_best <- glmnet(x, y, alpha = Regularization, lambda = best_lam, family="multinomial")
		pred <- predict(lasso_best, s = best_lam, newx = x)	
		write.table(pred,file.path(outputDir,paste0(seed_N,"Gaussian_pred_lasso_train.txt")),sep="\t",row.names=TRUE,col.names=TRUE)
		x_test <- mat[,!colnames(mat) %in% rownames(x)]
		x_test <- t(x_test)
		x_test <- x_test[,colnames(x_test) %in% colnames(x)]
		x_test <- x_test[,order(match(colnames(x_test),colnames(x)))]
		pred_test <- predict(lasso_best, s = best_lam, newx = x_test)	
		write.table(pred_test,file.path(outputDir,paste0(seed_N,"Gaussian_pred_lasso_test.txt")),sep="\t",row.names=TRUE,col.names=TRUE)

		results <- coef(lasso_best)
		coef_sig_CO <- results$CO[which(results$CO[,1] != 0),]
		coef_sig_CA_H <- results$CA_H[which(results$CA_H[,1] != 0),]
		coef_sig_CA_L <- results$CA_L[which(results$CA_L[,1] != 0),]		
		coef_sig <- data.frame(Group=c(rep("CO",length(coef_sig_CO)),rep("CA_H",length(coef_sig_CA_H)),rep("CA_L",length(coef_sig_CA_L))),Coef=c(coef_sig_CO,coef_sig_CA_H,coef_sig_CA_L),CpGs=c(names(coef_sig_CO),names(coef_sig_CA_H),names(coef_sig_CA_L)))
		coef_sig$CpGs[coef_sig$CpGs == ""] <- NA
		coef_sig <- coef_sig[complete.cases(coef_sig$CpGs),]
		write.table(coef_sig,file.path(outputDir,paste0(seed_N,"Gaussian_features_lasso.txt")),sep="\t",row.names=TRUE,col.names=FALSE)
		final_matrix <- mat[rownames(mat) %in% coef_sig$CpGs,]
		write.table(final_matrix,file.path(outputDir,paste0(seed_N,"Gaussian_features.txt")),sep="\t",row.names=TRUE,col.names=NA)
		}
		

}else{
	final_matrix <- mat[rownames(mat) %in% rownames(mat_filt_GA),]
	write.table(final_matrix,file.path(outputDir,paste0(seed_N,"cutoff",cutoff,"Gaussian_features.txt")),sep="\t",row.names=TRUE,col.names=NA)
}


write.table(metadata,file.path(outputDir,"Covariates_input.txt"),sep="\t",row.names=FALSE)



if(nrow(mat_filt_GA2) > ncol(mat)){
	print("Conducting Additional Multivariate Feature Selection Gaussian with cutoff 2.4x10-7")
	x <- t(mat_filt_GA2)
	y <- metadata_70[,2]
	print(identical(as.character(metadata_70[,1]),colnames(mat_filt_GA2)))

	if(method == "ElasticNet"){
		#alpha = Regularization
		lambda_seq <- 10^seq(2, -2, by = -.1)

		# Lasso 
		cv_output <- cv.glmnet(x, y, alpha = Regularization, lambda = lambda_seq, family="multinomial")
		best_lam <- cv_output$lambda.min
		lasso_best <- glmnet(x, y, alpha = Regularization, lambda = best_lam, family="multinomial")
		pred <- predict(lasso_best, s = best_lam, newx = x)	
		write.table(pred,file.path(outputDir,paste0(seed_N,"Gaussian_pred_lasso_train_cutoff.txt")),sep="\t",row.names=TRUE,col.names=TRUE)
		x_test <- mat[,!colnames(mat) %in% rownames(x)]
		x_test <- t(x_test)
		x_test <- x_test[,colnames(x_test) %in% colnames(x)]
		x_test <- x_test[,order(match(colnames(x_test),colnames(x)))]
		pred_test <- predict(lasso_best, s = best_lam, newx = x_test)	
		write.table(pred_test,file.path(outputDir,paste0(seed_N,"Gaussian_pred_lasso_test_cutoff.txt")),sep="\t",row.names=TRUE,col.names=TRUE)

		results <- coef(lasso_best)
		coef_sig_CO <- results$CO[which(results$CO[,1] != 0),]
		coef_sig_CA_H <- results$CA_H[which(results$CA_H[,1] != 0),]
		coef_sig_CA_L <- results$CA_L[which(results$CA_L[,1] != 0),]		
		coef_sig <- data.frame(Group=c(rep("CO",length(coef_sig_CO)),rep("CA_H",length(coef_sig_CA_H)),rep("CA_L",length(coef_sig_CA_L))),Coef=c(coef_sig_CO,coef_sig_CA_H,coef_sig_CA_L),CpGs=c(names(coef_sig_CO),names(coef_sig_CA_H),names(coef_sig_CA_L)))
		coef_sig$CpGs[coef_sig$CpGs == ""] <- NA
		coef_sig <- coef_sig[complete.cases(coef_sig$CpGs),]
		write.table(coef_sig,file.path(outputDir,paste0(seed_N,"Gaussian_features_lasso_cutoff.txt")),sep="\t",row.names=TRUE,col.names=FALSE)
		final_matrix <- mat[rownames(mat) %in% coef_sig$CpGs,]
		write.table(final_matrix,file.path(outputDir,paste0(seed_N,"Gaussian_features_cutoff.txt")),sep="\t",row.names=TRUE,col.names=NA)
		}
		

}else{
	final_matrix <- mat[rownames(mat) %in% rownames(mat_filt_GA),]
	write.table(final_matrix,file.path(outputDir,paste0(seed_N,"cutoff",cutoff,"Gaussian_features_cutoff.txt")),sep="\t",row.names=TRUE,col.names=NA)
}


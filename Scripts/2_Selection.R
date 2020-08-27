
library(mgcv)
library(ggpubr)
library(caret)
library(pcLasso)
library(glmnet)
library(ggplot2)

args = commandArgs(trailingOnly=TRUE)
norm = as.character(args[1]) #id selection
method = as.character(args[2]) # method multivariate (ElasticNet or pcLasso)
Regularization = as.numeric(args[3]) # Alpha parameter elasticnet or ratio pcLasso
Nsplit = as.numeric(args[4]) # split train-test
scaling <- args[5] # If TRUE, scale data per gene to zero mean and one standard deviation
filtering <- args[6] #If TRUE, removing sex chromosomes and loci with SNPs based on their MAF (10%)
input_file <- as.character(args[7]) #It works for both .rds and .txt files 
input_file_metadata <- as.character(args[8]) # Labels. First column IDs and second column Label
input_file_chr <- as.character(args[9]) # Chromosomes (if filtering is not TRUE, use false or any other argument)
input_file_snps <- as.character(args[10]) # SNPs (if filtering is not TRUE, use false or any other argument)
outputDir <- as.character(args[11]) # Directory for output files

dir.create(file.path(outputDir,norm,method,Regularization),showWarnings = FALSE,recursive = TRUE)
outputDir <- file.path(outputDir,norm,method,Regularization)
set.seed(1)


# Importing files
 
if(grepl(".rds",input_file) == TRUE){
	mat <- readRDS(input_file)
}

if(grepl(".txt",input_file) == TRUE){
	mat <- read.table(input_file,sep="\t",row.names=1,header=TRUE)
}
mat <- as.data.frame(mat)
metadata <- read.table(input_file_metadata,header=TRUE,sep="\t")
metadata <- metadata[metadata[,1] %in% colnames(mat),]
mat <- mat[,colnames(mat) %in% metadata[,1]] 
metadata <- metadata[order(match(metadata[,1],colnames(mat))),]

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

index <- createDataPartition(metadata[,2], p = Nsplit, list = FALSE)
metadata_70 <- metadata[index,]
mat_70 <- mat[,index]
print(identical(colnames(mat_70),as.character(metadata_70[,1])))
write.table(colnames(mat_70),file.path(outputDir,paste0("SamplesTraining.txt")),sep="\t",row.names=TRUE)


##################################  Univariate 
if(!file.exists(file.path(outputDir,paste0("Results_univar.txt")))){

	total <- nrow(mat_70)
				
	# Gaussian Models
	results_GA <- data.frame(Estimate=rep(NA,total),Std..Error=rep(NA,total),z.value=rep(NA,total),Pr...z..=rep(NA,total))
	for(x in 1:total){
		results_GA[x,] <- coef(summary(lm(as.numeric(mat_70[x,]) ~ as.factor(metadata_70[,2]), data = mat_70)))[2,1:4]
	}
	pval_GA <- results_GA[,"Pr...z.."]
	pval_bonf_GA <- p.adjust(pval_GA, method="bonferroni")
	pval_fdr_GA <- p.adjust(pval_GA, method="fdr")
	results_GA <- data.frame(features=rownames(mat_70),results_GA)
	results_GA <- data.frame(results_GA,pval_bonf=pval_bonf_GA,pval_fdr=pval_fdr_GA)
	results_GA <- results_GA[order(results_GA$Pr...z..),]
	write.table(results_GA,file.path(outputDir,paste0("Results_univar.txt")),sep="\t",row.names=FALSE,col.names=TRUE)

	# Generalized Additive Models
	results_GAM <- data.frame(Pr...z..=rep(NA,total))
	for(x in 1:total){
			dat = data.frame(y=as.integer(metadata_70[,2]),x0=as.numeric(mat_70[x,]))
			results_GAM[x,] <- summary(gam(y~s(x0), data = dat, family=binomial, method = "REML"))$s.p
	}
	pval_GAM <- results_GAM[,"Pr...z.."]
	pval_bonf_GAM <- p.adjust(pval_GAM, method="bonferroni")
	pval_fdr_GAM <- p.adjust(pval_GAM, method="fdr")
	results_GAM <- data.frame(features=rownames(mat_70),results_GAM)
	results_GAM <- data.frame(results_GAM,pval_bonf=pval_bonf_GAM,pval_fdr=pval_fdr_GAM)
	results_GAM <- results_GAM[order(results_GAM$Pr...z..),]
	write.table(results_GAM,file.path(outputDir,paste0("Results_univar_GAM.txt")),sep="\t",row.names=FALSE,col.names=TRUE)

}else{
	results_GA <- read.table(file.path(outputDir,paste0("Results_univar.txt")),sep="\t",header=TRUE)
	results_GAM <- read.table(file.path(outputDir,paste0("Results_univar_GAM.txt")),sep="\t",header=TRUE)
}

##################################  Selection Univariate  

results_GA_sig <- results_GA[which(results_GA$pval_fdr < 0.05),]
print(paste0("Significant Features Gaussian Model:",nrow(results_GA_sig)))
results_GAM_sig <- results_GAM[which(results_GAM$pval_fdr < 0.05),]
print(paste0("Significant Features Generalized Additive Model:",nrow(results_GAM_sig)))
print(paste0("Common Features Gaussian and GAM:",length(intersect(results_GA_sig$features,results_GAM_sig$features))))

mat_filt_GA <- mat_70[rownames(mat_70) %in% unique(c(as.character(results_GA_sig$features))),] 
mat_filt_GAM <- mat_70[rownames(mat_70) %in% unique(c(as.character(results_GAM_sig$features))),] 

# Removing SNPs and sex chromosomes
if(filtering == TRUE){
	chr <- read.table(input_file_chr,sep="\t",header=TRUE)
	snps <- read.table(input_file_snps,sep="\t",header=TRUE)
	rownames(snps) <- chr$CpGs

	chr_filt <- chr[-which(chr[,2] == "chrX" | chr[,2] == "chrY"),]
	snps_filt <- snps[-which(snps$SNPS.CpG_maf > 0.1 | snps$SNPS.SBE_maf > 0.1),]
	
	mat_filt_GA <- mat_filt_GA[rownames(mat_filt_GA) %in% chr_filt[,1],]
	print(paste0("Features Gaussian after filtering out those from sex chromosomes:",nrow(mat_filt_GA)))
	mat_filt_GA <- mat_filt_GA[rownames(mat_filt_GA) %in% rownames(snps_filt),]
	print(paste0("Features Gaussian after filtering out those from SNPs:",nrow(mat_filt_GA)))

	mat_filt_GAM <- mat_filt_GAM[rownames(mat_filt_GAM) %in% chr_filt[,1],]
	print(paste0("Features GAM after filtering out those from sex chromosomes:",nrow(mat_filt_GAM)))
	mat_filt_GAM <- mat_filt_GAM[rownames(mat_filt_GAM) %in% rownames(snps_filt),]
	print(paste0("Features GAM after filtering out those from SNPs:",nrow(mat_filt_GAM)))

}


################################## Selection Multivariate 

if(nrow(mat_filt_GA) > ncol(mat)){
	print("Conducting Additional Multivariate Feature Selection")
	x <- t(mat_filt_GA)
	y <- metadata_70[,2]
	print(identical(as.character(metadata_70[,1]),colnames(mat_filt_GA)))

	if(method == "ElasticNet"){
		lambda_seq <- 10^seq(2, -2, by = -.1)
		cv_output <- cv.glmnet(x, y, alpha = Regularization, lambda = lambda_seq, family="binomial")
		best_lam <- cv_output$lambda.min
		lasso_best <- glmnet(x, y, alpha = Regularization, lambda = best_lam, family="binomial")
		
		pred <- predict(lasso_best, s = best_lam, newx = x)	
		x_test <- mat[,!colnames(mat) %in% rownames(x)]
		x_test <- t(x_test)
		x_test <- x_test[,colnames(x_test) %in% colnames(x)]
		x_test <- x_test[,order(match(colnames(x_test),colnames(x)))]
		pred_test <- predict(lasso_best, s = best_lam, newx = x_test)	
		predic_all <- rbind(pred,pred_test)
		metadata_pred <- metadata[order(match(metadata[,1],rownames(predic_all))),]
		identical(rownames(predic_all),as.character(metadata_pred[,1]))
		Partition <- ifelse(metadata_pred[,1] %in% metadata_70[,1],"Train","Test")
		Status <- ifelse(metadata_pred[,2] == 0,"Prosp.CO","Prosp.NHL")
		data_plot <- data.frame(Status=Status,Prediction=as.numeric(predic_all),Partition=Partition)
		write.table(data_plot,file.path(outputDir,"Pred_ElasticNet.txt"),sep="\t",row.names=FALSE,col.names=TRUE)
		p<-ggplot(data_plot, aes(x=Partition, y=Prediction, fill=Status)) +
			labs(title = "Predictive Values Across Groups") +
			theme_bw() +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
			geom_boxplot() +
			scale_fill_manual(values=c("#E69F00","#0072B2")) +
			stat_compare_means(method = "wilcox.test",size=4.5, label.y = max(data_plot$Prediction+1))+
			theme(text=element_text(size=20))
		ggsave(file.path(outputDir,"Pred_ElasticNet.png"))

		results <- coef(lasso_best)
		coef_sig <- results[which(results[,1] > 0),]
		final_matrix <- mat[rownames(mat) %in% names(coef_sig),]
		write.table(final_matrix,file.path(outputDir,paste0("Features.txt")),sep="\t",row.names=TRUE,col.names=NA)
		}
		
		if(method == "pcLasso"){
		cvfit <- cv.pcLasso(x, y, ratio = Regularization, family="binomial")
		index_lambda <- which(cvfit$lambda == cvfit$lambda.min)
		features_lasso <- cvfit$glmfit$mx[which(cvfit$glmfit$beta[,index_lambda] != 0)]

		final_matrix <- mat[rownames(mat) %in% names(features_lasso),]
		write.table(final_matrix,file.path(outputDir,paste0("Features.txt")),sep="\t",row.names=TRUE,col.names=NA)
	}
}else{
	final_matrix <- mat[rownames(mat) %in% rownames(mat_filt_GA),]
	write.table(final_matrix,file.path(outputDir,paste0("Features.txt")),sep="\t",row.names=TRUE,col.names=NA)
}

if(nrow(mat_filt_GAM) > ncol(mat)){
	print("Conducting Additional Multivariate Feature Selection GAM")
	x <- t(mat_filt_GAM)
	y <- metadata_70[,2]
	identical(as.character(metadata_70[,1]),colnames(mat_filt_GAM))

	if(method == "ElasticNet"){
		lambda_seq <- 10^seq(2, -2, by = -.1)
		cv_output <- cv.glmnet(x, y, alpha = Regularization, lambda = lambda_seq, family="binomial")
		best_lam <- cv_output$lambda.min
		lasso_best <- glmnet(x, y, alpha = Regularization, lambda = best_lam, family="binomial")
		pred <- predict(lasso_best, s = best_lam, newx = x)	
		x_test <- mat[,!colnames(mat) %in% rownames(x)]
		x_test <- t(x_test)
		x_test <- x_test[,colnames(x_test) %in% colnames(x)]
		x_test <- x_test[,order(match(colnames(x_test),colnames(x)))]
		pred_test <- predict(lasso_best, s = best_lam, newx = x_test)	
		predic_all <- rbind(pred,pred_test)
		metadata_pred <- metadata[order(match(metadata[,1],rownames(predic_all))),]
		identical(rownames(predic_all),as.character(metadata_pred[,1]))
		Partition <- ifelse(metadata_pred[,1] %in% metadata_70[,1],"Train","Test")
		Status <- ifelse(metadata_pred[,2] == 0,"Prosp.CO","Prosp.NHL")
		data_plot <- data.frame(Status=Status,Prediction=as.numeric(predic_all),Partition=Partition)
		write.table(data_plot,file.path(outputDir,"GAM_pred_ElasticNet.txt"),sep="\t",row.names=FALSE,col.names=TRUE)
		p<-ggplot(data_plot, aes(x=Partition, y=Prediction, fill=Status)) +
			labs(title = "Predictive Values Across Groups") +
			theme_bw() +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
			geom_boxplot() +
			scale_fill_manual(values=c("#E69F00","#0072B2")) +
			stat_compare_means(method = "wilcox.test",size=4.5, label.y = max(data_plot$Prediction+1))+
			theme(text=element_text(size=20))
		ggsave(file.path(outputDir,"GAM_pred_ElasticNet.png"))
				
		results <- coef(lasso_best)
		coef_sig <- results[which(results[,1] > 0),]
		final_matrix <- mat[rownames(mat) %in% names(coef_sig),]
		write.table(final_matrix,file.path(outputDir,paste0("GAM_features.txt")),sep="\t",row.names=TRUE,col.names=NA)
		}
		
		if(method == "pcLasso"){
		# pcLasso 
		cvfit <- cv.pcLasso(x, y, ratio = Regularization, family="binomial")
		index_lambda <- which(cvfit$lambda == cvfit$lambda.min)
		features_lasso <- cvfit$glmfit$mx[which(cvfit$glmfit$beta[,index_lambda] != 0)]

		final_matrix <- mat[rownames(mat) %in% names(features_lasso),]
		write.table(final_matrix,file.path(outputDir,paste0("GAM_features.txt")),sep="\t",row.names=TRUE,col.names=NA)
	}
}else{
	final_matrix <- mat[rownames(mat) %in% rownames(mat_filt_GA),]
	write.table(final_matrix,file.path(outputDir,paste0("GAM_features.txt")),sep="\t",row.names=TRUE,col.names=NA)
}
	

write.table(metadata,file.path(outputDir,"Outcome_input.txt"),sep="\t",row.names=FALSE)


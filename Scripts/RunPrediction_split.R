
library(ggplot2)
library(caret)
library(glmnet)
library(xgboost)
library(pROC)
library(dplyr)
library(pcLasso)
library(umap)
library(diffusionMap)
library(NMF)

args = commandArgs(trailingOnly=TRUE)
j <- as.character(args[1]) #Algorithm
Transformation <- as.character(args[2]) #Transformation:"PCA","stand","Difussion","CV", "UMAP" or "None"
features_input <- as.character(args[3]) #Name file containing features input
covariates_input <- as.character(args[4]) #Name file containing covariates input  
MY_DIR <- as.character(args[5]) #Directory with features and covariates files 
dir_script_pred <- as.character(args[6]) #Directory with scripts 
train_split <- as.character(args[7]) #text file with IDs from the training dataset to split the training and test datasets. For random, "Run_Prediction_slurm.R"
train_split <- read.table(train_split,sep="\t")
train_split <- train_split[,1]

source(file.path(dir_script_pred,"Metrics_results.R"))
source(file.path(dir_script_pred,"Transformations.R"))
source(file.path(dir_script_pred,"Data_split_group.R"))
source(file.path(dir_script_pred,"ConfusionMatrix.R"))
source(file.path(dir_script_pred,"Algorithms.R"))
source(file.path(dir_script_pred,"NMF.R"))

labels_pred_merged <- c()
labels_pred_merged_train <- c()
metrics_all <- data.frame(accuracy=numeric(),class.err=numeric(),precision=numeric(),sensitivity=numeric(),specificity=numeric(),f1score=numeric(),auc=numeric())
metrics_all_train <- data.frame(accuracy=numeric(),class.err=numeric(),precision=numeric(),sensitivity=numeric(),specificity=numeric(),f1score=numeric(),auc=numeric())


#ML/DL

	dir.create(file.path(MY_DIR,j), showWarnings = FALSE)
	dir.create(file.path(MY_DIR,j,Transformation), showWarnings = FALSE)
	setwd(file.path(MY_DIR,j,Transformation))

	#Splitting the data into training and test datasets
	Partitions_data <- Data_split_group(covariates_input,train_split,features_input)
	
	#Transforming the data
	trans_obj <- Transformation_function(Partitions_data[[1]][,-1],Partitions_data[[2]][,-1],Transformation)
	write.table(rownames(trans_obj[[1]]),paste0("labels_train.txt"),sep="\t",row.names=FALSE)
	write.table(rownames(trans_obj[[2]]),paste0("labels_test.txt"),sep="\t",row.names=FALSE)
	
	if(j == "adaboost"){	
			results_test <- adaboost_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- adaboost_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))				
	}else if(j == "avNNet"){
			results_test <- avNNet_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- avNNet_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "bayesglm"){
			results_test <- bayesglm_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- bayesglm_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "BstLm"){
			results_test <- BstLm_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- BstLm_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "C5.0"){
			results_test <- C5.0_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- C5.0_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "cforest"){
			results_test <- cforest_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- cforest_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "glmnet"){
			results_test <- glmnet_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- glmnet_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "fda"){
			results_test <- fda_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- fda_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "gamboost"){
			results_test <- gamboost_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- gamboost_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "gbm"){
			results_test <- gbm_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- gbm_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "glm"){
			results_test <- glm_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- glm_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "glmboost"){
			results_test <- glmboost_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- glmboost_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "knn"){
			results_test <- knn_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- knn_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))			
	}else if(j == "lda"){
			results_test <- lda_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- lda_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "LogitBoost"){
			results_test <- LogitBoost_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- LogitBoost_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "lssvmRadial"){
			results_test <- lssvmRadial_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- lssvmRadial_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "naive_bayes"){
			results_test <- naive_bayes_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- naive_bayes_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "nnet"){
			results_test <- nnet_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- nnet_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "parRF"){
			results_test <- parRF_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- parRF_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))			
	}else if(j == "pcaNNet"){
			results_test <- pcaNNet_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- pcaNNet_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))			
	}else if(j == "pcLasso"){
			results_test <- pcLasso_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- pcLasso_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "pls"){
			results_test <- pls_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- pls_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))			
	}else if(j == "regLogistic"){
			results_test <- regLogistic_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- regLogistic_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "rf"){
			results_test <- rf_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- rf_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "rf"){
			results_test <- rf_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- rf_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "rFerns"){
			results_test <- rFerns_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- rFerns_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "svmLinear"){
			results_test <- svmLinear_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- svmLinear_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "svmLinearWeights2"){
			results_test <- svmLinearWeights2_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- svmLinearWeights2_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "xgboost"){
			results_test <- xgboost_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- xgboost_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}else if(j == "wsrf"){
			results_test <- wsrf_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]))
			results_train <- wsrf_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]))
	}

	

	# Predictive and true label per sample id
	write.table(results_test[-4],file.path(MY_DIR,j,Transformation,paste0("Labels_Pred_True_test.txt")),sep="\t",row.names=TRUE,col.names=NA)
	write.table(results_train[-4],file.path(MY_DIR,j,Transformation,paste0("Labels_Pred_True_train.txt")),sep="\t",row.names=TRUE,col.names=NA)
		

	# Assessment metrics for each and all iterations
	file_test <- file.path(MY_DIR,j,Transformation,"Labels_Pred_True_test.txt")
	metrics_results <- Metrics(file_test)
	metrics_iteration <- metrics_results[[1]]
	metrics_all <- rbind(metrics_all,metrics_iteration)
	labels_pred <- metrics_results[[2]]
	labels_pred_merged <- rbind(labels_pred_merged,labels_pred)

	file_train <- file.path(MY_DIR,j,Transformation,"Labels_Pred_True_train.txt")
	metrics_results_train <- Metrics(file_train)
	metrics_iteration_train <- metrics_results_train[[1]]
	metrics_all_train <- rbind(metrics_all_train,metrics_iteration_train)
	labels_pred_train <- metrics_results_train[[2]]
	labels_pred_merged_train <- rbind(labels_pred_merged_train,labels_pred_train)

	rownames(metrics_all) <- seq(1,1,1)
	colnames(metrics_all) <- c("accuracy","class.err","precision","sensitivity","specificity","f1score","auc")
	write.table(metrics_all,file.path(MY_DIR,j,Transformation,paste0("Metrics_Results.txt")),sep="\t",row.names=TRUE,col.names=NA)
	write.table(labels_pred_merged,file.path(MY_DIR,j,Transformation,"All_predic_labels.txt"),sep="\t",row.names=TRUE,col.names=NA)

	rownames(metrics_all_train) <- seq(1,1,1)
	colnames(metrics_all_train) <- c("accuracy","class.err","precision","sensitivity","specificity","f1score","auc")
	write.table(metrics_all_train,file.path(MY_DIR,j,Transformation,paste0("Metrics_Results_train.txt")),sep="\t",row.names=TRUE,col.names=NA)
	write.table(labels_pred_merged_train,file.path(MY_DIR,j,Transformation,"All_predic_labels_train.txt"),sep="\t",row.names=TRUE,col.names=NA)


# Confusion matrix 
png(file.path(MY_DIR,j,Transformation,'confusionMatrix.png'))	
conf_matrix_plot(results_test[[4]])
dev.off()
png(file.path(MY_DIR,j,Transformation,'confusionMatrix_train.png'))	
conf_matrix_plot(results_train[[4]])
dev.off()
	











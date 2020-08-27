
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
library(gridExtra) 
library(grid) 

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

source(file.path(dir_script_pred,"Transformations.R"))
source(file.path(dir_script_pred,"Data_split_group.R"))
source(file.path(dir_script_pred,"ConfusionMatrix_multinomial.R"))
source(file.path(dir_script_pred,"ConfusionMatrix_multinomial_prop.R"))
source(file.path(dir_script_pred,"Algorithms_run.R"))
source(file.path(dir_script_pred,"NMF.R"))
set.seed(1)

labels_pred_merged <- c()
metrics_all <- data.frame(accuracy=numeric(),class.err=numeric(),precision=numeric(),sensitivity=numeric(),specificity=numeric(),f1score=numeric(),auc=numeric())


#ML/DL

	dir.create(file.path(MY_DIR,j), showWarnings = FALSE)
	dir.create(file.path(MY_DIR,j,Transformation), showWarnings = FALSE)
	setwd(file.path(MY_DIR,j,Transformation))

	# Splitting the data into training and test datasets
	Partitions_data <- Data_split_group(covariates_input,train_split,features_input)
	
	# Transforming the data
	trans_obj <- Transformation_function(Partitions_data[[1]][,-1],Partitions_data[[2]][,-1],Transformation)
	write.table(rownames(trans_obj[[1]]),paste0("labels_train.txt"),sep="\t",row.names=FALSE)
	write.table(rownames(trans_obj[[2]]),paste0("labels_test.txt"),sep="\t",row.names=FALSE)
	
	# Run algorithm
	results_test <- runAlg_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]),j)
	results_train <- runAlg_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),j)				

	# Predictive and true label per sample id
	write.table(results_test[-4],file.path(MY_DIR,j,Transformation,"Labels_Pred_True_test.txt"),sep="\t",row.names=TRUE,col.names=NA)
	write.table(results_train[-4],file.path(MY_DIR,j,Transformation,"Labels_Pred_True_train.txt"),sep="\t",row.names=TRUE,col.names=NA)
	write.table(results_test$conf$byClass,file.path(MY_DIR,j,Transformation,"Metrics_Results_byClass.txt"),sep="\t",row.names=TRUE,col.names=NA)
		
	# Confusion matrix 
	png(file.path(MY_DIR,j,Transformation,'confusionMatrix.png'))	
	conf_matrix_plot_multi(results_test[[4]],MY_DIR,j,Transformation)
	dev.off()
	conf_matrix_plot_multi_prop(results_test[[4]],MY_DIR,j,Transformation)


		







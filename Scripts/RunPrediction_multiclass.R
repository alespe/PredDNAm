
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
N_iteration <- as.numeric(args[7]) #Number of iterations (Monte-Carlo Cross Validation)
split_value <- as.numeric(args[8]) #Split value e.g. 0.7  means 70% train, 30% train 

source(file.path(dir_script_pred,"Metrics_results_multi.R"))
source(file.path(dir_script_pred,"Transformations.R"))
source(file.path(dir_script_pred,"Data_split.R"))
source(file.path(dir_script_pred,"ConfusionMatrix_multinomial.R"))
source(file.path(dir_script_pred,"ConfusionMatrix_multinomial_prop.R"))
source(file.path(dir_script_pred,"Algorithms_run.R"))
source(file.path(dir_script_pred,"NMF.R"))
set.seed(1)
labels_pred_merged_H <- c()
labels_pred_merged_train_H <- c()
labels_pred_merged_L <- c()
labels_pred_merged_train_L <- c()
metrics_all_H <- data.frame(accuracy=numeric(),class.err=numeric(),precision=numeric(),sensitivity=numeric(),specificity=numeric(),f1score=numeric(),auc=numeric())
metrics_all_L <- data.frame(accuracy=numeric(),class.err=numeric(),precision=numeric(),sensitivity=numeric(),specificity=numeric(),f1score=numeric(),auc=numeric())
metrics_all_train_H <- data.frame(accuracy=numeric(),class.err=numeric(),precision=numeric(),sensitivity=numeric(),specificity=numeric(),f1score=numeric(),auc=numeric())
metrics_all_train_L <- data.frame(accuracy=numeric(),class.err=numeric(),precision=numeric(),sensitivity=numeric(),specificity=numeric(),f1score=numeric(),auc=numeric())


#ML/DL
for(iteration in 1:N_iteration){

	dir.create(file.path(MY_DIR,j), showWarnings = FALSE)
	dir.create(file.path(MY_DIR,j,Transformation), showWarnings = FALSE)
	setwd(file.path(MY_DIR,j,Transformation))

	# Splitting the data into training and test datasets
	Partitions_data <- Data_split(covariates_input,split_value,features_input)
	
	# Transforming the data
	trans_obj <- Transformation_function(Partitions_data[[1]][,-1],Partitions_data[[2]][,-1],Transformation)
	
	# Run algorithm
	results_test <- runAlg_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[2]][,1],trans_obj[[2]]),j)
	results_train <- runAlg_function(data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),data.frame(classes=Partitions_data[[1]][,1],trans_obj[[1]]),j)				

	# Predictive and true label per sample id
	write.table(results_test[-4],file.path(MY_DIR,j,Transformation,"Labels_Pred_True_test.txt"),sep="\t",row.names=TRUE,col.names=NA)
	write.table(results_train[-4],file.path(MY_DIR,j,Transformation,"Labels_Pred_True_train.txt"),sep="\t",row.names=TRUE,col.names=NA)
	write.table(results_test$conf$byClass,file.path(MY_DIR,j,Transformation,"Metrics_Results_byClass.txt"),sep="\t",row.names=TRUE,col.names=NA)
		
	# Confusion matrix for the first iteration	
	if(iteration == 1){
		png(file.path(MY_DIR,j,Transformation,'confusionMatrix.png'))	
		conf_matrix_plot_multi(results_test[[4]],MY_DIR,j,Transformation)
		dev.off()
		conf_matrix_plot_multi_prop(results_test[[4]],MY_DIR,j,Transformation)
		png(file.path(MY_DIR,j,Transformation,'confusionMatrix_train.png'))	
		conf_matrix_plot_multi(results_train[[4]],MY_DIR,j,Transformation)
		dev.off()
		conf_matrix_plot_multi_prop(results_train[[4]],MY_DIR,j,Transformation)
	}

	
	# Assessment metrics for each and all iterations
	file_test <- file.path(MY_DIR,j,Transformation,"Labels_Pred_True_test.txt")
	metrics_results_H <- Metrics_multi(file_test,"CO","CA_H")
	metrics_iteration_H <- metrics_results_H[[1]]
	metrics_all_H <- rbind(metrics_all_H,metrics_iteration_H)
	labels_pred_H <- metrics_results_H[[2]]
	labels_pred_merged_H <- rbind(labels_pred_merged_H,labels_pred_H)
	metrics_results_L <- Metrics_multi(file_test,"CO","CA_L")
	metrics_iteration_L <- metrics_results_L[[1]]
	metrics_all_L <- rbind(metrics_all_L,metrics_iteration_L)
	labels_pred_L <- metrics_results_L[[2]]
	labels_pred_merged_L <- rbind(labels_pred_merged_L,labels_pred_L)


	file_train <- file.path(MY_DIR,j,Transformation,"Labels_Pred_True_train.txt")
	metrics_results_train_H <- Metrics_multi(file_train,"CO","CA_H")
	metrics_iteration_train_H <- metrics_results_train_H[[1]]
	metrics_all_train_H <- rbind(metrics_all_train_H,metrics_iteration_train_H)
	labels_pred_train_H <- metrics_results_train_H[[2]]
	labels_pred_merged_train_H <- rbind(labels_pred_merged_train_H,labels_pred_train_H)
	metrics_results_train_L <- Metrics_multi(file_train,"CO","CA_L")
	metrics_iteration_train_L <- metrics_results_train_L[[1]]
	metrics_all_train_L <- rbind(metrics_all_train_L,metrics_iteration_train_L)
	labels_pred_train_L <- metrics_results_train_L[[2]]
	labels_pred_merged_train_L <- rbind(labels_pred_merged_train_L,labels_pred_train_L)

		
} #end of iterations

# Confusion matrix for the all iterations	
png(file.path(MY_DIR,j,Transformation,'confusionMatrix_all_H.png'))	
conf_matrix_plot_all(labels_pred_merged_H,metrics_all,N_iteration)
dev.off()
png(file.path(MY_DIR,j,Transformation,'confusionMatrix_all_L.png'))	
conf_matrix_plot_all(labels_pred_merged_L,metrics_all,N_iteration)
dev.off()
png(file.path(MY_DIR,j,Transformation,'confusionMatrix_all_train_H.png'))	
conf_matrix_plot_all(labels_pred_merged_train_H,metrics_all_train,N_iteration)
dev.off()
png(file.path(MY_DIR,j,Transformation,'confusionMatrix_all_train_L.png'))	
conf_matrix_plot_all(labels_pred_merged_train_L,metrics_all_train,N_iteration)
dev.off()







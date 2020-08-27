
args = commandArgs(trailingOnly=TRUE)
j_input <- as.character(args[1]) # algorithm
Transformation_input <- as.character(args[2]) # data transformation: c("none","PCA","Difussion","UMAP","stand")
input_feat <- as.character(args[3])
input_label <- as.character(args[4])
dir_scripts <- as.character(args[5])
dir_ouput <- as.character(args[6])
Nsubjects <- as.character(args[7]) #ALL (all subjects unsupervised feature selection) or PREDEFINED (training set already defined)
labels_split <- as.character(args[8]) #If PREDEFINED mode, file listing labels used for training
N.iter <- as.numeric(args[8]) #Number of iterations: only with unsupervised feature selection
split.val<- as.numeric(args[9]) #Split for train/test dataset: only with unsupervised feature selection

setwd(dir_scripts)

if(Nsubjects == "ALL"){
	cmd_line = paste("Rscript --vanilla",
			file.path("RunPrediction.R"),
			j=j_input,
			Transformation=Transformation_input,
			features_input=file.path(input_feat),
			covariates_input=file.path(input_label),
			MY_DIR=file.path(dir_ouput),
			dir_script_pred=dir_scripts,
			N_iteration=N.iter,
			split_value=split.val
			)
	system(cmd_line)		
}
if(Nsubjects == "PREDEFINED"){
	cmd_line = paste("Rscript --vanilla",
			file.path("RunPrediction_split.R"),
			j=j_input,
			Transformation=Transformation_input,
			features_input=file.path(input_feat),
			covariates_input=file.path(input_label),
			MY_DIR=file.path(dir_ouput),
			dir_script_pred=dir_scripts,
			train_split=labels_split
			)
	system(cmd_line)		
}



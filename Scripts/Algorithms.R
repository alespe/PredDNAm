	

adaboost_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 2, repeats = 1)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "adaboost",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)	
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
	
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}


avNNet_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "avNNet",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)	
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

bayesglm_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "bayesglm",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

BstLm_function <- function(train_dataset,test_dataset,MY_DIR,Transformation){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "BstLm",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)

	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

C5.0_function <- function(train_dataset,test_dataset,MY_DIR,Transformation){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "C5.0",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)

	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

glmnet_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "glmnet",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)	
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
	
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

fda_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "fda",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)	
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

gamboost_function <- function(train_dataset,test_dataset,MY_DIR,Transformation){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "gamboost",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

gbm_function <- function(train_dataset,test_dataset,MY_DIR,Transformation){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "gbm",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

glm_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "glm",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)	
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

glmboost_function <- function(train_dataset,test_dataset,MY_DIR,Transformation){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "glmboost",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)

	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

knn_function <- function(train_dataset,test_dataset,Neighbors){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "knn",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)	
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

lda_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "lda",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)	
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

LogitBoost_function <- function(train_dataset,test_dataset,MY_DIR,Transformation){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "LogitBoost",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)

	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

lssvmRadial_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "lssvmRadial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
	
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

naive_bayes_function <- function(train_dataset,test_dataset,MY_DIR,Transformation){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "naive_bayes",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)

	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

nnet_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "nnet",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

parRF_function <- function(train_dataset,test_dataset,MY_DIR,Transformation){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "parRF",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

pcaNNet_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "pcaNNet",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)	
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}


pcLasso_function <- function(train_dataset,test_dataset){	

	ratio_values <- c(0.25,0.5,0.75,0.8,0.85,0.9,0.95,1)
	y <- as.numeric(as.character(train_dataset$classes))
	x <- as.matrix(train_dataset[,-1])
	x_test <- as.matrix(test_dataset[,-1])

	groups <- NMF_function(x)

	run_pcLasso <- function(ratio_value){
		cvfit <- cv.pcLasso(x, y, ratio = ratio_value, groups=groups, family="binomial", standardize=FALSE)
		pred_pcLasso <- predict(cvfit, x_test)
		
		conf_tmp_list <- list()
		for(clasif in seq(0.1,0.9,0.1)){
			pred_tmp <- as.factor(ifelse(pred_pcLasso <= clasif, 0, 1))
			conf_tmp <- confusionMatrix(as.factor(pred_tmp), as.factor(test_dataset$classes))
			conf_tmp_list <- append(conf_tmp_list,conf_tmp$overall[1])
		}
		names(conf_tmp_list) <- seq(0.1,0.9,0.1)
		cutoff_clasif <- names(which(conf_tmp_list == do.call(max, conf_tmp_list)))
		accuracy_tmp <- do.call(max, conf_tmp_list)

		return(list(accuracy_tmp,pred_pcLasso,cutoff_clasif))
	}

	results_all <- lapply(ratio_values,function(x)run_pcLasso(x))
	names(results_all) <- ratio_values
	acc_ratio <- sapply(1:length(results_all),function(x)results_all[[x]][[1]])
	optim_ratio <- ratio_values[acc_ratio == max(acc_ratio)]
	if(length(optim_ratio)>1){optim_ratio <- optim_ratio[length(optim_ratio)]} #if two are optimal, the largest one will be chosen (the one more similar to the standard lasso method)
	write.table(optim_ratio,"pcLasso_ratio.txt",sep="\t",row.names=FALSE,col.names=FALSE, append = TRUE)

	pclasso_optim <- results_all[[as.character(optim_ratio)]]
	pred <- pclasso_optim[[2]]
	clasif_test <- pclasso_optim[[3]]
	write.table(clasif_test,"clasiffier_cutoff.txt",sep="\t",row.names=FALSE,col.names=FALSE, append = TRUE)
	pred_num <- as.factor(ifelse(pred <= clasif_test, 0, 1))
		
	conf <- confusionMatrix(pred, as.factor(test_dataset$classes))
	
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_num))
}

pls_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "pls",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

regLogistic_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "regLogistic",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)	
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

rf_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "rf",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

rFerns_function <- function(train_dataset,test_dataset,MY_DIR,Transformation){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "rFerns",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

svmLinear_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "svmLinear",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
	
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

svmLinearWeights2_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "svmLinearWeights2",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
	
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

xgboost_function <- function(train_dataset,test_dataset){	

	X_train = xgb.DMatrix(as.matrix(train_dataset %>% select(-classes)))
	y_train = train_dataset$classes
	X_test = xgb.DMatrix(as.matrix(test_dataset %>% select(-classes)))
	y_test = test_dataset$classes

	xgb_trcontrol = trainControl(
	  method = "cv",
	  number = 5,  
	  allowParallel = TRUE,
	  verboseIter = FALSE,
	  returnData = FALSE
	)

	
	model <- train(
		X_train, y_train,
		method = "xgbTree",
		trControl=xgb_trcontrol,
		tuneLength = 30
		)
	print(model)
	
	pred = predict(model, X_test)

	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = X_test, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}

wsrf_function <- function(train_dataset,test_dataset){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = "wsrf",
		family = "binomial",
		trControl=trctrl,
		tuneLength = 30
		)
	print(model)
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	test_dataset$classes <- factor(test_dataset$classes, levels=c(0,1))
	conf <- confusionMatrix(pred, test_dataset$classes)
	pred_prob <- predict(model, newdata = test_dataset, type = "prob")			
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf,pred_prob=pred_prob))
}
	

runAlg_function <- function(train_dataset,test_dataset,alg){	
	trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

	train_dataset$classes <- factor(train_dataset$classes)
	
	model <- train(
		as.factor(classes) ~.,
		data = train_dataset,
		method = alg,
		trControl=trctrl,
		tuneLength = 30,
		MaxNWts = 2000
		)
	print(model)	
	
	pred <- predict(model, newdata = test_dataset)
	test_dataset$classes <- as.factor(test_dataset$classes)
	conf <- confusionMatrix(pred, test_dataset$classes)
				
	return(list(Sample_Name=rownames(test_dataset),True_Label=test_dataset$classes,Predicted_Label=pred,conf=conf))
}

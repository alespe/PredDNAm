	
Metrics_multi <- function(file_test,label_co,label_ca){	

	labels_pred <- read.table(file_test,sep="\t",row.names=1,header=TRUE)
	
	TN <- length(which(labels_pred["True_Label"]==label_co & labels_pred["Predicted_Label"]==label_co))
	TP <- length(which(labels_pred["True_Label"]==label_ca & labels_pred["Predicted_Label"]==label_ca))
	FP <- length(which(labels_pred["True_Label"]==label_co & labels_pred["Predicted_Label"]==label_ca))
	FN <- length(which(labels_pred["True_Label"]==label_ca & labels_pred["Predicted_Label"]==label_co))

	accu <- (sum(TP,TN, na.rm=T))/sum(TN,TP,FP,FN)
	class.err <- (sum(FP,FN, na.rm=T))/sum(TN,TP,FP,FN)
	prec <- TP/(sum(TP,FP, na.rm=T))
	sens <- TP/(sum(TP,FN, na.rm=T))
	spec <- TN/(sum(TN,FP, na.rm=T))
	f1sc <- 2*prec*sens/(prec+sens)
	

	if(length(levels(as.factor(labels_pred[,2]))) < 2){
		auc <- NA
	}else{
		roc_obj <- roc(response=labels_pred[,2],predictor=labels_pred[,3])
		auc <- as.numeric(auc(roc_obj))		
	}

	results_metrics <- c(accu,class.err,prec,sens,spec,f1sc,auc)
	names(results_metrics) <- c("accuracy","class.err","precision","sensitivity","specificity","f1score","auc")

	return(list(results_metrics,labels_pred))
}

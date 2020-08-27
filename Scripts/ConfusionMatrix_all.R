
conf_matrix_plot_all <- function(labels_pred,metrics,N_iteration){	

  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(120, 157), c(250, 400), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)

  # create the matrix 
  rect(125, 320, 140, 380, col='#0072B2')
  rect(141, 320, 156, 380, col='#E69F00')
  text(132.5, 385, 'CO', cex=2, font=2)
  text(148.5, 385, 'CA', cex=2, font=2)
  text(121, 318, 'Predicted', cex=2.8, srt=90, font=2)
  text(140, 393, 'Actual', cex=2.8, font=2)
  rect(125, 255, 140, 315, col='#E69F00')
  rect(141, 255, 156, 315, col='#0072B2')
  text(124, 285, 'CA', cex=2, srt=90, font=2)
  text(124, 350, 'CO', cex=2, srt=90, font=2)

  # add in the conf results 
  TN <- length(which(labels_pred["True_Label"]==0 & labels_pred["Predicted_Label"]==0))
  TP <- length(which(labels_pred["True_Label"]==1 & labels_pred["Predicted_Label"]==1))
  FP <- length(which(labels_pred["True_Label"]==0 & labels_pred["Predicted_Label"]==1))
  FN <- length(which(labels_pred["True_Label"]==1 & labels_pred["Predicted_Label"]==0))

  text(132.5, 350, round(TN/N_iteration,2), cex=5, font=2, col='white')
  text(132.5, 285, round(FP/N_iteration,2), cex=5, font=2, col='black')
  text(148.5, 350, round(FN/N_iteration,2), cex=5, font=2, col='black')
  text(148.5, 285, round(TP/N_iteration,2), cex=5, font=2, col='white')

  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", cex.main=2, main = "PERFORMANCE", xaxt='n', yaxt='n')
  text(10, 88, "Sensitivity", cex=2, font=2)
  text(10, 71, paste0(round(mean(metrics$sensitivity), 2)," (",round(sd(metrics$sensitivity), 2),")"), cex=2.2)
  text(39, 88, "Specificity", cex=2, font=2)
  text(39, 71, paste0(round(mean(metrics$specificity), 2)," (",round(sd(metrics$specificity), 2),")"), cex=2.2)
  text(66, 88, "Precision", cex=2, font=2)
  text(66, 71, paste0(round(mean(metrics$precision), 2)," (",round(sd(metrics$precision), 2),")"), cex=2.2)
  text(90, 88, "F1 score", cex=2, font=2)
  text(90, 71, paste0(round(mean(metrics$f1score), 2)," (",round(sd(metrics$f1score), 2),")"), cex=2.2)

  # add in the accuracy information 
  text(30, 40, "Accuracy", cex=3.5, font=2)
  text(30, 18, paste0(round(mean(metrics$accuracy), 2)," (",round(sd(metrics$sensitivity), 2),")"), cex=3.5)
  text(70, 40, "AUC", cex=3.5)
  text(70, 18, paste0(round(mean(metrics$auc), 2)," (",round(sd(metrics$sensitivity), 2),")"), cex=3)

}
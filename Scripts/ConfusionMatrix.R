
conf_matrix_plot <- function(conf){	

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
  res <- as.numeric(conf$table)
  text(132.5, 350, res[1], cex=5, font=2, col='white')
  text(132.5, 285, res[2], cex=5, font=2, col='black')
  text(148.5, 350, res[3], cex=5, font=2, col='black')
  text(148.5, 285, res[4], cex=5, font=2, col='white')

  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", cex.main=2, main = "PERFORMANCE", xaxt='n', yaxt='n')
  text(10, 88, names(conf$byClass[1]), cex=2, font=2)
  text(10, 71, round(as.numeric(conf$byClass[1]), 3), cex=3)
  text(39, 88, names(conf$byClass[2]), cex=2, font=2)
  text(39, 71, round(as.numeric(conf$byClass[2]), 3), cex=3)
  text(66, 88, names(conf$byClass[5]), cex=2, font=2)
  text(66, 71, round(as.numeric(conf$byClass[5]), 3), cex=3)
  text(90, 88, names(conf$byClass[7]), cex=2, font=2)
  text(90, 71, round(as.numeric(conf$byClass[7]), 3), cex=3)

  # add in the accuracy information 
  text(30, 40, names(conf$overall[1]), cex=3.5, font=2)
  text(30, 18, round(as.numeric(conf$overall[1]), 3), cex=4)
  text(70, 40, names(conf$overall[2]), cex=3.5)
  text(70, 18, round(as.numeric(conf$overall[2]), 3), cex=3)

}
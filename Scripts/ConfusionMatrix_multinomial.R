
conf_matrix_plot_multi <- function(conf,MY_DIR,j,Transformation){	

  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(120, 156), c(240, 470), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)

  # create the matrix 
  rect(125, 380, 135, 445, col='#0072B2')
  rect(135.5, 380, 145.5, 445, col='#E69F00')
  rect(146, 380, 156, 445, col='#E69F00')
  text(130, 452, 'CO', cex=2, font=2)
  text(140.5, 452, 'CA-L', cex=2, font=2)
  text(151, 452, 'CA-H', cex=2, font=2)
  text(121, 345, 'Predicted', cex=2.5, srt=90, font=2)
  text(140.5, 467, 'Actual', cex=2.5, font=2)
  rect(125, 310, 135, 375, col='#E69F00')
  rect(135.5, 310, 145.5, 375, col='#0072B2')
  rect(146, 310, 156, 375, col='#0072B2')
  rect(125, 240, 135, 305, col='#E69F00')
  rect(135.5, 240, 145.5, 305, col='#0072B2')
  rect(146, 240, 156, 305, col='#0072B2') 
  text(124, 272.5, 'CA-H', cex=2, srt=90, font=2)
  text(124, 342.5, 'CA-L', cex=2, srt=90, font=2)
  text(124, 412.5, 'CO', cex=2, srt=90, font=2)

  # add in the conf results 
  res <- as.numeric(conf$table)
  text(130, 412.5, res[9], cex=5, font=2, col='white')
  text(130, 342.5, res[8], cex=5, font=2, col='black')
  text(130, 272.5, res[7], cex=5, font=2, col='black')
  text(140.5, 412.5, res[6], cex=5, font=2, col='black')
  text(140.5, 342.5, res[5], cex=5, font=2, col='white')
  text(140.5, 272.5, res[4], cex=5, font=2, col='white')
  text(151, 412.5, res[3], cex=5, font=2, col='black')
  text(151, 342.5, res[2], cex=5, font=2, col='white')
  text(151, 272.5, res[1], cex=5, font=2, col='white')

  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", cex.main=2, main = "PERFORMANCE", xaxt='n', yaxt='n')
	
  text(16, 68, colnames(conf$byClass)[1], cex=2, font=2)
  text(16, 43, colnames(conf$byClass)[2], cex=2, font=2)
  text(16, 18, colnames(conf$byClass)[11], cex=2, font=2)
  text(46.5, 88, "CO", cex=2, font=2)
  text(66, 88, "CA-L", cex=2, font=2)
  text(86, 88, "CA-H", cex=2, font=2)

  text(46, 68, round(as.numeric(conf$byClass[3]), 3), cex=2.8)
  text(66, 68, round(as.numeric(conf$byClass[2]), 3), cex=2.8)
  text(86, 68, round(as.numeric(conf$byClass[1]), 3), cex=2.8)
  text(46, 43, round(as.numeric(conf$byClass[6]), 3), cex=2.8)
  text(66, 43, round(as.numeric(conf$byClass[5]), 3), cex=2.8)
  text(86, 43, round(as.numeric(conf$byClass[4]), 3), cex=2.8)
  text(46, 18, round(as.numeric(conf$byClass[33]), 3), cex=2.8)
  text(66, 18, round(as.numeric(conf$byClass[32]), 3), cex=2.8)
  text(86, 18, round(as.numeric(conf$byClass[31]), 3), cex=2.8)

}
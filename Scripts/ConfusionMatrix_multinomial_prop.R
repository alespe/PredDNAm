
conf_matrix_plot_multi_prop <- function(conf,MY_DIR,j,Transformation){	

	# extract the confusion matrix values as data.frame
	cm_d <- as.data.frame(conf$table)
	# confusion matrix statistics as data.frame
	cm_st <-data.frame(conf$overall)
	# round the values
	cm_st$conf.overall <- round(cm_st$conf.overall,2)

	# here we also have the rounded percentage values
	cm_p <- as.data.frame(prop.table(conf$table))
	cm_d$Perc <- round(cm_p$Freq*100,2)

	# plotting the matrix
	cm_d_p <-  ggplot(data = cm_d, aes(x = Prediction , y =  Reference, fill = Freq))+
	  geom_tile() +
	  geom_text(aes(label = paste("",Freq,",",Perc,"%")), color = 'red', size = 8) +
	  theme_light() +
	  guides(fill=FALSE) 
	# plotting the stats
	cm_st_p <-  tableGrob(cm_st)

	# all together
	grid.arrange(cm_d_p, cm_st_p,nrow = 1, ncol = 2, 
				 top=textGrob("Confusion Matrix and Statistics",gp=gpar(fontsize=25,font=1)))
	ggsave(file.path(MY_DIR,j,Transformation,"Confusion_matrix_prop.png"))
}
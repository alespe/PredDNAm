	

Transformation_function <- function(train_dataset,test_dataset,Transformation){	
	
	if(Transformation == "PCA"){
		sds_c=apply(train_dataset,2,sd,na.rm=T)
		mns_c=apply(train_dataset,2,mean,na.rm=T)
		x = (train_dataset-as.numeric(mns_c))/as.numeric(sds_c)
		y = (test_dataset-as.numeric(mns_c))/as.numeric(sds_c)	
		
		pca_x <- prcomp(x, scale = FALSE)
		pca_y <- predict(pca_x, newdata = y)
		x <- pca_x$x
		x <- as.matrix(x[,1:50])
		y <- pca_y
		y <- as.matrix(y[,1:50])
		
	}else if(Transformation == "stand"){
		sds_c=apply(train_dataset,2,sd,na.rm=T)
		mns_c=apply(train_dataset,2,mean,na.rm=T)
		x = (train_dataset-as.numeric(mns_c))/as.numeric(sds_c)
		y = (test_dataset-as.numeric(mns_c))/as.numeric(sds_c)	
		
		x <- as.matrix(x)
		y <- as.matrix(y)
		
	}else if(Transformation == "Difussion"){
		sds_c=apply(train_dataset,2,sd,na.rm=T)
		mns_c=apply(train_dataset,2,mean,na.rm=T)
		x = (train_dataset-as.numeric(mns_c))/as.numeric(sds_c)
		y = (test_dataset-as.numeric(mns_c))/as.numeric(sds_c)	

		D <- as.matrix(dist(rbind(x,y)))
		Dx <- D[1:nrow(x), 1:nrow(x)]
		Dy <- D[(nrow(x)+1):(nrow(x)+nrow(y)), 1:nrow(x)]

		x_D <- diffuse(Dx, maxdim = 50)
		y_D <- nystrom(x_D, Dy)
		
		x <- as.matrix(x_D$X)
		rownames(x) <- rownames(train_dataset)
		colnames(x) <- paste0(rep("DM",ncol(x)),seq(1:ncol(x)))
		y <- as.matrix(y_D)
		rownames(y) <- rownames(test_dataset)
		colnames(y) <- paste0(rep("DM",ncol(x)),seq(1:ncol(x)))
		
	}else if(Transformation == "UMAP"){
		sds_c=apply(train_dataset,2,sd,na.rm=T)
		mns_c=apply(train_dataset,2,mean,na.rm=T)
		x = (train_dataset-as.numeric(mns_c))/as.numeric(sds_c)
		y = (test_dataset-as.numeric(mns_c))/as.numeric(sds_c)	
		
		x <- umap(x, n_components = 50, ret_model=TRUE)		
		y <- predict(x,y)
		
		x <- as.matrix(x$layout)
		colnames(x) <- paste0(rep("UMAP",ncol(x)),seq(1:ncol(x)))
		colnames(y) <- paste0(rep("UMAP",ncol(x)),seq(1:ncol(x)))
				
	}else if(Transformation == "CV"){
		sds_c=apply(train_dataset,2,sd,na.rm=T)
		mns_c=apply(train_dataset,2,mean,na.rm=T)
		x = (train_dataset-as.numeric(mns_c))/as.numeric(sds_c)
		y = (test_dataset-as.numeric(mns_c))/as.numeric(sds_c)	

		cv = as.numeric(sds_c)/as.numeric(mns_c)
		x <- train_dataset[,cv > 0.1]
		if(ncol(x) == 0){
			x <- train_dataset[,cv > 0.05]
		}
		if(ncol(x) == 0){
			cat(paste("ERROR: no features with coefficient of variation in train dataset > 0.05 \n"))
			quit()
		}
		y <- y[,colnames(y) %in% colnames(x)]
		x <- as.matrix(x)
		y <- as.matrix(y)
		cat(paste(ncol(x),"Features selected \n"))
		
	}else if(Transformation == "none"){
		x <- as.matrix(train_dataset)
		y <- as.matrix(test_dataset)
	}
	
	return(list(x,y))
}


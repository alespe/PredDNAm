
NMF_function <- function(x){
	
	input_NMF <- t(x)
	input_NMF = posneg(as.matrix(input_NMF))
	nmf_out <- nmf(input_NMF, 2:10, nrun=1, seed=1, method = "brunet", .opt='vP1')
	nmf_out_sum <- summary(nmf_out)
	rownames(nmf_out_sum) <- seq(2,10,1)
	N_clusters <- as.numeric(rownames(nmf_out_sum)[which.min(nmf_out_sum$rss)])
	
	Ncluster_function <- function(Ncluster){
	
		nmf_out2 <- nmf(input_NMF, Ncluster, nrun = 1, seed=1, method = "brunet", .opt='vP1')
		s <- extractFeatures(nmf_out2)
		
		for(i in 1:Ncluster) {
			s[[i]] <- rownames(input_NMF)[s[[i]]]
			}
		w <- basis(nmf_out2)
		a <- apply(w,1,function(x)which.max(x))

		groups <- vector("list", length(s))
		for(i in 1:Ncluster) {
			groups[[i]] <- names(a[which(a == i)])
		}
		return(groups)
	}
	
	groups <- Ncluster_function(N_clusters)
	
	while(any(lengths(groups) < 2)){
		N_clusters <- N_clusters-1
		groups <- Ncluster_function(N_clusters)	
	}
	
	return(groups)
}
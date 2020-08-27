

library(ggplot2)
library(reshape)

args = commandArgs(trailingOnly=TRUE)
ref_type = as.character(args[1])
dir_scripts = as.character(args[2])
signature = as.character(args[3])
annot_file = as.character(args[4])
dir_output = as.character(args[5]) 

load(file.path(dir_scripts,"annotation.450k.ER111Study.RData"))
features <- read.table(signature,sep="\t",header=TRUE)

############################# Chromatin states: Reference vs  signature

#CpGs from each chromatin state across reference genome
annot_ref <- ann450k.ER111[ref_type]
#write.table(annot_E062,file.path(dir_main,"CS_annot.txt"),sep="\t")

#CpGs from each chromatin state from the signature
annot_sig <- annot_ref[rownames(annot_ref) %in% features[,1],]

#Number of CpGs from each chromatin state in reference genome if the size of CpGs is the same as the signature
Reference <- table(annot_ref)/dim(annot_ref)[1]*dim(features)[1]

#Number of CpGs from each chromatin state in the signature
Signature <- table(annot_sig)

#Plot number of CpGs from both reference and NHL signature when total = N CpGs signature
annot_m <- melt(cbind(Reference,Signature))
annot_E062_plot2 <- data.frame(CS=annot_m[,1],value=annot_m[,3],Source=annot_m[,2])
#cbbPalette <- c("#E69F00", "#0072B2", "#009E73", "#F0E442", "#56B4E9", "#D55E00", "#CC79A7") #color-blindly friendly
cbbPalette <- c("#E69F00", "#0072B2")
ggplot(data=annot_E062_plot2, aes(x=CS,y=value,fill=Source)) +
  geom_bar(stat="identity", position=position_dodge()) +
	scale_fill_manual(values=cbbPalette) +
	theme_bw() +
    theme(plot.title = element_text(size=10),text = element_text(size=15),
        axis.text.x = element_text(angle=45, hjust=1)) +
			xlab("Chromatin States") + ggtitle("Chromatin States: 111 epigenomes study vs Prospective cohorts")
ggsave(file.path(dir_output,"SignaturevsReference_CS.png"))



#Number and percentage of CpGs for each chromatin state from both reference and signature
a <- table(annot_ref)/dim(annot_ref)[1]*100
b <- table(annot_sig)/length(annot_sig)*100
y <- do.call(merge,c(list(a,b), by="row.names",all=TRUE))
y <- y[,-c(1,2,4)]
colnames(y) <- c("Perc.Ref","Perc.Sig")
ref_sig <- data.frame(NRef=table(annot_ref),NSig=table(annot_sig),y)
rownames(ref_sig) <- ref_sig[,1]
ref_sig <- ref_sig[,-c(1,3)]
write.table(ref_sig,file.path(dir_output,"SignaturevsReference_CS.txt"),sep="\t",row.names=TRUE,col.names=NA)


#Significance levels (select random CpGs (length=N CpGs signature) from all genome and check the distribution of chromatin states, 1000 times)
#a) generate 1000 lists of randomly selected CpGs  and store their chromatin state
annot_resh_list <- list()
for(i in 1:1000){
	annot_resh <- as.character(annot_ref[rownames(annot_ref) %in% sample(rownames(annot_ref),nrow(features)),])
	annot_resh_list <- append(annot_resh_list,list(annot_resh))
}
for(i in 1:1000){if(length(names(table(annot_resh_list[[i]]))) == 15){index=i;break}}
#b) Number of times that each chromatin state in each of the 1000 lists 
CR_all <- list()
for(j in names(table(annot_resh_list[[index]]))){ #1 because has all names
	for(i in 1:1000){
		CR_all[[j]] <- c(CR_all[[j]],table(annot_resh_list[[i]])[names(table(annot_resh_list[[i]]))==j])
	}	
}
#c) Asigning 0 to those that were not represent in each of the 1000 lists so each chromatin state have 1000 elements (even if some are zero)
for(j in names(table(annot_resh_list[[index]]))){
	CR_all[[j]] <- c(CR_all[[j]], rep(0, 1000 - length(CR_all[[j]])))
}	
#d) Plotting distribution of chromatin states across 1000 lists  
# data_plot_m <- melt(CR_all)
# colnames(data_plot_m) <- c("N","CS")
# ggplot(data_plot_m, aes(x=CS, y=N)) + 
  # geom_boxplot() +
	# theme_bw() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave(file.path(dir_output,"Distribution.png"))
# e) Generate normal distributions based on the mean and sd of the frecuencies of N of CpGs from each chromatin states across 1000 lists and pvalues from the distribution of CpGs from the signature
mean_all <- list()
sd_all <- list()
pval <- list()
for(j in names(table(annot_resh_list[[index]]))){
	mean_all[[j]] <- mean(CR_all[[j]])
	sd_all[[j]] <- sd(CR_all[[j]])	
	pval[[j]] <- dnorm(table(annot_sig)[which(names(table(annot_sig)) == j)],mean=mean_all[[j]],sd=sd_all[[j]])
}
names(pval) <- gsub(".*//.","",names(pval))
fdr <- p.adjust(as.numeric(pval),method = "BH")
table_results <- data.frame(CS=names(table(annot_sig)),pval=unlist(pval),FDR=fdr)
write.table(table_results,file.path(dir_output,"Significance_CS.txt"),sep="\t",row.names=FALSE,col.names=TRUE)



################################# Location probes: Reference vs Signature
annot <- readRDS(annot_file)
annot2 <- annot[annot@rownames %in% features[,1],]
Relation_to_Island <- data.frame(cpgs=annot2@rownames,Relation_to_Island=annot2@listData$Relation_to_Island)

#CpGs from each chromatin state across reference genome
All_annot <- annot$Relation_to_Island

#Number of CpGs from each chromatin state in reference genome if the size of CpGs is the same as our NHL signature
Reference <- table(All_annot)/length(All_annot)*dim(Relation_to_Island)[1]

#Number of CpGs from each chromatin state in NHL signature
Signature <- table(Relation_to_Island[,2])

#Plot number of CpGs from both reference and NHL signature when total = N CpGs signature
annot_m <- melt(cbind(Reference,Signature))
annot_plot2 <- data.frame(CS=annot_m[,1],value=annot_m[,3],Source=annot_m[,2])
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #color-blindly friendly
cbbPalette <- c("#0072B2","#E69F00")
ggplot(data=annot_plot2, aes(x=CS,y=value,fill=Source)) +
  geom_bar(stat="identity", position=position_dodge()) +
	scale_fill_manual(values=cbbPalette) +
	theme_bw() +
    theme(plot.title = element_text(size=10),text = element_text(size=15),
        axis.text.x = element_text(angle=45, hjust=1)) +
			xlab("Genome Location") + ggtitle("Genome Location: 111 epigenomes study vs Prospective cohorts")
ggsave(file.path(dir_output,"Location_genome.png"))

#Number and percentage of CpGs for each chromatin state from both reference and signature
a <- table(All_annot)/length(All_annot)*100
b <- table(Relation_to_Island[,2])/length(Relation_to_Island[,2])*100
y <- do.call(merge,c(list(a,b), by="row.names",all=TRUE))
y <- y[,-c(1,2,4)]
colnames(y) <- c("Perc.Ref","Perc.Sig")
ref_sig <- data.frame(NRef=table(All_annot),NSig=table(Relation_to_Island[,2]),y)
rownames(ref_sig) <- ref_sig[,1]
ref_sig <- ref_sig[,-c(1,3)]
write.table(ref_sig,file.path(dir_output,"SignaturevsReference_Location.txt"),sep="\t",row.names=TRUE,col.names=NA)


#Significance levels
Relation_to_Island_list <- list()
for(i in 1:1000){
	annot_resh <- annot[annot@rownames %in% sample(annot@rownames,nrow(features)),]
	Relation_to_Island_list <- append(Relation_to_Island_list,list(annot_resh@listData$Relation_to_Island))
}

Relation_to_Island_all <- list()
for(j in names(table(annot_resh@listData$Relation_to_Island))){ 
	for(i in 1:1000){
		Relation_to_Island_all[[j]] <- c(Relation_to_Island_all[[j]],table(Relation_to_Island_list[[i]])[names(table(Relation_to_Island_list[[i]]))==j])
	}	
}

for(j in names(table(annot_resh@listData$Relation_to_Island))){
	Relation_to_Island_all[[j]] <- c(Relation_to_Island_all[[j]], rep(0, 1000 - length(Relation_to_Island_all[[j]])))
}

# data_plot_m <- melt(Relation_to_Island_all)
# colnames(data_plot_m) <- c("N","CS")
# ggplot(data_plot_m, aes(x=CS, y=N)) + 
  # geom_boxplot() +
	# theme_bw() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave(file.path(dir_output,"Distribution_location.png"))


mean_all <- list()
sd_all <- list()
pval <- list()
for(j in names(table(annot_resh@listData$Relation_to_Island))){ 
	mean_all[[j]] <- mean(Relation_to_Island_all[[j]])
	sd_all[[j]] <- sd(Relation_to_Island_all[[j]])	
	pval[[j]] <- dnorm(table(Relation_to_Island[,2])[which(names(table(Relation_to_Island[,2])) == j)],mean=mean_all[[j]],sd=sd_all[[j]]) #2.3041e-20
}
names(pval) <- gsub(".*//.","",names(pval))
fdr <- p.adjust(as.numeric(pval),method = "BH")
table_results <- data.frame(CS=names(table(annot@listData$Relation_to_Island)),pval=unlist(pval),FDR=fdr)
write.table(table_results,file.path(dir_output,"Significance_Location.txt"),sep="\t",row.names=FALSE,col.names=TRUE)


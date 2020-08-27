
clinical <- read.table("/oak/stanford/groups/andrewg/users/aespin/PredDNAm/Raw_data/demographic 5_5_15.txt",header=TRUE,row.names=1,sep="\t")
MET <- readRDS(file.path("/oak/stanford/groups/andrewg/users/aespin/PredDNAm/Github/1_Preprocessing/NHL_Quantile","Preprocessed_ComBat_Beta.rds"))
MET <- MET[,colnames(MET) %in% rownames(clinical)]
clinical2 <- clinical[rownames(clinical) %in% colnames(MET),]
clinical2 <- clinical2[order(match(rownames(clinical2),colnames(MET))),]
identical(colnames(MET),rownames(clinical2))

type <- as.factor(clinical2$type)
type <- as.factor(ifelse(type==1,0,1))
metadata <- data.frame(ID=rownames(clinical2),Label=type)
write.table(metadata,"/oak/stanford/groups/andrewg/users/aespin/PredDNAm/Github/2_Selection/metadata.txt",sep="\t",row.names=FALSE,col.names=TRUE)


outputDir_updated <- "/oak/stanford/groups/andrewg/users/aespin/PredDNAm/Github/1_Preprocessing/NHL_Quantile" 
Additional_info <- read.table(file.path(outputDir_updated,"Additional_info.txt"),sep="\t",header=TRUE)
SNPs <- read.table(file.path(outputDir_updated,"SNPs.txt"),sep="\t",header=TRUE)

chr <- data.frame(CpGs=Additional_info$CpGs,Chromosomes=Additional_info$Chromosomes)
write.table(chr,"/oak/stanford/groups/andrewg/users/aespin/PredDNAm/Github/2_Selection/chr.txt",sep="\t",row.names=FALSE,col.names=TRUE)

SNPs <- data.frame(IDs=Additional_info$CpGs,SNPs)
write.table(SNPs,"/oak/stanford/groups/andrewg/users/aespin/PredDNAm/Github/2_Selection/SNPs.txt",sep="\t",row.names=FALSE,col.names=TRUE)

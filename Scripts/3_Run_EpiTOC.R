
library(ggplot2)
library(reshape)
library(ggpubr)
library(EpiDISH)
library(caret)
library(dummies)


args = commandArgs(trailingOnly=TRUE)
dir_scripts <- as.character(args[1]) 
MET_file <- as.character(args[2]) # normalized DNA methylation file with all CpGs
dir_output <- as.character(args[3])

# Loading data
if(grepl(".rds",MET_file) == TRUE){
	MET <- readRDS(MET_file)
}
if(grepl(".txt",MET_file) == TRUE){
	MET <- read.table(MET_file,row.names=1,sep="\t",header=TRUE)
}


# Calculating EpiTOC 
Prosp_epitoc <- EstEpiTOC(MET,dir_scripts)
write.table(Prosp_epitoc,file.path(dir_output,"EpiTOC_estimates.txt"),sep="\t",row.names=TRUE,col.names=FALSE)

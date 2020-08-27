
############################ METHYLATION PREPROCESSING

require(minfi)

args = commandArgs(trailingOnly=TRUE)
outcome <- as.character(args[1]) #Code for the project
norm <- as.character(args[2]) # Normalization method: Quantile,Illumina,Noob, Funnorm or SWAN
baseDir <- as.character(args[3]) # Directory with raw data
outputDir <- as.character(args[4]) # Directory for output files
matched_ids <- as.character(args[5]) #txt file (with full path) with samples names (column name=ID) and idat file code (column name=Basename) (e.g. "5935294023_R01C02")


dir.create(file.path(outputDir,paste(outcome,norm,sep="_")), showWarnings = FALSE)
outputDir_updated <- file.path(outputDir,paste(outcome,norm,sep="_")) 

# Preparing target file
file_targets <- read.table(matched_ids,sep="\t",header=TRUE)
Basename <- list.files(baseDir,full.names=TRUE)
Basename <- gsub("_Grn.*","",Basename)
Basename <- gsub("_Red.*","",Basename)
Basename <- unique(Basename)
Basename <- Basename[order(match(basename(Basename),file_targets$Basename))]
Basename <- Basename[basename(Basename) %in% file_targets$Basename]
identical(basename(Basename),as.character(file_targets$Basename))
Slide <- gsub("_.*","",basename(Basename))
Array <- gsub(".*_","",basename(Basename))
ids <- file_targets$ID
targets <- data.frame(Basename=Basename,Slide=Slide,Array=Array,ids=ids)
write.table(targets,file.path(baseDir,"target.csv"),sep=",",row.names=FALSE,col.names=TRUE)

# Importing files
targets <- read.metharray.sheet(baseDir)
RGset <- read.metharray.exp(targets = targets)
pd <- pData(RGset)

# Save a copy in R
#saveRDS(RGset,file=file.path(outputDir_updated,"RGset.RData"))

# Normalize 
if(norm == "Quantile"){MSet.norm <- preprocessQuantile(RGset)}
if(norm == "Illumina"){MSet.norm <- preprocessIllumina(RGset, bg.correct = TRUE)}
if(norm == "Noob"){MSet.norm <- preprocessNoob(RGset)}
if(norm == "Funnorm"){MSet.norm <- preprocessFunnorm(RGset)}
if(norm == "SWAN"){MSet.norm <- preprocessSWAN(RGset)}
MSet.norm = addSnpInfo(mapToGenome(MSet.norm))
#saveRDS(MSet.norm, file=file.path(outputDir_updated,"MSet.norm.RData"))

# Get probe annotations
annot = getAnnotation(MSet.norm)
saveRDS(annot, file=file.path(outputDir_updated,"annot.RData"))
chr <- annot$chr
CpGs <- annot@rownames
Gene <- annot$UCSC_RefGene_Name
Position <- annot$pos
Chromosomes <- annot$chr
IslandStatus <- getIslandStatus(MSet.norm)
gran <- granges(MSet.norm)
Additional_info <- data.frame(CpGs=CpGs,Gene=Gene,Position=Position,Chromosomes=Chromosomes,IslandStatus=IslandStatus,gran)
write.table(Additional_info,file.path(outputDir_updated,"Additional_info.txt"),sep="\t",row.names=TRUE,col.names=NA)

SNPs_info <- getSnpInfo(MSet.norm)
SNPs <- data.frame(SNPS=SNPs_info@listData)
write.table(SNPs,file.path(outputDir_updated,"SNPs.txt"),sep="\t",row.names=TRUE,col.names=NA)

# Get beta and M values
values_norm_Beta = getBeta(MSet.norm)
phenoData_B <- pd[rownames(pd) %in% colnames(values_norm_Beta),]
identical(basename(targets$Basename),colnames(values_norm_Beta))
identical(basename(targets$ids),phenoData_B$ids)
colnames(values_norm_Beta) <- phenoData_B$ids
write.table(values_norm_Beta,file.path(outputDir_updated,"Preprocessed_Beta.txt"),sep="\t",row.names=TRUE,col.names=NA)
saveRDS(values_norm_Beta, file = file.path(outputDir_updated,"Preprocessed_Beta.rds"))

values_norm_M = getM(MSet.norm)
phenoData_M <- pd[rownames(pd) %in% colnames(values_norm_M),]
colnames(values_norm_M) <- phenoData_M$ids
write.table(values_norm_M,file.path(outputDir_updated,"Preprocessed_M.txt"),sep="\t",row.names=TRUE,col.names=NA)
saveRDS(values_norm_M, file = file.path(outputDir_updated,"Preprocessed_M.rds"))




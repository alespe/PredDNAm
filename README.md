# Cancer Prediction based on DNA methylation (PredDNAm)

PredDNAm is a command line computational framework that includes 450k DNA methylation preprocessing (1), batch correction (2) (e.g. date of chip analysis), filtering for SNPs and sex chromosomes, feature selection, dimensionality reduction and machine learning-/deep learning-based prediction using DNA methylation data. The pipeline was developed to study predictive lymphoma DNA methylation signals from a prospective lymphoma cohort (REF) and publicly available datasets (GSE4027919, GSE3736239, GSE4237240, GSE10938141, The Cancer Genome Atlas DLBC and GSE4903142) as described in the manuscript (REF). The pipeline also includes DNA methylation deconvolution using CIBERSORTx (3) to infer fractions and DNA methylation profiles from immune cells, estimates of mitotic clock (4) to classify samples into subtypes (e.g. high/low mitotic index) and enrichment of chromatin states and location of the features from the signature by comparing the selected features with a reference epigenome atlas (5). 

What PredDNAm can do:
-	Preprocessing of DNA methylation 450K data (“minfi” R package) and optional batch correction (“ComBat” from the “sva” R package)
-	Feature selection features using the training set based on univariately and multivariately significant, including linear and non-linear relationship with the outcome of interest. Optional: dimensionality reduction techniques (PCA, UMAP, Diffusion maps). Optional standardization (mean of 0 and a standard deviation of 1) and filter based on the coefficient of variation (ratio of the standard deviation to the mean).
-	Prediction models and performance metrics on the test dataset (“caret” R package)
-	Additional steps:
	-	DNA methylation deconvolution using on CIBERSORTx (https://cibersortx.stanford.edu/)
	-	Mitotic clock (EpiTOC) estimation and multivariate prediction based on control, low-index and high-index categories
	-	Enrichment of chromatin states and CpGs location of the signature compared to the reference epigenome atlas from the Roadmap Epigenomics Project 
	-	Validation in independent datasets

## Getting Started

The instructions below define the workflow that lead to the identification of predictive CpG sites of future lymphoma in a prospective cohort. The input data for the workflow is Infinium HumanMethylation450 BeadChip or any other preprocessed input matrix (to skip preprocessing step). The output will be a confusion matrix and metrics of the performance assessment of the classifiers: accuracy (percent of correct classifications obtained), sensitivity (ability to detect future disease in the population of future diseased individuals), specificity (ability to correctly rule out the future disease in a disease-free population), F1 Score = 2 * Precision * Recall / (Precision + Recall) and AUC value of the resulting ROC curves.

## Prerequisites

R 

## Installing

```
lib_install <- c("mgcv", "caret","pcLasso","glmnet","ggplot2","pROC","umap","diffusionMap","ggpubr")
install.lib <- lib_install[!lib_install %in% installed.packages()]
for(lib in install.lib){install.packages(lib,dependencies=TRUE)}
sapply(lib_install,require,character=TRUE)
BiocManager::install()
bioc_install <- c("minfi","sva")
install.libioc <- bioc_install[!bioc_install %in% installed.packages()]
for(libioc in install.libioc){BiocManager::install(libioc)}
```

## Pipeline

![alt text](https://https://github.com/alespe/PredDNAm/blob/master/images/pipeline.GIF)

### Running PredDNAm:

**Preprocessing**
Input arguments:
-	Name of the project
-	Normalization method: Quantile, Illumina, Noob, Funnorm or SWAN
-	Directory with raw data
-	Directory for output files
-	Metadata txt file (with full path) that matches the raw files with sample names. Minimum information: samples names (column name=ID) and IDAT file code (column name=Basename) (e.g. "5935294023_R01C02")

Example:
```
Rscript 1_Preprocessing.R NHL Quantile Raw_data 1_Preprocessing Matched_ids.txt
```

Output: 
-	Normalized DNA methylation (M and beta values, TXT and RDS extension. E.g. "Preprocessed_Beta.txt")
-	Additional info: Gene name, chromosome, position, island status and range of each probe ("Additional_info.txt")
-	SNPs: SNP information of each probe ("SNPs.txt")
-	Full annotation for each probe as R object (“annot.RData”)

Optional step: Filtering and/or adjustment for known batches using an empirical Bayesian framework. 
Input arguments:
-	Filtering and imputation: Boolean value TRUE if filtering and imputation requested (e.g. Illumina normalization method generates missing values).
-	Batch correction: Boolean value TRUE if batch correction requested
-	Input file: output from previous step (accepts .txt or .rds file extensions)
-	Metadata file with batch information. Minimum information: first column IDs and second column batch id.
-	Directory for output files

Example:
```
Rscript 1_Preprocessing_2.R FALSE TRUE Preprocessed_M.rds Batch.txt 1_Preprocessing/NHL_Quantile
```

Output:
The function will check if the metadata and normalized DNA methylation matrix have the same order, perform filtering and imputation if the argument is set to TRUE (rows with more than 80% of missing values will be filtered out and the remaining missing values imputed) and correct for batch effect if the batch correction argument is set to TRUE. It will return beta and M values matrices.
 
**Univariate feature selection (linear and GAM models) + Multivariate feature selection (Lasso or pcLasso)**

Input arguments:
-	Name of the run
-	Method for multivariate feature selection: “ElasticNet” or “pcLasso”
-	Regularization: Alpha parameter for ElasticNet (numeric value between 0 and 1, being 1=Lasso, 0=Ridge) and ratio for pcLasso (numeric value between 0 and 1, being 1=Lasso)
-	Split train-test: e.g. 0.7 would split the data into 70% training set and 30% test set
-	Scaling: Boolean value TRUE or FALSE
-	Filtering for removing sex chromosomes and loci with SNPs based on their MAF (10%): Boolean value TRUE or FALSE
-	DNA methylation matrix: output from previous step (accepts .txt or .rds file extensions)
-	Outcomes: text file with labels (minimum information: first column IDs and second column Label). If the order does not match the DNA methylation file, the function will match the order and select the intersect of both
-	Metadata chromosome file from preprocessing: Additional_info$CpGs first column and Additional_info$Chromosomes second column
-	Metadata SNPs file from preprocessing: CpGs first column and SNPs second column
-	Directory for output files


Example:
```
Rscript  2_Selection.R NHL ElasticNet 0.1 0.7 FALSE TRUE 1_Preprocessing/NHL_Quantile/Preprocessed_ComBat_Beta.rds metadata.txt chr.txt SNPs.txt 2_Selection
```

Output:
-	List of sample ids used for the feature selection steps (“SamplesTraining .txt”). This list of samples correspond to the training set and it will be needed for the prediction step
-	Results from linear models where DNA methylation is modelled as outcome and the labels as explanatory variable, including p-values adjusted for multiple testing correction using Benjamini-Hochberg and Bonferroni methods (“Results_univar.txt”)
-	Results from generalized additive models, REML method and sigmoid activation function including p-values adjusted for multiple testing correction using Benjamini-Hochberg and Bonferroni methods (“Results_univar _GAM.txt”)
-	Selected features from multivariate analyses to be used as input in the predictive models (e.g. “Features.txt” corresponding to Benjamini-Hochberg adjusted p-value < 0.05 from the linear models)
-	Metadata file with labels that matches the features file for the prediction step (“Covariates_input.txt”)
-	Predictive values for both training and test datasets for each group (e.g. “Pred_ElasticNet.txt”)
-	Comparison of predictive values across two groups (“e.g. “Pred_ElasticNet.png.png”)

**Predictions for binary and multiclass classification. Optional data transformation: PCA, Diffusion Maps, UMAP, scaling. Also additional filter based on the coefficient of variation**

Input arguments for binary classification:
-	Algorithm, choose one of the list: adaboost, avNNet, bayesglm, BstLm, C5.0, cforest, glmnet, fda, gamboost, gbm, glm, glmboost, knn, lda, LogitBoost, lssvmRadial, naive_bayes, nnet, parRF, pcaNNet, pcLasso, pls, regLogistic, rFerns, svmLinear, svmLinearWeights2, xgboost, wsrf.
-	Transformation, choose one of the list: PCA, stand, Difussion, CV, UMAP or None
-	Selected features: matrix from previous step if feature selection performed or user input matrix
-	Labels: text file from previous step if feature selection performed or user input label file
-	Train samples labels: text file from previous step listing train dataset sample ids
-	Directory where scripts are located 
-	Directory for output files
-	Indicate mode: “PREDEFINED” if previous steps were followed or “ALL” if unsupervised feature selection or no feature selection was conducted to generate the input matrix
-	If PREDEFINED mode: file listing labels used for training, otherwise “FALSE”
-	If ALL mode: number of iterations
-	If ALL mode: Split for train/test dataset (e.g. 0.7 would split the data into 70% training set and 30% test set)

```
Rscript 3_Predictions.R bayesglm none Features.txt Covariates_input.txt Scripts 3_Predictions PREDEFINED 2_Selection/Beta/ElasticNet/0.1 /SamplesTraining.txt
Rscript 3_Predictions.R bayesglm none AlternProc_Features.txt AlternProc_Covariates_input.txt Scripts AlternProc ALL FALSE 100 0.7
```

Output:
-	Labels with binary and numeric prediction for each sample (“All_predic_labels.txt”)
-	Confusion matrix and performance values (sensitivity, specificity, precision, F1, Accuracy, kappa) for the test dataset (“confusionMatrix.png”).
-	Performance values sensitivity, specificity, precision, F1, Accuracy and kappa (“Metrics_Results.txt”)

Labels can be further stratified into e.g. high and low EpiTOC estimations:
Input arguments for EpiTOC estimation:
-	Directory where scripts are located 
-	Normalized DNA methylation file with all CpGs (accepts .txt or .rds file extensions)
-	Directory for output files

```
Rscript 3_Run_EpiTOC.R Scripts 1_Preprocessing/NHL_Quantile/Preprocessed_ComBat_Beta.rds 3_Predictions/EpiTOC
```

Output:
-	Mitotic clock estimates for each sample (“EpiTOC_estimates.txt”)

Input arguments for multiclass classification:
-	Algorithm, choose one of the caret list for multiclass classification (e.g. multinom)
-	Transformation, choose one of the list: PCA, stand, Difussion, CV, UMAP or None
-	Selected features: matrix from previous step if feature selection performed or user input matrix
-	Labels: text file from previous step if feature selection performed or user input label file
-	Train samples labels: text file from previous step listing the samples from the train dataset 
-	Directory where scripts are located 
-	Directory for output files
-	Indicate mode: “PREDEFINED” if previous steps were followed or “ALL” if unsupervised feature selection or no feature selection was conducted to generate the input matrix
-	If “PREDEFINED” mode: file listing labels used for training, otherwise “FALSE”
-	If “ALL” mode: number of iterations
-	If “ALL” mode: Split for train/test dataset (e.g. 0.7 would split the data into 70% training set and 30% test set)

```
Rscript 3_Multinomial.R multinom none Features_multiclass.txt Covariates_input_multiclass.txt Scripts 3_Predictions PREDEFINED 2_Selection/Beta/ElasticNet/0.1 /SamplesTraining.txt
```

Output:
-	True and predicted labels for each sample (“Labels_Pred_True_test.txt”)
-	Confusion matrix and performance values (sensitivity, specificity and balanced accuracy from each group) for the test dataset (“confusionMatrix.png”)
-	Performance values including sensitivity, specificity, positive predicted value, negative predictive value, precision, recall, F1, prevalence, detection rate, detection prevalence and balanced accuracy, for each group (“Metrics_Results_byClass.txt”)

Immune cell fraction and cell-specific DNA methylation profiles for each of a set of immune fractions can be further inferred using CIBERSORTx (https://cibersortx.stanford.edu/). The signature matrix “Immune_Signature.txt” consists of CpGs that can discriminate each immune cell subset (macrophages, B-cells, helper T cells, T regulatory cells, cytotoxic T cells, natural killers, eosinophils, neutrophils) in DNA methylation 450k data, and can subsequently be used to impute cell fractions and DNA methylation profiles. 

Input: 
-	“Immune_Signature.txt”
-	DNA methylation dataset with same probes as the ones listed in the “Immune_Signature.txt”
Follow Tutorial 2 to infer immune cells fractions, Tutorial 4 to infer DNA methylation profiles for each immune cells to further compare between groups (recommended if the number of samples is < 30) and Tutorial 5 to infer DNA methylation profiles for each immune cell for each sample (recommended if the number of samples is >= 30).

Output:
-	Immune cell fraction for each sample
-	Single representative DNA methylation profile for each cell type
-	Sample-level DNA methylation profile for each cell type

The Roadmap Epigenomics Project Reference can be used to interrogate the biology of the DNA methylation signature. To improve the understanding of the biology behind significant features, it can be run using the selected features from the entire dataset, which can be obtained running “2_Selection.R” with 1 split train-test (100% train and 0% test). It is important to use this split ONLY when studying the biology of the selected features and NOT for prediction assessment purposes (unless an independent test dataset is used for assessment).
Example with data derived from primary mononuclear cells: 
```
Rscript 4_CS_Loc.R E062 Scripts /2_Selection/All/Beta/ElasticNet/0.1/Features.txt 1_Preprocessing/NHL_Quantile/annot.RData 4_ReferenceEpigenome
```
Output:
-	Chromatin state for all the CpGs from the signature compared with the reference epigenome scaled to match the length of the signature (“SignaturevsReference_CS.png”)
-	Chromatin state for all the CpGs from the signature compared with the reference epigenome scaled to match the length of the signature, in absolute and relative numbers (“SignaturevsReference_CS.txt”)
-	P-value and FDR value (Benjamini-Hochberg procedure) referring to the level of enrichment for each chromatin state compared to the reference epigenome (“Significance_CS.txt”)
-	Genome location for all the CpGs from the signature compared with the reference epigenome scaled to the length of the signature (“Location_genome.png”)
-	Chromatin state for all the CpGs from the signature compared with the reference epigenome (scaled to the length of the signature), in absolute and relative numbers (“SignaturevsReference_Location.txt”)
-	P-value and FDR value (Benjamini-Hochberg procedure) referring to the level of enrichment for each genome location compared to the reference epigenome (“Significance_Location.txt”)



## References

1-	Fortin J, Triche TJ, Hansen KD. “Preprocessing, normalization and integration of the Illumina HumanMethylationEPIC array with minfi.” Bioinformatics, 33(4) (2017). doi: 10.1093/bioinformatics/btw691.
2-	Leek J, Jonhson WE, Parker HS. et al. “The sva package for removing batch effects and other unwanted variation in high-throughput experiments”. Bioinformatics, 28(6): 882–883 (2012). doi: 10.1093/bioinformatics/bts034
3-	Newman A.M., Steen C.B., Liu C.L. et al. “Determining cell type abundance and expression from bulk tissues with digital cytometry”. Nat Biotechnol 37, 773–782 (2019). doi: 10.1038/s41587-019-0114-2
4-	Yang Z., Wong A., Kuh D. et al. “Correlation of an epigenetic mitotic clock with cancer risk”. Genome Biol 17, 205 (2016). doi: 10.1186/s13059-016-1064-3
5-	Kundaje A., Meuleman W., Ernst J. et al. “Integrative analysis of 111 reference human epigenomes”. Nature 518, 317–330 (2015). doi.org/10.1038/nature142483

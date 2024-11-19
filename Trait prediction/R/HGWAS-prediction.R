library(plyr)
library(hglm)
source("functions.R")

files <- paste0("test_qtl_haplotype.txt")
haplotype_test <- read.table(files,colClasses = "character")
phe_file <- paste0("test_pheno.txt")
phe <- read.table(phe_file,header=FALSE,sep="\t")
##choose trait
trait = phe[,2]
#read covar
sex_covar <- read.table("test_patch.txt",header=FALSE,sep="\t")
covar = sex_covar[,-1]
k=5
cor_prs_out_name <- "CV_cor_HGWAS_PRS.txt"
get_prs(haplotype_test,trait,k,cor_prs_out_name)

cor_blup_out_name <- "CV_cor_HGWAS_BLUP.txt"
get_blup(haplotype_test,trait,covar,k,cor_blup_out_name)

library(lme4qtl)
library(plinkFile)
phe <- read.table("test_pheno.txt",header=F,sep="\t")
##use GCTA/PLINK GRM
#grm <- readGRM("allpca")
#Z2 <- as.matrix(grm)
##use haplotype GRM
Z1 <- read.table("test_hap_matrix.txt",header=FALSE,sep="\t")
Z2=as.matrix(Z1)
row.names(Z2) <- colnames(Z2) <- phe[,1]

dat <- data.frame(ID = phe[,1],trait = phe[,2])
##get  NULL MODEL
m0 <- relmatLmer(trait ~ 1 + (1|ID), dat, relmat = list(ID = Z2))
mm <- summary(m0)
##get new trait
resi <- mm$residuals
trait_resi <- rep(NA,nrow(phe))
trait_resi[which(is.na(phe[,2]) == F)] <- resi
writename <- paste0("resi_phe_grm.txt")
write.table(trait_resi,writename,col.names = F,row.names = F,quote = F,sep = "\t")


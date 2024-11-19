##k as haplotype frequency cutline
k <- 0.05
result <- 0
genoname <- paste0("test_hap_tra_count_freq",k,".txt")
geno_in <- read.table(genoname)
phe_name <- paste0("resi_phe_grm.txt")
phe <- read.table(phe_name,header=FALSE,sep="\t")
trait_resi <- phe[,1]

##filter rare haplotype for sample
motif_resi <- function(genotype,resi){
  shan <- which(is.na(genotype) == T)
  resi[shan] <- NA
  return(resi)
}

for (i in 1:ncol(geno_in)) {
  genotype <- as.factor(as.numeric(geno_in[,i]))
  trait_resi1 <- motif_resi(genotype,trait_resi)
  m0 <- try(lm(trait_resi1 ~  1))
  m1 <- try(update(m0,.~. + genotype))
  if(class(m1) == "try-error"){
    print(c(0,0))
  }else{
    m2 <- anova(m0,m1)
    result <- rbind(result,m2$`Pr(>F)`)    
  }
}
result <- result[-1,]
result <- result[,2]
write.table(result,"P_value.txt",col.names = F,row.names = F,quote = F,sep = "\t")


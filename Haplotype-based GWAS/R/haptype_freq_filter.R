geno_name <- paste0("test_hap_tra_count.txt")
geno_test <- read.table(geno_name)
##define filter cutline
filter_cut <- 0.05
for (i in 1:ncol(geno_test)) {
  Var1 <- as.numeric(geno_test[,i])
  aa <- as.data.frame(table(Var1))
  aa$Freq <- aa$Freq/nrow(geno_test)
  out_num <- as.numeric(aa$Var1[which(aa$Freq <= filter_cut)])
  for (j in out_num) {
    geno_test[which(geno_test[,i] == j),i] <- NA
  }
  print(paste0(i,"/",ncol(geno_test)))
}
outname <- paste0("test_hap_tra_count_freq0.05.txt")
write.table(geno_test,outname,col.names = F,row.names = F,quote = F,sep = "\t")

  
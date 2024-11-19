library(data.table)
source("functions.R")
xxm_name <- paste0("test_allele_matrix.txt")
xxm <- fread(xxm_name,sep = "\t",header = F,drop = 1,nThread = 1)
#xxm is the SNP matrix, the columns is 2* the number of samples, and the rows is the number of SNP
#input：xxm, the length of window
code <- get_tra_count(xxm,window = 5)
fwrite(code,paste0("test_hap_tra_count.txt"),col.names = F,row.names = F,quote = F,sep = "\t")

#调用GHap包提取单倍型序列
library(GHap)

##将vcf文件压缩为phase格式
ghap.vcf2phase(vcf.files = "test.vcf",
               sample.files = "test.sample",
               out.file = "ghap_test")
ghap.compress(input.file="ghap_test",out.file="ghap_test")
##提取allele
phase <- ghap.loadphase("ghap_test")
sam <- read.table("test.sample")
marker <- read.table("ghap_test.markers")
hap_out <- ghap.slice(object = phase, ids = sam$V1,
                 variants = marker$V2, index = F,sparse = F,ncores = 1)
##输出单倍型allele结果
write.table(hap_out,"test_allele_matrix.txt",col.names = T,row.names = T,quote = F,sep = "\t")

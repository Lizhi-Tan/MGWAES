source("functions.R")
phone <- read.xlsx( "egg_recode.xlsx")
phonenum <- phone[2:897,3:163]
dat <- apply(phonenum, 2,as.numeric)
trait_time <- get_trait(dat,21,26)
trait_out <- cbind(phone[,1],trait_time)
write.table(trait_out,"21_26week_trait.txt",col.names = F,row.names = F,quote = F,sep = "\t")

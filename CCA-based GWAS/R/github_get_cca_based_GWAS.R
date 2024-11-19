library(openxlsx)
library(plinkFile)
library(vcfR)
library(CCA)
library(ggplot2)
library(paletteer) 
source("functions.R")
stage <- "up"
trait_name <- paste0(stage,"_stage_trait.txt")
trait <- read.table(trait_name,header = T)
genoname <- paste0("test")
geno_in<- readBED(genoname)
snpid <- readFAM(genoname)
##elimate id 
trait1 <- trait[,2:19]
colnames(trait1) <- paste0("f",1:18)
trait2 <- apply(trait1,2,as.numeric)
allele2 <- geno_in
allele3 <- as.data.frame(allele2)
allele3 <- apply(allele3, 1,as.numeric)
##match genotype and phenotype
trait3 <- trait2[match(snpid[,1],trait[,1]),]
##elimate NA
aa <- 0
for (i in 1:ncol(trait3)) {
  aa <- c(aa,which(is.na(trait3[,i]) == T))
}
aa <- aa[-1]
trait_new <- trait3[-unique(aa),]
snp2 <- allele3[,-unique(aa)]
##elimate NA of genotype
#trait_new1 <- trait_new[which(is.na(snp2[,1]) == F),]
#snp3 <- snp2[which(is.na(snp2[,1]) == F),]

##test per SNP
for (ff in 1:nrow(snp2)) {
  ca <- cancor.test(snp2[ff,],trait_new,plot = F)
}

##get significant SNP cca 
kk1 <- get_cca_coefficients(stage,1)
write.table(out,paste0("ycoef_snp_",stage,"_Y.xscores.txt"),col.names = F,row.names = T,quote = F,sep = "\t")


##plot
aa <- read.table(paste0("ycoef_snp_",stage,"_Y.xscores.txt"),fill = T)
aa <- aa[-1,]
aa <- aa[-which(aa[,1] == 0),]
n=3
start <- 14*(0:(n-1))+1
end <- 14*(c(1:n))
up_zaihe <- 1
for (i in 1:n) {
  bb <- aa[start[i]:end[i],]
  rownames(bb) <- bb[,1]
  up_zaihe <- rbind(up_zaihe,bb)
}
up_zaihe <- up_zaihe[-1,]
up_zaihe <- cbind(up_zaihe,rep(c("rs317757057","rs737739123","rs734813104"),each = 14))
colnames(up_zaihe)[3] <- "V3"

stage <- "sustain"
cc <- read.table(paste0("ycoef_snp_",stage,"_Y.xscores.txt"),fill = T)
cc <- cc[-1,]
cc <- cc[-which(cc[,1] == 0),]
n=2
start <- 14*(0:(n-1))+1
end <- 14*(c(1:n))
down_zaihe <- 1
for (i in 1:n) {
  dd <- cc[start[i]:end[i],]
  rownames(dd) <- dd[,1]
  down_zaihe <- rbind(down_zaihe,dd)
}
down_zaihe <- down_zaihe[-1,]
down_zaihe <- cbind(down_zaihe,rep(c("6_24438513","6_24441778"),each = 14))
colnames(down_zaihe)[3] <- "V3"
zaihe_all <- rbind(up_zaihe,down_zaihe)
zaihe_all1<- rbind(zaihe_all[c(2:7,1,8,10:14),],zaihe_all[c(2:7,1,8,10:14)+14,],zaihe_all[c(2:7,1,8,10:14)+28,],zaihe_all[c(2:7,1,8,10:14)+42,],zaihe_all[c(2:7,1,8,10:14)+56,])
trait_name <- c("EV","WMLR","WEV","BWMLR","BWEV","CPN","TCS","ECI","LIT","AILI","TILI","MILI","II")
vector = factor(rep(trait_name,5),level = rev(trait_name))
zaihe_dat <- data.frame(coef = zaihe_all1[,2],snp = zaihe_all1[,3],trait = vector)
zaihe_dat$snp <- factor(zaihe_dat$snp,levels = c("6_24438513","6_24441778","rs317757057", "rs734813104", "rs737739123"))
zaihe_dat$coef <- as.numeric(zaihe_dat$coef)

ann_colors <- c(paletteer_d("RColorBrewer::PuBu")[5:9],paletteer_d("rcartocolor::Burg")[4:6],paletteer_d("ggsci::orange_material")[5:9])
ggplot(zaihe_dat,aes(x=snp,y=coef,fill = trait)) +  
  geom_col(position = "dodge",width = 0.8) +
  scale_fill_manual(limits = trait_name,values = ann_colors) + 
  coord_flip() + 
  scale_x_discrete(breaks=c("6_24438513","6_24441778","rs317757057", "rs734813104", "rs737739123"),
                   limits = c("6_24438513","6_24441778","rs317757057", "rs734813104", "rs737739123")) + 
  labs(title = paste0(stage," stage")) + 
  xlab("SNP")+ylab("Canonical Coefficients") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5,size=rel(1.5)),
        #legend.position="none",
        panel.border = element_blank(),
        legend.title = element_text(color="black", # 修改图例的标题
                                    size=rel(1.2), 
                                    face="bold"),
        legend.text = element_text(color="black", # 设置图例标签文字
                                   size = rel(1), 
                                   face = "bold"),
        axis.title = element_text(color='black',size=rel(1.5)),
        axis.text.x= element_text(color='black',size=rel(1.5)),
        axis.text.y= element_text(color='black',size=rel(1.5)),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
  )
ggsave(paste0(stage," stage SNP CCA coeff.png"),width = 8,height = 9)


library(vcfR)
library(effects)
library(emmeans)
source("functions.R")
geno <- read.vcfR("test.vcf")
geno_gt <- geno@gt
geno_result <- cbind(geno@fix,geno@gt)
trait_num = 3
geno_position <- 1
get_post_hoc_effect(trait_num,geno_position)
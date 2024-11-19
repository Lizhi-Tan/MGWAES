library(openxlsx)
library(ggplot2)
library(grid)
source("functions.R")
phone <- read.xlsx( "egg_recode.xlsx")
phone <- phone[-1,]
##read ID
sample<- read.xlsx("egg_id.xlsx",sheet=1)
#sample<- read.xlsx("test2.xlsx",sheet=1)
sample <- sample[,1]
###reorder
phone1 <- phone[match(sample,phone[,1]),]
plotdat <- phone1[,-2]
plotdat <- plotdat[,-1]
##plot
get_curve(plotdat)









get_curve <- function(plotdat){
  ##get timeline
  days <- rep(1:23,each=7)
  #get per week production for per chicken
  daysu <- matrix(NA,ncol = 23,nrow = 872)
  for (i in 1:872) {
    daysu[i,] <- as.numeric(by(as.numeric(plotdat[i,]),days,sum))
  }
  #get per week production odd for per chicken
  weekper <- daysu/7
  
  #make x-axis
  wl <- c(1:23)
  yy <- weekper[1,]
  #get mean production odd
  yy <- apply(weekper,2,mean)
  weekper1 <- rbind(weekper,wl)
  
  
  #AIC & BIC model
  AIC_M <- function(y_test,y_pred,k,n){
    resid <- y_test-y_pred
    SSR <- sum(resid ** 2)
    return(2*k+n*log(SSR/n)) 
  }
  BIC_M <- function(y_test,y_pred,k,n){
    resid <- y_test-y_pred
    SSR <- sum(resid ** 2)
    return((k*log(n)+n*log(SSR/n)))}
  
  
  #wood model
  f1<- function(x,a) {a[1]*x^a[2]*exp(-a[3]*x)}
  #McMillan model
  f2<- function(x,a) {a[1]*(1-exp(-a[3]*(x-a[4])))*exp(-a[2]*x)}
  #yang-ning model
  f3<- function(x,a) {a[1]*exp(-a[2]*x)/(1+exp(-a[3]*(x-a[4])))}
  
  loss1<-function(a,x,y){sum((y-f1(x,a))^2)}
  loss2<-function(a,x,y){sum((y-f2(x,a))^2)}
  loss3<-function(a,x,y){sum((y-f3(x,a))^2)}
  
  #a00<-c(0.001103781,1.184645105,1.184645105)
  a00 <- c(0.1,0.1,0.1)
  #a0<-c(0.001103781,1.184645105,1.184645105,1.184645105)
  a0 <- c(0.1,0.1,0.1,0.1)
  
  optim1<-optim(a00,loss1,x=wl,y=yy,method = "BFGS",control = list(maxit= 1000))
  optim2<-optim(a0,loss2,x=wl,y=yy,method = "BFGS",control = list(maxit= 1000))
  optim3<-optim(a0,loss3,x=wl,y=yy,method = "BFGS",control = list(maxit= 1000))
  
  a1 <- optim1$par
  a2 <- optim2$par
  a3 <- optim3$par
  
  op1<-optim(a1,loss1,x=wl,y=yy,method = "BFGS",control = list(maxit= 1000))
  op2<-optim(a2,loss2,x=wl,y=yy,method = "BFGS",control = list(maxit= 1000))
  op3<-optim(a3,loss3,x=wl,y=yy,method = "BFGS",control = list(maxit= 1000))
  
  dat11 <- data.frame(x=c(1:23),y=(f1(c(1:23),op1$par)))
  dat22 <- data.frame(x=c(1:23),y=(f2(c(1:23),op2$par)))
  dat33 <- data.frame(x=c(1:23),y=(f3(c(1:23),op3$par)))
  
  dat11$x <- dat11$x + 20
  dat22$x <- dat22$x + 20
  dat33$x <- dat33$x + 20
  
  BIC1 <- BIC_M(yy,dat11$y,3,c(length(yy)))
  BIC2 <- BIC_M(yy,dat22$y,3,c(length(yy)))
  BIC3 <- BIC_M(yy,dat33$y,3,c(length(yy)))
  
  label1 <- paste("Wood model: BIC= ",round(BIC1,3),sep = "")
  label2 <- paste("compartmental model: BIC= ",round(BIC2,3),sep = "")
  label3 <- paste("Yang model: BIC= ",round(BIC3,3),sep = "")
  
  ##get mean curve
  ff <- ggplot() + 
    geom_point(aes(x=wl,y=yy),col="#999999") + 
    geom_line(data=dat11,aes(x,y),col="#9b3a74",lwd = 1) + 
    geom_line(data=dat22,aes(x,y),col="#f46f20",lwd = 1) + 
    geom_line(data=dat33,aes(x,y),col="#156077",lwd = 1) + 
    xlab("Time(week)")+ylab("Egg production rate") +
    #ylim(-0.15,1) + 
    #annotate(geom="text", x=13, y=0, label=label1,col = "#9b3a74",hjust = 0) + 
    #annotate(geom="text", x=13, y=-0.1, label=label2,col = "#f46f20",hjust = 0) +
    #annotate(geom="text", x=13, y=-0.05, label=label3,col = "#156077",hjust = 0) +
    theme_bw() + 
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.title.x = element_text(color='black',size=rel(1.2),margin = 
                                        margin(0.5,1,0,1,"cm")),
          axis.title.y = element_text(color='black',size=rel(1.2),margin = 
                                        margin(0,0.8,0,0,"cm")))
  
  sam <- sample(c(1:872),300)
  ##fit curve for all sample
  f0 <- ggplot()
  for (j in 1:872) {
    f3<- function(x,a) {a[1]*exp(-a[2]*x)/(1+exp(-a[3]*(x-a[4])))}
    loss3<-function(a,x,y){sum((y-f3(x,a))^2)}
    a00 <- c(0.1,0.1,0.1,0.1)
    optim3<-optim(a0,loss3,x=wl,y=weekper[j,],method = "BFGS",control = list(maxit= 1000))
    a3 <- optim3$par
    op3<-optim(a3,loss3,x=wl,y=weekper[j,],method = "BFGS",control = list(maxit= 1000))
    
    dattt <- data.frame(x=c(1:23),y=(f3(c(1:23),op3$par)))
    dattt$x <- dattt$x + 20
    f1 <- f0 + 
      geom_line(data=dattt,aes(x,y),col="#999999",lwd = 1,alpha = 0.3) + 
      xlab("Time(week)")+ylab("Egg production rate") +
      #ylim(-0.15,1) + 
      #annotate(geom="text", x=13, y=0, label=label1,col = "#9b3a74",hjust = 0) + 
      #annotate(geom="text", x=13, y=-0.1, label=label2,col = "#f46f20",hjust = 0) +
      #annotate(geom="text", x=13, y=-0.05, label=label3,col = "#156077",hjust = 0) +
      theme_bw() + 
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.title.x = element_text(color='black',size=rel(1.2),margin = 
                                          margin(0.5,1,0,1,"cm")),
            axis.title.y = element_text(color='black',size=rel(1.2),margin = 
                                          margin(0,0.8,0,0,"cm")))
    f0 <- f1
  }
  dat11$x <- dat11$x + 20
  dat22$x <- dat22$x + 20
  dat33$x <- dat33$x + 20
  
  f2 <- f0 + 
    geom_line(data=dat11,aes(x,y),col="#9b3a74",lwd = 1.5) + 
    geom_line(data=dat22,aes(x,y),col="#f46f20",lwd = 1.5) + 
    geom_line(data=dat33,aes(x,y),col="#156077",lwd = 1.5) + 
    xlab("Time(week)")+ylab("Egg production rate") +
    scale_x_continuous(breaks = seq(21,41,5)) +
    #ylim(-0.15,1) + 
    annotate(geom="text", x=34, y=0.1, label=label1,col = "#9b3a74",hjust = 0) + 
    annotate(geom="text", x=34, y=0.05, label=label2,col = "#f46f20",hjust = 0) +
    annotate(geom="text", x=34, y=0.15, label=label3,col = "#156077",hjust = 0) +
    theme_bw() + 
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.text.x = element_text(color='black',size=rel(1.5)),
          axis.text.y = element_text(color='black',size=rel(1.5)),
          axis.title.x = element_text(color='black',size=rel(1.5),margin = 
                                        margin(0.5,1,0,1,"cm")),
          axis.title.y = element_text(color='black',size=rel(1.5),margin = 
                                        margin(0,0.8,0,0,"cm")))
  
  f2
  ggsave("egg curve.png",width = 8,height = 6)
}

get_trait <- function(dat,day_in,day_out){
  start <- (day_in - 20)*7-6
  end <- (day_out - 20)*7
  plotdat <- dat[,start:end]
  chanday <- apply(plotdat,1,sum)
  tingday <- matrix(NA,nrow = nrow(plotdat),ncol = 1)
  for (i in 1:nrow(plotdat)) {
    tingday[i,] <- as.numeric(table(plotdat[i,]))[1]
  }
  sumvar <- apply(plotdat ,1,var)
  week <- day_out-day_in+1
  days <- rep(1:week,each=7)
  weekeach<- matrix(0,ncol = week,nrow = nrow(plotdat))
  datpure <- plotdat[,1:length(days)]
  for (j in 1:nrow(plotdat)) {
    dayeach <- by(as.numeric(datpure[j,]),days,sum)
    weekeach[j,] <- as.numeric(dayeach)}
  ##week egg production per chicken
  max_weekeach_percent <- apply(weekeach/7,1,max)
  #week egg variance per chicken
  var.weekeach <- apply(weekeach,1,var)
  
  two_week <- trunc(week/2)
  days <- rep(1:two_week,each=2)
  doubleweekeach<- matrix(0,ncol = two_week,nrow = nrow(plotdat))
  for (j in 1:nrow(plotdat)) {
    dayeach <- by(as.numeric(weekeach[j,1:(two_week*2)]),days,sum)
    doubleweekeach[j,] <- as.numeric(dayeach)}
  
  max_doubleweek_percent <- apply(doubleweek_percent <- doubleweekeach/14,1,max)
  var.doubleweekeach <- apply(doubleweekeach,1,var)
  #clutch trait
  datpure <- plotdat[,1:ncol(plotdat)]
  liantimes <- matrix(0,ncol = 1,nrow = nrow(plotdat))
  sumlian <- matrix(0,ncol = 1,nrow = nrow(plotdat))
  averagelian <- matrix(0,ncol = 1,nrow = nrow(plotdat))
  faillain <- matrix(0,ncol = 1,nrow = nrow(plotdat))
  for (i in 1:nrow(plotdat)) {
    if(datpure[i,][1] == 1){
      if(length(rle(datpure[i,])$length) == 1){
        lian <- 1
        ting<- 0
      }else{
        lian <- seq(from = 1, to = length(rle(datpure[i,])$length), by = 2)
        ting <- seq(from = 2, to = length(rle(datpure[i,])$length), by = 2)
      }}else{
        if(length(rle(datpure[i,])$length) == 2){
          lian <- seq(from = 2, to = length(rle(datpure[i,])$length), by = 2)
          ting <- seq(from = 1, to = length(rle(datpure[i,])$length), by = 2)
        }
        if(length(rle(datpure[i,])$length) == 1){
          lian <- 0
          ting <- 0
        }
        if(length(rle(datpure[i,])$length) >2){
          lian <- seq(from = 2, to = length(rle(datpure[i,])$length), by = 2)
          ting <- seq(from = 3, to = length(rle(datpure[i,])$length), by = 2)
        }
      }
    liandays <- rle(datpure[i,])$length[lian]
    if(ting[1] == 0){
      tingdays <- 0
    }else{
      tingdays <- rle(datpure[i,])$length[ting]
    }
    tingdays <- c(tingdays,0)
    averagelian[i,] <- mean(liandays[which(liandays >= 2)])
    faillain[i,] <- (chanday[1]+tingday[1,]-sum(liandays[which(liandays >= 2)]))/length(liandays[which(liandays >= 2)])
    liantimes[i,] <- length(which(liandays >= 2))
    sumlian[i,] <- sum(liandays[which(liandays >= 2)])
  }
  
  #interval trait
  datpure <- plotdat[,1:ncol(plotdat)]
  tingtimes <- matrix(0,ncol = 1,nrow = nrow(plotdat))
  jiangetimes <- matrix(0,ncol = 1,nrow = nrow(plotdat))
  averagelianting <- matrix(0,ncol = 1,nrow = nrow(plotdat))
  maxtingdays <- matrix(0,ncol = 1,nrow = nrow(plotdat))
  sumjiange <- matrix(0,ncol = 1,nrow = nrow(plotdat))
  for (i in 1:nrow(plotdat)) {
    if(datpure[i,][1] == 1){
      if(length(rle(datpure[i,])$length) ==1){
        ting <- 0
      }else{
        ting <- seq(from = 2, to = length(rle(datpure[i,])$length), by = 2)
      }
      
    }else{
      if(length(rle(datpure[i,])$length) <= 2){
        ting <- 1 
      }else{
        #ting <- seq(from = 3, to = length(rle(datpure[i,])$length), by = 2)
        ting <- seq(from = 1, to = length(rle(datpure[i,])$length), by = 2)
      }
    }
    tingdays <- rle(datpure[i,])$length[ting]
    jiangetimes[i,] <- length(which(tingdays >= 1))
    #averagelianting[i,] <- mean(tingdays[which(tingdays >= 2)])
    averagelianting[i,] <- mean(tingdays[which(tingdays >= 2)])
    tingtimes[i,] <- length(which(tingdays >= 2))
    sumjiange[i,] <- sum(tingdays[which(tingdays >= 1)])
    maxtingdays[i,] <- max(tingdays[which(tingdays >= 2)])
  }
  jianxielength <- sumjiange/liantimes
  
  multi_trait <- cbind(c("EV",sumvar),c("WMLR",max_weekeach_percent),c("WEV",var.weekeach))
  multi_trait <- cbind(multi_trait,c("BWMLR",max_doubleweek_percent),c("BWEV",var.doubleweekeach))
  multi_trait <- cbind(multi_trait,c("CPN",liantimes),c("TCS",sumlian),c("ECI",averagelian))
  multi_trait <- cbind(multi_trait,c("LIT",jiangetimes),c("AILI",averagelianting),c("TILI",sumting),c("MILI",maxtingdays),c("II",jianxielength))
  return(multi_trait)
  
}

get_tra_count <- function(xxm,window){
  ##comput block number of fix window
  nh <- trunc(nrow(xxm)/window)
  ##get seq 
  xhnum <- seq(1+window,window*nh,window)
  code_ok <- 0
  for (i in c(1,xhnum)){
    #paste haplotype
    hm <-apply(xxm[i:(i+window-1),],2,function(x){paste0(x,collapse = "")})
    #make initial code seq
    code <- rep(0,dim(table(hm)))
    ##match haplotype of two chr and positision
    code1 <- match(hm,as.data.frame(table(hm))$hm)[seq(1,ncol(xxm),by = 2)]
    code2 <- match(hm,as.data.frame(table(hm))$hm)[seq(2,ncol(xxm),by = 2)]
    code_out <- matrix(0,nrow = ncol(xxm)/2,ncol = dim(table(hm)))
     for (j in 1:length(which(code1 == code2))) {
      code_out[which(code1 == code2)[j],code1[which(code1 == code2)[j]]] <- 2
    }
    for (j in 1:length(which(!code1 == code2))) {
      code_out[which(!code1 == code2)[j],code1[which(!code1 == code2)[j]]] <- 1
      code_out[which(!code1 == code2)[j],code2[which(!code1 == code2)[j]]] <- 1
    }
    ##recode
    code_pas <- apply(code_out,1,function(x){paste0(x,collapse = "")})
    ##get order
    code_match <- match(code_pas,as.data.frame(table(code_pas))$code_pas)
    code_ok <- cbind(code_ok,code_match)
    print(paste0(1+(i-1)/window,"/",nh))
  }
  code_ok <- code_ok[,-1]
  return(code_ok)
  
}

get_haplotype_grm_fix_window <- function(xh,window){
  #nc is the length of snp 
  nc <- nrow(xh)
  ##nh is the total number of haplotype
  nh <- trunc(nc/window)
  ##nr is haplotype type number——2* number of individual
  nr <- ncol(xh)
  hm <- matrix(NA,ncol = nh,nrow = nr)
  xhnum <- seq(1,window*nh,window)
  #combine haplotype
  xxh <- as.matrix(apply(xh,2,function(x){paste0(x,collapse = "")}))
  hm <- 1
  ##cut by window
  for (i in xhnum) {
    hm <- cbind(hm,substr(xxh,i,i+window-1))
  }
  hm <- hm[,-1]
  ##comput GRM by haplotype similar
  h1 <- matrix(NA,ncol = nr,nrow = nr)
  for (j in 1:nr) {
    for (i in 1:nr) {
      if(hm[i,1] == hm[j,1]){
        h1[j,i] <- 1
      }else{h1[j,i] <- 0}
    }
  }
  for (o in 2:nh) {
    for (j in 1:nr) {
      for (i in 1:nr) {
        if(hm[i,o] == hm[j,o]){
          h1[j,i] <- h1[j,i] + 1
        }else{h1[j,i] <- h1[j,i] + 0}
      }
    }
  }
  #hh is gametic relationship matrix
  hh <- h1/nh
  t1 <- matrix(c(1,1),byrow = F)
  t2 <- t(t1)
  Iah <- diag(rep(1,5),nr/2,nr/2)
  K <- kronecker(t(Iah),t2)
  #H11 is haplotype matrix
  H11 <- K%*%hh%*%t(K)/2 
  return(H11)
}

cancor.test<-function(x,y,plot=FALSE){
  x=scale(x); y=scale(y);
  n=nrow(x);p=ncol(x);q=ncol(y);
  ca=cancor(x,y)
  cat("\n"); print(ca);
  #cancor.test(ca$cor,n,p,q)
  r=ca$cor 
  m <-length(r); Q<-rep(0, m); P=rep(0,m); lambda <- 1
  for (k in m:1){
    lambda<-lambda*(1-r[k]^2); 
    Q[k]<- -log(lambda)  
  }
  s<-0; i<-m 
  for (k in 1:m){
    Q[k]<- (n-k+1-1/2*(p+q+3)+s)*Q[k]
    P[k]<-1-pchisq(Q[k], (p-k+1)*(q-k+1))
  }
  
  cat("cancor test: \n");print(cbind(ff-1,r,Q,P))
  if(plot){
    u=as.matrix(x)%*%ca$xcoef
    v=as.matrix(y)%*%ca$ycoef
    plot(u[,1],v[,1],xlab='u1',ylab='v1')
    abline(lm(u[,1]~v[,1]))
  }
}

get_cca_coefficients <- function(stage,snp_pos){
  qtl <- geno_result[snp_pos,10:ncol(geno_result)]
  aa <- t(as.data.frame(strsplit(qtl,"|")))
  geno <- as.numeric(aa[,1]) + as.numeric(aa[,3])
  geno1 <- geno
  geno1 <- cbind(geno1,geno)
  t1 <- read.table(paste0(stage,"_stage_trait.txt"),header = T)
  phe1 <- t1[match(rownames(aa),t1$样品名称),]
  aa <- 0
  for (i in 1:ncol(phe1)) {
    aa <- c(aa,which(is.na(phe1[,i]) == T))
  }
  aa <- aa[-1]
  trait_new <- phe1[-unique(aa),]
  snp2 <- geno[-unique(aa)]
  ff <- snp_pos
  ca <- cancor.test(snp2,trait_new[,2:19],plot = F)
  snp <- as.matrix(snp2)
  trait1 <- as.matrix(trait_new[,c(2,4:14,16:19)])
  cc1 <- cc(snp,trait1)
  com1 <- comput(snp,trait1,cc1)
  com1
  kk <- -com1$corr.Y.xscores
  kk1 <- matrix(kk[-c(8:9),])
  row.names(kk1) <- cc[2:15,1]
  return(kk1)
}

CVgroup <- function(k,datasize){
  cvlist <- list()
  #divide K group
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]  
  #distrub n
  temp <- sample(n,datasize)   
  x <- 1:k
  dataseq <- 1:datasize
  #random make k datacol for dataseq 
  cvlist <- lapply(x,function(x) dataseq[temp==x])  
  return(cvlist)
}

get_result <- function(result,start,end){
  outnum <- 1
  for (i in 1:length(start)) {
    pos1 <- which(as.numeric(result[(start[i]:end[i]),4]) == min(as.numeric(result[(start[i]:end[i]),4])))
    if(!length(pos1) == 1){
      aa <- which(result[(start[i]+pos1-1),2] == max(as.numeric(result[(start[i]+pos1-1),2])))
      pos1 <- pos1[aa[1]]
    }
    outnum <- c(outnum,(start[i]+pos1-1))
  }
  outnum <- outnum[-1]
  result2 <- result[outnum,]
  colnames(result2) <- c("allele","estimate","se","P")
  result2$P <- as.numeric(result2$P)
  return(result2)
}

get_effect_prs <- function(haplotype,result,start,end){
  benifical_pos <- 1:length(start)
  result_eff <- matrix(0,ncol = ncol(haplotype),nrow = length(benifical_pos))
  for (i in benifical_pos) {
    result_block <- result[(start[i]:end[i]),]
    for (j in 1:ncol(haplotype)) {
      #get haplotype order
      pos_allele <- which(!t(as.data.frame(strsplit(haplotype[i,j],""))) == 0)
      ##match haplotype per sample and result
      match_allele <- match(pos_allele,result_block$genotype)
      ##recode
      if(length(pos_allele) == 1){
        result_eff[i,j] <- result_block$estimate[match_allele] * 2
        if (is.na(result_eff[i,j]) == T){
          result_eff[i,j] <- 0
        }
      }else{
        if (length(unique(match_allele)) == 1){
          result_eff[i,j] <- 0 
        }else{
          result_eff[i,j] <- sum(result_block$estimate[match_allele])
           if (is.na(result_eff[i,j]) == T){
            result_eff[i,j] <- result_block$estimate[match_allele][which(is.na(result_block$estimate[match_allele]) == F)]
          }
          
        }
      }
    }
  }
  return(result_eff)
}

get_effect_blup <- function(haplotype,result,start,end){
  benifical_pos <- 1:length(start)
  result_eff <- matrix(0,ncol = ncol(haplotype),nrow = length(benifical_pos))
  for (i in benifical_pos) {
    result_block <- result[(start[i]:end[i]),]
    for (j in 1:ncol(haplotype)) {
      pos_allele <- which(!t(as.data.frame(strsplit(haplotype[i,j],""))) == 0)
        match_allele <- match(paste0("genotype",pos_allele),result_block$genotype)
       if(length(pos_allele) == 1){
        result_eff[i,j] <- result_block$estimate[match_allele] * 2
      }else{
           result_eff[i,j] <- sum(result_block$estimate[match_allele])
      }
          if (is.na(result_eff[i,j]) == T){
        result_eff[i,j] <- 0
      }
    }
  }     
  return(result_eff)
}

get_prs  <- function(haplotype_test,trait,k,cor_prs_out_name){
  progress.bar <- create_progress_bar("text") 
  progress.bar$init(k)   
  ##elimate NA
  shan <- which(is.na(trait) == T)
  trait1 <- trait[-shan]
  datasize <- length(trait1)
  ##get CV list
  cvlist <- CVgroup(k = k,datasize = datasize)
  cor_prs_out <- 1
  for (m in 1:k) {
    ##get training set
    train_trait <- trait1[-cvlist[[m]]]
    result <- 0
    m4 <- 0
    ##comput effect size per block
    for (i in 1:ncol(haplotype_test)) {
      geno_test <- haplotype_test[,i]
      geno_test2 <- as.character(geno_test)
      ##transfer haplotype to sparse matrix
      geno_test1 <- t(as.data.frame(strsplit(geno_test2,""))) 
      genotype <- apply(geno_test1,2,as.numeric)
      ##elimate unknown
      genotype1 <- genotype[-shan,]
      train_genotype <- genotype1[-cvlist[[m]],]
      ##GLM
      m1 <- lm(train_trait ~ train_genotype)
      m2 <- summary(m1)
      beta_eff <- cbind(rownames(m2$coefficients),m2$coefficients)
      beta_eff1 <- beta_eff[2:(nrow(m2$coefficients)),]
      result <- rbind(result,beta_eff1,m4)
    }
    colnames(result) <- c("genotype","estimate","se","t","p")
    #can choose output result
    #return(m3)
    #m3_name <- paste0("lm_PRS_k_",k,"_fold_",m,".txt")
    #write.table(m3,m3_name,col.names = T,row.names = F,quote = F,sep = "\t")
    ####PRS 
    #elimate unknown
    haplotype1 <- haplotype_test[-shan,]
    ##reflect validation set
    haplotype <- t(haplotype1[cvlist[[m]],])
    haplotype <- as.data.frame(haplotype)
    ##can choose read result
    #result_name <- paste0("lm_PRS_k_",k,"_fold_",m,".txt")
    #result <- read.table(result_name,header = T)
    result <- as.data.frame(result)
    result <- result[-nrow(result),]
    result <- result[,-4]
    result$estimate <- as.numeric(result$estimate)
    ##rename
    hapname1 <- as.data.frame(strsplit(result$genotype,"train_genotype"))[2,]
    result$genotype <- unlist(hapname1)
    start <- which(result$genotype == 0) + 1
    end <- which(result$genotype == 0) -1
    end <- c(end[-1],nrow(result))
    ##get haplotype allele(sparse matrix)
    result3 <- get_result(result,start,end)
    ##elimate block(P > 0.05)
    shanchu <- which(result3$P >= 0.05)
    result4 <- result3[-shanchu,]
    ##put individual haplotype effect of within each block into sample
    result_eff <- get_effect_prs(haplotype,result,start,end)
    eff_hap <- as.data.frame(result_eff)
    eff_hap <- eff_hap[-shanchu,]
    ##define benefical allele and unbeneficial allele
    shanchu2 <- which(result4$estimate < 0)
    eff_hap1 <- eff_hap[-shanchu2,]
    eff_hap2 <- eff_hap[shanchu2,]
    ##comput PRS
    prs_hap_bene <- apply(eff_hap1,2,sum)
    prs_hap_dele <- apply(eff_hap2,2,sum)
    prs_hap_all <- apply(result_eff,2,sum)
    phe_test <- trait1[cvlist[[m]]]
    ##correlation of PRS and trait
    cor_prs_bene <- cor(phe_test,prs_hap_bene) 
    cor_prs_dele <- cor(phe_test,prs_hap_dele)
    cor_prs_all <- cor(phe_test,prs_hap_all)
    cor_prs <- c(cor_prs_bene,cor_prs_dele,cor_prs_all,m)
    cor_prs_out <- rbind(cor_prs_out,cor_prs)
    progress.bar$step()
  }
  ##cross validation result
  cor_prs_out <- cor_prs_out[-1,]
  cor_prs_out <- as.data.frame(cor_prs_out)
  colnames(cor_prs_out) <- c("PRS_bene","PRS_dele","PRS_all",paste0("K_",k,"fold"))
  #cor_prs_out_name <- paste0("CV_cor_HGWAS_PRS_k_",k,"_fold.txt")
  write.table(cor_prs_out,cor_prs_out_name,col.names = T,row.names = F,quote = F,sep = "\t")
  #return(cor_prs_out)
}

get_blup  <- function(haplotype_test,trait,covar,k,cor_blup_out_name){
  progress.bar <- create_progress_bar("text") 
  #set task num
  progress.bar$init(k)   
  dat <- cbind(phe$V1,trait,covar)
  dat <- as.data.frame(dat)
  colnames(dat)[1:2] <- c("ID","trait")
  ##elimate NA
  shan <- which(is.na(dat$trait) == T)
  dat1 <- dat[-shan,]
  datasize <- nrow(dat1)
  ##get CV list
  cvlist <- CVgroup(k = k,datasize = datasize)
  cor_blup_out <- 1
  for (m in 1:k) {
    ##get training set
    train_dat <- dat1[-cvlist[[m]],]
    train_dat$trait <- as.numeric(train_dat$trait)
    result <- 0
    m4 <- 0
    ## comput effect size per block
    for (i in 1:ncol(haplotype_test)) {
       geno_test <- haplotype_test[,i]
      geno_test2 <- as.character(geno_test)
      ##transfer haplotype to sparse matrix
      geno_test1 <- t(as.data.frame(strsplit(geno_test2,"")))
      genotype <- apply(geno_test1,2,as.numeric)
      genotype1 <- as.matrix(genotype)
      ##eliminate unknown haplotype
      genotype1 <- genotype1[-shan,]
      ##reflect haplotype in training set
      train_genotype <- genotype1[-cvlist[[m]],]
      covariate <- train_dat[,3:ncol(train_dat)]
      if (ncol(train_dat)>3){
        train_covariate <- apply(covariate,1,as.numeric)
      }else{
        train_covariate <- as.numeric(covariate)
      }     
      ##comput effect size while sparse matrix as random effect by hglm
      m1 <- try(hglm(y = train_dat$trait,X = cbind(1,train_covariate),Z = train_genotype))
      m2 <- try(summary(m1))
      if (class(m2)[1] == "try-error"){
        m2$RandCoefMat <- cbind(1,1)
      }
      if(m2[2] == "try-error"){
        m2$RandCoefMat <- cbind(1,1)
      }
      ##get effect size per allele
      m3 <- cbind(paste0("genotype",1:dim(genotype1)[2]),m2$RandCoefMat)
      result <- rbind(result,m3,m4)
      #print(i)
    }
    colnames(result) <- c("genotype","estimate","se")
    #cna choose output  result
    #return(m3)
    #m3_name <- paste0("hglm_BLUP_k_",k,"_fold_",m,".txt")
    #write.table(m3,m3_name,col.names = T,row.names = F,quote = F,sep = "\t")
    
    
    ####BLUP
    ##eliminate unknown haplotype
    haplotype1 <- haplotype_test[-shan,]
    ##reflect haplotype in Validation set
    haplotype <- t(haplotype1[cvlist[[m]],])
    haplotype <- as.data.frame(haplotype)
    ##can choose read result txt
    #result_name <- paste0("hglm_BLUP_k_",k,"_fold_",m,".txt")
    #result <- read.table(result_name,header = T)
    result <- result[-nrow(result),]
    result <- as.data.frame(result)
    result$estimate <- as.numeric(result$estimate)
    start <- which(result$genotype == 0) + 1
    end <- which(result$genotype == 0) -1
    end <- c(end[-1],nrow(result))
    ###put individual haplotype effect of within each block into sample
    result_eff <- get_effect_blup(haplotype,result,start,end)
    ##correlation of BLUP and trait
    eff_hap <- as.data.frame(result_eff)
    test_dat <- dat1[cvlist[[m]],]
    test_trait <- as.numeric(test_dat$trait)
    blup_hap <- apply(eff_hap,2,sum)
    cor_blup <- c(cor(test_trait,blup_hap),m)
    cor_blup_out <- rbind(cor_blup_out,cor_blup)
    progress.bar$step()
  }
  ##get cross validation result
  cor_blup_out <- cor_blup_out[-1,]
  cor_blup_out <- as.data.frame(cor_blup_out)
  colnames(cor_blup_out) <- c("BLUP",paste0("K_",k,"fold"))
  #cor_blup_out_name <- paste0("CV_cor_HGWAS_BLUP_k_",k,"_fold.txt")
  write.table(cor_blup_out,cor_blup_out_name,col.names = T,row.names = F,quote = F,sep = "\t")
}

get_post_hoc_effect <- function(trait_num,geno_position){
  qtl <- geno_result[i,10:ncol(geno_result)]
  aa <- t(as.data.frame(strsplit(qtl,"|")))
  geno <- as.numeric(aa[,1]) + as.numeric(aa[,3])
  t1 <- read.table("21_26week_paishenhg_trait_qujizhi.txt",header = T)
  phe1 <- t1[match(rownames(aa),t1$样品名称),(trait_num + 1)]
  t2 <- read.table("27_43week_paishenhg_trait_qujizhi.txt",header = T)
  phe2 <- t2[match(rownames(aa),t2$样品名称),(trait_num + 1)]
  dat <- cbind(c(phe1,phe2),c(geno,geno),c(rep("up",nrow(aa)),rep("down",nrow(aa))))
  dat <- as.data.frame(dat)
  colnames(dat) <- c("trait","geno","stage")
  dat$geno <- as.factor(dat$geno)
  #dat$geno <- as.numeric(dat$geno)
  dat$stage <- as.factor(dat$stage)
  re2 <- lm(trait ~ 1 + geno * stage,data = dat)
  rr <- summary(re2)
  EMM1 <- emmeans(re2,pairwise ~ geno*stage)
  cont <- contrast(EMM1[[1]], simple = "geno", combine = TRUE, adjust = "mvt")
  aa <- summary(cont)
  return(aa)
}



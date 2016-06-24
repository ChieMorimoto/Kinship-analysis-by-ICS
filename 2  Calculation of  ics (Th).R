# Analysis the length of the shared region

# HapFam.Number: the number of the shared SNPs
# n: the number of simulation
# x: the number of pedigree
# y: the number of relationships
# cm1: Th (the threthold of the genetic length of the IBS segments between two individuals)
# Chr: Chr.ver5
# NUM: the number of SNPs (174,254)


Simu.Array.Len.Seg1 <- function(HapFam.Number,n,x,y,cm1,Chr,NUM){ 
 
  Count.Result <- rep(-1,y*n)
  
  for(k in 1:n){    
    for(i in 1:y){
      array <- HapFam.Number[(k-1)*y+i,]
  
      ShareSeg1 <- numeric(0)     
      ShareSeg2 <- numeric(0)    
      for(j in 1:(NUM-1)){
        if(array[j] == 0 && array[j+1] != 0){
          ShareSeg1[(length(ShareSeg1)+1)] <- j+1
        }else if(array[j] != 0 && array[j+1] == 0){
          ShareSeg2[(length(ShareSeg2)+1)] <- j　　　　
        }
      }

      if( length(ShareSeg1) == 0 && length(ShareSeg2) == 0 ){
        SegcM <- Chr[NUM,11]-Chr[1,11]
      }else if(length(ShareSeg1) == 0 && length(ShareSeg2) == 1){
        if( (Chr[ShareSeg2[1],11] - Chr[1,11]) >= cm1){
          SegcM <- Chr[ShareSeg2[1],11] - Chr[1,11]
        }
      }else if(length(ShareSeg1) == 1 && length(ShareSeg2) == 0){
        if( (Chr[NUM,11] - Chr[ShareSeg1[1],11]) >= cm1){
          SegcM <- Chr[NUM,11] - Chr[ShareSeg1[1],11]
        }
      }else if( length(ShareSeg1) == length(ShareSeg2) && ShareSeg1[1] <= ShareSeg2[1]){
        SegcM <- numeric(0)
        for(j in 1:length(ShareSeg1)){
          if( (Chr[(ShareSeg2[j]),11] - Chr[(ShareSeg1[j]),11]) >= cm1){ 
            SegcM[length(SegcM)+1] <- Chr[(ShareSeg2[j]),11] - Chr[(ShareSeg1[j]),11]
          }
        }
        SegcM <- sum(SegcM)
      }else if( length(ShareSeg1) == length(ShareSeg2) && ShareSeg1[1] > ShareSeg2[1]){
        ShareSeg1 <- c(c(1),ShareSeg1)
        ShareSeg2 <- c(ShareSeg2,c(NUM))
        SegcM <- numeric(0)
        for(j in 1:length(ShareSeg1)){
          if( (Chr[(ShareSeg2[j]),11] - Chr[(ShareSeg1[j]),11]) >= cm1){
            SegcM[length(SegcM)+1] <- Chr[(ShareSeg2[j]),11] - Chr[(ShareSeg1[j]),11]
          }
        }
        SegcM <- sum(SegcM)
      }else if( length(ShareSeg1) != length(ShareSeg2) && ShareSeg1[1] > ShareSeg2[1]){
        ShareSeg1 <- c(c(1),ShareSeg1)
        SegcM <- numeric(0)
        for(j in 1:length(ShareSeg1)){
          if( (Chr[(ShareSeg2[j]),11] - Chr[(ShareSeg1[j]),11]) >= cm1){
            SegcM[length(SegcM)+1] <- Chr[(ShareSeg2[j]),11] - Chr[(ShareSeg1[j]),11]
          }
        }
        SegcM <- sum(SegcM)
      }else if( length(ShareSeg1) != length(ShareSeg2) && ShareSeg1[1] <= ShareSeg2[1]){
        ShareSeg2 <- c(ShareSeg2,c(NUM))
        SegcM <- numeric(0)
        for(j in 1:length(ShareSeg1)){
          if( (Chr[(ShareSeg2[j]),11] - Chr[(ShareSeg1[j]),11]) >= cm1){
            SegcM[length(SegcM)+1] <- Chr[(ShareSeg2[j]),11] - Chr[(ShareSeg1[j]),11]
          }
        }
        SegcM <- sum(SegcM)
      }

      Count.Result[(k-1)*y+i] <- SegcM
    }
  }
return(Count.Result)
}

# example, cm1=0
# change cm1 from 0 to 63 discretely 
Simu.From_HapFam.Result.cm0 <- list()
for(i in 1:22){
  Simu.From_HapFam.Result.cm0[[i]] <- Simu.Array.Len.Seg1(From_HapFam.Number[[i]],249,12,10,0,Chr.ver5[[i]],ncol(From_HapFam.Number[[i]]))
}

Total.Simu.From_HapFam.Result.cm0 <- Simu.From_HapFam.Result.cm0[[1]]
for(i in 2:22){
  Total.Simu.From_HapFam.Result.cm0 <- Total.Simu.From_HapFam.Result.cm0 + Simu.From_HapFam.Result.cm0[[i]]
}

Total.ShareSeg1.cm0.From_Hap <- matrix(Total.Simu.From_HapFam.Result.cm0,10,249)　#Seg1





# find the most appropriate Th values

# ROC, AUC
# function quated from http://aoki2.si.gunma-u.ac.jp/R/ROC.html

ROC0 <- function(disease,normal,lowest=NULL,width=NULL,COL) 
{
        my.hist <- function(x, brks)                         
        {
                k <- length(brks)
                freq <- numeric(k)
                for (i in 1:(k-1)) {
                        freq[i] <- sum(brks[i] <= x & x < brks[i+1])
                }
                freq[k] <- sum(x >= brks[k])
                freq
        }

        x <- c(disease, normal)                                      
        min.x <- min(x)                                              
        max.x <- max(x)                                              
        cat("a x = ", min.x, "\n")
        cat("b x = ", max.x, "\n\n")
        if (is.null(lowest) || is.null(width)) {
                temp <- pretty(c(disease, normal), n=min(length(disease)+length(normal), 50))
                lowest <- temp[1]
                width <- diff(temp)[1]
                cat("c = ", lowest, "\n")
                cat("d = ", width, "\n\n")
        }
        
        brks <- seq(lowest, max.x+width, by=width)
        ROC(brks, my.hist(disease, brks), my.hist(normal, brks),COL)
        
}

# ics(Th): ics(Th) values of one relationship (ShareSeg1.cm.From_Hap)
# ics(Th)2: ics(Th) values of another relationship  (ShareSeg1.cm.From_Hap)
ROC0(ics(Th),ics(Th)2,0,1,1)



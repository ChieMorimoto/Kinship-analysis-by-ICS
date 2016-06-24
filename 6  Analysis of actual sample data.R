# SNPgenotype: matrix including genotypes and chromosome
 # input file: CSV file (We show an example file, Examplefile_samplegenotype.csv.)
SNPgenotype <- read.csv("Examplefile_samplegenotype.csv",header=T)

OrderGenotype24 <- function(SNPgenotype,Chr){
  OrderedSNP <- list()　　
  for(k in 1:22){
    SNPChr <- list()
    SNPChr[[k]] <- subset(SNPgenotype,Chr==k)  
    SNPorder <- match(Chr[[k]][,1],SNPChr[[k]][,1])
    OrderedSNP[[k]] <- SNPChr[[k]][SNPorder,]
    OrderedSNP[[k]] <- OrderedSNP[[k]][c(1,2,3,4)]
 }
return(OrderedSNP)
}

# Chr.ver5: list of 174,254SNPs
Orderedgenotype <- OrderGenotype24(SNPgenotype,Chr.ver5)


# Count the number of the shared SNPs
SNPcount2 <- function(A){
  if(is.element(5,A)){   #NoCall
    z <- 3
  }else if(!any(is.element(A[1:2],A[3:4]))){
    z <- 0
  }else if(setequal(A[1:2],A[3:4])){
    z <- 2
  }else{
    z <- 1
  } 
  return(z)
}  


SNPcompare <- function(Person1,Person2){
  SNPtypes <- matrix(0,nrow(Person1),4)
  for(j in 1:nrow(Person1)){
    for(i in 3:4){    
      if(Person1[j,i] == "A"){
        SNPtypes[j,i-2] <- 1
      }else if(Person1[j,i] == "T"){
        SNPtypes[j,i-2] <- 2
      }else if(Person1[j,i] == "C"){
        SNPtypes[j,i-2] <- 3
      }else if(Person1[j,i] == "G"){
        SNPtypes[j,i-2] <- 4
      }else{
        SNPtypes[j,i-2] <- 5  
      }
      if(Person2[j,i] == "A"){
        SNPtypes[j,i] <- 1
      }else if(Person2[j,i] == "T"){
        SNPtypes[j,i] <- 2
      }else if(Person2[j,i] == "C"){
        SNPtypes[j,i] <- 3
      }else if(Person2[j,i] == "G"){
        SNPtypes[j,i] <- 4
      }else{
        SNPtypes[j,i] <- 5　　　　
      }
    }
  }
SNPshare <- apply(SNPtypes,1,SNPcount2)
return(SNPshare)    
}



AllChro.array <- function(Ordered1,Ordered2){
  Chro22 <- list()
  for(k in 1:22){
    Chro22[[k]] <- SNPcompare(Ordered1[[k]],Ordered2[[k]])
  }
  return(Chro22)
}


# example
PC.array1 <- AllChro.array(OrderedParent,OrderedChild)



## Analysis of the shared length

Array.Str.cm <- function(array,cm1,Chr){
  
  NUM <- length(array)
  if(length(which(array==3))!=0){ 
    Callarray <- array[-c(which(array==3))]
  }else{
    Callarray <- array
  }
  　
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
  return(c(sum(Callarray),length(which(array==3)),SegcM)) 
}


All.array.cm <- function(array,cm1,Chr){
  Share.array <- matrix(-1,22,3)
  for(i in 1:22){  
    Share.array[i,] <- Array.Str.cm(array[[i]],cm1,Chr[[i]])
  }
  return(Share.array)
}

# example
# cm1=3
PCpattern.cm3 <- matrix(-3,21,3)
PCpattern.cm3[1,] <- apply(All.array.cm(PC.array1,3,Chr.ver5),2,sum)
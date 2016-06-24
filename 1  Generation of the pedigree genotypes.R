# List of haplotypes 

Haplo.2500k <- list()
 # input file: text file transrated from the HAPS file
 # example (chromosome 1)
Haplo.2500k[[1]] <- read.table(" file name ".txt,header=F)
 # repeat up to chromosome 22
 

# List of the recombination rate
 # Chr.ver5 : list of SNPs in R workspace
SNP.Naga.cList <- list()　　
for(j in 1:22){
  NUM <- nrow(Chr.ver5[[j]])  
  CMvec <- rep(0, NUM-1)
  for(i in 1:(NUM-1)){
    CMvec[i] <- (Chr.ver5[[j]][i+1,11]-Chr.ver5[[j]][i,11])/100
  }
  SNP.Naga.cList[[j]] <- CMvec
}


# Family tree
x <- 12                               
familytree <- matrix(0,x,2)       
familytree[4,] <- c(1,2)
familytree[5,] <- c(1,2)
familytree[8,] <- c(3,4)
familytree[9,] <- c(5,6)
familytree[11,] <- c(7,8)
familytree[12,] <- c(9,10)
random <- which(familytree[,1]==0)          
notrandom <- which(familytree[,1]!=0)


# Kinship
y <- 10
Kinship <- matrix(0,y,2)
Kinship[1,] <- c(2,5)    
Kinship[2,] <- c(4,5)   
Kinship[3,] <- c(5,8)    
Kinship[4,] <- c(1,9)     
Kinship[5,] <- c(8,9)     
Kinship[6,] <- c(1,11)    
Kinship[7,] <- c(5,11)    
Kinship[8,] <- c(8,12)    
Kinship[9,] <- c(11,12)　　　
Kinship[10,] <- c(7,10)　　　


# Count the number of shared SNPs
SNPcount <- function(A){
  if(!any(is.element(A[1:2],A[3:4]))){
    z <- 0
  }else if(setequal(A[1:2],A[3:4])){
    z <- 2
  }else{
    z <- 1
  } 
  return(z)
}  

####Computationally generate the genotypes of the pedigree####

# n : number of simulation
# Num.Pop : number of haplotypes
# Haplo : matrix of haplotype
# cList : list of the recombination rate
# NUM : number of used SNPs
# Chr.NUM : chromosome number

# function
Family.from_hap <- function(n,Num.Pop,Haplo,cList,NUM,Chr.Num){  
  ShareNumber <- matrix(-2,n*y,NUM)　　　　  
  　　　　　　　　　　　　　
  RandomPerson <- sample(c(1:Num.Pop),length(random)*n,replace=FALSE)    
  Num.Random <- length(random)
        
　　for(k in 1:n){    
    Patternlist <- matrix(-1,2*x,NUM)            
    Share <- matrix(0,y,NUM)                        
    patternA <- matrix(0,4,NUM)　　　　　　　　　　　　　　　

    Pick.Random <- RandomPerson[((k-1)*Num.Random+1):(k*Num.Random)]　
    Patternlist[sort(c(2*random-1, 2*random)),] <- t(Haplo[,sort(c((Pick.Random+2)*2,(Pick.Random+2)*2+1))])　　
    
　　　　for(j in notrandom){　　　　　　　　　　　　　　　　　　　　　　　               
      parent <- c(familytree[j,1],familytree[j,2])      
      for(g in 1:2){        
        PARENT <- Patternlist[(2*parent[g]-1):(2*parent[g]),]     
        recombination <- c((which((runif(NUM-1)-cList) < 0)),NUM)　　　　　
        even <- recombination[seq_along(recombination) %% 2==0]　　　　　　 
        uneven <- recombination[seq_along(recombination) %% 2==1]　　　　　
        if(((tail(uneven,n=1)) == NUM) && (length(uneven) > 1)){         
        　　for(h in 1:(length(uneven)-1)){       
          　　X <- PARENT[1,((uneven[h]+1):even[h])]
          　　PARENT[1,((uneven[h]+1):even[h])] <- PARENT[2,((uneven[h]+1):even[h])]　　　　　
          　　PARENT[2,((uneven[h]+1):even[h])] <- X
        　　}
      　　}
        if((tail(uneven,n=1)) != NUM){                                 
          for(h in 1:(length(uneven))){
            Y <- PARENT[1,((uneven[h]+1):even[h])]
            PARENT[1,((uneven[h]+1):even[h])] <- PARENT[2,((uneven[h]+1):even[h])]　　　　　
            PARENT[2,((uneven[h]+1):even[h])] <- Y
          }
        }        
        if(g==1){  
          Patternlist[2*j-1,] <- PARENT[sample(c(1,2),1),] 
        }else{　　　　　
          Patternlist[2*j,] <- PARENT[sample(c(1,2),1),]
        }
      }   
    }                

  　　for(i in 1:y){   
    　　patternA <- Patternlist[c(Kinship[i,1]*2-1,Kinship[i,1]*2,Kinship[i,2]*2-1,Kinship[i,2]*2),]　　　　　     
    　　Share[i,] <- apply(patternA,2,SNPcount)
  　　}   
  　　ShareNumber[c(((k-1)*y+1):(k*y)),] <- Share  
  }
　　return(ShareNumber)
}

# The shared number of SNPs on 22 autosomal chromosomes
From_HapFam.Number <- list()
for(i in 1:22){
  From_HapFam.Number[[i]] <- Family.from_hap(249,1498,Haplo.2500k[[i]],SNP.Naga.cList[[i]],nrow(Chr.ver5[[i]]),i)
}


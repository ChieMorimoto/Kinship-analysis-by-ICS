# Collateral
Kinship.determ <- function(cM){
  density <- rep(-1,6)
  density[1] <- dlnorm(cM,Sib.lnorm[[1]][1],Sib.lnorm[[1]][2])
  density[2] <- dlnorm(cM,Nep.lnorm[[1]][1],Nep.lnorm[[1]][2])
  density[3] <- dlnorm(cM,Fc.lnorm[[1]][1],Fc.lnorm[[1]][2])
  density[4] <- dlnorm(cM,Fcc.lnorm[[1]][1],Fcc.lnorm[[1]][2])
  density[5] <- dlnorm(cM,Sc.lnorm[[1]][1],Sc.lnorm[[1]][2])
  density[6] <- dlnorm(cM,Unr.lnorm[[1]][1],Unr.lnorm[[1]][2])
  weight <- density*(1/sum(density))
  return(weight)
}

Range.est <- function(max,min,width){
  result.mat <- matrix(-1,((max-min)/width)+1,7)
  for(i in 1:(((max-min)/width)+1)){
    result.mat[i,1] <- (min+width*(i-1))
    result.mat[i,2:7] <- Kinship.determ((min+width*(i-1)))
  }
  return(result.mat)
}

## >99.8%
Kinship.est <- Range.est(3663,0,1)
Sib.est <- Kinship.est[(which(Kinship.est[,2] > 0.998)),1]
Nep.est <- Kinship.est[(which(Kinship.est[,3] > 0.998)),1]
Fc.est <- Kinship.est[(which(Kinship.est[,4] > 0.998)),1]
Fcc.est <- Kinship.est[(which(Kinship.est[,5] > 0.998)),1]
Sc.est <- Kinship.est[(which(Kinship.est[,6] > 0.998)),1]
Unr.est <- Kinship.est[(which(Kinship.est[,7] > 0.998)),1]

## >99%
Kinship.est <- Range.est(3663,0,1)
Sib.est <- Kinship.est[(which(Kinship.est[,2] > 0.99)),1]
Nep.est <- Kinship.est[(which(Kinship.est[,3] > 0.99)),1]
Fc.est <- Kinship.est[(which(Kinship.est[,4] > 0.99)),1]
Fcc.est <- Kinship.est[(which(Kinship.est[,5] > 0.99)),1]
Sc.est <- Kinship.est[(which(Kinship.est[,6] > 0.99)),1]
Unr.est <- Kinship.est[(which(Kinship.est[,7] > 0.99)),1]

Kinship.determ(SIBpattern[SIB_2[3],3])
Kinship.determ(NEPpattern[NEP_2[2],3])
Kinship.determ(FCpattern[FC_2[2],3])
Kinship.determ(FCCpattern[FCC_2[1],3])
Kinship.determ(SCpattern[SC_2[1],3])
Kinship.determ(UNKpattern[UNK_2[3],3])

# percentage of >99.8%
1-plnorm(2658,Sib.lnorm[[1]][1],Sib.lnorm[[1]][2])
plnorm(2235,Nep.lnorm[[1]][1],Nep.lnorm[[1]][2])-plnorm(1739,Nep.lnorm[[1]][1],Nep.lnorm[[1]][2])
plnorm(232,Sc.lnorm[[1]][1],Sc.lnorm[[1]][2])-plnorm(206,Sc.lnorm[[1]][1],Sc.lnorm[[1]][2])
plnorm(114,Unr.lnorm[[1]][1],Unr.lnorm[[1]][2])

# percentage of >99%
1-plnorm(2585,Sib.lnorm[[1]][1],Sib.lnorm[[1]][2])
plnorm(2277,Nep.lnorm[[1]][1],Nep.lnorm[[1]][2])-plnorm(1664,Nep.lnorm[[1]][1],Nep.lnorm[[1]][2])
plnorm(1355,Fc.lnorm[[1]][1],Fc.lnorm[[1]][2])-plnorm(1054,Fc.lnorm[[1]][1],Fc.lnorm[[1]][2])
plnorm(266,Sc.lnorm[[1]][1],Sc.lnorm[[1]][2])-plnorm(191,Sc.lnorm[[1]][1],Sc.lnorm[[1]][2])
plnorm(124,Unr.lnorm[[1]][1],Unr.lnorm[[1]][2])


# Lineal
Kinship.determ2 <- function(cM){
  density <- rep(-1,3)
  density[1] <- dlnorm(cM,Gfa.lnorm[[1]][1],Gfa.lnorm[[1]][2])
  density[2] <- dlnorm(cM,Ggfa.lnorm[[1]][1],Ggfa.lnorm[[1]][2])
  density[3] <- dlnorm(cM,Unr.lnorm[[1]][1],Unr.lnorm[[1]][2])
  weight <- density*(1/sum(density))
  return(weight)
}

Range.est2 <- function(max,min,width){
  result.mat <- matrix(-1,((max-min)/width)+1,4)
  for(i in 1:(((max-min)/width)+1)){
    result.mat[i,1] <- (min+width*(i-1))
    result.mat[i,2:4] <- Kinship.determ2((min+width*(i-1)))
  }
  return(result.mat)
}

Kinship.determ2(GFApattern.cm3[GFA_2[1],3])
Kinship.determ2(GGFApattern.cm3[2,3])
Kinship.determ2(UNKpattern.cm3[UNK_2[1],3])



# Collateral

# C-1
LR.SIB_UNR <- rep(-1,length(SIB_2))
for(i in 1:length(SIB_2)){
  LR.SIB_UNR[i] <- dlnorm(SIBpattern.cm4[SIB_2[i],3],Sib.lnorm.cm4[[1]][1],Sib.lnorm.cm4[[1]][2])/dlnorm(SIBpattern.cm4[SIB_2[i],3],Unr.lnorm.cm4[[1]][1],Unr.lnorm.cm4[[1]][2])
}

# C-2
LR.NEP_UNR <- rep(-1,length(NEP_2))
for(i in 1:length(NEP_2)){
  LR.NEP_UNR[i] <- dlnorm(NEPpattern.cm4[NEP_2[i],3],Nep.lnorm.cm4[[1]][1],Nep.lnorm.cm4[[1]][2])/dlnorm(NEPpattern.cm4[NEP_2[i],3],Unr.lnorm.cm4[[1]][1],Unr.lnorm.cm4[[1]][2])
}

# C-3
LR.FC_UNR <- rep(-1,length(FC_2))
for(i in 1:length(FC_2)){
  LR.FC_UNR[i] <- dlnorm(FCpattern.cm4[FC_2[i],3],Fc.lnorm.cm4[[1]][1],Fc.lnorm.cm4[[1]][2])/dlnorm(FCpattern.cm4[FC_2[i],3],Unr.lnorm.cm4[[1]][1],Unr.lnorm.cm4[[1]][2])
}

# C-4
LR.FCC_UNR <- rep(-1,length(FCC_2))
for(i in 1:length(FCC_2)){
  LR.FCC_UNR[i] <- dlnorm(FCCpattern.cm4[FCC_2[i],3],Fcc.lnorm.cm4[[1]][1],Fcc.lnorm.cm4[[1]][2])/dlnorm(FCCpattern.cm4[FCC_2[i],3],Unr.lnorm.cm4[[1]][1],Unr.lnorm.cm4[[1]][2])
}

# C-5
LR.SC_UNR <- rep(-1,length(SC_2))
for(i in 1:length(SC_2)){
  LR.SC_UNR[i] <- dlnorm(SCpattern.cm4[SC_2[i],3],Sc.lnorm.cm4[[1]][1],Sc.lnorm.cm4[[1]][2])/dlnorm(SCpattern.cm4[SC_2[i],3],Unr.lnorm.cm4[[1]][1],Unr.lnorm.cm4[[1]][2])
}

# Lineal

# L-2
LR.GFA_UNR <- rep(-1,length(GFA_2))
for(i in 1:length(GFA_2)){
  LR.GFA_UNR[i] <- dlnorm(GFApattern.cm3[GFA_2[i],3],Gfa.lnorm.cm3[[1]][1],Gfa.lnorm.cm3[[1]][2])/dlnorm(GFApattern.cm3[GFA_2[i],3],Unr.lnorm.cm3[[1]][1],Unr.lnorm.cm3[[1]][2])
}

# L-3
LR.GGFA_UNR <- dlnorm(GGFApattern.cm3[2,3],Ggfa.lnorm.cm3[[1]][1],Ggfa.lnorm.cm3[[1]][2])/dlnorm(GGFApattern.cm3[2,3],Unr.lnorm.cm3[[1]][1],Unr.lnorm.cm3[[1]][2])




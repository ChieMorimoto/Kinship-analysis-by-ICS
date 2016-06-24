# Select half of samples randomly

PC <- c(1:21)
PC_1 <- sample(c(1:21),11)
PC_2 <- PC[-PC_1]


SIB <- c(1:20)
SIB_1 <- sample(c(1:20),10)
SIB_2 <- SIB[-SIB_1]

NEP <- c(1:17)
NEP_1 <- sample(c(1:17),9)
NEP_2 <- NEP[-NEP_1]

GFA <- c(1:17)
GFA_1 <- sample(c(1:17),9)
GFA_2 <- GFA[-GFA_1]

FC <- c(1:14)
FC_1 <- sample(c(1:14),7)
FC_2 <- FC[-FC_1]

GFSIB <- c(1:11)
GFSIB_1 <- sample(c(1:11),6)
GFSIB_2 <- GFSIB[-GFSIB_1]

FCC <- c(1:10)
FCC_1 <- sample(c(1:10),5)
FCC_2 <- FCC[-FCC_1]

SC <- c(1:7)
SC_1 <- sample(c(1:7),4)
SC_2 <- SC[-SC_1]

UNK <- c(1:15)
UNK_1 <- sample(c(1:15),8)
UNK_2 <- UNK[-UNK_1]



# z test

p.value.SIB <- pnorm(mean(log(SIBpattern.cm4[SIB_1,3])),Sib.lnorm.cm4[[1]][1],(Sib.lnorm.cm4[[1]][2]/sqrt(length(SIB_1))),lower.tail=FALSE)
p.value.SIB


p.value.NEP <- pnorm(mean(log(NEPpattern.cm4[NEP_1,3])),Nep.lnorm.cm4[[1]][1],(Nep.lnorm.cm4[[1]][2]/sqrt(length(NEP_1))),lower.tail=FALSE)
p.value.NEP


p.value.GFA <- pnorm(mean(log(GFApattern.cm3[GFA_1,3])),Gfa.lnorm.cm3[[1]][1],(Gfa.lnorm.cm3[[1]][2]/sqrt(length(GFA_1))),lower.tail=FALSE)
p.value.GFA


p.value.FC <- pnorm(mean(log(FCpattern.cm4[FC_1,3])),Fc.lnorm.cm4[[1]][1],(Fc.lnorm.cm4[[1]][2]/sqrt(length(FC_1))),lower.tail=FALSE)
p.value.FC

GGFA_1 <- 1 
p.value.GGFA <- pnorm(mean(log(GGFApattern.cm3[GGFA_1,3])),Ggfa.lnorm.cm3[[1]][1],(Ggfa.lnorm.cm3[[1]][2]/sqrt(length(GGFA_1))),lower.tail=FALSE)
p.value.GGFA



p.value.FCC <- pnorm(mean(log(FCCpattern.cm4[FCC_1,3])),Fcc.lnorm.cm4[[1]][1],(Fcc.lnorm.cm4[[1]][2]/sqrt(length(FCC_1))),lower.tail=FALSE)
p.value.FCC


p.value.SC <- pnorm(mean(log(SCpattern.cm4[SC_1,3])),Sc.lnorm.cm4[[1]][1],(Sc.lnorm.cm4[[1]][2]/sqrt(length(SC_1))),lower.tail=FALSE)
p.value.SC

p.value.UNK.cm3 <- pnorm(mean(log(UNKpattern.cm3[UNK_1,3])),Unr.lnorm.cm3[[1]][1],(Unr.lnorm.cm3[[1]][2]/sqrt(length(UNK_1))),lower.tail=FALSE)
p.value.UNK.cm3

p.value.UNK.cm4 <- pnorm(mean(log(UNKpattern.cm4[UNK_1,3])),Unr.lnorm.cm4[[1]][1],(Unr.lnorm.cm4[[1]][2]/sqrt(length(UNK_1))),lower.tail=FALSE)
p.value.UNK.cm4
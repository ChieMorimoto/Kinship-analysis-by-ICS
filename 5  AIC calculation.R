# AIC

AIC.calc <- function(lnL,K){
  AIC <- -2*(sum(lnL)) + 2*sum(K)
  return(AIC)
}

AIC_norm_L <- AIC.calc(c(Gfa.norm.cm3[[2]],Ggfa.norm.cm3[[2]],Unr.norm.cm3[[2]]),rep(2,3))

AIC_lnorm_L <- AIC.calc(c(Gfa.lnorm.cm3[[2]],Ggfa.lnorm.cm3[[2]],Unr.lnorm.cm3[[2]]),rep(2,3))

AIC_truncnorm_L <- AIC.calc(c(Gfa.truncnorm.cm3[[2]],Ggfa.truncnorm.cm3[[2]],Unr.truncnorm.cm3[[2]]),rep(2,3))

AIC_norm_L
AIC_lnorm_L
AIC_truncnorm_L


AIC_norm_C <- AIC.calc(c(Sib.norm.cm4[[2]],Nep.norm.cm4[[2]],Fc.norm.cm4[[2]],Fcc.norm.cm4[[2]],Sc.norm.cm4[[2]],
                       Unr.norm.cm4[[2]]),rep(2,6))

AIC_lnorm_C <- AIC.calc(c(Sib.lnorm.cm4[[2]],Nep.lnorm.cm4[[2]],Fc.lnorm.cm4[[2]],Fcc.lnorm.cm4[[2]],Sc.lnorm.cm4[[2]],
                       Unr.lnorm.cm4[[2]]),rep(2,6))

AIC_truncnorm_C <- AIC.calc(c(Sib.truncnorm.cm4[[2]],Nep.truncnorm.cm4[[2]],Fc.truncnorm.cm4[[2]],Fcc.truncnorm.cm4[[2]],Sc.truncnorm.cm4[[2]],
                       Unr.truncnorm.cm4[[2]]),rep(2,6))

AIC_norm_C
AIC_lnorm_C
AIC_truncnorm_C
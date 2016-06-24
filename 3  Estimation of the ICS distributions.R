# data : Total.ShareSeg1.cm3.From_Hap (lineal)
# data : Total.ShareSeg1.cm4.From_Hap (collateral)

# Install package truncnorm
library(truncnorm)

# Normal distribution

log_likelihood_for_norm <- function(data){
  return(function(par){
    sum(log( dnorm(data, par[1], par[2])))
  })
}


# Log-normal distribution

log_likelihood_for_lognorm <- function(data){
  return(function(par){
    sum(log( dlnorm(data, par[1], par[2])))
  })
}


# Truncated normal distribution 

log_likelihood_for_truncnorm <- function(data){
  return(function(par){
    sum(log( dtruncnorm(data,0,3662.522,par[1],par[2])))
  })
}


# C-1
Sib.norm.cm4 <- optim(par = c(2300, 100), 
                  fn = log_likelihood_for_norm(Total.ShareSeg1.cm4.From_Hap[2,]),
                  control = list(fnscale = -1)
                  )

Sib.lnorm.cm4 <- optim(par = c(8, 0.058), 
                   fn = log_likelihood_for_lognorm(Total.ShareSeg1.cm4.From_Hap[2,]), 
                   control = list(fnscale = -1)
                   )


Sib.truncnorm.cm4 <- optim(par = c(2300, 100), 
                       fn = log_likelihood_for_truncnorm(Total.ShareSeg1.cm4.From_Hap[2,]),
                       control = list(fnscale = -1)
                       )

# C-2
Nep.norm.cm4 <- optim(par = c(2000, 180), 
                  fn = log_likelihood_for_norm(Total.ShareSeg1.cm4.From_Hap[3,]),
                  control = list(fnscale = -1)
                  )

 
Nep.lnorm.cm4 <- optim(par = c(7.5, 0.096), 
                   fn = log_likelihood_for_lognorm(Total.ShareSeg1.cm4.From_Hap[3,]), 
                   control = list(fnscale = -1)
                   )


Nep.truncnorm.cm4 <- optim(par = c(2000, 180), 
                       fn = log_likelihood_for_truncnorm(Total.ShareSeg1.cm4.From_Hap[3,]),
                       control = list(fnscale = -1)
                       )

# L-2
Gfa.norm.cm3 <- optim(par = c(2000, 260), 
                  fn = log_likelihood_for_norm(Total.ShareSeg1.cm3.From_Hap[4,]),
                  control = list(fnscale = -1)
                  )

 
Gfa.lnorm.cm3 <- optim(par = c(7.5, 0.143), 
                   fn = log_likelihood_for_lognorm(Total.ShareSeg1.cm3.From_Hap[4,]), 
                   control = list(fnscale = -1)
                   )


Gfa.truncnorm.cm3 <- optim(par = c(2000, 260), 
                       fn = log_likelihood_for_truncnorm(Total.ShareSeg1.cm3.From_Hap[4,]),
                       control = list(fnscale = -1)
                       )

# C-3
Fc.norm.cm4 <- optim(par = c(985, 160), 
                  fn = log_likelihood_for_norm(Total.ShareSeg1.cm4.From_Hap[5,]),
                  control = list(fnscale = -1)
                  )

 
Fc.lnorm.cm4 <- optim(par = c(7.5, 0.143), 
                   fn = log_likelihood_for_lognorm(Total.ShareSeg1.cm4.From_Hap[5,]), 
                   control = list(fnscale = -1)
                   )


Fc.truncnorm.cm4 <- optim(par = c(985, 160), 
                       fn = log_likelihood_for_truncnorm(Total.ShareSeg1.cm4.From_Hap[5,]),
                       control = list(fnscale = -1)
                       )


# L-3
Ggfa.norm.cm3 <- optim(par = c(985, 160), 
                  fn = log_likelihood_for_norm(Total.ShareSeg1.cm3.From_Hap[6,]),
                  control = list(fnscale = -1)
                  )

 
Ggfa.lnorm.cm3 <- optim(par = c(7.5, 0.143), 
                   fn = log_likelihood_for_lognorm(Total.ShareSeg1.cm3.From_Hap[6,]), 
                   control = list(fnscale = -1)
                   )


Ggfa.truncnorm.cm3 <- optim(par = c(985, 160), 
                       fn = log_likelihood_for_truncnorm(Total.ShareSeg1.cm3.From_Hap[6,]),
                       control = list(fnscale = -1)
                       )


# C-4
Fcc.norm.cm4 <- optim(par = c(510, 130), 
                  fn = log_likelihood_for_norm(Total.ShareSeg1.cm4.From_Hap[8,]),
                  control = list(fnscale = -1)
                  )

 
Fcc.lnorm.cm4 <- optim(par = c(6.2, 0.254), 
                   fn = log_likelihood_for_lognorm(Total.ShareSeg1.cm4.From_Hap[8,]), 
                   control = list(fnscale = -1)
                   )


Fcc.truncnorm.cm4 <- optim(par = c(510, 130), 
                       fn = log_likelihood_for_truncnorm(Total.ShareSeg1.cm4.From_Hap[8,]),
                       control = list(fnscale = -1)
                       )

# C-5
Sc.norm.cm4 <- optim(par = c(270, 89), 
                  fn = log_likelihood_for_norm(Total.ShareSeg1.cm4.From_Hap[9,]),
                  control = list(fnscale = -1)
                  )

 
Sc.lnorm.cm4 <- optim(par = c(5.6, 0.33), 
                   fn = log_likelihood_for_lognorm(Total.ShareSeg1.cm4.From_Hap[9,]), 
                   control = list(fnscale = -1)
                   )


Sc.truncnorm.cm4 <- optim(par = c(270, 89), 
                       fn = log_likelihood_for_truncnorm(Total.ShareSeg1.cm4.From_Hap[9,]),
                       control = list(fnscale = -1)
                       )

# UN
Unr.norm.cm3 <- optim(par = c(40,15), 
                  fn = log_likelihood_for_norm(Total.ShareSeg1.cm3.From_Hap[10,]),
                  control = list(fnscale = -1)
                  )

 
Unr.lnorm.cm3 <- optim(par = c(3.56, 0.424), 
                   fn = log_likelihood_for_lognorm(Total.ShareSeg1.cm3.From_Hap[10,]), 
                   control = list(fnscale = -1)
                   )


Unr.truncnorm.cm3 <- optim(par = c(40,15), 
                       fn = log_likelihood_for_truncnorm(Total.ShareSeg1.cm3.From_Hap[10,]),
                       control = list(fnscale = -1)
                       )


Unr.norm.cm4 <- optim(par = c(40,15), 
                  fn = log_likelihood_for_norm(Total.ShareSeg1.cm4.From_Hap[10,]),
                  control = list(fnscale = -1)
                  )

 
Unr.lnorm.cm4 <- optim(par = c(3.56, 0.424), 
                   fn = log_likelihood_for_lognorm(Total.ShareSeg1.cm4.From_Hap[10,]), 
                   control = list(fnscale = -1)
                   )


Unr.truncnorm.cm4 <- optim(par = c(40,15), 
                       fn = log_likelihood_for_truncnorm(Total.ShareSeg1.cm4.From_Hap[10,]),
                       control = list(fnscale = -1)
                       )



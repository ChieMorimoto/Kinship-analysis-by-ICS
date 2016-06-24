# C-1
par(plt=c(0.2, 0.95, 0.2, 0.85))
plot(qnorm(ppoints(249),Sib.norm.cm4[[1]][1],Sib.norm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[2,]),
    xlim=c(2300,3300),ylim=c(2300,3300),las=1,xlab="Theoretical Quantiles",ylab="",cex=1)
par(new=T)
plot(qlnorm(ppoints(249),Sib.lnorm.cm4[[1]][1],Sib.lnorm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[2,]),
    xlim=c(2300,3300),ylim=c(2300,3300),axes=F,ann=F,xlab="",ylab="",col=2,cex=1)
par(new=T)
plot(qtruncnorm(ppoints(249),0,3662.522,
    Sib.truncnorm.cm4[[1]][1],Sib.truncnorm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[2,]),
    xlim=c(2300,3300),ylim=c(2300,3300),axes=F,ann=F,xlab="",ylab="",col=3,cex=1)
par(new=T)
abline(0,1)
par(new=T)
text(2080,2800,"Sample Quantiles",srt=90,xpd=T)


# C-2
par(plt=c(0.2, 0.95, 0.2, 0.85))
plot(qnorm(ppoints(249),Nep.norm.cm4[[1]][1],Nep.norm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[3,]),
    xlim=c(1300,2700),ylim=c(1300,2700),las=1,xlab="Theoretical Quantiles",ylab="",cex=1)
par(new=T)
plot(qlnorm(ppoints(249),Nep.lnorm.cm4[[1]][1],Nep.lnorm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[3,]),
    xlim=c(1300,2700),ylim=c(1300,2700),axes=F,ann=F,xlab="",ylab="",cex=1,col=2)
par(new=T)
plot(qtruncnorm(ppoints(249),0,3662.522,
    Nep.truncnorm.cm4[[1]][1],Nep.truncnorm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[3,]),
    xlim=c(1300,2700),ylim=c(1300,2700),axes=F,ann=F,xlab="",ylab="",cex=1,col=3)
par(new=T)
abline(0,1)
par(new=T)
text(1000,2000,"Sample Quantiles",srt=90,xpd=T)



# L-2
par(plt=c(0.2, 0.95, 0.2, 0.85))
plot(qnorm(ppoints(249),Gfa.norm.cm3[[1]][1],Gfa.norm.cm3[[1]][2]),
    sort(Total.ShareSeg1.cm3.From_Hap[4,]),
    xlim=c(1100,2800),ylim=c(1100,2800),las=1,xlab="Theoretical Quantiles",ylab="",cex=1)
par(new=T)
plot(qlnorm(ppoints(249),Gfa.lnorm.cm3[[1]][1],Gfa.lnorm.cm3[[1]][2]),
    sort(Total.ShareSeg1.cm3.From_Hap[4,]),
    xlim=c(1100,2800),ylim=c(1100,2800),axes=F,ann=F,xlab="",ylab="",col=2)
par(new=T)
plot(qtruncnorm(ppoints(249),0,3662.522,
    Gfa.truncnorm.cm3[[1]][1],Gfa.truncnorm.cm3[[1]][2]),
    sort(Total.ShareSeg1.cm3.From_Hap[4,]),
    xlim=c(1100,2800),ylim=c(1100,2800),axes=F,ann=F,xlab="",ylab="",col=3)
par(new=T)
abline(0,1)
par(new=T)
text(700,2000,"Sample Quantiles",srt=90,xpd=T)



# C-3
par(plt=c(0.2, 0.95, 0.2, 0.85))
plot(qnorm(ppoints(249),Fc.norm.cm4[[1]][1],Fc.norm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[5,]),
    xlim=c(500,1700),ylim=c(500,1700),las=1,xlab="Theoretical Quantiles",ylab="")
par(new=T)
plot(qlnorm(ppoints(249),Fc.lnorm.cm4[[1]][1],Fc.lnorm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[5,]),
    xlim=c(500,1700),ylim=c(500,1700),axes=F,ann=F,xlab="",ylab="",col=2)
par(new=T)
plot(qtruncnorm(ppoints(249),0,3662.522,
    Fc.truncnorm.cm4[[1]][1],Fc.truncnorm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[5,]),
    xlim=c(500,1700),ylim=c(500,1700),axes=F,ann=F,xlab="",ylab="",col=3)
par(new=T)
abline(0,1)
par(new=T)
text(240,1100,"Sample Quantiles",srt=90,xpd=T)


# L-3
par(plt=c(0.2, 0.95, 0.2, 0.85))
plot(qnorm(ppoints(249),Ggfa.norm.cm3[[1]][1],Ggfa.norm.cm3[[1]][2]),
    sort(Total.ShareSeg1.cm3.From_Hap[6,]),
    xlim=c(500,1700),ylim=c(500,1700),las=1,xlab="Theoretical Quantiles",ylab="")
par(new=T)
plot(qlnorm(ppoints(249),Ggfa.lnorm.cm3[[1]][1],Ggfa.lnorm.cm3[[1]][2]),
    sort(Total.ShareSeg1.cm3.From_Hap[6,]),
    xlim=c(500,1700),ylim=c(500,1700),axes=F,ann=F,xlab="",ylab="",col=2)
par(new=T)
plot(qtruncnorm(ppoints(249),0,3662.522,
    Ggfa.truncnorm.cm3[[1]][1],Ggfa.truncnorm.cm3[[1]][2]),
    sort(Total.ShareSeg1.cm3.From_Hap[6,]),
    xlim=c(500,1700),ylim=c(500,1700),axes=F,ann=F,xlab="",ylab="",col=3)
par(new=T)
abline(0,1)
par(new=T)
text(240,1100,"Sample Quantiles",srt=90,xpd=T)


# C-4
par(plt=c(0.2, 0.95, 0.2, 0.85))
plot(qnorm(ppoints(249),Fcc.norm.cm4[[1]][1],Fcc.norm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[8,]),
    xlim=c(100,1200),ylim=c(100,1200),las=1,xlab="Theoretical Quantiles",ylab="")
par(new=T)
plot(qlnorm(ppoints(249),Fcc.lnorm.cm4[[1]][1],Fcc.lnorm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[8,]),
    xlim=c(100,1200),ylim=c(100,1200),axes=F,ann=F,xlab="",ylab="",col=2)
par(new=T)
plot(qtruncnorm(ppoints(249),0,3662.522,
    Fcc.truncnorm.cm4[[1]][1],Fcc.truncnorm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[8,]),
    xlim=c(100,1200),ylim=c(100,1200),axes=F,ann=F,xlab="",ylab="",col=3)
par(new=T)
abline(0,1)
par(new=T)
text(-110,700,"Sample Quantiles",srt=90,xpd=T)



# C-5
par(plt=c(0.2, 0.95, 0.2, 0.85))
plot(qnorm(ppoints(249),Sc.norm.cm4[[1]][1],Sc.norm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[9,]),
    xlim=c(0,600),ylim=c(0,600),las=1,xlab="Theoretical Quantiles",ylab="")
par(new=T)
plot(qlnorm(ppoints(249),Sc.lnorm.cm4[[1]][1],Sc.lnorm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[9,]),
    xlim=c(0,600),ylim=c(0,600),axes=F,ann=F,xlab="",ylab="",pch=2)
par(new=T)
plot(qtruncnorm(ppoints(249),0,3662.522,
    Sc.truncnorm.cm4[[1]][1],Sc.truncnorm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[9,]),
    xlim=c(0,600),ylim=c(0,600),axes=F,ann=F,xlab="",ylab="",pch=0)
par(new=T)
abline(0,1)
par(new=T)
text(-130,300,"Sample Quantiles",srt=90,xpd=T)


# UN
# cm=3
par(plt=c(0.2, 0.95, 0.2, 0.85))
plot(qnorm(ppoints(249),Unr.norm.cm3[[1]][1],Unr.norm.cm3[[1]][2]),
    sort(Total.ShareSeg1.cm3.From_Hap[10,]),
    xlim=c(100,300),ylim=c(100,300),las=1,xlab="Theoretical Quantiles",ylab="Sample Quantiles",cex=1)
par(new=T)
plot(qlnorm(ppoints(249),Unr.lnorm.cm3[[1]][1],Unr.lnorm.cm3[[1]][2]),
    sort(Total.ShareSeg1.cm3.From_Hap[10,]),
    xlim=c(100,300),ylim=c(100,300),axes=F,ann=F,xlab="",ylab="",col=2,cex=1)
par(new=T)
plot(qtruncnorm(ppoints(249),0,3662.522,
    Unr.truncnorm.cm3[[1]][1],Unr.truncnorm.cm3[[1]][2]),
    sort(Total.ShareSeg1.cm3.From_Hap[10,]),
    xlim=c(100,300),ylim=c(100,300),axes=F,ann=F,xlab="",ylab="",col=3,cex=1)
par(new=T)
abline(0,1)

# cm=4
par(plt=c(0.2, 0.95, 0.2, 0.85))
plot(qnorm(ppoints(249),Unr.norm.cm4[[1]][1],Unr.norm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[10,]),
    xlim=c(20,200),ylim=c(20,200),las=1,xlab="Theoretical Quantiles",ylab="Sample Quantiles",cex=1)
par(new=T)
plot(qlnorm(ppoints(249),Unr.lnorm.cm4[[1]][1],Unr.lnorm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[10,]),
    xlim=c(20,200),ylim=c(20,200),axes=F,ann=F,xlab="",ylab="",col=2,cex=1)
par(new=T)
plot(qtruncnorm(ppoints(249),0,3662.522,
    Unr.truncnorm.cm4[[1]][1],Unr.truncnorm.cm4[[1]][2]),
    sort(Total.ShareSeg1.cm4.From_Hap[10,]),
    xlim=c(20,200),ylim=c(20,200),axes=F,ann=F,xlab="",ylab="",col=3,cex=1)
par(new=T)
abline(0,1)




library(ggplot2)
library(ggpubr)
#install.packages("svglite")
library(svglite)
# Read data
cm <- read.csv("Data/amb_cam.csv",sep=";")
head(cm)

###### Indice año i con R año i+2
HCI.lag <- numeric()
SOI.lag <- numeric()
Hake.lag <- numeric()
SSBn.lag <- numeric()
SSBs.lag <- numeric()
Rn <- numeric()
Rs <- numeric()
Hake <- numeric()
HCI <- numeric()
SOI <- numeric()
SSBn <- numeric()
SSBs <- numeric()
for(i in 1:(length(cm$YY)-2)){
  HCI.lag[i] <- cm$hci[i]
  HCI[i] <- cm$hci[i+2]
  SOI[i] <- cm$soi[i+2]
  SOI.lag[i] <- cm$soi[i]
  Hake.lag[i] <- cm$MGBfour[i]
  Hake[i] <- cm$MGBfour[i+2]
  SSBn.lag[i] <- cm$SSBnorte[i]/1000
  SSBs.lag[i] <- cm$SSBsur[i]/1000
  Rn[i] <- cm$Rnorte[i+2]/1000
  Rs[i] <- cm$Rsur[i+2]/1000
  SSBn[i] <- cm$SSBnorte[i+2]/1000
  SSBs[i] <- cm$SSBsur[i+2]/1000
}

SSB=SSBn+SSBs
SSB.lag=SSBn.lag+SSBs.lag
Rt=Rn+Rs

df <- data.frame(HCI.lag,HCI,SOI.lag,SOI,Hake.lag,Hake,SSB.lag,SSB,Rt,SSBn.lag,SSBs.lag)
head(df)
library(rcompanion)
# GLM Table log(Rt) = f(SSB,Hake,HCI)
m1 <- glm(log(Rt) ~ log(SSB.lag),data=df)
summary(m1)$coeff
(1-m1$deviance/m1$null.deviance)*100
nagelkerke(m1)
AIC(m1)
m2 <- glm(log(Rt) ~ log(SSB.lag) + Hake.lag,data=df)
summary(m2)$coeff
(1-m2$deviance/m2$null.deviance)*100
nagelkerke(m2)
AIC(m2)
m3 <- glm(log(Rt) ~ log(SSB.lag) + Hake,data=df)
summary(m3)$coeff
(1-m3$deviance/m3$null.deviance)*100
AIC(m3)
nagelkerke(m3)
m4 <- glm(log(Rt) ~ log(SSB.lag) + Hake + HCI.lag,data=df)
summary(m4)$coeff
(1-m4$deviance/m4$null.deviance)*100
AIC(m4)
nagelkerke(m4)
m5 <- glm(log(Rt) ~ log(SSB.lag) + Hake + HCI,data=df)
summary(m5)$coeff
(1-m5$deviance/m5$null.deviance)*100
AIC(m5)
nagelkerke(m5)
m6 <- glm(log(Rt) ~ log(SSB.lag) + Hake + SOI.lag,data=df)
summary(m6)$coeff
(1-m6$deviance/m6$null.deviance)*100
AIC(m6)
nagelkerke(m6)
#R=aBbexp(cH)
#log(R)=log(a)+blog(B)+cH+cSOI
m7 <- glm(log(Rt) ~ log(SSB.lag) + Hake + SOI,data=df)
summary(m7)$coeff
(1-m7$deviance/m7$null.deviance)*100
AIC(m7)
nagelkerke(m7)
library(sjPlot)

Fig6a <- plot_model(m7, type="pred",terms = "SSB.lag")+
  geom_point(data=df,aes(x=SSB.lag,y=Rt),size=0.6)+
  mi.tema()+labs(x="SSB(t-2) (thousand ton)")
Fig6b <- plot_model(m7, type="pred",terms = "Hake")+
  geom_point(data=df,aes(x=Hake,y=Rt),size=0.6)+
  mi.tema()+labs(x="Hake(t) (thousand ton)", y="Recruitment (millions)")
Fig6c <- plot_model(m7, type="pred",terms = "SOI")+
  geom_point(data=df,aes(x=SOI,y=Rt),size=0.6)+
  mi.tema()+labs(x="SOI(t)")

Fig7aa <- ggarrange(Fig6a,Fig6b,Fig6c,nrow=3,ncol=1)

Fig7a <- plot_model(m5, type="pred",terms = "SSB.lag")+
  geom_point(data=df,aes(x=SSB.lag,y=Rt),size=0.6)+
  mi.tema()+labs(x="SSB(t-2) (thousand ton)")
Fig7b <- plot_model(m5, type="pred",terms = "Hake")+
  geom_point(data=df,aes(x=Hake,y=Rt),size=0.6)+
  mi.tema()+labs(x="Hake(t) (thousand ton)")
Fig7c <- plot_model(m5, type="pred",terms = "HCI")+
  geom_point(data=df,aes(x=HCI,y=Rt),size=0.6)+
  mi.tema()+labs(x="HCI(t)",y="")

Fig7bb <- ggarrange(Fig7a,Fig7b,Fig7c,nrow=3,ncol=1)

Fig7 <- ggarrange(Fig6a,Fig7a,Fig6b,Fig7b,Fig6c,Fig7c,nrow=3,ncol=2)

ggsave("Figs/Figura6_RvsGLMs.jpg",plot=Fig7,dpi=300,width = 20,height = 32,units = "cm" )
ggsave("Figs/Figura6_RvsGLMs_ok.svg",plot=Fig7,dpi=300,width = 20,height = 32,units = "cm" )
tab_model(m1,m2,m3,m4,m6,m7)



##############################################################
## Prepara la Figura 3: Tesis Camaron y Clima, Camila Sagua
############################################################
#LIBRARY_SECTION
library(ggplot2)
library(ggpubr)
theme_set(theme_bw())
library(svglite)
#LEE LOS DATOS
cm <- read.csv("Data/amb_cam.csv",sep=";")
names(cm)

## PREPARA LOS DATOS
cm$Rt <- cm$Rnorte+cm$Rsur
cm$SSBt <- cm$SSBnorte+cm$SSBsur
anomRnorte <- (log(cm$Rnorte)-mean(log(cm$Rnorte)))/sd(log(cm$Rnorte))
anomRsur <- (log(cm$Rsur)-mean(log(cm$Rsur)))/sd(log(cm$Rsur))
anomRt    <- (log(cm$Rt)-mean(log(cm$Rt)))/sd(log(cm$Rt))
anomSSBnorte <- (log(cm$SSBnorte)-mean(log(cm$SSBnorte)))/sd(log(cm$SSBnorte))
anomSSBsur <- (log(cm$SSBsur)-mean(log(cm$SSBsur)))/sd(log(cm$SSBsur))
anomSSBt   <- (log(cm$SSBt)-mean(log(cm$SSBt)))/sd(log(cm$SSBt))
anomHakeB <- (log(cm$MGBfour)-mean(log(cm$MGBfour)))/sd(log(cm$MGBfour))

Year <- rep(cm$YY,2)
#Stock <- c(rep("North",length(cm$YY)),rep("South",length(cm$YY)))
#Recruits <- data.frame(c(anomRnorte,anomRsur))
#Biomass <- data.frame(c(anomSSBnorte,anomSSBsur))
#df2 <- data.frame(cbind(Year,Stock,Recruits,Biomass))
#colnames(df2)<-c("Year","Stock","Recruits","SSB")
df2 <- data.frame(Year=cm$YY,Recruits=anomRt,SSB=anomSSBt)

Fig3a <- ggplot(data=df2,aes(x=Year,y=Recruits))+
  geom_line()+geom_hline(yintercept=0, linetype = "dashed")+
  scale_x_continuous(breaks=c(1970,1980,1990,2000,2010))+
  mi.tema()+labs(x="", y="R anomaly")+theme(legend.position = c(.85, .3))
Fig3a
Fig3b <- ggplot(data=df2,aes(x=Year,y=SSB))+
  geom_line()+geom_hline(yintercept=0, linetype = "dashed")+
  scale_x_continuous(breaks=c(1970,1980,1990,2000,2010))+
  mi.tema()+labs(x="", y="SSB anomaly")+theme(legend.position ="none")
Fig3b
Fig3c <- ggplot(data=cm,aes(x=YY,y=anomHakeB))+
  geom_line()+geom_hline(yintercept=0, linetype = "dashed")+
  scale_x_continuous(breaks=c(1970,1980,1990,2000,2010))+
  mi.tema()+labs(x="Year", y="Hake anomaly")
Fig3c

Fig3 <- ggarrange(Fig3a,Fig3b,Fig3c,labels=c("A","B","C"),nrow=3,ncol=1)
Fig3

ggsave("Figs/Figura3_R-SSB-Hake_camaron.jpg",plot=Fig3,dpi=300,width = 16,height = 22,units = "cm" )
ggsave("Figs/Figura3_R-SSB-Hake_camaron.svg",plot=Fig3,dpi=300,width = 16,height = 22,units = "cm" )

#ggsave("Figura3_Captura_AreaStudio_camaron.jpg",plot=Fig3,dpi=300)
corcruzada <- ccf(rank(df2$Recruits),rank(df2$SSB))
library(tseries)
adf.test(df2$Recruits)
dRt <- diff(df2$Recruits)
adf.test(dRt)
adf.test(df2$SSB)
dSSBt <- diff(df2$SSB)
plot(dRt)
lines(dSSBt)
ccf(dSSBt,dRt)
library(forecast)
Fig4 <- ggCcf(x=dSSBt,y=dRt,lag.max = 6)+
  mi.tema()+labs(x="Lags (years)", y="Correlation")
Fig4
ggsave("Figs/Figura4_CCF_R-SSB_camaron.jpg",plot=Fig4,dpi=300,width = 16,height = 18,units = "cm" )
ggsave("Figs/Figura4_CCF_R-SSB_camaron_ok.svg",plot=Fig4,dpi=300,width = 16,height = 18,units = "cm" )

##############################################################
##  Figura 4: Tesis Camaron y Clima, Camila Sagua
##  Correlación acumulada
############################################################
#LIBRARY_SECTION
library(ggplot2)
library(ggpubr)
library(svglite)

# Read data
cm <- read.csv("Data/amb_cam.csv",sep=";")
names(cm)

critical.r <- function(n, alpha = .05 ) {
  df <- n - 2
  critical.t <- qt( alpha/2, df, lower.tail = F )
  critical.r <- sqrt( (critical.t^2) / ( (critical.t^2) + df ) )
  return( critical.r )
}

critical.r(n=10)
## COMPUTE ANOMALIES
anomHCI <- (cm$hci-mean(cm$hci))/sd(cm$hci)
anomSOI <- (cm$soi-mean(cm$soi))/sd(cm$soi)
cm$Rt <- cm$Rnorte+cm$Rsur #Recruitment
cm$SSBt <- cm$SSBnorte+cm$SSBsur #Spawning biomass
anomRt    <- (log(cm$Rt)-mean(log(cm$Rt)))/sd(log(cm$Rt))
anomSSBt   <- (log(cm$SSBt)-mean(log(cm$SSBt)))/sd(log(cm$SSBt))
anomHakeB <- (log(cm$MGBfour)-mean(log(cm$MGBfour)))/sd(log(cm$MGBfour))
anomHakeS <- (log(cm$hakeSSB)-mean(log(cm$hakeSSB)))/sd(log(cm$hakeSSB))

## CUMULATIVE CORRELATION (lag=0)
cormovhci <- numeric()
cormovsoi <- numeric()
cormovhake <- numeric()
yrs <- numeric()
np <- length(anomRt)
rhat <- numeric()
rini <- 10
for(i in 1:(np-rini)){
  ini <- 1
  fin <- ini+i+(rini-1)
    cormovhci[i] <- cor(anomHCI[ini:fin],anomRt[ini:fin])
    cormovhake[i]<- cor(anomHakeB[ini:fin],anomRt[ini:fin])
    cormovsoi[i]<- cor(anomSOI[ini:fin],anomRt[ini:fin])
    rhat[i] <- critical.r(n=fin)
    yrs[i] <- cm$YY[fin] 
}

Indice <- c(rep("HCI",length(yrs)),rep("SOI",length(yrs)),rep("Hake",length(yrs)))
cormov <- c(cormovhci,cormovsoi,cormovhake)
dat.cormo <- data.frame(Year=yrs,Indice,cormov,rcor=rhat)
head(dat.cormo)


Fig5a <- ggplot(data=dat.cormo,aes(x=Year,y=cormov,group=Indice))+
  geom_ribbon(aes(ymin=-rcor,ymax=rcor),fill="grey90")+
  geom_line(aes(linetype=Indice))+geom_text(x=2005,y=0.8,label="",size=5)+
  geom_hline(yintercept = 0,size=0.2)+
  scale_x_continuous(breaks=c(1980,1990,2000,2010),limits = c(1978,2015))+
  mi.tema()+labs(x="", y="Correlation")+ylim(c(-1,1))+
  theme(legend.position = c(.8, .9))+theme(legend.title = element_blank())
Fig5a


####################

## CUMULATIVE CORRELATION (variables lag=2)
## Indices year i with recruitment i+2
anomHCI_t <- numeric()
anomSOI_t <- numeric()
anomHake_t <- numeric()
anomRt_t <- numeric()
anomHake_t <- numeric()
anomSt_t <- numeric()

for(i in 1:(length(cm$YY)-2)){
  anomHCI_t[i] <- anomHCI[i]
  anomSOI_t[i] <- anomSOI[i]
  anomHake_t[i] <- anomHakeB[i]
  anomSt_t[i] <- anomSSBt[i]
  anomRt_t[i] <- anomRt[i+2]
}
cormovhci_lag <- numeric()
cormovsoi_lag <- numeric()
cormovhake_lag <- numeric()
np <- length(anomRt_t)
rhat <- numeric()
YY <- numeric()
rini <- 10
for(i in 1:(np-rini)){
  ini <- 1
  fin <- ini+i+(rini-1)
  cormovhci_lag[i] <- cor(anomHCI_t[ini:fin],anomRt_t[ini:fin])
  cormovhake_lag[i]<- cor(anomHake_t[ini:fin],anomRt_t[ini:fin])
  cormovsoi_lag[i]<- cor(anomSOI_t[ini:fin],anomRt_t[ini:fin])
  rhat[i] <- critical.r(n=fin)
  YY[i] <- cm$YY[fin] 
}

Indice <- c(rep("HCI",length(YY)),rep("SOI",length(YY)),rep("Hake",length(YY)))
cormov <- c(cormovhci_lag,cormovsoi_lag,cormovhake_lag)
dat.cormo2 <- data.frame(Year=YY,Indice,cormov,rcor=rhat)
head(dat.cormo)


Fig5b <- ggplot(data=dat.cormo2,aes(x=Year,y=cormov,group=Indice))+
  geom_ribbon(aes(ymin=-rcor,ymax=rcor),fill="grey90")+
  geom_line(aes(linetype=Indice))+geom_text(x=2005,y=0.8,label="",size=5)+
  geom_hline(yintercept = 0,size=0.2)+
  scale_x_continuous(breaks=c(1980,1990,2000,2010),limits = c(1978,2015))+
  mi.tema()+labs(x="", y="Correlation")+ylim(c(-1,1))+
  theme(legend.position = "none")+theme(legend.title = element_blank())
Fig5b





Fig5 <- ggarrange(Fig5a,Fig5b,labels=c("A","B"),nrow=2,ncol=1)
Fig5

ggsave("Figs/Figura5_RvsClimate_camaron.jpg",plot=Fig5,dpi=300,width = 16,height = 32,units = "cm" )

ggsave("Figs/Figura5_RvsClimate_camaron_ok.svg",plot=Fig5,dpi=300,width = 16,height = 32,units = "cm" )


# ANALISIS SEM

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

df <- data.frame(HCI.lag,HCI,SOI.lag,SOI,Hake.lag,Hake,SSB.lag,SSB,Rt,logR=log(Rt),logSSB=log(SSB),logSSB.lag=log(SSB.lag))
head(df)

#install.packages("semPlot",dependencies=T)
library(lavaan)
library(semPlot)

#### ANALISIS PARA Tasa de Reclutamiento ####
# ZONA NORTE
names(df)
modeloA1 <-'
 HCI ~ SOI
 Hake.lag ~ HCI.lag
 logSSB.lag ~ Hake.lag
 logSSB.lag ~ HCI.lag + SOI.lag
 SSB ~ Hake
 Hake ~ HCI + SOI
 logR ~ logSSB.lag + Hake + SOI + HCI 
 '
#cor(cm2$SOI.lag,cm2$HCI.lag,cm2$SSB.lag,cm2$Hake.lag,cm2$SSB,cm2$Hake,cm2$TRn)
dt.cor <- cbind(df$SOI.lag,df$SOI,df$HCI.lag,df$HCI,df$Hake.lag,df$Hake,df$logSSB.lag,df$logSSB,df$logR)
cor.m1 <- round(cor(dt.cor),3)
cor.m1

fitmodeloA1 <- sem(model=modeloA1,data=df)
summary(fitmodeloA1,standardized=TRUE)
varTable(fitmodeloA1)
semPaths(fitmodeloA1,whatLabels="par",layout="spring")
semPaths(fitmodeloA1,whatLabels="est",layout="spring")
?semPaths

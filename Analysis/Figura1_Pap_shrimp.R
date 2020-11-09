##############################################################
## Prepara la Figura 1: Tesis Camaron y Clima, Camila Sagua
############################################################
#LIBRARY_SECTION
library(ggplot2)
library(ggpubr)
theme_set(theme_bw())
library(sf)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
#install.packages("rnaturalearthhires", dependencies = TRUE)
#library(devtools)
#install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
## LEE LOS DATOS DE CAPTURA
cap<-read.table("Data/Cap_cn1.txt", header = TRUE)
head(cap)
## LEE LOS DATOS DE CHILE 
chile <- ne_countries(scale=10,country = "Chile",returnclass = "sf")

# Area de estudio
x<-c(-74, -74,  -69.5,-69.5)
y<-c(-37.2,-25.2,-25.2,-37.2)
#x <- c(-74,-69.5,-69.5,-74)
#y <- c(-32,-32,-25.2,-25.2)
df <- data.frame(cbind(x,y))
#study_area <- st_sfc(st_polygon(list(cbind(x,y))))
#st_crs(study_area) = 4326

## TEMA
#mapa <- ggplot(data = chile) + 
#  geom_sf() + geom_line(aes(x=x,y=y),data=df) + ylab("")+xlab("")+
#  coord_sf(xlim = c(-78, -66), ylim = c(-17, -17), expand = FALSE)+
#  geom_hline(yintercept = -37.2)+
#  annotate(geom = "text", x = -75, y = -24, label = "North", color = "grey22", size = 5)+
#  annotate(geom = "text", x = -75, y = -33, label = "South", color = "grey22", size = 5)
#mapa
mapa <- ggplot(data = chile) + 
  geom_sf() +
  coord_sf(xlim = c(-78, -66), ylim = c(-57, -17), expand = FALSE)+
  geom_hline(yintercept = c(-32,-25.2,-37.2))+
  annotate(geom = "text", x = -75, y = -26, label = "North", color = "grey22", size = 5)+
  annotate(geom = "text", x = -75, y = -33, label = "South", color = "grey22", size = 5)+
  xlab("Longitude (W)") + ylab("Latitude (S)")+
  scale_x_continuous(breaks = c(-76,-72,-68))
mapa

Capturas <- ggplot(cap, aes(x=Year, y=Catch))+
  geom_col() + labs(x="Year", y="Catch (ton)")+
  facet_wrap(~Stock,ncol=1) +
  scale_x_continuous(breaks=c(1950,1960,1970,1980,1990,2000,2010))+mi.tema()

Fig1 <- ggarrange(Capturas,mapa,labels=c("A","B"),nrow=1,ncol=2)
Fig1

ggsave("Figs/Figura1_Captura_AreaStudio_camaron.jpg",plot=Fig1,dpi=300)

ggsave("Figs/Figura1_Captura_AreaStudio_camaron.svg",plot=Fig1,dpi=300)







######## ALTERNATIVA
# Function 1
PBSclr <- function(){
  PBSclr <- list(black=rgb(0,0,0,max=255),      sea=rgb(224,253,254,max=255),      land="#E4CA96",
                 red=rgb(255,0,0,max=255),      green=rgb(0,255,0,max=255),        blue=rgb(0,0,255,max=255),
                 yellow=rgb(255,0,0,max=255),   cyan=rgb(0,255,255,max=255),       magenta=rgb(255,0,255,max=255),
                 purple=rgb(150,0,150,max=255), lettuce=rgb(205,241,203,max=255),  moss=rgb(132,221,124,max=255),
                 irish=rgb(54,182,48,max=255),  forest=rgb(29,98,27,max=255),      white=rgb(255,255,255,max=255),
                 fog=rgb(223,223,223,max=255)
  )  # rgb(255,255,95,max=255)
}

#DATA_SECTION
#Lee el mapa mundial
data(wrld_simpl)
#lee arreglos
data(worldLLhigh)

#PROCEDURE_SECTION
# Arreglos
worldLLhigh2 <- worldLLhigh
worldLLhigh2$X <- worldLLhigh2$X-360
clr <- PBSclr()
names(clr)



plot(cap$Year[cap$Stock=="North"],cap$Catch[cap$Stock=="North"],type="h",lwd=4, lend=1, las=1,ylim=c(0,12000),ylab="Catch (ton)")
plot(cap$Year[cap$Stock=="South"],cap$Catch[cap$Stock=="South"],type="h",lwd=4, lend=1, las=1,ylim=c(0,12000),ylab="Catch (ton)")



par(mar=c(3,4,1,1),mgp=c(2,0.5,0))
layout.matrix <- matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE)
layout(mat = layout.matrix)
plot(cap$Year[cap$Stock=="North"],cap$Catch[cap$Stock=="North"],type="h",lwd=4, lend=1, las=1,ylim=c(0,12000),ylab="Catch (ton)")
plot(cap$Year[cap$Stock=="South"],cap$Catch[cap$Stock=="South"],type="h",lwd=4, lend=1, las=1,ylim=c(0,12000),ylab="Catch (ton)")

plotMap(worldLLhigh2, col="grey",bg="white",ylim=c(-39,-25.19),xlim=c(-76,-70),las=1)
points(-72.24,-23.38,pch=19,cex=1,col="black")
points(-71.36,-33.02,pch=19,cex=1,col="black")
abline(h=-32)
text(-73,-26,"North",cex=2)
text(-74.5,-36,"South",cex=2)
text(-74.5,-33.02,"Valparaíso",pos=4,cex=0.9,col="black")
text(-75,-23.38,"Antofagasta",pos=4,cex=0.9,col="black")




#MAPA 1
plot(wrld_simpl,xlim=c(-80,-60),ylim=c(-60,-16),axes=F,col="grey")
x<-c(-74,-74,-68,-68,-74)
y<-c(-39,-25.19,-25.19,-39,-39)
lines(x,y,lwd=2)
#MAPA 2
plotMap(worldLLhigh2, col="grey",bg="white",ylim=c(-39,-25.19),xlim=c(-76,-70),las=1)
points(-72.24,-23.38,pch=19,cex=1,col="black")
points(-71.36,-33.02,pch=19,cex=1,col="black")
abline(h=-32)
text(-73,-26,"North",cex=2)
text(-74.5,-36,"South",cex=2)
text(-74.5,-33.02,"Valparaíso",pos=4,cex=0.9,col="black")
text(-75,-23.38,"Antofagasta",pos=4,cex=0.9,col="black")

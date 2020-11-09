##############################################################
##  Figura 2: Tesis Camaron y Clima, Camila Sagua
############################################################
#LIBRARY_SECTION
library(ggplot2)
library(ggpubr)
library(svglite)
# PREPARA LOS DATOS
index<-read.table("Data/amb_cam.txt", header = TRUE)
head(index)
Year <- rep(index$YY,2)
HCI <- index$hci
SOI <- index$soi
Indices <- c(rep("HCI",length(index$hci)),rep("SOI",length(index$hci)))
Values <- data.frame(c(HCI,SOI))

df <- data.frame(cbind(Year,Indices,Values))
colnames(df)<-c("Year","Indices","Values")

# Tema
mi.tema <- function (base_size = 14, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.title.x = element_text(margin = margin(10,0,0,0)),
          #axis.title.x = element_text(vjust = -1.5),
          #axis.title.y = element_text(margin = margin(0,20,0,0)),
          #axis.title.y = element_text(vjust = -0.1),
          axis.text = element_text(size = rel(0.8)),
          axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey50"),
          panel.grid.major = element_line(colour = "grey90", size = 0.2),
          panel.grid.minor = element_line(colour = "grey98", size = 0.5),
          strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2))
}



## FIGURA
Fig2 <- ggplot(df, aes(x=Year, y=Values))+ geom_line() + labs(x="Year", y="Indices")+facet_wrap(~Indices,ncol=1) + geom_hline(yintercept=0, linetype = "dashed")+ scale_x_continuous(breaks=c(1970,1980,1990,2000,2010))+mi.tema()

Fig2
ggsave("Figs/Figura2_IndicesClimaticos_camaron.jpg",plot=Fig2,dpi=300,width = 16,height = 22,units = "cm" )

ggsave("Figs/Figura2_IndicesClimaticos_camaron.svg",plot=Fig2,dpi=300,width = 16,height = 22,units = "cm" )



##################


IHCI<-ggplot(index, aes(x=YY, y=hci))+ geom_line(aes(x=YY, y=hci)) + geom_hline(yintercept=0, linetype = "dashed") + labs(title="Humboldt Current Index (HCI)", x="Year", y="Anomalie")
IHCI + scale_y_continuous(limit = c(-2.5,2.5)) + scale_x_continuous(limits = c(1968, 2018)) + theme(plot.title=element_text(size=15, face="bold"), 
                                                                                                    axis.text.x=element_text(size=10), 
                                                                                                    axis.text.y=element_text(size=10),
                                                                                                    axis.title.x=element_text(size=12),
                                                                                                    axis.title.y=element_text(size=12))

ISOI<-ggplot(index, aes(x=YY, y=soi))+ geom_line(aes(x=YY, y=soi)) + geom_hline(yintercept=0, linetype = "dashed") + labs(title="Southern Oscillation Index (SOI)", x="Year", y="Anomalie")
ISOI + scale_y_continuous(limit = c(-2.5,2.5)) + scale_x_continuous(limits = c(1968, 2018)) + theme(plot.title=element_text(size=15, face="bold"), 
                                                                                                    axis.text.x=element_text(size=10), 
                                                                                                    axis.text.y=element_text(size=10),
                                                                                                    axis.title.x=element_text(size=12),
                                                                                                    axis.title.y=element_text(size=12))  

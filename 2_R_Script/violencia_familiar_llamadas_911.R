#library 
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(lubridate)
library(ggrepel)

#data import & transformation
df <- read.csv("violencia_familiar_llamadas_911.csv", 
               colClasses = c("character","character","character",
                              "character","character","character",
                              "numeric","numeric","numeric","numeric",
                              "numeric","numeric","numeric"),
               header = TRUE)

df1 <- df %>% 
  mutate_at(vars(fecha), as.Date, format="%Y-%m-%d")

df2<- df%>%
  filter(cve_entidad==33)

#plot absolutos nacional comparativo 2019,2020
a<-seq(45000,75000,by=5000)
abs_nac<- df2 %>%
  ggplot(aes(x=cve_mes, y=vio_fam_911_mensual, group=año,label = vio_fam_911_mensual))+
  geom_line(aes(color=año), size=1)+
  #geom_point(aes(color=año),alpha=0.5, size=3)+
  #geom_text(aes(color=año),vjust=-0.95,hjust=-0.3,size=3)+
  geom_label(aes(color=año),size=3, show.legend = F)+
  scale_color_manual(values=c("deeppink3", "mediumorchid"))+
  labs(x="",
       y="",
       title ="Llamadas de emergencia relacionadas con incidentes de violencia familiar",
       subtitle ="Nacional 2019 - 2020",
       caption = "Elaborado por: Marcela Dolores Guerra Osorno | @osornomarcela \nFuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública")+
  theme_light() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.title=element_text(size=25, face="bold", colour="mediumorchid4",
                                margin=margin(20,0,5,0)),
        plot.subtitle=element_text(size=20, face="bold", color="black",
                                   margin=margin(0,0,20,0)),
        plot.caption =element_text(size=10, color="black"),
        axis.text.x=element_text(size= 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) +
  scale_x_discrete(labels=c("A"="Enero","B"="Febrero","C"="Marzo",
                            "D"="Abril","E"="Mayo","F"="Junio",
                            "G"="Julio","H"="Agosto","I"="Septiembre",
                            "J"="Octubre","K"="Noviembre","L"="Diciembre"))+
  scale_y_continuous(breaks = a,limits = c(45000,75000))

abs_nac

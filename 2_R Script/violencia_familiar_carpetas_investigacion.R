#library 
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(lubridate)
library(ggrepel)

#data import & transformation
df <- read.csv("violencia_familiar.csv", 
               colClasses = c("character","character","character",
                              "character","numeric","numeric",
                              "numeric","numeric","numeric","numeric"),
               header = TRUE)

df1<- df %>%
  filter(año %in% c(2018,2019,2020))

df2 <- df %>% 
  mutate_at(vars(fecha), as.Date, format="%Y-%m-%d")
str(df2)

#plot tendencia absoluta estatal: Veracruz 2015-2020
a<-seq(0,950,by=100)
p <- ggplot(df2, aes(x=fecha, y=viol_fam_ver, label=viol_fam_ver)) +
  geom_line(color="mediumorchid4")+
  geom_point() +
  labs(x="",
       y="",
       title ="Presuntos delitos de violencia familiar: Tendencia estatal",
       subtitle ="Veracruz 2015 - 2020",
       caption = "Elaborado por: Marcela Dolores Guerra Osorno | @osornomarcela \nFuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública")+
  theme_ipsum() +
  theme(plot.title=element_text(size=20, face="bold", colour="mediumorchid4", vjust=-1),
        plot.subtitle=element_text(size=18, face="bold", color="black"),
        plot.caption =element_text(size=12, color="black"),
        axis.text.y=element_text(size= 9),
        axis.text.x=element_text(size= 8, angle=90,vjust = 2.2)) +
  scale_x_date(date_breaks = "1 month",limits = as.Date(c("2015-01-31","2020-02-29")),
               date_labels = "%b %y") +
  scale_y_continuous(breaks = a)

p

#plot tasas veracruz comparativo 2018,2019,2020
b<-seq(0,12,by=1)
tasa_ver<- df1 %>%
  ggplot( aes(x=month, y=tasa_ver, group=año,label = tasa_ver)) +
  geom_line(aes(color=año), size=1)+
  #geom_point(aes(color=año),alpha=0.5, size=3)+
  #geom_text(aes(color=año),vjust=-0.95,hjust=-0.3,size=3)+
  geom_label(aes(color=año),size=3, show.legend = F)+
  scale_color_manual(values=c("deeppink3", "seagreen4", "mediumorchid"))+
  labs(x="",
       y="",
       title ="Presuntos delitos de violencia familiar por cada 100 mil habitantes",
       subtitle ="Veracruz 2018 - 2020",
       caption = "Elaborado por: Marcela Dolores Guerra Osorno | @osornomarcela \nFuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública")+
  theme_light() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.title=element_text(size=25, face="bold", colour="mediumorchid4",
                                margin=margin(20,0,5,0)),
        plot.subtitle=element_text(size=20, face="bold", color="black",
                                   margin=margin(0,0,20,0)),
        plot.caption =element_text(size=10, color="black"),
        axis.text.x=element_text(size= 10),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) +
  scale_x_discrete(labels=c("A"="Enero","B"="Febrero","C"="Marzo",
                            "D"="Abril","E"="Mayo","F"="Junio",
                            "G"="Julio","H"="Agosto","I"="Septiembre",
                            "J"="Octubre","K"="Noviembre","L"="Diciembre"))+
  scale_y_continuous(breaks = b)

tasa_ver

#plot absolutos veracruz comparativo 2018,2019,2020
c<-seq(0,1000,by=100)
abs_ver<- df1 %>%
  ggplot( aes(x=month, y=viol_fam_ver, group=año,label = viol_fam_ver)) +
  geom_line(aes(color=año), size=1)+
  #geom_point(aes(color=año),alpha=0.5, size=3)+
  #geom_text(aes(color=año),vjust=-0.95,hjust=-0.3,size=3)+
  geom_label(aes(color=año),size=3, show.legend = F)+
  scale_color_manual(values=c("deeppink3", "seagreen4", "mediumorchid"))+
  labs(x="",
       y="",
       title ="Presuntos delitos de violencia familiar",
       subtitle ="Veracruz 2018 - 2020",
       caption = "Elaborado por: Marcela Dolores Guerra Osorno | @osornomarcela \nFuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública")+
  theme_light() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.title=element_text(size=25, face="bold", colour="mediumorchid4",
                                margin=margin(20,0,5,0)),
        plot.subtitle=element_text(size=20, face="bold", color="black",
                                   margin=margin(0,0,20,0)),
        plot.caption =element_text(size=10, color="black"),
        axis.text.x=element_text(size= 10),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) +
  scale_x_discrete(labels=c("A"="Enero","B"="Febrero","C"="Marzo",
                            "D"="Abril","E"="Mayo","F"="Junio",
                            "G"="Julio","H"="Agosto","I"="Septiembre",
                            "J"="Octubre","K"="Noviembre","L"="Diciembre"))+
  scale_y_continuous(breaks = c)


abs_ver

#plot absolutos nacional comparativo 2018,2019,2020
d<-seq(0,20000,by=1000)
abs_nac<- df1 %>%
  ggplot( aes(x=month, y=viol_fam_nac, group=año,label = viol_fam_nac)) +
  geom_line(aes(color=año), size=1)+
  #geom_point(aes(color=año),alpha=0.5, size=3)+
  #geom_text(aes(color=año),vjust=-0.95,hjust=-0.3,size=3)+
  geom_label(aes(color=año),size=3, show.legend = F)+
  scale_color_manual(values=c("deeppink3", "seagreen4", "mediumorchid"))+
  labs(x="",
       y="",
       title ="Presuntos delitos de violencia familiar",
       subtitle ="Nacional 2018 - 2020",
       caption = "Elaborado por: Marcela Dolores Guerra Osorno | @osornomarcela \nFuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública")+
  theme_light() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.title=element_text(size=25, face="bold", colour="mediumorchid4",
                                margin=margin(20,0,5,0)),
        plot.subtitle=element_text(size=20, face="bold", color="black",
                                   margin=margin(0,0,20,0)),
        plot.caption =element_text(size=10, color="black"),
        axis.text.x=element_text(size= 10),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) +
  scale_x_discrete(labels=c("A"="Enero","B"="Febrero","C"="Marzo",
                            "D"="Abril","E"="Mayo","F"="Junio",
                            "G"="Julio","H"="Agosto","I"="Septiembre",
                            "J"="Octubre","K"="Noviembre","L"="Diciembre"))+
  scale_y_continuous(breaks = d, limits = c(12000,20000))

abs_nac

#plot tasas nacional
e<-seq(0,16,by=0.5)
tasa_nac<- df1 %>%
  ggplot( aes(x=month, y=tasa_nac, group=año,label = tasa_nac)) +
  geom_line(aes(color=año), size=1)+
  #geom_point(aes(color=año),alpha=0.5, size=3)+
  #geom_text(aes(color=año),vjust=-0.95,hjust=-0.3,size=3)+
  #geom_label(aes(color=año),size=3, show.legend = F)+
  geom_label_repel(aes(color=año),size=3, box.padding=0.03, show.legend = F)+
  scale_color_manual(values=c("deeppink3", "seagreen4", "mediumorchid"))+
  labs(x="",
       y="",
       title ="Presuntos delitos de violencia familiar por cada 100 mil habitantes",
       subtitle ="Nacional 2018 - 2020",
       caption = "Elaborado por: Marcela Dolores Guerra Osorno | @osornomarcela \nFuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública")+
  theme_light() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.title=element_text(size=25, face="bold", colour="mediumorchid4",
                                margin=margin(20,0,5,0)),
        plot.subtitle=element_text(size=20, face="bold", color="black",
                                   margin=margin(0,0,20,0)),
        plot.caption =element_text(size=10, color="black"),
        axis.text.x=element_text(size= 10),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) +
  scale_x_discrete(labels=c("A"="Enero","B"="Febrero","C"="Marzo",
                            "D"="Abril","E"="Mayo","F"="Junio",
                            "G"="Julio","H"="Agosto","I"="Septiembre",
                            "J"="Octubre","K"="Noviembre","L"="Diciembre"))+
  scale_y_continuous(breaks = e)

tasa_nac

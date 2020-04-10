#library 
library(tidyverse)

#data import & transformation
df <- read.csv("violencia_familiar_salud.csv",
               header = TRUE)

df1<- df%>%
  filter(entidad=="NACIONAL")

#plot absolutos nacional 2019
a<-seq(0,12000,by=1000)
ggplot(df1, aes(x=cve_mes, y=paciente_mujer)) +
  geom_segment( aes(x=cve_mes, xend=cve_mes, y=0, yend=paciente_mujer), color="skyblue", size=1) +
  geom_point( size=5, color="mediumorchid3", fill=alpha("navy", 0.3), alpha=0.5, shape=21, stroke=3)+
  labs(x="",
       y="",
       title ="Mujeres con lesiones por violencia familiar en vivienda",
       subtitle ="Nacional 2019 | Total al año: 101,977",
       caption = "Elaborado por: Marcela Dolores Guerra Osorno | @osornomarcela \nFuente: Dirección General de Información en Salud")+
  theme_minimal() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.title=element_text(size=28, face="bold", colour="mediumorchid4",
                                margin=margin(20,0,5,0)),
        plot.subtitle=element_text(size=20, face="bold", color="black",
                                   margin=margin(0,0,20,0)),
        plot.caption =element_text(size=10, color="black"),
        axis.text.x=element_text(size= 12),
        axis.text.y = element_text(size = 12),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels=c("A"="Enero","B"="Febrero","C"="Marzo",
                            "D"="Abril","E"="Mayo","F"="Junio",
                            "G"="Julio","H"="Agosto","I"="Septiembre",
                            "J"="Octubre","K"="Noviembre","L"="Diciembre"))+
  scale_y_continuous(breaks = a)

#extra info
suma<-aggregate(df$paciente_mujer, by=list(entidad=df$entidad), FUN=sum)
pob_muj<- df%>%
  filter(mes=="Enero")%>%
  select(entidad,pob_muj_mit_año)
new<-inner_join(suma,pob_muj,by="entidad")%>%
  rename(total_pac_muj=x)%>%
  mutate(tasa_pac_muj = ((total_pac_muj*100000)/pob_muj_mit_año))%>%
new<-new[-18,]

#barplot
new %>%
  arrange(tasa_pac_muj) %>%
  mutate(entidad=factor(entidad,entidad)) %>%
  ggplot( aes(x=entidad, y=tasa_pac_muj)) +
  geom_bar(stat="identity", fill="#69b3a2", width =0.7) +
  coord_flip() +
  labs(x="",
       y="",
       title ="Mujeres con lesiones por violencia familiar en vivienda 2019",
       subtitle ="Por cada 100 mil mujeres",
       caption = "Elaborado por: Marcela Dolores Guerra Osorno | @osornomarcela \nFuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública")+
  theme_minimal() +
  theme(
    plot.margin = margin(0, 0.5, 0.2, 0, "cm"),
    plot.title=element_text(size=25, face="bold", colour="#69b3a2",
                            margin=margin(15,0,5,0)),
    plot.subtitle=element_text(size=20, face="bold", color="black",
                               margin=margin(0,0,15,0)),
    plot.caption =element_text(size=10, color="black"),
    axis.text.x=element_text(size= 12),
    axis.text.y = element_text(size = 10),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") +
  ylab("")

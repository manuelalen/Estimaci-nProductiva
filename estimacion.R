############################################################
#                Producción Necesaria                      #
#                                                          #
#                                                          #
############################################################

#Librerias
library(ggplot2)
library(extrafont)
library(tidyverse)  #Para el procesamiento de datos
library(ggridges)   #Para introducir las ridgelines
library(readxl)

#Obtención de Datos (E de la ETL)
setwd("[FOLDER]")
produccion <- read_excel('Estimacion_Produccion.xlsx',
                            col_names = TRUE)

setwd("C:/Users/Usuario/Documents/AprenderR/meta-analisis")
cargas <- read_excel('LOAD_COUNTRY.xlsx',
                         col_names = TRUE)

#Transformación de datos (T de ETL)
produccion_diaria<-produccion$Produccion
demanda_diaria<-produccion$Demanda
inicial<- 35
unidades_necesarias<- 0
caducados<-produccion$Caducados



df_produccion <- data.frame()
for (i in 1:length(produccion$Dia)){
  evolucion_produccion <- produccion_diaria[i] + produccion_diaria[i-1] - caducados[i]
  (demanda_diaria[i] - evolucion_produccion) -> unidades_necesarias
  list(unidades_necesarias) 

  df_produccion <- rbind(df_produccion,unidades_necesarias)
}

#Carga de datos (L de ETL)
colnames(df_produccion)<-c("Unidades_Productivas_Necesarias")

df_produccion <- df_produccion %>% 
  mutate(mycolor = ifelse(Unidades_Productivas_Necesarias>0, "lightpink2", "lightgreen"))

#Graficando Resultados.
ggplot(df_produccion,aes(x=(1:30),y=Unidades_Productivas_Necesarias))+
  geom_bar(stat = "identity",fill=df_produccion$mycolor)+
  theme_light() +
  theme(text = element_text(family = "Tahoma"),
        panel.background = element_rect(color = "white", # Border color
                                        size = 1, fill = "#FFFFFF"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "mono", face = "bold",hjust = 0.5,size = 25),
        plot.subtitle = element_text(family = "mono",hjust = 0.5),
        plot.title.position = "plot" ,
        panel.border = element_blank())+
  labs(title = "Estimación de la Producción Necesaria por día", 
       y="Producción",x="Día",
       subtitle = "Autor: Manuel Alén")

  


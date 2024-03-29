
######PRUEBAAAAA######


library(rgdal)
library(leaflet)
library(dplyr)
library(readxl)
library(htmltools)
library(htmlwidgets)
#Cargamos la bbdd lista del pueblo
LP <- read_excel("C:/Users/ricar/Desktop/Semestre 3/Nucleo investigaci�n/LP-INN RM.xlsx")
LP <- LP[-c(1),-c(1,5)]
LP <- transform(LP, ...4 = as.numeric(...4))

LP <- LP %>% 
  mutate(porcentaje = LP$...4 * 100)

# Trabajo de las separaciones de comuna
comunas <- readOGR("COMUNA/COMUNAS_2020.shp") #se carga el shp
comunas_santiago <- subset(comunas, REGION == "Metropolitana de Santiago")  #filtramos solo comunas de santiago
#cambio de nombres
comunas_santiago$COMUNA[3] <- "San Jos� de Maipo"
comunas_santiago$COMUNA[6] <- "Alhu�"
comunas_santiago$COMUNA[7] <- "Curacav�"
comunas_santiago$COMUNA[8] <- "Mar�a Pinto"
comunas_santiago$COMUNA[20] <- "Conchal�"
comunas_santiago$COMUNA[26] <-  "�u�oa"
comunas_santiago$COMUNA[29] <- "Estaci�n Central"
comunas_santiago$COMUNA[30] <- "Maip�"
comunas_santiago$COMUNA[34] <- "San Joaqu�n"
comunas_santiago$COMUNA[37] <- "Pe�alol�n"
comunas_santiago$COMUNA[41] <- "San Ram�n"
comunas_santiago$COMUNA[51] <- "Pe�aflor"

LP <- LP[order(match(LP$...2, comunas_santiago$COMUNA)),] #ordenamos las comunas de la bdd para que hagan match con las del shp

#is.element(comunas_santiago$COMUNA, LP$...2) #para ver que las comunas sean iguales en ambos

bins <- c(0, 5, 10, 15, 20, 25, 30, 35, 40) #generamos los rangos
pal <- colorBin("Reds", domain = LP$porcentaje, bins = bins) #Colores, funcion que retorna un color

labels <- paste("<p>", LP$...2, "</p>",
                "<p>", "Porcentaje de votos LP", round(LP$porcentaje, digits = 1), "</p>",
                sep = " ")






#creamos el mapa
m2 <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%  #a�adimos el formato del mapa, Stamen.Toner puede ser
  setView(lng=-70.6312, lat = -33.55, zoom=10) %>%  #Fijamos las coordenadas y zoom de que queremos mostrar
  addPolygons(data = comunas_santiago,     #a�adimos los poligonos de comunas de santiago del shp
              weight = 1,
              smoothFactor = 0.5,
              color = "black",
              fillOpacity = 0.8,
              fillColor = pal(LP$porcentaje),
              highlight = highlightOptions(
                weight = 5,
                bringToFront = TRUE),
              label = lapply(labels, HTML)) %>%
  addLegend(pal = pal,
            values = LP$porcentaje,
            opacity = 0.7,
            position = "topright") %>%
  addLabelOnlyMarkers(lng = -70.3921, lat = -33.3, label =  LP$...2[1],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) %>%
  addLabelOnlyMarkers(lng = -70.34, lat = -33.35, label =  LP$...2[2],
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)) 


m2

#Cerrillos, Cerro Navia, Conchal�, El Bosque, Estaci�n Central, Huechuraba, Independencia, 
#La Cisterna, La Florida, La Granja, La Pintana, La Reina, Las Condes,  Lo Espejo,
#Lo Prado, Macul, Maip�, �u�oa, Pedro Aguirre Cerda, Pe�alol�n, Providencia, Pudahuel, Quilicura, 
#Quinta Normal, Recoleta, Renca, San Joaqu�n, San Miguel, San Ram�n, Santiago y Vitacura
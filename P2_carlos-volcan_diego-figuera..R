#Autores: Carlos Volcan y Diego Figuera
#Proposito: limpiar las bases de datos y exportarlas limpias, además de generar los graficos

#1. Cargo las librerías a usar: ------------------------
library(tidyverse)
#Esta sirve para temas en los graficos
library(ggthemes)

library(readxl)

#2. Cargo las bases de datos: ------------------------
#Base de PIB para todos los paises de Our World in Data
gdp <- read_excel("base_sucia.xlsx", sheet = "gdp")

#Base de capacidad de compra de importaciones para todos los paises del World Bank
importcapacity_s <- read_excel("base_sucia.xlsx", sheet = "importcapacity")

#Base de datos con datos de 
pibeduc_s <- read_excel("base_sucia.xlsx", sheet = "pwt100")



#3. Limpieza y generacion de graficos: ------------------------

#3.1 Grafico 1, linea del tiempo de crecimiento economico de Corea del Sur y USA

#Filtro: La base de Our World In Data viene ya limpia, entonces calculo la variable de mi interes
#El crecimiento economico:
gdp <- gdp %>% 
  #Agrupo por pais
  group_by(Entity) %>%
  #Genero una nueva columna en la tabla que es el crecimiento economico
  mutate(growth = (GDP-lag(GDP))/lag(GDP)*100) 


#Hago el grafico 
gdp %>% 
  #Filtro por los paises que hare el analisis, Corea y EE.UU.
  filter(Entity %in% c("South Korea", "United States") & Year %in% c(1960:2020)) %>%
  #Uso GGPLOT para hacer el grafico
  ggplot(aes(x = Year, y = growth, color = Entity)) +
  geom_line(size = 1.3) + 
  geom_point(size = 2) +
  #Inserto una linea vertical donde hubo un cambio relevante: se murio el presidente Park
  geom_vline(xintercept = 1980, linetype = "dashed", color = "grey", size = 1) +
  #Cambio ejes y titulos
  xlab("Año") + 
  ylab("Crecimiento económico") +
  ggtitle("Crecimiento económico anual de Corea del Sur y Estados Unidos") +
  guides(color = guide_legend(title = "Países")) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.position="bottom",
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"))+
  #Inserto un cuadro de texto para identificar el significado de la linea vvertical
  geom_label(aes(x = 1980, y = -4, label ="Muerte de Park"),
            size = 5, color = "black", family = "Times New Roman") +
  #Pongo unos colores bonitos
  scale_color_tableau("Classic Cyclic") 
  

#Grafico 2, Histograma entre ambos
#Como es la misma variable, ya esta limpia su base
gdp%>% 
  #Filtro por los paises y periodo 
  filter(Entity %in% c("South Korea", "United States") & Year %in% c(1960:2020)) %>%
  #GGPLOT para el grafico
  ggplot(aes(growth, fill = Entity,)) +
  #Escala bonita
  scale_fill_tableau("Classic Cyclic") +
  #Histograma
  geom_histogram(bins = 10, color = "grey") +
  #Cambio de ejes y titulo
  xlab("Crecimiento económico") + 
  ylab("Frecuencia") +
  ggtitle("Histograma del crecimiento anual") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.position="none",
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold")) +
  #Colocar el grafico de una manera comparable
  facet_grid(.~Entity)


#Grafico 3, evolucion de la capacidad de compra
#Esta base corresponde a la del worldbank y si esta sucia
importcapacity <- importcapacity_s %>% 
  #Elimino las columnas que no me interesan
  select(-c("Country Code", "Indicator Name", "Indicator Code")) %>%
  #Coloco en formato tidy
  pivot_longer(cols = -"Country Name", 
               names_to = "ano",
               values_to = "importcapacity") 
  #Reemplazo nombres de columnas por unos mas faciles
  colnames(importcapacity) <- c("pais", "ano", "importc")
  #La variable ano esta en caracter y la coloco numerico
  importcapacity$ano <- as.numeric(importcapacity$ano)

#Hago el grafico
importcapacity %>% 
  #Me quedo con los paises que quiero
  filter(pais == "Korea, Rep.") %>%
  #GGPLOT para el grafico
  ggplot(aes(x = ano, y = importc/10^14)) +
  geom_line(aes(color = "#31A1B3" ), size = 1.3) + 
  geom_point(aes(color = "#31A1B3"), size = 2) +
  #Cambio de ejes
  xlab("Año") + 
  ylab("Exportaciones") +
  ggtitle("Poder de compra de importaciones")+
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.position="none",
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold")) +
  #Color
  scale_color_manual(values = "#31A1B3")


#Grafico 4, PIB per capita a traves del tiempo y stock de capital
#Esta base ya estaba en formato tidy, por lo que solo me quedo con las columnas que me importan
pibeduc <- pibeduc_s %>%
  select(c(country, cn, rgdpe, pop, year))
#cn -> capital nominal
#rgdpe -> PIB Real
#pop -> poblacion

#Hago grafico
pibeduc%>%
  #Filtro por los paises que me importan, esta vez quiero comparar con Venezuela
  filter(country %in% c("Republic of Korea", "Venezuela (Bolivarian Republic of)")) %>%
  #GGPLOT para graficos, otra vez
  ggplot(aes(x = year, y = rgdpe, color = country, size = cn/10^6 )) +
  geom_point() +
  #Colores bonitos
  scale_color_tableau("Classic Cyclic") +
  #Modifico ejes y titulo
  xlab("Año") + 
  ylab("PIB Real per cápita") +
  ggtitle("PIB Real per cápita para Corea del Sur y Venezuela")+
  theme(plot.title = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold")) +
  #Modifico leyenda
  labs(size = "Stock de capital", col = "País")

#4. Descripcion estadistica con summary: ---------

#4.1. Resumen del PIB de USA y Corea del Sur:
#PIB USA
gdp %>% 
  #Agrupo por pais y ano
  filter(Entity == "United States" & Year %in% c(1960:2020)) %>%
  select(GDP) %>%
  summary(GDP)

#PIB Corea
gdp%>%
  filter(Entity == "South Korea" & Year %in% c(1960:2020)) %>%
  select(GDP) %>%
  summary(GDP)

#Capacidad de compra
importcapacity %>% 
  #Me quedo con los paises que quiero
  filter(pais == "Korea, Rep.") %>%
  select(importc) %>%
  summary(importc) 

#Capital nominal corea
pibeduc%>%
  #Filtro por los paises que me importan
  filter(country %in% c("Republic of Korea")) %>%
  select(cn) %>%
  summary(cn)

#Capital nominal venezuela
pibeduc%>%
  #Filtro por los paises que me importan
  filter(country %in% c("Venezuela (Bolivarian Republic of)")) %>%
  select(cn) %>%
  summary(cn)

#5. Exporto la base limpia -------------
#Datos a guardar
guardar <- list(gdp, importcapacity, pibeduc)

#Los guardo en una hoja creada con write.xlsx
write_xlsx(guardar[[1]], "base_limpia1.xlsx")
write_xlsx(guardar[[2]], "base_limpia2.xlsx")
write_xlsx(guardar[[3]], "base_limpia3.xlsx")

#Pego manualmente las tres hojas. PREGUNTA: HAY UNA MEJORAR MANERA DE HACERLO?
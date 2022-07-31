# TALLER-FINAL
## taller final
## Claudia Garzon Diego Villaizan
library( ggplot2)
library(gapminder)
library(patchwork)
##1.1
Calificacion <- 15

if(Calificacion >= 0 & Calificacion < 10 ) {
  print("¡hay mucho que mejorar!")
} else if (Calificacion >=10 & Calificacion < 20 ){
  print("¡Bien! Pero podria ser excelente." )
} else if (Calificacion >= 20 & Calificacion <= 30){
  print("¡Excelente servicio! sigue asi")
} 


##1.2 
sistemacalifica <- function(calidad=10, servicio=15, decoracion=5){
  sc <- calidad + servicio + decoracion
  return(sc)
}
## punto 2
library(tidyverse)
library(readr)
price_ratings <- read_csv("~/price_ratings.csv")
restaurant_locations <- read_csv("~/restaurant_locations.csv")






## 2.2
datoprecios <- price_ratings %>%
  pivot_wider(names_from = Variable, values_from = `Valor;;`)
## 2.3
dacompletos <- datoprecios %>%
  inner_join(restaurant_locations, by = c("Id"= "Id_restaurant"))
## 2.4

dacompletos2 <- dacompletos %>%
  arrange(desc(Price))%>%
  select(c(Restaurant, Price, Service))
## la comida mas cara se encuentra en el restaurante harry cipriani con un precio de 65 aunque en el restaurante san domenico tiene un precio del 65 y un servicio mas alto y el mas barato es la marca con un precio de 19

## punto 3
reg <- lm(Price ~ Food + Decor + Service + `East;;`, data = dacompletos )
summary(reg)
## podemos concluir que el r cuadrado es del 62% por lo tanto no es tan aceptable para explicar el modelo
## la variable de servicio no tiene significancia respecto al precio
## en este caso la variable east es la que tiene mas significancia debido a que por cada que aumente el precio en un una unidad la vaiable east aumentara en 2.091

## punto 4

ggplot(dacompletos, aes(Service, linetype= factor(`East;;`)))+
  geom_density()+
  labs( 
    title = "grafico densidad",
        x= "Servicio",
        y= "Densidad",
    linetype= "este"
    
    
    )

## se puede concluir que la zona de Manhattan este tiene mejor servicio en el este

setwd("D:/OneDrive - Universidad Peruana de Ciencias/Documents/Carrera/2023-1/Fundamentos Data Science/TP/Trabajo-Parcial-de-Data-Science/src/hotel_bookings")
rm(list = ls())

datos <- read.csv("hotel_bookings.csv", sep = ",")
View(datos)
complete.cases(datos)

summary(datos)
#Eliminamos las columnas que no vamos a necesitar

datos$meal <- NULL
datos$arrival_date_week_number <- NULL
datos$adults <- NULL
datos$children <- NULL
datos$babies <- NULL
datos$market_segment <- NULL
datos$booking_changes <- NULL
datos$deposit_type <- NULL
datos$agent <- NULL
datos$agent <- NULL
datos$reservation_status <- NULL
datos$reservation_status_date <- NULL

# Visualizamos los paises por hotel


## Creacion de tabla con columnas Hotel y Pais 

paises_top <- table(hotel = datos$hotel, country = datos$country )

## Filtrado de los datos

paises_filter <- paises_top[,(paises_top[2,] > 1000 & paises_top[1,] > 1000)]
paises_filter
paises_names <- names(paises_filter[1,])
paises_names

## Visualizacion paises top por tipo de hotel
barplot(paises_filter,main="Países con mayor afluencia por tipo de hotel",
        col=c("lightgray","lightpink"), 
        names = c(paises_names), 
        ylab = "Frecuencia", xlab = "Paises", ylim = c(0,50000))

legend("topleft", legend=c("Resort","City"), fill = c("lightgray","lightpink"), bty = "n")

# Visualizacion afluencia en fines de semana y dias de semana por tipo de hotel

## Instalamos la libreria necesaria
library(ggplot2)
library(reshape2)
library(scales)

## Extraemos la data
datos.hotel.stay <- datos[, c("hotel", "stays_in_week_nights", "stays_in_weekend_nights")]

View(datos.hotel.stay)
## Convertir las variables numéricas a factor
datos.hotel.stay$hotel <- as.factor(datos.hotel.stay$hotel)

summary(datos.hotel.stay)
## Convertir los datos a formato "largo" para poder usarlos con ggplot2
df_sum <- aggregate(cbind(stays_in_week_nights, stays_in_weekend_nights) ~ hotel, data = datos.hotel.stay, FUN = sum)

## Convertir los datos a formato "largo" para poder usarlos con ggplot2
df_long <- melt(df_sum, id.vars = "hotel")
complete.cases(df_long)

## Visualizamos la data

ggplot(df_long,aes(x = hotel, y = value, fill = variable, y_max= 10000, y_seq=1000)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = comma_format()) +
  xlab("Hotel") +
  ylab("Cantidad de noches de estancia") +
  ggtitle("Comparación de noches de estancia en días de semana y fines de semana por hotel")

porcentaje_paises <- prop.table(table(datos$country))
porcentaje_paises_ordenado_desc <- head(round(sort(porcentaje_paises, decreasing = TRUE), 3), 10)
porcentaje_paises_ordenado_desc <- porcentaje_paises_ordenado_desc * 100


# Numero de veces que la habitacion asignada fue diferente a la reservada por hotel y por mes y año(minimizar casos de

datos.booking <- datos[, c("hotel", "arrival_date_year", "arrival_date_month",
                              "reserved_room_type","assigned_room_type")]

#Creacion de una tabla Overbooking
datos.booking$overbooking <- ifelse(datos.booking$reserved_room_type == datos.booking$assigned_room_type, "No", "Si")

# Factor de las columnas
datos.booking$hotel <- factor(datos.booking$hotel)
datos.booking$arrival_date_year <- factor(datos.booking$arrival_date_year)
datos.booking$arrival_date_month <- factor(datos.booking$arrival_date_month)
datos.booking$reserved_room_type <- factor(datos.booking$reserved_room_type)
datos.booking$assigned_room_type <- factor(datos.booking$assigned_room_type)
datos.booking$overbooking <- factor(datos.booking$overcooking)

#Filtrado de columnas

datos.booking.filter.year <- datos.booking[, c("hotel", "arrival_date_year","overbooking")]

View(datos.booking.filter.year)

## Filtrado de los datos

datos.booking <- paises_top[,(paises_top[2,] > 1000 & paises_top[1,] > 1000)]

# Separamos la tabla en dos tipos de hotel

datos.resort <- datos[datos$hotel == "Resort Hotel" ]
datos.city <- datos[datos$hotel == "City Hotel",]

View(datos.resort)
View(datos.city)


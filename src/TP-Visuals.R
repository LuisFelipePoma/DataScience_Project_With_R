setwd("C:\\Users\\pms_l\\OneDrive\\Escritorio\\uni\\Introduccion a Data Science\\Trabajo-Parcial-de-Data-Science\\src")
rm(list = ls())

datos <- read.csv("hotel_bookings/hotel_bookings.csv", sep = ",")
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

# Visualizamos los nuevos datos

## Visualizacion paises
paises = get_paises(datos,country)
paises = as.character(datos[["country"]])
paises <- sort(unique(datos$country))

## Limpieza paises top
paises_top <- table(datos$country)
paises_top <- paises_top[paises_top > 1000]
paises_n <- names(paises_top)

## Visualizacion paises top
barplot(paises_top,main="Paises del Mundo Top",names = c(paises_n))

# Visualizacion afluencia en fines de semana y dias de semana por tipo de hotel

## Instalamos la libreria necesaria
library(ggplot2)

# Extraemos la data
datos.hotel.stay <- datos[, c("hotel", "stays_in_week_nights", "stays_in_weekend_nights")]

# Convertir las variables numéricas a factor
datos.hotel.stay$hotel <- as.factor(datos.hotel.stay$hotel)
datos.hotel.stay$stays_in_week_nights <- as.factor(datos.hotel.stay$stays_in_week_nights)
datos.hotel.stay$stays_in_weekend_nights <- as.factor(datos.hotel.stay$stays_in_weekend_nights)

# Convertir los datos a formato "largo" para poder usarlos con ggplot2
df_sum <- aggregate(cbind(stays_in_week_nights, stays_in_weekend_nights) ~ hotel, data = datos.hotel.stay, FUN = sum)

# Convertir los datos a formato "largo" para poder usarlos con ggplot2
df_long <- reshape2::melt(df_sum, id.vars = "hotel")


# Visualizamos la data

ggplot(df_long, aes(x = hotel, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("Hotel") +
  ylab("Cantidad de noches de estancia") +
  ggtitle("Comparación de noches de estancia en días de semana y fines de semana por hotel")



# Separamos la tabla en dos tipos de hotel

datos.resort <- datos[datos$hotel == "Resort Hotel" ]
datos.city <- datos[datos$hotel == "City Hotel",]

View(datos.resort)
View(datos.city)

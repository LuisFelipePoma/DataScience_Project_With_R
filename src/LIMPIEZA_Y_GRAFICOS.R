#<<<<<<< HEAD
#limpiar variables de memoria
rm(list=ls(all=TRUE))
#limpiar consola
cat("\014")

#librerias
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)

#Carga de datos
setwd('C:/Users/pms_l/OneDrive/Escritorio/uni/Introduccion a Data Science/TP')
datos <- read.csv ('hotel_bookings.csv', header = TRUE, sep = ",", dec='.')

#Inpección de datos

View(datos)

str(datos)

summary(datos)

#Identificación de columnas para convertir a factor
#hotel, is_canceled, arrival_date_year, arrival_date_month, meal, country, 
#distribution_channel, is_repeated_guest, reserved_room_type, assigned_room_type
#deposit_type, customer_type, reservation_status
datos$hotel  <- as.factor(datos$hotel)
datos$is_canceled  <- as.factor(datos$is_canceled)
datos$arrival_date_year  <- as.factor(datos$arrival_date_year)
datos$arrival_date_month  <- as.factor(datos$arrival_date_month)
datos$meal  <- as.factor(datos$meal)
datos$country  <- as.factor(datos$country)
datos$distribution_channel  <- as.factor(datos$distribution_channel)
datos$is_repeated_guest  <- as.factor(datos$is_repeated_guest)
datos$reserved_room_type  <- as.factor(datos$reserved_room_type)
datos$assigned_room_type  <- as.factor(datos$assigned_room_type)
datos$deposit_type  <- as.factor(datos$deposit_type)
datos$customer_type  <- as.factor(datos$customer_type)
datos$reservation_status  <- as.factor(datos$reservation_status)

#Inspección de datos
str(datos)

summary(datos)

#Visualizar datos sin valor, en blanco y nulos
sin_valor <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores NA:",colSums(is.na(x[i])),"\n")
  }
}
sin_valor(datos)

en_blanco <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores en blanco:",colSums(x[i]==""),"\n")
  }
}
en_blanco(datos)

en_NULL <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores NULL:",colSums(x[i]=="NULL"),"\n")
  }
}
en_NULL(datos)

#eliminamos las columnas company y agent por alta cantidad de "NULL" y poca relevancia en el análisis
datos <- select(datos, -company, -agent)

#reemplazamos los valores NA por 0, porque la media es 0.1039
datos$children[is.na(datos$children)] <- 0

#outliers
boxplot(datos$adr)
min(datos$adr)
max(datos$adr)
#eliminamos los outliers por ser negativos o muy distintos a los demás
datos <- datos %>% filter(adr != min(datos$adr) & adr != max(datos$adr))

summary(datos)


# Visualizacion afluencia en fines de semana y dias de semana por tipo de hotel

## Seleccionar las columnas necesarias
datos.hotel.stay <- datos[, c("hotel","arrival_date_month","stays_in_week_nights", "stays_in_weekend_nights")]

# Resumir los datos por hotel, mes y tipo de estadía
datos.hotel.resume <- aggregate(cbind(stays_in_week_nights, stays_in_weekend_nights) ~ hotel + arrival_date_month, 
                             data = datos.hotel.stay, 
                             FUN = sum)

## Crear la gráfica
ggplot(datos.hotel.resume, aes(x = arrival_date_month, y = stays_in_week_nights, fill = "Días de semana")) +
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0, 21000), breaks = seq(0, 21000, by = 1500)) +
  geom_col(aes(y = stays_in_weekend_nights, fill = "Fin de semana"), position = "dodge") +
  facet_wrap(~ hotel, ncol = 2, scales = "free_y") +
  labs(x = "Mes", y = "Noches de estadía", fill = "") +
  ggtitle("Afluencia en fines de semana y dias de semana por tipo de hotel y mes ") +
  theme_minimal() +
  scale_fill_manual(values = c("Días de semana" = "#2E8B57", "Fin de semana" = "#FFA500")) +
  theme(plot.title = element_text(hjust = 0.2, size = 16, face = "italic"))

#----------------------------------------------------------------------------------------------------------

#Numero de veces que la habitacion asignada fue diferente a la reservada por hotel y por año(minimizar casos de

## Creamos una variable con las columnas que necesitamos
datos.booking <- datos[, c("hotel", "arrival_date_year", "arrival_date_month",
                           "reserved_room_type","assigned_room_type")]

##Creacion de una tabla Overbooking
datos.booking$booking <- ifelse(as.character(datos.booking$reserved_room_type) == as.character(datos.booking$assigned_room_type), "Coincide", "Diferente")

##Filtrado de columnas

datos.booking.filter.year <- datos.booking[, c("hotel", "arrival_date_year","booking")]

## Agrupar los datos por hotel, año y overbooking
datos.booking.filter.year.grouped <- datos.booking.filter.year %>%
  group_by(hotel, arrival_date_year, booking) %>%
  summarize(count = n())

### Crear el gráfico de barras agrupadas por año
ggplot(datos.booking.filter.year.grouped, aes(x = arrival_date_year, y = count, fill = booking)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous(limits = c(0, 35000), breaks = seq(0, 35000, by = 2500)) +
  facet_wrap(~ hotel, ncol = 2) +
  labs(title = "Comparación reservas asignadas y realizadas por año y hotel",
       x = "Año",
       y = "Cantidad",
       fill = "Estado")

#Numero de veces que la habitacion asignada fue diferente a la reservada por hotel y por mes(minimizar casos de

## Creamos una variable con las columnas que necesitamos
datos.booking <- datos[, c("hotel", "arrival_date_year", "arrival_date_month",
                           "reserved_room_type","assigned_room_type")]

##Creacion de una tabla Overbooking
datos.booking$booking <- ifelse(as.character(datos.booking$reserved_room_type) == as.character(datos.booking$assigned_room_type), "Coincide", "Diferente")

##Filtrado de columnas
datos.booking.filter.month <- datos.booking[, c("hotel", "arrival_date_month","booking")]

## Agrupar los datos por hotel, mes y overbooking
datos.booking.filter.month.grouped <- datos.booking.filter.month %>%
  group_by(hotel, arrival_date_month, booking) %>%
  summarize(count = n())

### Crear el gráfico de barras agrupadas por mes
ggplot(datos.booking.filter.month.grouped, aes(x = arrival_date_month, y = count, fill = booking)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous(limits = c(0, 8500), breaks = seq(0, 8500, by = 500)) +
  facet_wrap(~ hotel, ncol = 2) +
  labs(title = "Comparación de reservas asignadas y realizadas por mes y hotel",
       x = "Mes",
       y = "Reservas realizadas",
       fill = "Estado")

#Países con mayor afluencia por tipo de hotel----------------------------------------------------

#Países top
paises_top <- table(hotel = datos$hotel, country = datos$country )

## Filtrado de los datos

paises_filter <- paises_top[,(paises_top[2,] > 1000 & paises_top[1,] > 1000)]

paises_names <- names(paises_filter[1,])


barplot(paises_filter,
        main = "Países con mayor afluencia por tipo de hotel",
        col = c("lightgray", "lightpink"),
        names = c(paises_names),
        ylab = "Frecuencia", 
        xlab = "Paises", 
        ylim = c(0, 50000))


total <- sum(paises_top)

# Dividir cada valor por la suma total y multiplicar por 100 para obtener el porcentaje
porcentaje <- round((paises_filter / total) * 100, 2)

# Mostrar la tabla de porcentajes resultante
porcentaje
#-------------------------------------------------------------------------------------------------


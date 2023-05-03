rm(list=ls(all=TRUE))
cat("\014")

library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)



setwd('D:/Documentos/fundamentos de data science/TP/archive')
datos <- read.csv ('hotel_bookings.csv', header = TRUE, sep = ",", dec='.')

#View(datos)

str(datos)

summary(datos)

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

str(datos)

summary(datos)


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

#eliminamos las columnas company y agent por alta cantidad de "NULL"
datos <- select(datos, -company, -agent)

#reemplazamos los valores NA por 0, porque la media es 0.1039
datos$children[is.na(datos$children)] <- 0

summary(datos)

paises_top <- table(hotel = datos$hotel, country = datos$country )

## Filtrado de los datos

paises_filter <- paises_top[,(paises_top[2,] > 1000 & paises_top[1,] > 1000)]
paises_filter
paises_names <- names(paises_filter[1,])
paises_names


# Visualizacion afluencia en fines de semana y dias de semana por tipo de hotel

## Extraemos la data
datos.hotel.stay <- datos[, c("hotel", "stays_in_week_nights", "stays_in_weekend_nights")]

#View(datos.hotel.stay)
## Convertir las variables numéricas a factor
datos.hotel.stay$hotel <- as.factor(datos.hotel.stay$hotel)


summary(datos.hotel.stay)
## Convertir los datos a formato "largo" para poder usarlos con ggplot2
df_sum <- aggregate(cbind(stays_in_week_nights, stays_in_weekend_nights) ~ hotel, data = datos.hotel.stay, FUN = sum)

## Convertir los datos a formato "largo" para poder usarlos con ggplot2
df_long <- melt(df_sum, id.vars = "hotel")
complete.cases(df_long)

#Comparación de noches de estancia en días de semana y fines de semana por hotel---------------------------
ggplot(df_long,aes(x = hotel, y = value, fill = variable, y_max= 10000, y_seq=1000)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = comma_format()) +
  xlab("Hotel") +
  ylab("Cantidad de noches de estancia") +
  scale_fill_manual(values = c("stays_in_week_nights" = "green", "stays_in_weekend_nights" = "cyan")) +
  ggtitle("Comparación de noches de estadía en días de semana y fines de semana por hotel")
total_nights <- sum(df_long$value)

# Agrega una nueva columna con el porcentaje de cada hotel
df_long$percentage <- df_long$value / total_nights * 100

# Imprime la tabla resultante
print(df_long)
#----------------------------------------------------------------------------------------------------------

porcentaje_paises <- prop.table(table(datos$country))
porcentaje_paises_ordenado_desc <- head(round(sort(porcentaje_paises, decreasing = TRUE), 3), 10)
porcentaje_paises_ordenado_desc <- porcentaje_paises_ordenado_desc * 100


# Numero de veces que la habitacion asignada fue diferente a la reservada por hotel y por mes y año(minimizar casos de

## Creamos una variable con las columnas que necesitamos
datos.booking <- datos[, c("hotel", "arrival_date_year", "arrival_date_month",
                           "reserved_room_type","assigned_room_type")]

##Creacion de una tabla Overbooking
datos.booking$overbooking <- ifelse(as.character(datos.booking$reserved_room_type) == as.character(datos.booking$assigned_room_type), "Coincide", "Diferente")

summary(datos.booking)

##Filtrado de columnas

datos.booking.filter.year <- datos.booking[, c("hotel", "arrival_date_year","overbooking")]
datos.booking.filter.month <- datos.booking[, c("hotel", "arrival_date_month","overbooking")]



## Agrupar los datos por hotel, año y overbooking
datos.booking.filter.year.grouped <- datos.booking.filter.year %>%
  group_by(hotel, arrival_date_year, overbooking) %>%
  summarize(count = n())

### Crear el gráfico de barras agrupadas
ggplot(datos.booking.filter.year.grouped, aes(x = arrival_date_year, y = count, fill = overbooking)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ hotel, ncol = 2) +
  labs(title = "Comparación reservas asignadas y realizadas por año y hotel",
       x = "Año",
       y = "Cantidad",
       fill = "Overbooking")

## Agrupar los datos por hotel, mes y overbooking
datos.booking.filter.month.grouped <- datos.booking.filter.month %>%
  group_by(hotel, arrival_date_month, overbooking) %>%
  summarize(count = n())

#Comparacion de reservas realizadas por mes y por hotel-----------------------------------------
ggplot(datos.booking.filter.month.grouped, aes(x = arrival_date_month, y = count, fill = overbooking)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ hotel, ncol = 2) +
  labs(title = "Comparación de reservas asignadas y realizadas por mes y por hotel",
       x = "Mes",
       y = "Reservas realizadas",
       fill = "Estado")

#Países con mayor afluencia por tipo de hotel----------------------------------------------------
barplot(paises_filter,main="Países con mayor afluencia por tipo de hotel",
        col=c("lightgray","lightpink"), 
        names = c(paises_names), 
        ylab = "Frecuencia", xlab = "Paises", ylim = c(0,50000))

legend("topleft", legend=c("Resort","City"), fill = c("lightgray","lightpink"), bty = "n")

total <- sum(paises_top)

# Dividir cada valor por la suma total y multiplicar por 100 para obtener el porcentaje
porcentaje <- round((paises_filter / total) * 100, 2)

# Mostrar la tabla de porcentajes resultante
porcentaje
#-------------------------------------------------------------------------------------------------
hotel_data_summary <- datos %>%
  group_by(arrival_date_year, arrival_date_month) %>%
  summarize(different_count = sum(as.character(reserved_room_type) != as.character(assigned_room_type)),
            same_count = n() - different_count)

View(hotel_data_summary)
#Comparación de reservas asignadas y realizadas por año y mes--------------------------------------
ggplot(hotel_data_summary, aes(x = interaction(arrival_date_year, arrival_date_month), y = same_count, fill = "Coincidencia entre asignación y reserva")) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, by = 500)) +
  geom_bar(aes(y = different_count, fill = "Diferencia entre asignación y reserva"), stat = "identity") +
  scale_fill_manual(values = c("Coincidencia entre asignación y reserva" = "blue", "Diferencia entre asignación y reserva" = "red")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Año-Mes", y = "Reservas realizadas", fill = "") +
  ggtitle("Comparación de reservas asignadas y realizadas por año y mes")
#----------------------------------------------------------------------------------------------------


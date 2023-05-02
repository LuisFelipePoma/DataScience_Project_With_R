rm(list=ls(all=TRUE)) 
cat("\014")

url <- "D:/OneDrive - Universidad Peruana de Ciencias/Documents/Carrera/2023-1/Fundamentos Data Science/TP"
setwd(url)
hotel_data<-read.csv('hotel_bookings.csv', header=TRUE, sep=',',dec='.')

View(hotel_data)

head(hotel_data)
tail(hotel_data)

names(hotel_data)

str(hotel_data)
summary(hotel_data)
summary(hotel_data$children)

hotel_data$hotel  <- as.factor(hotel_data$hotel)
hotel_data$is_canceled  <- as.factor(hotel_data$is_canceled)
hotel_data$arrival_date_month  <- as.factor(hotel_data$arrival_date_month)
hotel_data$meal  <- as.factor(hotel_data$meal)
hotel_data$country  <- as.factor(hotel_data$country)

hotel_data$market_segment  <- as.factor(hotel_data$market_segment)
hotel_data$distribution_channel  <- as.factor(hotel_data$distribution_channel)
hotel_data$is_repeated_guest  <- as.factor(hotel_data$is_repeated_guest)
hotel_data$reserved_room_type  <- as.factor(hotel_data$reserved_room_type)
hotel_data$assigned_room_type  <- as.factor(hotel_data$assigned_room_type)

hotel_data$deposit_type  <- as.factor(hotel_data$deposit_type)
hotel_data$customer_type  <- as.factor(hotel_data$customer_type)
hotel_data$reservation_status  <- as.factor(hotel_data$reservation_status)

sin_valor <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores NA:",colSums(is.na(x[i])),"\n")
  }
}
sin_valor(hotel_data)

en_blanco <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores en blanco:",colSums(x[i]==""),"\n")
  }
}
en_blanco(hotel_data)

#[CAMPOS QUE NO UTILIZAREMOS]
library(dplyr)
summary(hotel_data_util)
hotel_data_util <- select(hotel_data, -arrival_date_week_number, -adults, -children, -babies, -meal, -market_segment, -previous_bookings_not_canceled, -booking_changes, -deposit_type, -agent, -company, -reservation_status, -reservation_status_date)
View(hotel_data_util)

sin_valor(hotel_data_util)
en_blanco(hotel_data_util)

#hotel_data_util <- hotel_data_util %>% filter(!is.na(children))

library(ggplot2) 
library(scales)
#Visualizacion grafica

#1. Clientes por Pais
summary(hotel_data_util)
table(hotel_data_util$country)

paises <- function(x) {
  paises <- unique(x)
  paises_vector <- as.vector(paises)
  return(paises_vector)
}

summary(hotel_data_util$country)
str(hotel_data_util)

paises = hotel_data_util[["country"]]
paises = paises(hotel_data_util$country)
paises <- sort(paises)

barplot(table(hotel_data_util$country), main="Paises de las personas del hotel", names= c(paises))

prop.table(table(hotel_data_util$country))

paises_ocurrencias <- table(hotel_data_util$country)
paises_mas_1000 <- paises_ocurrencias[paises_ocurrencias > 1000]

y_min <- 0
y_max <- max(paises_mas_1000) + 10000
y_seq <- seq(y_min, y_max, 10000)

barplot(paises_mas_1000, main = "Países con más de 1000 ocurrencias", xlab = "Países", ylab = "Ocurrencias", ylim = c(y_min, y_max), yaxp = c(y_min, y_max, length.out = length(y_seq)))
#barplot(paises_mas_1000, main = "Países con más de 1000 ocurrencias", xlab = "Países", ylab = "Ocurrencias")




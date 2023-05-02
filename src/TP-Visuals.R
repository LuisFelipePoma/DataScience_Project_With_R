setwd("C:\\Users\\pms_l\\OneDrive\\Escritorio\\uni\\Introduccion a Data Science\\TP")

datos <- read.csv("hotel_bookings/hotel_bookings.csv", sep = ",")
View(datos)
complete.cases(datos)

summary(datos)

datos.resort <- datos[datos$hotel == "Resort Hotel",]
datos.city <- datos[datos$hotel == "City Hotel",]
View(datos.resort)
View(datos.city)

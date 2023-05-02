setwd("C:\\Users\\pms_l\\OneDrive\\Escritorio\\uni\\Introduccion a Data Science\\Trabajo-Parcial-de-Data-Science\\src")

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

# Visualizacion paises

paises = get_paises(datos,country)
paises = as.character(datos[["country"]])
paises <- sort(unique(datos$country))

#Limpieza paises top

paises_top <- table(datos$country)
paises_top <- paises_n[paises_n > 1000]
paises_n <- names(paises_top)


barplot(paises_top,main="Paises del Mundo Top",names = c(paises_n))

boxplot(datos$adr, col = "deepskyblue", main= "Boxplot VEntas", horizontal = TRUE)
boxplot(datos$adr ~datos$lead_time ,col="deepskyblue",main ="Histograma de ventas",xlab = "tiendas",ylab = "ventas $")


# Separamos la tabla en dos tipos de hotel

datos.resort <- datos[datos$hotel == "Resort Hotel",]
datos.city <- datos[datos$hotel == "City Hotel",]

View(datos.resort)
View(datos.city)

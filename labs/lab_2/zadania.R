dane <- read.csv("q1_data.csv")

cnames <- colnames(dane)
cnames

head(dane, 2)

str(dane)

tail(dane, 2)

dane$Ozone[47]
dane[47, "Ozone"]

sum(is.na(dane$Ozone))

mean_ozone <- mean(dane$Ozone, na.rm = TRUE)
mean_ozone

dane2 <- dane[dane$Ozone > 31 & dane$Temp > 90, ]
dane2
mean_solar <- mean(dane2$Solar.R, na.rm = TRUE)
mean_solar

dane_month6 <- dane[dane$Month == 6, ]
dane_month6
mean_temp <- mean(dane_month6$Temp)
mean_temp

dane_month5 <- dane[dane$Month == 5, ]
dane_month5
max_ozone <- max(dane_month5$Ozone, na.rm = TRUE)
max_ozone



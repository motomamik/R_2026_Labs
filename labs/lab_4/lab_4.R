install.packages(c("dplyr", "tidyr", "readr", "readxl"))
library(dplyr)
library(tidyr)
library(readr)
library(readxl)

setwd("C:/Users/karol/Documents/semestr 1/R/R_2026_Labs/labs/lab_4/quiz_data/data")

data <- read_csv("daily_SPEC_2014.csv.bz2")

print(data)
head(data)
colnames(data)
ncol(data)
nrow(data)

# zad 1 

data_wisconsin <- filter(data, `State Name` == "Wisconsin")
head(data_wisconsin)

data_bromine <- filter(data_wisconsin, `Parameter Name` == "Bromine PM2.5 LC")

data_bromine_mean <- mean(data_bromine$`Arithmetic Mean`, na.rm = TRUE)
data_bromine_mean

# jako jeden stream 
data_bromine_mean <- data %>%
  filter(`State Name` == "Wisconsin",
         `Parameter Name` == "Bromine PM2.5 LC") %>%
  summarise(mean_value = mean(Arithmetic.Mean, na.rm = TRUE)) %>%
  pull(mean_value)

data_bromine_mean

# zad 2
# Oblicz średnią wartość każdego składnika chemicznego we wszystkich stanach, 
# miejscach monitorowania i wszystkich punktach czasowych.
# Który składnik Parameter Name ma najwyższy średni poziom?

data_mean_grouped <- data %>%
  group_by(`Parameter Name`) %>%
  summarise(mean_value = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(mean_value))
  
data_mean_grouped

# zad 3
# Które miejsce monitorowania ma najwyższy średni poziom "Sulfate PM2.5 LC" 
# we wszystkich punktach czasowych?
# Wskaż kod stanu, kod hrabstwa i numer miejsca (code, county code, site number).
data_sulfate <- data %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  group_by(`State Code`, `County Code`, `Site Num`) %>%
  summarize(mean_value = mean(`Arithmetic Mean`, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(mean_value)) 

# zad 4
# Jaka jest absolutna różnica w średnich poziomach "EC PM2.5 LC TOR" między stanami 
# Kalifornia i Arizona, we wszystkich punktach czasowych i wszystkich miejscach monitorowania?


data_diff <- data %>%
  filter(`Parameter Name` == "EC PM2.5 LC TOR", `State Name` %in% c("California", "Arizona")) %>%
  group_by(`State Name`) %>%
  summarize(mean_value = mean(`Arithmetic Mean`, na.rm = TRUE))

mean_CA <- data_diff$mean_value[data_diff$`State Name` == "California"]
mean_AZ <- data_diff$mean_value[data_diff$`State Name` == "Arizona"]
diff <- mean_CA - mean_AZ
diff

# zad 5 
# Jaki jest mediana "OC PM2.5 LC TOR" w zachodnich Stanach Zjednoczonych, 
# we wszystkich punktach czasowych? Zdefiniuj zachodnie Stany jako dowolną 
# lokalizację monitorowania, która ma długość geograficzną MNIEJSZĄ NIŻ -100.

mediana_east <- data %>%
  filter(`Parameter Name` == "OC PM2.5 LC TOR", Longitude < -100) %>%
  summarize(median_value = median(`Arithmetic Mean`, na.rm = TRUE))

mediana_east

# zad 6
# Użyj pakietu readxl do wczytania pliku aqs_sites.xlsx do R 
# (może być konieczna wcześniejsza instalacja pakietu). 
# Ten plik zawiera metadane o każdym z miejsc monitorowania w systemie monitorowania EPA. 
# W szczególności zmienne "Land Use" (Użytkowanie Gruntów) i "Location Setting" 
# (Ustawienie Lokalizacji) zawierają informacje o tym, w jakich rodzajach obszarów 
# znajdują się monitory (tj. "mieszkalne" vs. "leśne").
# Ile miejsc monitorowania jest oznaczonych zarówno jako RESIDENTIAL (MIESZKALNE) 
# w "Land Use" (Użytkowanie Gruntów), jak i SUBURBAN (PODMIEJSKIE) w "Location Setting" 
# (Ustawienie Lokalizacji)?

library(readxl)

data2 <- read_xlsx("aqs_sites.xlsx")
colnames(data2)

data2 %>%
  filter(`Land Use` == "RESIDENTIAL", `Location Setting` == "SUBURBAN") %>%
  nrow()
  print
  
  
# zad 7 
# Jaki jest medianowy poziom "EC PM2.5 LC TOR" wśród miejsc monitorowania oznaczonych zarówno 
# jako "RESIDENTIAL" (MIESZKALNE), jak i "SUBURBAN" (PODMIEJSKIE) we wschodnich Stanach Zjednoczonych, 
# gdzie wschodnie Stany są zdefiniowane jako długość geograficzna większa lub równa -100?
  
miejsca_wschodnie <- data2 %>%
  filter(
    `Land Use` == "RESIDENTIAL", 
    `Location Setting` == "SUBURBAN", 
    Longitude >= -100
  )
  
mediana_wschod <- data %>%
  filter(`Parameter Name` == "EC PM2.5 LC TOR") %>%
  mutate(
    `State Code` = as.numeric(`State Code`),
    `County Code` = as.numeric(`County Code`),
    `Site Num` = as.numeric(`Site Num`)
  ) %>%
  inner_join(miejsca_wschodnie, by = c("State Code", "County Code", "Site Num" = "Site Number")) %>%
  summarize(median_value = median(`Arithmetic Mean`, na.rm = TRUE))
  
mediana_wschod

# zad 8
# Wśród miejsc monitorowania oznaczonych jako COMMERCIAL (KOMERCYJNE) w "Land Use" 
# (Użytkowanie Gruntów), który miesiąc roku ma najwyższe średnie poziomy "Sulfate PM2.5 LC"?

miejsca_komercyjne <- data2 %>%
  filter(`Land Use` == "COMMERCIAL")

srednie_miesieczne <- data %>%
  filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
  mutate(
    `State Code` = as.numeric(`State Code`),
    `County Code` = as.numeric(`County Code`),
    `Site Num` = as.numeric(`Site Num`),
    Miesiac = format(as.Date(`Date Local`), "%m") 
  ) %>%
  inner_join(miejsca_komercyjne, by = c("State Code", "County Code", "Site Num" = "Site Number")) %>%
  group_by(Miesiac) %>%
  summarize(mean_value = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(mean_value))

print(srednie_miesieczne)

head(srednie_miesieczne, 1)

# zad 9
## Przyjrzyj się danym dla miejsca monitorowania zidentyfikowanego przez kod stanu 6, kod hrabstwa 65 i 
##numer miejsca 8001 (ten monitor znajduje się w Kalifornii). W tym monitorze, przez ile dni suma "Sulfate PM2.5 LC" 
## i "Total Nitrate PM2.5 LC" jest większa niż 10?
## Dla każdego ze składników chemicznych będą pewne daty, które mają wiele wartości Arithmetic Mean w tym miejscu 
##monitorowania. Gdy w danym dniu jest wiele wartości, weź średnią wartości składników dla tej daty.

wynik_zad9 <- data %>%
  mutate(
    `State Code` = as.numeric(`State Code`),
    `County Code` = as.numeric(`County Code`),
    `Site Num` = as.numeric(`Site Num`)
  ) %>%
  filter(
    `State Code` == 6,
    `County Code` == 65,
    `Site Num` == 8001,
    `Parameter Name` %in% c("Sulfate PM2.5 LC", "Total Nitrate PM2.5 LC")
  ) %>%
  group_by(`Date Local`, `Parameter Name`) %>%
  summarize(mean_value = mean(`Arithmetic Mean`, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = `Parameter Name`, values_from = mean_value) %>%
  mutate(Suma = `Sulfate PM2.5 LC` + `Total Nitrate PM2.5 LC`) %>%
  filter(Suma > 10)

liczba_dni <- nrow(wynik_zad9)
print(liczba_dni)
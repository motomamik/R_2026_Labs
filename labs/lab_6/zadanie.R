# wczytywanie danych
full_data <- read.csv("household_power_consumption.csv", sep = ";", na.strings = "?")

full_data$Date <- as.Date(full_data$Date, format = "%d/%m/%Y")
data <- subset(full_data, Date >= "2007-02-01" & Date <= "2007-02-02")
rm(full_data)

datetime <- paste(as.character(data$Date), data$Time)
data$Datetime <- strptime(datetime, "%Y-%m-%d %H:%M:%S")

png("plot1.png", width = 480, height = 480)
hist(data$Global_active_power, 
     col = "red", 
     main = "Global Active Power", 
     xlab = "Global Active Power (kilowatts)", 
     ylab = "Frequency")
dev.off()


png("plot2.png", width = 480, height = 480)
plot(data$Datetime, data$Global_active_power, 
     type = "l", 
     xlab = "", 
     ylab = "Global Active Power (kilowatts)",
     xaxt = "n")

ticks <- as.POSIXct(c("2007-02-01 00:00:00", 
                      "2007-02-02 00:00:00", 
                      "2007-02-03 00:00:00"))

axis(side = 1, at = ticks, labels = c("Thu", "Fri", "Sat"))
dev.off()


png("plot3.png", width = 480, height = 480)
plot(data$Datetime, data$Sub_metering_1, 
     type = "l", 
     col = "black", 
     xlab = "", 
     ylab = "Energy sub metering",
     xaxt = "n")

lines(data$Datetime, data$Sub_metering_2, col = "red")
lines(data$Datetime, data$Sub_metering_3, col = "blue")

ticks <- as.POSIXct(c("2007-02-01 00:00:00", 
                      "2007-02-02 00:00:00", 
                      "2007-02-03 00:00:00"))
axis(side = 1, at = ticks, labels = c("Thu", "Fri", "Sat"))

legend("topright", 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
       col = c("black", "red", "blue"), 
       lty = 1,
       bty = "n")
dev.off()



png("plot4.png", width = 480, height = 480)
par(mfrow = c(2, 2))
ticks <- as.POSIXct(c("2007-02-01 00:00:00", 
                      "2007-02-02 00:00:00", 
                      "2007-02-03 00:00:00"))

# wykres 1
plot(data$Datetime, data$Global_active_power, 
     type = "l", 
     xlab = "", 
     ylab = "Global Active Power (kilowatts)",
     xaxt = "n")
axis(side = 1, at = ticks, labels = c("Thu", "Fri", "Sat"))

# wykres 2
plot(data$Datetime, data$Voltage, 
     type = "l", 
     xlab = "datetime", 
     ylab = "Voltage", 
     xaxt = "n")
axis(side = 1, at = ticks, labels = c("Thu", "Fri", "Sat"))

# wykres 3
plot(data$Datetime, data$Sub_metering_1, 
     type = "l", 
     col = "black", 
     xlab = "", 
     ylab = "Energy sub metering", 
     xaxt = "n")
lines(data$Datetime, data$Sub_metering_2, col = "red")
lines(data$Datetime, data$Sub_metering_3, col = "blue")
axis(side = 1, at = ticks, labels = c("Thu", "Fri", "Sat"))
legend("topright", 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
       col = c("black", "red", "blue"), lty = 1, bty = "n")

# wykres 4
plot(data$Datetime, data$Global_reactive_power, 
     type = "l", 
     xlab = "datetime", 
     ylab = "Global_reactive_power", 
     xaxt = "n")
axis(side = 1, at = ticks, labels = c("Thu", "Fri", "Sat"))

dev.off()
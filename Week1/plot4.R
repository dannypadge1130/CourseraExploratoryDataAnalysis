household <- read.csv("ExploratoryDataAnalysis/Data/household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE)

household <- transform(household, Date = as.Date(Date, "%d/%m/%Y"))
household <- subset(household, Date >= '2007-02-01' & Date <= '2007-02-02')

datetime <- paste(as.Date(household$Date), household$Time)
household$Datetime <- as.POSIXct(datetime)

png("ExploratoryDataAnalysis/Week1/ExData_Plotting1/plot4.png", width=480, height=480)

par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))

household$Global_active_power <- as.numeric(household$Global_active_power)
plot(household$Global_active_power~household$Datetime, xlab="", ylab="Global Active Power", type="l")

household$Voltage <- as.numeric(household$Voltage)
plot(household$Voltage~household$Datetime, xlab="datetime", ylab="Voltage", type="l")

household$Sub_metering_1 <- as.numeric(household$Sub_metering_1)
household$Sub_metering_2 <- as.numeric(household$Sub_metering_2)
household$Sub_metering_3 <- as.numeric(household$Sub_metering_3)

plot(household$Sub_metering_1~household$Datetime, ylab="Engergy sub metering", xlab="", type="l")
lines(household$Sub_metering_2~household$Datetime, col="Red")
lines(household$Sub_metering_3~household$Datetime, col="Blue")
legend("topright", legend=c("Sub_metering_1            ", "Sub_metering_2            ", "Sub_metering_3            "), col=c("Black", "Red", "Blue"), lwd=2, cex = 0.7)

household$Global_reactive_power <- as.numeric(household$Global_reactive_power)
plot(household$Global_reactive_power~household$Datetime, ylab="Global_reactive_power", xlab="datetime", type="l")

dev.off()
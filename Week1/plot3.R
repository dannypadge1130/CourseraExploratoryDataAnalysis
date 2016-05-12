household <- read.csv("ExploratoryDataAnalysis/Data/household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE)

household <- transform(household, Date = as.Date(Date, "%d/%m/%Y"))
household <- subset(household, Date >= '2007-02-01' & Date <= '2007-02-02')

household$Sub_metering_1 <- as.numeric(household$Sub_metering_1)
household$Sub_metering_2 <- as.numeric(household$Sub_metering_2)
household$Sub_metering_3 <- as.numeric(household$Sub_metering_3)

datetime <- paste(as.Date(household$Date), household$Time)
household$Datetime <- as.POSIXct(datetime)

png("ExploratoryDataAnalysis/Week1/ExData_Plotting1/plot3.png", width=480, height=480)

plot(household$Sub_metering_1~household$Datetime, ylab="Energy sub metering", xlab="", type="l")

lines(household$Sub_metering_2~household$Datetime, col="Red")
lines(household$Sub_metering_3~household$Datetime, col="Blue")

legend("topright", legend=c("Sub_metering_1            ", "Sub_metering_2            ", "Sub_metering_3            "), col=c("Black", "Red", "Blue"), lty=1, lwd=2, cex = 0.7)

dev.off()
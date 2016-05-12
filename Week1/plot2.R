household <- read.csv("ExploratoryDataAnalysis/Data/household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE)

household <- transform(household, Date = as.Date(Date, "%d/%m/%Y"))
household <- subset(household, Date >= '2007-02-01' & Date <= '2007-02-02')

household$Global_active_power <- as.numeric(household$Global_active_power)

datetime <- paste(as.Date(household$Date), household$Time)
household$Datetime <- as.POSIXct(datetime)

png("ExploratoryDataAnalysis/Week1/ExData_Plotting1/plot2.png", width=480, height=480)
plot(household$Global_active_power~household$Datetime, ylab="Global Active Power (kilowatts)", xlab="", type="l")
dev.off()
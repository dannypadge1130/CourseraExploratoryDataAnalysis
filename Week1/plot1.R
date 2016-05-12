household <- read.csv("ExploratoryDataAnalysis/Data/household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE)

household <- transform(household, Date = as.Date(Date, "%d/%m/%Y"))
household <- subset(household, Date >= '2007-02-01' & Date <= '2007-02-02')

household$Global_active_power <- as.numeric(household$Global_active_power)

png("ExploratoryDataAnalysis/Week1/ExData_Plotting1/plot1.png", width=480, height=480)
hist(household$Global_active_power, main="Global Active Power", ylab="Frequency", xlab="Global Active Power (kilowatts)", col="red")
dev.off()
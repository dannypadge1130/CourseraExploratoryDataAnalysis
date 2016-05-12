
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## (Question 1) Have total emissions from PM2.5 decreased in the United States 
## from 1999 to 2008? Using the base plotting system, make a plot showing the 
## total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

totals <- aggregate(Emissions ~ year, NEI, sum)

emissionsInMill <- totals$Emissions / 1000000

plot(x = totals$year, y = emissionsInMill, xlab = "Year", ylab = "PM2.5 Emissions (million tons)", main = "Total US PM2.5 Emissions")
lines(lowess(x = totals$year, y = emissionsInMill))

## (Question 2) Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008? Use the base 
## plotting system to make a plot answering this question.

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

balti <- NEI[NEI$fips=="24510",]
baltiTotals <- aggregate(Emissions ~ year, balti, sum)

baltiEmissionsInMill <- baltiTotals$Emissions / 1000

plot(x = baltiTotals$year, y = baltiEmissionsInMill, xlab = "Year", ylab = "Baltimore PM2.5 Emissions (thousand tons)", main = "Total Baltimore PM2.5 Emissions")
lines(lowess(x = baltiTotals$year, y = baltiEmissionsInMill))

## (Question 3) Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, 
## onroad) variable, which of these four sources have seen decreases in emissions from 1999–2008
## for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 
## plotting system to make a plot answer this question.

library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

balti <- NEI[NEI$fips=="24510",]

ggplot(balti,aes(factor(year), Emissions, fill=type)) + geom_bar(position = "dodge", stat = "identity") + labs(x="Year", y="Baltimore PM2.5 Emissions (tons)") + ggtitle("Baltimore PM2.5 Emissions by Source Type")

## (Question 4) Across the United States, how have emissions from coal combustion-related 
## sources changed from 1999–2008?

library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

combustion <- grepl("combustion", SCC$SCC.Level.One, ignore.case=TRUE)
coal <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
coalCombustion <- (coal & combustion)
coalCombustionSCC <- SCC[coalCombustion,]$SCC
coalCombustionNEI <- NEI[NEI$SCC %in% coalCombustionSCC,]
coalCombustionNEI$Emissions <- coalCombustionNEI$Emissions / 100000

ggplot(coalCombustionNEI,aes(factor(year), Emissions)) + geom_bar(stat = "identity", fill="grey") + labs(x="Year", y="Coal Combustion PM2.5 Emissions (100 thousand ton)") + ggtitle("Coal Combustion PM2.5 Emissions")

## (Question 5) How have emissions from motor vehicle sources changed from 1999–2008 
## in Baltimore City?

library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

vehicles <- grepl("vehicles", SCC$SCC.Level.Two, ignore.case=TRUE)
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]
baltiVehiclesNEI <- vehiclesNEI[vehiclesNEI$fips==24510,]

ggplot(baltiVehiclesNEI,aes(factor(year), Emissions)) + geom_bar(stat = "identity", fill="grey") + labs(x="Year", y="Baltimore Vehicle PM2.5 Emissions (ton)") + ggtitle("Baltimore Vehicle PM2.5 Emissions")

## (Question 6) Compare emissions from motor vehicle sources in Baltimore City with 
## emissions from motor vehicle sources in Los Angeles County, California 
## (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽"). Which city has seen greater changes over time in motor 
## vehicle emissions? 

library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

vehicles <- grepl("vehicles", SCC$SCC.Level.Two, ignore.case=TRUE)
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]

baltiVehiclesNEI <- vehiclesNEI[vehiclesNEI$fips == "24510",]
baltiVehiclesNEI$city <- "Baltimore City"

LaVehiclesNEI <- vehiclesNEI[vehiclesNEI$fips == "06037",]
LaVehiclesNEI$city <- "Los Angeles County"

cityNEI <- rbind(baltiVehiclesNEI,LaVehiclesNEI)

ggplot(cityNEI,aes(factor(year), Emissions, fill=city)) + geom_bar(position = "dodge", stat = "identity") + labs(x="Year", y="PM2.5 Emissions (tons)") + ggtitle("PM2.5 Emissions by City")

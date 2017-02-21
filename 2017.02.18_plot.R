# import the data
setwd("2017.02.18_exdata")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
setwd("../")

# plot 1
# use base plotting system to show total pm2.5 emission from 
# all resources for 1999, 2002, 2005,2008
results <- with(NEI,aggregate(Emissions,list(year),sum))
colnames(results) <- c("year", "total_pollution")

# out put is png file
options("scipen" = 999)
png("plot1.png") # the default is 480 * 480

with(results, plot(year, total_pollution, ylim = c(0, 9999999)))

dev.off()

# ============================================================================== #
# plot 2
# Have total emissions from pm2.5 decrease in Baltimore (fips == "24510")?
Baltimore_NEI <- subset(NEI, fips == "24510")

results_2 <- with(Baltimore_NEI,aggregate(Emissions,list(year),sum))
colnames(results_2) <- c("year", "total_pollution")

# out put is png file
png("plot2.png") # the default is 480 * 480

with(results_2, plot(year, total_pollution, ylim = c(0, 1.25*max(results_2$total_pollution))))

dev.off()

# ============================================================================== #
# plot 3
# Of four types of sources, which has been decrease for Baltimore?
# Which has been increased? Use ggplot2 to answer this.
library(ggplot2)

Baltimore_NEI <- subset(NEI, fips == "24510")

results_3 <- aggregate(Emissions ~ year + type, data = Baltimore_NEI, sum)

png("plot3.png", width = 1200, height = 500) # the default is 480 * 480
qplot(year, Emissions, data = results_3, facets = .~ type)
dev.off()

# ============================================================================== #
# plot 4
# subset coal related emission from NEI
coal <- subset(SCC, EI.Sector %in% c("Fuel Comb - Electric Generation - Coal",
                                   "Fuel Comb - Industrial Boilers, ICEs - Coal",
                                   "Fuel Comb - Comm/Institutional - Coal"))
NEI_coal <- subset(NEI, SCC %in% coal$SCC)

results_4 <- aggregate(Emissions ~ year, data = NEI_coal, sum)

png("plot4.png") # the default is 480 * 480

with(results_4,plot(year, Emissions, ylim = c(0, 1.25*max(results_4$Emissions))))

dev.off()

# ============================================================================== #
# plot 5
# subset all motor vehicle
mobile <- grep("Mobile - On-Road*",SCC$EI.Sector)
SCC_mobile <- SCC[mobile,]
NEI_mobile <- subset(NEI, SCC %in% SCC_mobile$SCC)

results_5 <- aggregate(Emissions ~ year, data = NEI_mobile, sum)

png("plot5.png") # the default is 480 * 480

with(results_5,plot(year, Emissions, ylim = c(0, 1.25*max(results_5$Emissions))))

dev.off()

# ============================================================================== #
# plot 6
# subset all motor vehicle
mobile <- grep("Mobile - On-Road*",SCC$EI.Sector)
SCC_mobile <- SCC[mobile,]
NEI_mobile <- subset(NEI, SCC %in% SCC_mobile$SCC)

NEI_Bal <- subset(NEI_mobile, fips == "24510")
NEI_LA <- subset(NEI_mobile, fips == "06037")

results_6.1 <- aggregate(Emissions ~ year, data = NEI_Bal, sum)
results_6.2 <- aggregate(Emissions ~ year, data = NEI_LA, sum)

results_6.0 <- rbind(results_6.1, results_6.2)
county <- c("Bal","Bal","Bal","Bal","LA","LA","LA","LA")
results_6 <- cbind(results_6.0, county)

png("plot6.png", width = 800, height = 500) # the default is 480 * 480
qplot(year, Emissions, data = results_6, facets = .~ county)
dev.off()

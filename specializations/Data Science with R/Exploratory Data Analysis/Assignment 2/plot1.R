# Getting Started ---------------------------------------------------------
options(scipen = 999)
# seting working directory and listing files in it
setwd('C:/Users/Administrator/Desktop/Coursera/Data Science/EDA');
list.files(getwd(), recursive = TRUE)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

aggregatedTotalByYear <- aggregate(Emissions ~ year, NEI, sum)

png('plots1.png')
barplot(height=aggregatedTotalByYear$Emissions, names.arg=aggregatedTotalByYear$year,
        xlab="years", ylab=expression('total PM'[2.5]*' emission'),
        main=expression('Total PM'[2.5]*' emissions at various years'))
dev.off()
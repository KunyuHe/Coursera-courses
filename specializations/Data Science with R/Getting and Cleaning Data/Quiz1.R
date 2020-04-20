setwd('C:/Users/Administrator/Desktop/Coursera/Data Science/Getting and Cleaning Data')
if(!file.exists("Quiz1")){ dir.create("Quiz1")}


# Question 1 --------------------------------------------------------------
if(!file.exists("./Quiz1/American Community Survey 2006.csv")){
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
  download.file(fileUrl, destfile = "./Quiz1/American Community Survey 2006.csv")
}
list.files("./Quiz1")
dateDownloaded1 <- date()

ACS2006 <- read.csv("./Quiz1/American Community Survey 2006.csv", sep = ",", header = TRUE)
head(ACS2006)

str(ACS2006$VAL)
AnswerKey1 <- sum(ACS2006$VAL == 24, na.rm = TRUE)


# Question 2 --------------------------------------------------------------
str(ACS2006$FES)


# Question 3 --------------------------------------------------------------
if(!file.exists("./Quiz1/Natural Gas Aquisition Program.xlsx")){
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx "
  download.file(fileUrl, destfile = "./Quiz1/Natural Gas Aquisition Program.xlsx")
}
list.files("./Quiz1")
dateDownloaded2 <- date()

library(xlsx)
dat <- read.xlsx("./Quiz1/Natural Gas Aquisition Program.xlsx", sheetIndex = 1,
                  colIndex = 7:15, rowIndex = 18:23)
AnswerKey3 <- sum(dat$Zip*dat$Ext, na.rm = TRUE)

# Question 4 --------------------------------------------------------------
library(XML)
library(bitops)
library(RCurl)
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileUrl, useInternalNodes = TRUE)
ZIP <- xpathApply(doc, "//zipcode", xmlValue)
AnswerKey4 <- length(ZIP[ZIP ==  21231])


# Question 5 --------------------------------------------------------------





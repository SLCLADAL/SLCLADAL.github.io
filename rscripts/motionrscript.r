
# "Motion Charts in R"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("RgoogleMaps", "ggmap", "mapproj", "sf",
                   "dplyr", "OpenStreetMap", "devtools"))
# install package from github
devtools::install_github("dkahle/ggmap", ref = "tidyup")
# load library
library(googleVis)
# create motion chart object
Geo=gvisGeoMap(Population, locationvar="Country", numvar="Population", options = list(height=350, daatMode='regions'))
# display motion chart
plot(Geo)
# read data
undata = read.csv("D:\\Uni\\Korpora\\Data/UNdata_Export_20190805_053628349.csv", header = T, encoding = "UTF-8")
# inspect data
#str(undata)
# inspect age
#table(undata$Age)
# inspect age levels
#names(table(undata$Age))
# create dictionaries
age014 <- c("0", "0 - 14", "0 - 4", "1", "1 - 4", "10", 
            "10 - 14", "11", "12", "13", "14", "2", "3",
            "4", "5", "6",  "7", "8", "9")
age1524 <- c("15", "15 - 19", "15 - 24", "16", "17", 
             "18", "19", "20", "20 - 24", "21", "22", 
             "23", "24")
age2534 <- c("25", "25 - 29", "25 - 34", "26", "27", 
             "28", "29", "30", "30 - 34", "31", "32", 
             "33", "34")
age3544 <- c("35", "35 - 39", "35 - 44", "36", "37", 
             "38", "39", "40", "40 - 44", "41", "42", 
             "43", "44")
age4554 <- c("45", "45 - 49", "45 - 54", "46", "47", 
             "48", "49", "5 - 9", "50", "50 - 54", "51", 
             "52", "53", "54")
age5564 <- c("55", "55 - 59", "55 - 64", "56", "57", "58", 
             "59", "60", "60 - 64", "61", "62", "63", "64")
age6574 <- c("65", "65 - 69", "65 +", "66", "67", "68", 
             "69", "70", "70 - 74", "71", "72", "73", "74")
age7584 <- c("75", "75 - 79", "75 +", "76", "77", "78", 
             "79", "80", "80 - 84", "80 +", "81", "82", 
             "83", "84")
age8594 <- c("85", "85 - 89", "85 +", "86", "87", "88", "89", 
             "90", "90 - 94", "90 +", "91", "92", "93", "94")
age95110 <- c("95", "95 - 99", "95 +", "96", "97", "98", "99", 
              "99 +", "100", "100 - 104", "100 +", "101", "102", 
             "103", "104", "105", "105 - 109", "106", 
             "107", "108", "109", "110 +")
# change age levels
undata$Age <- ifelse(undata$Age %in% age014, "0-14", undata$Age)
undata$Age <- ifelse(undata$Age %in% age1524, "15-24", undata$Age)
undata$Age <- ifelse(undata$Age %in% age2534, "25-34", undata$Age)
undata$Age <- ifelse(undata$Age %in% age3544, "35-44", undata$Age)
undata$Age <- ifelse(undata$Age %in% age4554, "45-54", undata$Age)
undata$Age <- ifelse(undata$Age %in% age5564, "55-64", undata$Age)
undata$Age <- ifelse(undata$Age %in% age6574, "65-74", undata$Age)
undata$Age <- ifelse(undata$Age %in% age7584, "75-84", undata$Age)
undata$Age <- ifelse(undata$Age %in% age8594, "85-94", undata$Age)
undata$Age <- ifelse(undata$Age %in% age95110, "95+", undata$Age)
# inspect age levels
#names(table(undata$Age))
# remove empty lines and clean data
#nrow(undata)
undata <- undata[undata$Age != "",]
undata <- undata[undata$Age != "Unknown",]
#nrow(undata)
# extract only all age groups combined
undata <- undata[undata$Age == "Total",]
# extract relevant columns
library(dplyr)
undata <- undata %>%
  select(Country.or.Area, Year, Value)
head(undata)
write.table(undata, "D:\\Uni\\UQ\\LADAL\\SLCLADAL.github.io\\data/undata.txt", sep = "\t", col.names = T, row.names = F)
# load gapminder data
download.file(url = "https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv",
              destfile = "data/gapminderdata.csv")
gapminder <- read.csv("data/gapminderdata.csv")
# save gapminder data
write.table(gapminder, "D:\\Uni\\UQ\\LADAL\\SLCLADAL.github.io\\data/gapminder.txt", sep = "\t", col.names = T, row.names = F)
# load data 
gapminderdata <- read.delim("D:\\Uni\\UQ\\LADAL\\SLCLADAL.github.io\\data/gapminder.txt", header = T, sep = "\t")
# create map
Map <- data.frame(gapminderdata$country, 
                  gapminderdata$year, gapminderdata$lifeExp)
# def. column names
names(Map) <- c("Country", "Year", "LifeExpectancy")
names(table(Map$Country))
# remove data point not from 2007
Map <- Map[Map$Year == "2007",]
# create motion chart object
Geo=gvisGeoMap(Map, locationvar = "Country", numvar = "LifeExpectancy", 
               options = list(height = 350, dataMode = 'regions'))
# display motion chart
plot(Geo)
# inspect fruid data set
str(Fruits)
# create motion chart object
M <- gvisMotionChart(Fruits, idvar = "Fruit", timevar = "Year")
# display motion chart object
plot(M)
# read in data
compdata <- read.table("D:\\R\\MapsInR/compdata.txt", sep = "\t", header = T)
# inspect data structure
str(compdata)
# create motion chart object
MC <- gvisMotionChart(compdata, idvar = "verb", timevar = "decade")
# display motion chart object
plot(MC)
# load data
convdata <- read.table("D:\\R\\MapsInR/convdata.txt", sep = "\t", header = T)
# inspect data structure
str(convdata)
# create motion chart object
MC <- gvisMotionChart(convdata, idvar = "VERB", timevar = "DECADE")
# display motion chart object
plot(MC)
# load data
ampadjdata <- read.table("D:\\Uni\\Projekte\\02-Intensification\\AmpCOHA/collex_decade.txt", sep = "\t", header = T)
# load library
library(dplyr)
# process data
ampadjdata <- ampadjdata %>%
  select(Decade, Amp, Adjective, OBS) %>%
  rename(Frequency = OBS) %>%
  mutate(Bigram = paste(Amp, Adjective, sep = "")) %>%
  select(Decade, Bigram, Frequency)
# inspect data
head(ampadjdata)
# create motion chart
MC <- gvisMotionChart(ampadjdata, idvar = "Bigram", timevar = "Decade")
plot(MC)
intsaus <- read.table("D:\\Uni\\Projekte\\02-Intensification\\AmpAusENZE/datadf5.txt", 
                      sep = "\t", header = T)
intsaus <- intsaus[intsaus$int == 1,]
intsaus <- intsaus[intsaus$txtyp == "PrivateDialogue",]
# label infrequent amplifier as "other"
freqpinttb <- table(intsaus$pint)
freqpint <- names(freqpinttb)[which(freqpinttb >= 10)]
intsaus$pint <- as.vector(unlist(sapply(intsaus$pint, function(x){
  x <- ifelse(x %in% freqpint, x, "other")
})))
# label infrequent adjective as "other"
freqadjtb <- table(intsaus$adj)
freqadj <- names(freqadjtb)[which(freqadjtb >= 20)]
intsaus$adj <- as.vector(unlist(sapply(intsaus$adj, function(x){
  x <- ifelse(x %in% freqadj, x, "other")
})))
table(intsaus$pint)
str(intsaus)
t1 <- ftable(intsaus$pint, intsaus$adj, intsaus$age)
t1
amplifiers <- as.vector(unlist(attr(t1, "row.vars")[1]))
adjectives <- as.vector(unlist(attr(t1, "row.vars")[2]))
age <- as.vector(unlist(attr(t1, "col.vars")[1]))
age
amp <- c(rep(c(rep(amplifiers, each = 5)), 5))
adj <- c(rep(c(rep(adjectives, 5)), 5))
age <- rep(age, 25)         
freqs <- as.vector(unlist(t1))
ampaus <- data.frame(age, amp, adj, freqs)
colnames(ampaus) <- c("age", "amp", "adj", "freqs")
ampaus$age <- as.numeric(as.factor(ampaus$age))
ampaus <- ampaus[order(ampaus$age),]
#tapply(ampaus$age, list(ampaus$amp, ampaus$adj, ampaus$freq), mean)
head(ampaus, 20)
MC <- gvisMotionChart(ampaus, idvar = "amp", timevar = "age")
plot(MC)
intsaus <- read.table("D:\\Uni\\Projekte\\02-Intensification\\07AmpAusENZE\\AmpAusENZE/ptbintsausattr_tb01.txt", 
                      sep = "\t", header = T)
intsaus <- t(intsaus)
intsausdf <- as.data.frame(intsaus)
colnames(intsausdf) <- c("pretty", "really", "so", "very")
rownames(intsausdf) <- c(5, 4, 3, 2, 1)
str(intsausdf)
intsausdf
age <- rep(rownames(intsausdf), 4)
amplifier <- rep(colnames(intsausdf), each = 5)
frequency <- as.vector(unlist(intsausdf))
pintaus <- data.frame(age, amplifier, frequency)
pintaus$age <- as.numeric(pintaus$age)
str(pintaus)
pintaus
MC <- gvisMotionChart(pintaus, idvar = "amplifier", timevar = "age")
plot(MC)
#write.table(compdata, "D:\\R\\MapsInR/compdata.txt", sep = "\t", col.names = T)
#write.table(convdata, "D:\\R\\MapsInR/convdata.txt", sep = "\t", col.names = T)

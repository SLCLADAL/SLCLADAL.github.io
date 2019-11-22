
# "Motion Charts in R"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("googleVis", "dplyr"))
# load library
library(googleVis)
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

###################################################
###        VORBEREITUNG: DATEN EINLESEN IN R
###################################################
# installieren der libraries
#install.packages(c("ape", "boot", "ca", "calibrate", "car", "cluster", "data.table",
#  "devtools", "effects", "ez", "factoextra", "FactoMineR", "ggplot2", "glmulti",
#  "gplots", "graphics", "grDevices", "Hmisc", "languageR", "lme4", "lmtest",
#  "MASS", "mlogit", "modeest", "NbClust", "nlme", "NLP", "plyr", "pvclust",
#  "QuantPsyc", "RLRsim", "rms", "seriation", "sjPlot", "stringr", "tm", "vcd",
#  "visreg"))
# WARNING! Pfad anpassen (adapt path)
# Workspeace festlegen
#setwd("D:\\Uni\\Lehre\\00-Workshops\\StatisticsInTheHumanitiesWithR")
setwd("D:\\Uni\\Lehre\\00-Workshops\\StatisticsInTheHumanitiesWithR")
getwd()

###################################################
# WARNING: To use this script you need to set our own paths!
# Your path should be the path to the corpus on your own computer!
# Remember to use double backslash instead of single backslash, if
# you use Windows on your machine.
# definiere pfad zu daten
path <- "data01.txt"
# verschiedene möglichkeiten daten einzuladen
# möglichkeit 1
mydata <- read.delim(path, sep="\t", header = T)
# möglichkeit 2
#mydata <- read.delim(choose.files(), sep="\t", header = T)
# möglichkeit 3
mydata <- read.delim(path, sep = "\t", header = T)
# daten betratchten
str(mydata)

# ersten 6 zeilen betrachten
head(mydata)

# save data for later use
data01 <- mydata
###################################################
###        VORBEREITUNG: DATEN SPEICHERN AUS R
###################################################
# zielpfad festlegen
outpath <- "saved_data01.txt"
# daten auf pc speichern
write.table(mydata, outpath, sep = "\t", col.names = TRUE, row.names = F,quote = F)
#######################################################
###          EINFACHE Grafiken
#######################################################
# load data
path <- "data03.txt"
mydata <- read.delim(path, header = T, sep = "\t")
# order data
mydata <- mydata[order(mydata$Variable2),]
par(mfrow = c(2,2)) # um vier grafiken pro fenster anzuzeigen
plot(Variable1 ~ Variable2, type = "p", data = mydata, ylab = "Variable1", xlab = "Variable2", main = "plot type 'p' (points)")
plot(Variable1 ~ Variable2, type = "l", data = mydata, ylab = "Variable1", xlab = "Variable2", main = "plot type 'l' (lines)")
plot(Variable1 ~ Variable2, type = "b", data = mydata, ylab = "Variable1", xlab = "Variable2", main = "plot type 'b' (both points and lines)")
plot(Variable1 ~ Variable2, type = "h", data = mydata, ylab = "Variable1", xlab = "Variable2", main = "plot type 'h' (histogram)")
par(mfrow = c(1,1)) # um eine grafik pro fenster anzuzeigen

# warum keien pie charts?
par(mfrow = c(1,2)) # um vier grafiken pro fenster anzuzeigen
datapie <- table(data01$reside)[which(table(data01$reside) > 10)]
datapie <- datapie[order(datapie, decreasing = T)]
pie(datapie)
barplot(datapie)
par(mfrow = c(1,1)) # um eine grafik pro fenster anzuzeigen

plot(Variable1 ~ Variable2, type = "b", data = mydata, ylab = "x-Achse", xlab = "y-Achse",  main = "New title", xlim = c(35, 55), ylim = c(30, 120),  pch = 3, col = "red", lty = 3, lwd = 2, cex.axis = 1.5, cex.lab = 1.5,  cex.main = 1.5)

plot(Variable1 ~ Variable2, data = mydata, ylab = "length", xlab = "Frequency")
abline(lm(Variable1 ~ Variable2, data = mydata), col = "blue", lty=2)

plot(Variable1 ~ Variable2, data = mydata, ylab = "length",  xlab = "Frequency", pch = 16)
abline(lm(Variable1 ~ Variable2, data = mydata), col = "red", lty=3)

newdata <- data.frame(c(rep("female", 50), rep("male", 50)), c(rnorm(n = 50, mean = 20, sd = 10), rnorm(n = 50, mean = 50, sd = 20)))
colnames(newdata) <- c("Gender", "Frequency")

boxplot(Frequency ~ Gender, data = newdata, ylab = "Frequency", xlab = "Gender")

boxplot(Frequency ~ Gender, data = newdata, ylab = "Frequency", xlab = "Gender", notch=TRUE,
 col=(c("lightgreen","lightgrey")))

barplot(tapply(newdata$Frequency, newdata$Gender, mean), ylim = c(0,80), ylab = "Frequency", xlab = "Gender", col=(c("lightgreen","lightgrey")))
text(c(0.7,1.9), tapply(newdata$Frequency, newdata$Gender, mean)+5, paste("mean = ", round(tapply(newdata$Frequency, newdata$Gender, mean), 3), sep = ""))
grid()
box()
###
opar <- par() # speichern der originalen grafikparameter
par(mar=c(0,0,0,0), mfrow = c(1,2)) # um zwei grafiken pro fenster anzuzeigen
x <- c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5), rep(1:5, 5))
m <- matrix(x, ncol = 2)
plot(m, pch = 0:24, xlim = c(0,6), ylim = c(0,6), axes = F, xlab = "", ylab = "")
text(m[,1]+.25, m[,2], 0:24)
grid()
plot(m, pch = 0:24, xlim = c(0,6), ylim = c(0,6), axes = F, xlab = "", ylab = "", type = "n")
lines(x=c(0:6), y= c(rep(1, 7)), type="l")
par(lty = 2)
lines(x=c(0:6), y= c(rep(2, 7)), type="l")
par(lty = 3)
lines(x=c(0:6), y= c(rep(3, 7)), type="l")
par(lty = 4)
lines(x=c(0:6), y= c(rep(4, 7)), type="l")
par(lty = 5)
lines(x=c(0:6), y= c(rep(5, 7)), type="l")
par(lty = 6)
lines(x=c(0:6), y= c(rep(6, 7)), type="l")
text(rep(1, 6), seq(0.75, 5.75, 1), paste("lty=", 1:6))
par(opar) # wiederherstellen der originalen grafikparameter

# darstellung der häufigkeit der geschlechter im datenset
barplot(table(data01$sex), ylim = c(0, 500), xlim = c(0, 2.5), axes = T,
  main = "", ylab = "absolute Häufigkeit", xlab = "Geschlecht",
  col = c("red", "green"))
box()
grid()
# hilfe bei der barplot funktion
#?barplot

###################################################
# schönerer barplot
barplot(table(data01$sex), ylim=c(0, 600), space=.95,
  col = c("lightgrey", "lightblue"), xlab="Gender/Sex",
  ylab = "Absolute Frequency")
text(c(1.45, 3.4), table(data01$sex) + 25, table(data01$sex))
box()
grid()
legend("topleft", inset=.05, title="Gender/Sex",
   c("male", "female"), fill=c("lightgrey", "lightblue"), horiz=T)

###################################################
# boxplot
boxplot(data01$word.count ~ data01$sex, ylim=c(0, 2500), space=.95,
  col = c("lightgrey", "lightblue"), xlab="Gender/Sex",
  ylab = "Absolute Frequency", notch = T)
box()
grid()

#######################################################
###          MASSE ZENTRALER TENDENZ
#######################################################
# lade bibliotheken
library(modeest)
# zielpfad festlegen
path <- "data06.txt"
# daten laden
mydata <- read.table(path, header = T, sep = "\t", quote = "", comment.char = "")
# arithmetisches mittel (mean)
mean(mydata$word.count)

# tablulieren der variable age
age <- table(mydata$age)
# grafik erstellen
barplot(age, ylim=c(0,200), xlim = c(0, 7), ylab = "Absolute Häufigkeit (Sprecher)", xlab = "Altersgruppen")
text(seq(0.7, 6.7, 1.2), age+10, cex = 0.85, labels = age)
box()
grid()
a1 <- rep("0-18", age[1])
a2 <- rep("19-25", age[2])
a3 <- rep("26-33", age[3])
a4 <- rep("34-41", age[4])
a5 <- rep("42-49", age[5])
a6 <- rep("50+", age[6])
a <- c(a1, a2, a3, a4, a5, a6)
m <- ceiling(length(a)/2)
a[m]

median(a)

# modalwert
mydata <- read.table(path, header = T, sep = "\t", quote = "", comment.char = "")
res <- table(mydata$reside)
res <- res[which(res> 10)]
barplot(res, ylim=c(0, 150), ylab = "Absolute Häufigkeit (Sprecher)", xlab = "Current Residence")
text(seq(0.7, 6.7, 1.2), res+10, cex = 0.85, labels = res)
box()
grid()

# modalwertermitteln
modalwert <- max(res)
names(modalwert) <- as.vector(unlist(attr(res, "dimnames")))[which(res == max(res))]
modalwert

# anmerkungen
k1 <- c(9.2, 11.4, 27.1, 13.7, 9.6)
k2 <- c(0.7, 0.0, 1.1, 68.7, 0.5)
mean (k1)

mean (k2)

# tabelle der werte erstellen
k <- rbind(k1, k2)
colnames(k) <- c("A", "B", "C", "D", "E")
rownames(k) <- c("Korpus 1", "Korpus 2")
test <- barplot(k, ylab = "Relative Häufigkeit (pro 1,000 Wörter)", xlab = "Sprecher",
  col = c("lightgreen", "lightblue"), beside=TRUE, ylim = c(0, 100), legend = rownames(k))
p <- c(1.5, 2.5, 4.5, 5.5, 7.5, 8.5, 10.5, 11.5, 13.5, 14.5)
w <- c(9.2, 0.7, 11.4, 0.0, 27.1, 1.1, 13.7, 68.7, 9.6, 0.5)
text(p, w+5, cex = 0.85, labels = w)
box()
grid()

medianzent1 <- k1[order(k1)]
medianzent2 <- k2[order(k2)]
median(k1)

median(k2)

#extraction of most likely value
# Bickel's measure of skewness: > 0  = leftskewed
# Bickel's measure of skewness: < 0  = rightskewed
mlv(k1)

#######################################################
###          STREUUNGSMAßE
#######################################################
s1 <- c(-5, -12, 5, 12, 15, 18, 22, 23, 20, 16, 8, 1)
s2 <- c(7, 7, 8, 9, 10, 13, 15, 15, 13, 11, 8, 7)
mean(s1, na.rm = T) #  if the vector contains NA values, mean doesn't work unless na.rm = T

mean(s2)

plot(s1, axes = F, type = "l", ylab = "Temperatur in °Celsius", xlab = "Monate",
  lty = 1, lwd = 2, col = "red", cex.axis = 1.5, cex = 1.5, cex.lab = 1.5)
lines(s2, lty = 2, col = "blue", lwd = 2)
lx <- c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August",
  "September", "Oktober", "November", "Dezember")
ly <- c(-10, -5, 0, 5, 10, 15, 20)
lyt <- c("-10", "-5", "0", "5", "10", "15", "20")
axis(1, at=1:12, labels=lx, cex.axis = 1.5)
axis(2, at = ly, labels = lyt, cex.axis = 1.5, las = 2)
legend("topleft", inset=.05, c("Stadt A","Stadt B"), lty = c(1, 2),
  col = c("red", "blue"), horiz=TRUE)
box()
grid()

# test for normality
# WARNING: inexact for low N, too precise or large N!
shapiro.test(s1) # p > .05 = normal

shapiro.test(s2) # p > .05 = normal

# extract variance
var(s1)

var(s2)

# extract standard deviation; sd(s1) = sqrt(var(s1))
sd(s1)

sd(s2)

#######################################################
###          NON-NORMAL DATA
# test for normality
# WARNING: inexact for low N, too precise or large N!
shapiro.test(k1) # p < .05 = non-normal

shapiro.test(k2) # p < .05 = non-normal

# streuungsmasse bei nicht normalverteilten daten
summary(k1)

summary(k2)

quantile(k1, c(0.25, 0.5, 0.75))

quantile(k2, c(0.25, 0.5, 0.75))

# quantile(k1, 0.5) = median(k1)

# extract range (lowest and highest value)
range(k1)

range(k2)

# extract median absolute deviation (median distance from median)
mad(k1, constant = 1)

mad(k2, constant = 1)

# extract interquartile range
IQR(k1) # sqrt((quantile(k1, .25) - quantile(k1, .75))^2)

IQR(k2)

#######################################################
###           WHAT TO DO WITH NON NORMAL DATA?
# inspect data
head(mydata)

# 4 plots in one window (2 rows and 2 columns)
par(mfrow=c(2,2))
# plot word.count (histogram)
hist(mydata$word.count)

# plot word.count (density plot)
plot(density(mydata$word.count))

# plot word.count (quantile-quantile plot)
qqnorm(mydata$word.count)

# add line showing normal distribution)
qqline(mydata$word.count)

# boxplot of data
boxplot(mydata$word.count) # dots in boxplot show outliers

# 1 plot per window
par(mfrow=c(1,1))

# WARNING: data is positively skewed!
# test for normality (confirm non-normality)
shapiro.test(mydata$word.count)

# remove outliers
# find outliers (boxplot)
boxplot(mydata$word.count) # dots in boxplot show outliers

# find outliers (z-value based)
wc <- scale(mydata$word.count, center = TRUE, scale = TRUE)
# remove values that exceed 1.96 (95%) sds
nrow(mydata)

mydataWoOutliers <- mydata[-c(which(wc >= 1.96 | wc <= -1.96)),]

nrow(mydataWoOutliers)

boxplot(mydataWoOutliers$word.count)

# transform variable (add 1 to value to avoid values of 0 (log(0) = -inf) and then log them
mydata$logwc <- log1p(mydata$word.count) # log (values +1)
mydata$sqrtwc <- sqrt(mydata$word.count) # take square root of data
# inspect logged data
par(mfrow=c(2,2))            # 4 plots in one window (2 rows and 2 columns)
hist(mydata$logwc)           # plot word.count (histogram)
plot(density(mydata$logwc))  # plot word.count (density plot)
qqnorm(mydata$logwc)         # plot word.count (quantile-quantile plot)
qqline(mydata$logwc)         # add line showing normal distribution)
boxplot(mydata$logwc)        # boxplot of data, dots in boxplot show outliers
par(mfrow=c(1,1))            # 1 plot per window
# test for normality
shapiro.test(mydata$logwc)  # logging not appropriate here

# inspect square root transformed data
par(mfrow=c(2,2))            # 4 plots in one window (2 rows and 2 columns)
hist(mydata$sqrtwc)          # plot word.count (histogram)
plot(density(mydata$sqrtwc)) # plot word.count (density plot)
qqnorm(mydata$sqrtwc)        # plot word.count (quantile-quantile plot)
qqline(mydata$sqrtwc)        # add line showing normal distribution)
boxplot(mydata$sqrtwc)       # boxplot of data, dots in boxplot show outliers
par(mfrow=c(1,1))            # 1 plot per window
# test for normality
shapiro.test(mydata$sqrtwc)  # square root transforming appropriate here

# rules of thumb for transformations
# sqrt(DepVar)    : moderatly pos. skewed data
# (DepVar)^2      : neg. skewed data
# 1/(DepVar + 1)  : J shaped data

data01 <- mydata # save data fro later use
#######################################################
###          WAHRSCHEINLICHKEITEN
#######################################################
# load packages
library(grDevices)
# set up data
p100 <- dbinom(0:100, 100, 0.5)
w100 <- c(0:100)
wtb <- rbind(w100, p100)
colnames(wtb) <- c(0:100)
rownames(wtb) <- c("anzahl (kopf)", "prob")
xseq <- seq(0.7, 120.7, 1.2)
xseq <- xseq[c(1, 20, 40, 60, 80, 100)]
# start plotting
par(mfrow = c(3,1))
barplot(wtb[2,], ylim = c(0, 0.08), ylab = "Probability", xlab = "Anzahl (Kopf)",
  axes = F, axisnames = F, col = "lightgrey")
axis(1, at= xseq, labels= c("0", "20", "40", "60", "80", "100"))
axis(2, at= c(0.0, 0.02, 0.04, 0.06, 0.08), labels= c("0.00", "", "0.04", "", "0.08"))
box()
grid()
# summ probs
sp <- cumsum(wtb[2,])
ttsp <- as.vector(unlist(sapply(sp, function(x){
  x <- ifelse(x <= 0.025, TRUE,
    ifelse(x >= 0.975, TRUE, FALSE))
    })))
barplot(wtb[2,], ylim = c(0, 0.08), ylab = "Probability", xlab = "Anzahl (Kopf)",
  axes = F, axisnames = F, , col = ifelse(ttsp == T, "red", "lightgrey"))
axis(1, at= xseq, labels= c("0", "20", "40", "60", "80", "100"))
axis(2, at= c(0.0, 0.02, 0.04, 0.06, 0.08), labels= c("0.00", "", "0.04", "", "0.08"))
box()
grid()
text(75, 0.06, expression(paste(mu^1 !=  mu^2, sep = "")))
###
ttsp <- as.vector(unlist(sapply(sp, function(x){
  x <- ifelse(x <= 0.95, TRUE, FALSE)
    })))
barplot(wtb[2,], ylim = c(0, 0.08), ylab = "Probability", xlab = "Anzahl (Kopf)",
  axes = F, axisnames = F, col = ifelse(ttsp == T, "lightgrey", "red"))
axis(1, at= xseq, labels= c("0", "20", "40", "60", "80", "100"))
axis(2, at= c(0.0, 0.02, 0.04, 0.06, 0.08), labels= c("0.00", "", "0.04", "", "0.08"))
box()
grid()
text(75, 0.06, expression(paste(mu^1 >=  mu^2, sep = "")))
par(mfrow = c(1,1))

#############################################
###            DIE NORMALVERTEILUNG
#############################################
par(mfrow = c(3,1))
### normal distribution: Mu=0, Sigma=1: Standard normal
library(graphics)
plot(dnorm, -4, 4, axes = F, xlab = "Standard Deviations", ylab = "Probability")
axis(1, at = seq(-4, 4, 1), labels = seq(-4, 4, 1))
axis(2, at= c(0.0, 0.2, 0.4), labels= c("0.0", "0.2", "0.4"))
text(3, 0.2, expression(paste(mu, "=0, ", sigma^2, "=1", sep = "")))
box()
lines(c(0, 0), c(-1, 0.5), col = "lightgrey")
lines(c(-0.675, 0.675), c(rep(0.3, 2)), col="red", lty=2)
text(0, 0.35, "67.5%")
lines(c(-1.96,1.96), c(rep(0.2, 2)), col="blue", lty=2)
text(0, 0.25, "95%")
lines(c(-2.576,2.576), c(rep(0.1, 2)), col="green", lty=2)
text(0, 0.15, "99%")
lines(c(-0.675, -0.675), c(-1, 0.5), col = "red")
lines(c(0.675, 0.675), c(-1, 0.5), col = "red")
lines(c(-1.96, -1.96), c(-1, 0.5), col = "blue")
lines(c(1.96, 1.96), c(-1, 0.5), col = "blue")
lines(c(-2.576, -2.576), c(-1, 0.5), col = "green")
lines(c(2.576, 2.576), c(-1, 0.5), col = "green")
text(-2.9, 0.025, "-2.576", cex = .75)
text(-1.6, 0.025, "-1.96", cex = .75)
text(-1, 0.025, "-0.675", cex = .75)
text(2.9, 0.025, "2.576", cex = .75)
text(1.7, 0.025, "1.96", cex = .75)
text(1, 0.025, "0.675", cex = .75)
###
plot(dnorm, -4, 4, axes = F, xlab = "Standard Deviations", ylab = "Probability")
axis(1, at = seq(-4, 4, 1), labels = seq(-4, 4, 1))
axis(2, at= c(0.0, 0.2, 0.4), labels= c("0.0", "0.2", "0.4"))
text(3, 0.2, expression(paste(mu, "=0, ", sigma^2, "=1", sep = "")))
box()
lines(c(1.96, 1.96), c(0, 0.5), col="lightgrey", lty=2)
text(2.25, 0.15, "2.5%")
text(2.25, 0.085, "(sd=1.96)")
lines(c(-1.96, -1.96), c(0, 0.5), col="lightgrey", lty=2)
text(-2.25, 0.15, "2.5%")
text(-2.25, 0.085, "(sd=-1.96)")
###
plot(dnorm, -4, 4, axes = F, xlab = "Standard Deviations", ylab = "Probability")
axis(1, at = seq(-4, 4, 1), labels = seq(-4, 4, 1))
axis(2, at= c(0.0, 0.2, 0.4), labels= c("0.0", "0.2", "0.4"))
text(3, 0.2, expression(paste(mu, "=0, ", sigma^2, "=1", sep = "")))
box()
lines(c(1.64, 1.64), c(0, 0.5), col="lightgrey", lty=2)
text(2.2, 0.15, "5% (sd=1.64)")
par(mfrow = c(1,1))

#############################################
###            NICHT-PARAMETRISCHE TESTS
#############################################
###            WILCOX TEST
#############################################
# load data
path <- "swire.txt"
mydata <- read.delim(path, sep="\t", header = T)
# perform preliminary statz on age
wilcox.test(mydata$ptw ~ mydata$sex)

#############################################
###            CHI QUADRAT TEST
#############################################
# einlesen der daten
path <- "data02.txt"
mydata <- read.table(path, header = F, sep = "\t", quote = "", comment.char = "")
# betiteln
colnames(mydata) <- c("BrE", "AmE")
rownames(mydata) <- c(" kindof ", " sortof ")
# daten betrachten
mydata

# daten visualisieren
par(mfrow =c(1, 2)) # zwei plots in zwei spalten in einem fenster
assocplot(as.matrix(mydata))
mosaicplot(mydata , shade = TRUE , type = "pearson", main = "")
par(mfrow =c(1, 1)) # herstellen der original parameter
# ein plot pro fenster
# testen
chisq.results <- chisq.test(mydata , corr = F)
# ergebnis ebtrachten
chisq.results

# effektstaerke berechnen
phi.coefficient = sqrt(chisq.results$statistic/sum(mydata)*(min(dim(mydata)) -1))
# ergebnis ebtrachten
phi.coefficient

#############################################
###            CHI QUADRAT TEST (2*k)
#############################################
source("RSkripte/x2.2k.r")
# daten generieren
chitb2 <- matrix(c(21 , 14, 18, 13, 24, 12, 13, 30), byrow = T, nrow = 4)
colnames(chitb2) <- c("erreicht", "nichterreicht")
rownames(chitb2) <- c("rweich", "rhart", "beta", "licht")
# teiltabelle extrahieren
chitb3 <- matrix(c(21 , 14, 18, 13) , byrow = T, nrow = 2)
colnames(chitb3) <- c("erreicht", "nichterreicht")
rownames(chitb3) <- c("rweich", "rhart")
# einfacher x2 - test
chisq.test(chitb3 , corr = F)

x2.2k(chitb2, 1, 2)

#############################################
###            CHI QUADRAT TEST (z*k)
#############################################
x <- matrix(c(8, 31, 44, 36, 5, 14, 25, 38, 4, 22, 17, 12, 8, 11, 16, 24), ncol =4)
attr(x, "dimnames") <-list(Register =c("acad", "spoken", "fiction", "new"),
Metaphor = c("Heated fluid", "Light", "NatForce", "Other"))

subtable <- matrix(c(14, 25, 22, 17) , ncol =2)
chisq.results <- chisq.test(subtable, correct = FALSE ) # WRONG !
phi.coefficient = sqrt(chisq.results$statistic/ sum(subtable) * (min(dim(subtable)) -1))
chisq.results
phi.coefficient

source("RSkripte/sub.table.r") # funktion zur analyse von untertabellen
results <- sub.table(x, 2:3 , 2:3 , out ="short")
results

#############################################
###            CORRESPONDENCE ANALYSIS
#############################################
# load packages
library(FactoMineR)
library(ggplot2)
library(ca)
library(factoextra)
library(gplots)
library(graphics)
library(vcd)
#library(devtools)
#devtools::install_github("kassambara/factoextra")
#############################################
###      EINFACHE KORRESPONDENZANALYSE
# introduction
# einlesen der daten
path <- "data07.txt"
housetasks <- read.table(path, header = T, sep = "\t", quote = "", comment.char = "", row.names = 1)
dt <- as.table(as.matrix(housetasks))
# balloonplot
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="", label = F, show.margins = F)
# mosaicplot
mosaicplot(dt, shade = TRUE, las=2, main = "housetasks")
# association plot
assoc(head(dt), shade = T, las=3)
# perform chi-squre on the table
chisq <- chisq.test(housetasks)
chisq

# perform correspondence analysis
res.ca <- CA(housetasks, graph = FALSE)
# print results
print(res.ca)
# summarize results
summary(res.ca, nb.dec = 2, ncp = 2)
# test if rows and columns are correlated
eig <- get_eigenvalue(res.ca)
trace <- sum(eig$eigenvalue)
cor.coef <- sqrt(trace)
cor.coef
# test if rows and columns are correlated
chi2 <- trace*sum(as.matrix(housetasks))              # extract chi2 value
df <- (nrow(housetasks) - 1) * (ncol(housetasks) - 1) # extract degrees of freedom
pval <- pchisq(chi2, df = df, lower.tail = FALSE)     # extract p-value
# inspect results
chi2; df; pval

# inspect correspondence values (model fit)
eigenvalues <- get_eigenvalue(res.ca)
head(round(eigenvalues, 2))

# plot correspondence
plot(res.ca)

# nicer plot
fviz_ca_biplot(res.ca, geom = "text") + theme_minimal()

#############################################
# dateipfad bestimmen und daten laden
path <- "data04.txt"
cadata <- read.table(path, header = T, sep = "\t", quote = "", comment.char = "")
# transform data
mytable <- with(cadata, table(pint,adjs)) # create a 2 way table
#prop.table(mytable, 1) # row percentages
#prop.table(mytable, 2) # column percentages
fit <- ca(mytable)
#print(fit) # basic results
#summary(fit) # extended results
plot(fit) # symmetric map

fviz_ca_biplot(fit, geom = "text") + theme_minimal()

#######################################################
###          EINFACHE LINEARE REGRESSION
#######################################################
# Scatterplot with lines from teh regression line to the dots
x <- c(173, 169, 176, 166, 161, 164, 160, 158, 180, 187)
y <- c(80, 68, 72, 75, 70, 65, 62, 60, 85, 92) # plot scatterplot and the regression line
mod1 <- lm(y ~ x)

par(mfrow=c(1, 3)) # zwei plots in zwei spalten in einem fenster
# ein plot pro fenster
plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
abline(mod1, lwd=2)
plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
abline(mod1, lwd=2)

# calculate residuals and predicted values
res <- signif(residuals(mod1), 5)
pre <- predict(mod1) # plot distances between points and the regression line
segments(x, y, x, pre, col="red")
# add labels (res values) to points
library(calibrate)
textxy(x, y, res, cex=1)
par(mfrow=c(1, 1)) # herstellen der original parameter

#######################################################
# set options # to avoid mathematical notation, e.g. 5.43 e-12
options("scipen" = 100, "digits" = 4)
# Initiate the packages
library(QuantPsyc)
library(car)
library(ggplot2)
source("RSkripte/multiplot_ggplot2.R")
source("RSkripte/slr.summary.tb.R") # load modified summary function for slr
###############################################################
### Load and manipulate data
###############################################################
# Read in data
path <- "slrdata.txt"
slrdata <- read.table(path, header = TRUE, sep = "\t", quote = "", comment.char = "")
# remove columns we do not need
slrdata <- as.data.frame(cbind(slrdata$datems, slrdata$pptw))
colnames(slrdata) <- c("year", "prep.ptw") # add column names
slr.data <- slrdata[!is.na(slrdata$year) == T, ] # delete incomplete cases
head(slrdata) # inspect data

str(slrdata) # inspect data structure

summary(slrdata) #inspect data values

###############################################################
### Visualize and eyeball data
###############################################################
# set up this plot from scratch
p2 <- ggplot(slrdata, aes(year, prep.ptw)) +
  geom_point() +
  labs(x = "Year") +
  labs(y = "Prepositions per 1,000 words") +
  geom_smooth()

# save plot (remove # to activate)
#imageDirectory <- "C:\\01-University\\03-Lehre\\StatisticsForLinguists"
#imageFile <- paste(imageDirectory,"PrepositionUseByYear.png",sep="/")
#ggsave(file = imageFile)
  
# set up this plot from scratch
p3 <- ggplot(slrdata, aes(year, prep.ptw)) +
  geom_point() +
  labs(x = "Year") +
  labs(y = "Prepositions per 1,000 words") +
  geom_smooth(method = "lm") # with linear model smoothing!
multiplot(p2, p3, cols = 2)

###############################################################
# center year
slrdata$year <- scale(slrdata$year, center = TRUE, scale = F)
# set up Simple Linear Regression model and inspect properties of the model
prep.lm <- lm(prep.ptw ~ year, data = slrdata)
summary(prep.lm)

# use plots to check if there are problems with the model
# set graphic's parameters to display 3 plots in one row
par(mfrow = c(1, 3))
plot(resid(prep.lm))
plot(rstandard(prep.lm))
plot(rstudent(prep.lm))
par(mfrow = c(1, 1)) # restore original graphic's parameters

# Create a 2x2 matrix of diagnostic plots
par(mfrow = c(2, 2))
plot(prep.lm)
par(mfrow = c(1, 1))

# problemtic cases with studentized residuals greater or equal to 1
sresid <- influence.measures(prep.lm)
which(sresid$infmat[, 5] >= 1)

# extract standrdized residuals
stdresid <- rstandard(prep.lm)
# standard residuals with values greater or equal to 3 (delete cases!)
which(stdresid >= sqrt(3^2))

# percent of standard residuals with values greater or equal to 2.58 (max: 1%)
length(which(stdresid >= sqrt(2.58^2)))/length(stdresid)*100

# percent of standard residuals with values greater or equal to 1.96 (max: 5%)
length(which(stdresid >= sqrt(1.96^2)))/length(stdresid)*100

# write diagnistic summary function
lmdiag <- function(x) {
  sresid <- influence.measures(x)
  ifelse(length(which(sresid[[1]][, 5] >= 1)) != 0,
    r1 <- paste("problematic cases: ", which(sresid[[1]][, 5] >= 1)),
    r1 <- paste("diagnostics acceptable: no cases with cook's distance greater than 1"))

  stdresid <- rstandard(x)
  ifelse(length(which(stdresid >= 3.29)) != 0,
    r2 <- paste("problematic cases (require removal): ", which(stdresid >= sqrt(3^2))),
    r2 <- paste("diagnostics acceptable: all standardized residuals have values lower than 3.29"))

  ifelse(length(which(stdresid >= 2.58))/length(stdresid)*100 >= 1,
    r3 <- paste(length(which(stdresid >= sqrt(2.58^2)))/length(stdresid)*100,
      "percent of studentized residuals have values greater than 2.58 (this should only be the case for max. 1 percent)"),
    r3 <- paste("diagnostics acceptable: less than 1 percent of standardized residuals exceed 2.58"))

  ifelse(length(which(stdresid >= 1.96))/length(stdresid)*100 >= 5,
    r4 <- paste(length(which(stdresid >= sqrt(1.96^2)))/length(stdresid)*100,
      "percent of studentized residuals have values greater than 1.96 (this should only be the case for max. 5 percent)"),
    r4 <- paste("diagnostics acceptable: less than 5 percent of standardized residuals exceed 1.96"))
  return(c(r1, r2, r3, r4))
}
lmdiag(prep.lm)

# Extract standardized betas
lm.beta(prep.lm)

slr.summary(prep.lm) # inspect the results

##############################################################
###                 MULTIPLE LINEARE REGRESSION
##############################################################
# pakete initialisieren
library(car)
library(QuantPsyc)
library(boot)
library(ggplot2)
# load functions
source("RSkripte/multiplot_ggplot2.R")
source("RSkripte/mlinr.summary.r")
source("RSkripte/SampleSizeMLR.r")
source("RSkripte/ExpR.r")
# optionen festlegen
options("scipen" = 100, "digits" = 4)
# daten laden
mlrdata <- read.delim("mlrdata.txt", header = TRUE)
# ersten zeilen der daten betrachten
head(mlrdata)

# struktur der daten betrachten
str(mlrdata)

# zusammenfassung der daten betrachten
summary(mlrdata)

p1 <- ggplot(mlrdata, aes(status, money)) +
 geom_boxplot(notch = T, aes(fill = factor(status))) +
 scale_fill_brewer() +
 theme_bw() + # backgroud white(inactive to default grey)
 labs(x = "") +
 labs(y = "Money spent on present (Euro)") +
 coord_cartesian(ylim = c(0, 250)) +
 guides(fill = FALSE) +
 ggtitle("Status")
p2 <- ggplot(mlrdata, aes(attraction, money)) +
 geom_boxplot(notch = T, aes(fill = factor(attraction))) +
 scale_fill_brewer() +
 theme_bw() + # backgroud white(inactive to default grey)
 labs(x = "") +
 labs(y = "Money spent on present (Euro)") +
 coord_cartesian(ylim = c(0, 250)) +
 guides(fill = FALSE) +
 ggtitle("Attraction")
p3 <- ggplot(mlrdata, aes(x = money)) +
 geom_histogram(aes(y=..density..),
 binwidth = 10,
 colour = "black", fill = "white") +
 geom_density(alpha=.2, fill = "#FF6666") # Overlay with transparent density plot
p4 <- ggplot(mlrdata, aes(status, money)) +
 geom_boxplot(notch = F, aes(fill = factor(status))) +
 scale_fill_brewer(palette="Paired") +
 facet_wrap(~ attraction, nrow = 1) +
 theme_bw() + # backgroud white(inactive to default grey)
 labs(x = "") +
 labs(y = "Money spent on present (Euro)") +
 coord_cartesian(ylim = c(0, 250)) +
 guides(fill = FALSE)
# Plot the plots
multiplot(p1, p3, p2, p4, cols = 2)

# generieren der minimalen baselinemodelle, die nur den
# intercept (mittelwert) als unabh. variable beinhalten
m0.mlr = lm(money ~ 1, data = mlrdata) # baseline model
m0.glm = glm(money ~ 1, family = gaussian, data = mlrdata)
# ergebnisse betrachten
summary(m0.mlr)

# ergebnisse betrachten
summary(m0.glm)

#############################
# generieren der saturated models, die alle
# unabh. variablen und interaktionen beinhalten
m1.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m1.glm = glm(money ~ status * attraction, family = gaussian, data = mlrdata)
# ergebnisse betrachten
summary(m1.mlr)

# ergebnisse betrachten
summary(m1.glm)

# automatisches modelfitting
# kriterium: AIC (um so kleiner umso besser)
step(m1.mlr, direction = "both")

# minimales adequates modell generieren
m2.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m2.glm = glm(money ~ (status + attraction)^2, family = gaussian, data = mlrdata)

# zusammenfassugn der modellergebnisse betrachten
summary(m2.mlr)

# konfidenzintervalle der koeffizineten
confint(m2.mlr)

# vergleich zwiscehn dem baseline-modell und dem minimal adequate model
anova(m0.mlr, m2.mlr)

Anova(m0.mlr, m2.mlr, type = "III")

# suche nach problematischen datenpunkten
# erzeugen diagnostischer grafiken
par(mfrow = c(3, 2))
plot(m2.mlr)
qqPlot(m2.mlr, main="QQ Plot")
# Cooks D plot
# D-werte > 4/(n-k-1) sind problematisch
cutoff <- 4/((nrow(mlrdata)-length(m2.mlr$coefficients)-2))
plot(m2.mlr, which=4, cook.levels = cutoff)
par(mfrow = c(1, 1))

# entfernen zu einflussreicher datenpunkte
# um dies zu tun extrahieren wir diagnostische
# werte zu allen datenpunkten und addieren die
# spalten mit diesen werten zu unserem
# datensatz hinzu
infl <- influence.measures(m2.mlr)

# addieren der einflussstatistiken zu dme datensatz
mydata <- data.frame(mlrdata, infl[[1]], infl[[2]])
head(mydata)

# zu einflussreiche datenpunkte erkennen
remove <- apply(infl$is.inf, 1, function(x) {
 ifelse(x == TRUE, return("remove"), return("keep")) } )

# informationen zu den zu einflussreichen datenpunkten
# zum datensatz hinzuaddieren
mlrdata <- data.frame(mlrdata, remove)
# zeilenzahl des alten datensatzes anzeigen
nrow(mydata)

outs <- mlrdata[mlrdata$remove == "remove", ]
mlrdata <- mlrdata[mlrdata$remove == "keep", ]
# zeilenzahl des neuen datensatzes anzeigen
nrow(mlrdata)

# generieren der minimalen baselinemodelle, die nur den
# intercept (mittelwert) als unabh. variable beinhalten
m0.mlr = lm(money ~ 1, data = mlrdata) # baseline model
m0.glm = glm(money ~ 1, family = gaussian, data = mlrdata)
# ergebnisse betrachten
summary(m0.mlr)

# ergebnisse betrachten
summary(m0.glm)

#############################
# generieren der saturated models, die alle
# unabh. variablen und interaktionen beinhalten
m1.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m1.glm = glm(money ~ status * attraction, family = gaussian, data = mlrdata)
# ergebnisse betrachten
summary(m1.mlr)

# ergebnisse betrachten
summary(m1.glm)

#############################
# automatisches modelfitting
# kriterium: AIC (um so kleiner umso besser)
step(m1.mlr, direction = "both")

# minimales adequates modell generieren
m2.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m2.glm = glm(money ~ (status + attraction)^2, family = gaussian, data = mlrdata)

# zusammenfassung der modellergebnisse betrachten
summary(m2.mlr)

# konfidenzintervalle der koeffizineten
confint(m2.mlr)

# vergleich zwiscehn dem baseline-modell und dem minimal adequate model
anova(m0.mlr, m2.mlr)

Anova(m0.mlr, m2.mlr, type = "III")

# suche nach problematischen datenpunkten
# erzeugen diagnostischer grafiken
par(mfrow = c(3, 2))
plot(m2.mlr)
qqPlot(m2.mlr, main="QQ Plot")
# Cooks D plot
# D-werte > 4/(n-k-1) sind problematisch
cutoff <- 4/((nrow(mlrdata)-length(m2.mlr$coefficients)-2))
plot(m2.mlr, which=4, cook.levels = cutoff)
par(mfrow = c(1, 1))

# addieren von modelldiagnostiken zum datasatz
mlrdata$residuals <- resid(m2.mlr)
mlrdata$standardized.residuals <- rstandard(m2.mlr)
mlrdata$studentized.residuals <- rstudent(m2.mlr)
mlrdata$cooks.distance <- cooks.distance(m2.mlr)
mlrdata$dffit <- dffits(m2.mlr)
mlrdata$leverage <- hatvalues(m2.mlr)
mlrdata$covariance.ratios <- covratio(m2.mlr)
mlrdata$fitted <- m2.mlr$fitted.values

# erstellen diagnostischer grafiken
# (drei grafiken in einem fenster)
p1 <- histogram<-ggplot(mlrdata, aes(studentized.residuals)) +
 theme(legend.position = "none") +
 geom_histogram(aes(y=..density..),
 binwidth = 1,
 colour="black",
 fill="white") +
 labs(x = "Studentized Residual", y = "Density")
p2 <- histogram + stat_function(fun = dnorm, args = list(mean = mean(mlrdata$studentized.residuals, na.rm = TRUE), sd = sd(mlrdata$studentized.residuals, na.rm = TRUE)), colour = "red", size = 1)
p3 <- scatter <- ggplot(mlrdata, aes(fitted, studentized.residuals))
p4 <- scatter + geom_point() + geom_smooth(method = "lm", colour = "Red")+ labs(x = "Fitted Values", y = "Studentized Residual")
p5 <- qqplot.resid <- qplot(sample = mlrdata$studentized.residuals, stat="qq") + labs(x = "Theoretical Values", y = "Observed Values")
p6 <- qqplot.resid
multiplot(p2, p4, p6, cols=3)

# 1: optimal = 0
# (aufgelistete datenpunkte sollten entfernt werden)
which(mlrdata$standardized.residuals > 3.29)

# 2: optimal = 1
# (aufgelistete datenpunkte sollten entfernt werden)
stdres_258 <- as.vector(sapply(mlrdata$standardized.residuals, function(x) {
 ifelse(sqrt((x^2)) > 2.58, 1, 0) } ))
(sum(stdres_258) / length(stdres_258)) * 100

# 3: optimal = 5
# (aufgelistete datenpunkte sollten entfernt werden)
stdres_196 <- as.vector(sapply(mlrdata$standardized.residuals, function(x) {
 ifelse(sqrt((x^2)) > 1.96, 1, 0) } ))
(sum(stdres_196) / length(stdres_196)) * 100

# 4: optimal = 0
# (aufgelistete datenpunkte sollten entfernt werden)
which(mlrdata$cooks.distance > 1)

# 5: optimal = 0
# (datenpunkte sollten entfernt werden, wenn cooks distanz nahe 1 ist)
which(mlrdata$leverage >= (3*mean(mlrdata$leverage)))

# 6: checking autocorrelation:
# Durbin-Watson test (optimal: grosser p-wert)
dwt(m2.mlr)

# 7: multicolliniaritaet testen 1 : optimal 2.5, ok bis 4, ab 10 geht gar nicht
vif(m2.mlr)

# 8: multicolliniaritaet testen 2 . wert sollte unter 1 liegen
1/vif(m2.mlr)

# 9: mittlerer vif wert sollte nicht groesser als 1 sein
mean(vif(m2.mlr))

# ist die stichprobe ausreichend gross
smplesz(m2.mlr)

# gefahr von beta-fehlern
expR(m2.mlr)

# ergebnisse der mlr betrachten
mlr.summary(m2.mlr, m2.glm, ia = T)

##########################################################################
### Linear Mixed-Effects Model
##########################################################################
# Initiate the packages
library(RLRsim)
library(car)
library(QuantPsyc)
library(boot)
library(ggplot2)
library(nlme)
library(lme4)
library(ez)
library(ggplot2)
source("RSkripte/multiplot_ggplot2.R")
# set options
options("scipen" = 100, "digits" = 4)
options(stringsAsFactors = F)
# Read in data
mydata <- read.delim("lmemdata.txt", header = TRUE)
# convert date into a numeric variable
mydata$date <- as.numeric(mydata$date)
# inspect updated data set
head(mydata); str(mydata); summary(mydata)

################################################################################
# visualize variables (2 plots per row)
# 3 plots in 1 window
def.par <- par(no.readonly = TRUE)
nf <- layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = T))
plot(mydata$pptw ~ mydata$date, ylab = "Frequency", xlab = "year of publication")
abline(lm(mydata$pptw ~ mydata$date), lty = 3, lwd = 2, col = "red")
lines(lowess(mydata$date, mydata$pptw), lty = 1, lwd = 2, col = "blue")
# re-set margins to fit the labels
par(mar = c(11, 4, 1, 2) + 0.1)
# reorder genre by median
genrebymedian <- with(mydata, reorder(genre, -pptw, median))
#	generate plots
plot(mydata$pptw ~ genrebymedian,
  col = "lightgrey",
  ylab = "Frequency",
  xlab = "",
  las = 2,
  cex = .5)
# re-set margins
par(mar = c(5, 4, 1, 2) + 0.1)
x = mydata$pptw
h = hist(mydata$pptw,
	ylim =c(0, 150),
	xlim = c(50, 200),
	xlab = "prepositions per text",
	col = "lightgrey",
	main = "")
xfit <- seq(min(mydata$pptw), max(mydata$pptw), length = 40)
yfit <- dnorm(xfit, mean = mean(mydata$pptw),sd = sd(mydata$pptw))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, lty = 2, lwd=2)
# restore original graphic's parameters
par(def.par)

##########################################################################
# set up this plot from scratch
p1 <- ggplot(mydata, aes(date, pptw)) +
  geom_point() +
  labs(x = "Year") +
  labs(y = "Prepositions per 1,000 words") +
  geom_smooth()
# set up this plot from scratch
p2 <- ggplot(mydata, aes(date, pptw)) +
  geom_point() +
  labs(x = "Year") +
  labs(y = "Prepositions per 1,000 words") +
  geom_smooth(method = "lm") # with linear model smoothing!
multiplot(p1, p2, cols = 2)

p3 <- ggplot(mydata, aes(region, pptw)) +
  geom_boxplot() +
  labs(x = "Region") +
  labs(y = "Prepositions per 1,000 words") +
  geom_smooth(method = "lm") # with linear model smoothing!
# include genre (lowess)
p4 <- ggplot(mydata, aes(date, pptw)) +
  geom_point() +
  facet_wrap(~ genre, nrow = 4) +
  geom_smooth() +
  theme_bw() +
  labs(x = "Year") +
  labs(y = "Prepositions per 1,000 words") +
  coord_cartesian(ylim = c(0, 220))
# display graphs
multiplot(p3, p4, cols = 2)

##########################################################################
# include genre (lm)
p5 <- ggplot(mydata, aes(date, pptw)) +
  geom_point() +
  facet_wrap(~ genre, nrow = 4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Year") +
  labs(y = "Prepositions per 1,000 words") +
  coord_cartesian(ylim = c(0, 220))
p5

################################################################################
# centering numeric variables is useful for later interpretation of regression models:
# if our numeric date variable was not centered, the regression would show the
# effects of variables at year 0(!). if a variable is scaled, other variables are
# variables are considered relative not to 0 but to the mean of year in our data.
# centering simply means that the mean of the numeric vairbale is subtracted from
# each value.
mydata$date <- scale(mydata$date, scale = F)
head(mydata)

str(mydata)

################################################################################
# generate a glm baseline model
m0.glm <- glm(pptw ~ 1, family = gaussian, data = mydata)
# generate a lm base-line model
m0.lm <- lm(pptw ~ 1, data = mydata)
# set up first lme model including only the random effect specifying the random intercepts
m0.lme = lme(pptw ~ 1, random = ~1|genre, data = mydata, method = "ML")
# set up first lmer model including only the random effect specifying the random intercepts
m0.lmer = lmer(pptw ~ 1 + (1|genre), data = mydata, REML = F)
#  compare the base-line mdoel without intercept to the model with intercept
# WARNING: set REML = T because REML provides better estimates for the random
# effects part of the model (cf. Field, Miles & Field 2012:879)
x2 = -2*logLik(m0.lm, REML = T)+2*logLik(m0.lmer, REML = T)
x2 <- x2 <- x2[[1]]
test.ran.eff <- list(x2, pchisq(x2, df=2, lower.tail=F))
test.ran.eff

# set up m0 model but using the lmer function from the lme4 package
# WARNING: REML must be FALSE OR method = "ML" (depending on the function)
# when using anova to compare models!!! (cf. Field, Miles & Field 2012:)
# "ML produces more accurate estimates of fixed regression parameters, whereas
# REML produces more accurate estimates of random variances (Twisk 2006). [...]
# Also, if you want to compare models you must use ML." (Field, Miles & Field 2012:879).
m0.lmer1 <- lmer(pptw ~ (1|genre) + 1, data = mydata, REML = T)
m0.lmer2 <- lmer(pptw ~ (1|region) + 1, data = mydata, REML = T)
m0.lmer3 <- lmer(pptw ~ (1|genre/region) + 1, data = mydata, REML = T)
anova(m0.lmer1, m0.lmer2, m0.lmer3)

# the model with the random effect structure (1|genre/region) performs
# significantly better (also it has a much lower AIC and deviance)
# therefore, m0.lmer3 is our new m0 model
m0.lmer <- m0.lmer3
# test if including the random effect is permitted by applying a restricted likelihood ratio test
# WARNING: this test can only take simple random effect (1|genre) but not
# (1|genre/date)
exactRLRT(m0.lmer1)

# there is another way to comper model with and without random effects: see below!

# create a second model with date as a fixed effect
# m1.lme <- lme(m0.lme, .~. + date) # alternative way to update the model
m1.lme = lme(pptw ~ date, random = ~1|genre/region, data = mydata, method = "ML")
# set up m1 model but using the lmer function from the lme4 package
m1.lmer = lmer(pptw ~ (1|genre/region) + date, data = mydata, REML = F)

# compare the models  to see if including date has improved the model
# the difference between the modesl is the effect (size) of date!
anova(m0.lme, m1.lme)

# m1.lme is the better model (sig. p-value & lower AIC)
# date correlates significantly with pptw (X2(1) = 8.81, p = .003);
# X2 = L.Ratio;
# df = subtract df smaller from df larger model
# inspect results
summary(m1.lme)

# alternative display of the results
anova(m1.lme)

# test if date is significant
anova(m0.lmer, m1.lmer)

# extract estimates and sd for fixed and random effects
intervals(m1.lme)

# diagnostic plot: examining residuals (Pinheiro & Bates 2000:175)
plot(m1.lme, genre ~ resid(.), abline = 0 )

# the plot shows that there are some outliers (points outside the boxes) and
# that the variability within letters is greater than in other genres
# we therefore examine the genres in isolation
# standardized residuals versus fitted values
plot(m1.lme, resid(., type = "p") ~ fitted(.) | genre, id = 0.05, adj = -0.3)

# the plot showing the standardized residuals versus fitted values
# confirms that there are outliers in the letters
# because there are obviously differences in the variance, we create a new model
# which uses weights to compensate variance heterogeneiety of variance
# (cf. Pinheiro & Bates 2000:177)
m2.lme <- update(m1.lme, weights = varIdent(form = ~ 1 | genre))
# test if m2.lme is more appropriate for the data than m1.lme
anova(m1.lme, m2.lme)

# the heteroscedastitic model (i.e. m2.lme which uses weights to account for
# unequal variance is performing significantly better than the homoscedastistic
# model m1.lme

# inspect results
summary(m2.lme)

# alternative display of the results
anova(m2.lme)

# test if date is significant
anova(m0.lme, m2.lme)

# extract estimates and sd for fixed and random effects
intervals(m2.lme)

# extract effect sizes (in the example: the effect size of date)
# calculate effect size (this effect size measure works for all fixed effects)
# to calculate the effect size, take the square root of the t-value squared divided
# by the t-value squared plus the degrees of freedom: r = sqrt(t^2/(t^2+df))
# WARNING: only apply this function to main effecst not involved in interactions
# or higher level interactions but not to the main effects involved in
# interactions as they are meaningless (cf. Field, Miles & Field 2012:641)
ef.lme <- function(x) {
  df <- summary(x)[[20]][6]
  t <-  summary(x)[[20]][8]
  #df <- summary(x)$tTable[, 3]
  #t <- summary(x)$tTable[, 4]
  r <- sqrt((t^2)/((t^2)+df))
  return(paste("Pearson's r = ", round(r, 3)))
  }
ef.lme(m2.lme)

# set up m1 model but using the lmer function from the lme4 package
m2.lmer = lmer(pptw ~ (1|genre/region) + date, data = mydata, REML = F)

# How to calculate the variance explained when only a simple random effect is involved:
m2.lmer = lmer(pptw ~ (1|genre/region) + date, data = mydata, REML = F)
summary(m2.lmer)
# variance of random effect (145.2) devided by variance of random effect plus
# residual variance (145.2+224.1) times 100 gives the variance explained by the random effect:
#(145.2/(145.2+2284.1))*100 # percentage of variance explained by random effect

# craete lmer with complex random effect structure
m2.lmer = lmer(pptw ~ (1|genre/region) + date, data = mydata, REML = F)
# an alternative for testing if including the random intercepts is permitted
# WARNING: this method is not as good as applying a restricted likelihood
# ratio test(!) because the p-value is only an approximation
# IMPORTANT: the second model is a glm object
2*pchisq(2*as.numeric(logLik(m2.lmer)-logLik(m0.glm)), 2, lower.tail = FALSE)

####################################################################
### --- model diagnostics
####################################################################
# diagnostic plot (Pinheiro & Bates 2000:11, 182)
# what we wish to see: a cloud of dots in the middle of the window without structure
# what we do not want to see: a funnel-shaped cloud because this indicates an increase
# of the errors/residuals with an increase of the predictor(s) (because this would indicate
# heteroscedasticity)
# in short: observed valuesagainst fitted values (cf. Pinheiro & Bates 2000:182)
plot(m2.lme)

# diagnostic plot (Pinheiro & Bates 2000:21)
plot(m2.lme, form = resid(., type = "p") ~ fitted(.) | genre, abline = 0)

# diagnostic plot: residuals of fitted values against observed values (cf. Pinheiro & Bates 2000:182)
qqnorm(m2.lme)

# normal plot of the estimated date %in% genre random effects
qqnorm(m2.lme, ~ranef(., level = 2), id = 0.05, cex = 0.7, xlim = c(-40, 40))

# diagnostic plot: normal plots of the residuals by genre (cf. Pinheiro & Bates 2000:22, 179)
qqnorm(m2.lme, ~resid(.) | genre )

# inspect the observed responses versus the within-group fitted values
# (cf. Pinheiro & Bates 2000:178)
plot(m2.lme, pptw ~ fitted(.), id = 0.05, adj = -0.3, xlim = c(80, 220), cex = .8)

# TO DO
# summary function for lme!

summary(m2.lmer)

###########################################################################
### Multiple Binomial Logistic Regression
###########################################################################
# Initiate the packages
library(sjPlot)
library(visreg)
library(mlogit)
library(plyr)
library(rms)
library(ggplot2)
library(effects)
source("RSkripte/multiplot_ggplot2.R")
source("RSkripte/blr.summary.R")
###########################################################################
### Load and manipulate data
###########################################################################
# set options
options("scipen" = 100, "digits" = 4)
# read in existing s´data set mblrdata.txt
mydata <- read.table("blrdata.txt", comment.char = "", quote = "", sep = "\t", header = T)
# convert age, sex, and ethnicity into factors
mydata$age <- as.factor(mydata$age)
mydata$sex <- as.factor(mydata$sex)
mydata$ethnicity <- as.factor(mydata$ethnicity)
mydata$suf.eh <- as.numeric(mydata$suf.eh)
# relevel factors age & ethnicity
mydata$age <- relevel(mydata$age, "young")
mydata$ethnicity <- relevel(mydata$ethnicity, "Pakeha")
# provide an overview of the data
head(mydata); str(mydata); summary(mydata)

###########################################################################
p1 <- ggplot(mydata, aes(sex, suf.eh, color = sex)) +
  scale_fill_brewer() +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_set(theme_bw(base_size = 10)) +
  coord_cartesian(ylim = c(0, 0.5)) +
  labs(x = "Sex", y = "Mean frequency of EH")
p2 <- ggplot(mydata, aes(age, suf.eh, color = age)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Age", y = "Mean frequency of EH") +
  scale_color_manual(values = c("darkblue", "lightblue"))
p3 <- ggplot(mydata, aes(ethnicity, suf.eh, colour = ethnicity)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Ethnicity", y = "Mean frequency of EH", colour = "ethnicity") +
  scale_color_manual(values = c("darkgreen", "lightgreen"))
p4 <- ggplot(mydata, aes(ethnicity, suf.eh, colour = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Ethnicity", y = "Mean frequency of EH", colour = "sex")
p5 <- ggplot(mydata, aes(sex, suf.eh, colour = age)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= age)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Sex", y = "Mean frequency of EH", colour = "age") +
  scale_color_manual(values = c("darkblue", "lightblue"))
p6 <- ggplot(mydata, aes(age, suf.eh, colour = ethnicity)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= ethnicity)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Age", y = "Mean frequency of EH", colour = "ethnicity") +
  scale_color_manual(values = c("darkgreen", "lightgreen"))
# display the plots
multiplot(p1, p4, p2, p5, p3, p6, cols = 3)

###########################################################################
p7 <- ggplot(mydata, aes(age, suf.eh, colour = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_wrap(~ ethnicity, nrow = 1) +
  coord_cartesian(ylim = c(0, 0.75)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "", y = "Mean frequency of EH", colour = "sex")
p8 <- ggplot(mydata, aes(age, suf.eh, colour = ethnicity)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group = ethnicity)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_wrap(~ sex, nrow = 1) +
  coord_cartesian(ylim = c(0, 0.75)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "", y = "Mean frequency of EH", colour = "ethnicity") +
  scale_color_manual(values = c("darkgreen", "lightgreen"))
p9 <- ggplot(mydata, aes(ethnicity, suf.eh, colour = age)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group = age)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_wrap(~ sex, nrow = 1) +
  coord_cartesian(ylim = c(0, 0.75)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "", y = "Mean frequency of EH", colour = "age") +
  scale_color_manual(values = c("darkblue", "lightblue"))
# display the plots
multiplot(p7, p8, p9, cols = 1)

###########################################################################
### --- Model Building
###########################################################################
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
mydata.dist <- datadist(mydata)
options(datadist = "mydata.dist")
# a few words on glm vs lrm: Baayen (2008:196-197) states that lrm should be
# the function of choice in cases where each row contains
# exactly 1 success OR failure (1 or 0) while glm is preferrable if there are two
# columns holding the number of successes and the number of failures
# respectively. i have tried it both ways and both functions work fine if
# each row contains exactly 1 success OR failure but only glm can handle the
# latter case.
# generate initial saturated regression model including
# all variables and their interactions
m0.glm = glm(suf.eh ~ 1, family = binomial, data = mydata) # baseline model glm
m0.lrm = lrm(suf.eh ~ 1, data = mydata, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

###########################################################################
# create saturated model
m1.glm = glm(suf.eh ~ age*sex*ethnicity, family = binomial, data = mydata)
m1.lrm = lrm(suf.eh ~ age*sex*ethnicity, data = mydata, x = T, y = T)
# inspect results
summary(m1.glm)

###########################################################################
# model fitting
# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use a step-wise step down procedure
#	manual modelfitting
m2.glm <- update(m1.glm, . ~ . -age:sex:ethnicity)
anova(m1.glm, m2.glm, test = "Chi")

summary(m2.glm)

m3.glm <- update(m2.glm, . ~ . -sex:ethnicity)
anova(m2.glm, m3.glm, test = "Chi")

summary(m3.glm)

m4.glm <- update(m3.glm, . ~ . -age:sex)
anova(m3.glm, m4.glm, test = "Chi")

summary(m4.glm)

m5.glm <- update(m4.glm, . ~ . -age:ethnicity)
anova(m4.glm, m5.glm, test = "Chi")

summary(m5.glm)

m6.glm <- update(m5.glm, . ~ . -ethnicity)
anova(m5.glm, m6.glm, test = "Chi")

summary(m6.glm)

m6.lrm <- lrm(suf.eh ~ age+sex, data = mydata, x = T, y = T, linear.predictors = T)
m6.lrm

anova(m6.lrm)

# validate model (this shows how many predictors are retained if the sample is
# re-selected with the same size but with placing back the drwan data point
validate(m1.lrm, bw = T, B = 200)

# the validate function shows that retaining 4 predictors is the best option

# determine penalty
pentrace(m6.lrm, seq(0, 0.8, by = 0.05))
# the pentrace function proposes a penaty of .8 but the values are so similar
# that a penalty is unneccessary

# rename final minimal adeqaute model
lr.glm <- m6.glm
lr.lrm <- m6.lrm
###########################################################################
# calculate model x^2 manually (reported in lr.lrm as Model Likelihood Ratio Test
modelChi <- lr.glm$null.deviance - lr.glm$deviance
chidf <- lr.glm$df.null - lr.glm$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

# calculate pseudo R^2
# number of cases
ncases <- length(fitted(lr.glm))
R2.hl <- modelChi/lr.glm$null.deviance
R.cs <- 1 - exp ((lr.glm$deviance - lr.glm$null.deviance)/ncases)
R.n <- R.cs /( 1- ( exp (-(lr.glm$null.deviance/ ncases))))
# function for extracting pseudo-R^2
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
	nullDev <- LogModel$null.deviance
	modelN <-  length(LogModel$fitted.values)
	R.l <-  1 -  dev / nullDev
	R.cs <- 1- exp ( -(nullDev - dev) / modelN)
	R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
	cat("Pseudo R^2 for logistic regression\n")
	cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
	cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
	cat("Nagelkerke R^2           ", round(R.n, 3),    "\n") }
logisticPseudoR2s(lr.glm)

# extract the confidence intervalls for the coefficients
confint(lr.glm)

#Compute odds ratio
exp(lr.glm$coefficients)

exp(confint(lr.glm))

# compare the initial saturated model to the minimal adeqaute model
#anova(m0.glm, m6.glm)
anova(m0.glm, lr.glm, test = "Chi")

##########################################################################
# convert data into a by.speaker format
mydata.by.spk <- table(mydata$file.speaker.id, mydata$suf.eh)
mydata.by.spk <- data.frame(rownames(mydata.by.spk), mydata.by.spk[, 1], mydata.by.spk[, 2])
names(mydata.by.spk) <- c("file.speaker.id", "no.eh", "eh")
rownames(mydata.by.spk) <- 1:length(mydata.by.spk[,1])
# add biodata
mydata.by.spk <- join(mydata.by.spk, mydata, by = "file.speaker.id", type = "left", match = "first")
# remove suf.eh
mydata.by.spk$suf.eh <- NULL
head(mydata.by.spk)

# check accuracy of the model
# use by.spk data to fit another model which we will use to test the accuracy of the model
lr.glm.spk <- glm(cbind(eh, no.eh) ~ age*sex + ethnicity + age: ethnicity, data = mydata.by.spk, family = binomial)
correct <- sum(mydata.by.spk$eh * (predict(lr.glm.spk, type = "response") >= 0.5)) + sum(mydata.by.spk$no.eh * (predict(lr.glm.spk, type="response") < 0.5))
tot <- sum(mydata.by.spk$eh) + sum(mydata.by.spk$no.eh)
predict.acc <- (correct/tot)*100
predict.acc

# 66.28% accuracy! WOW -but wait a second... ;)

# check accuracy of base line model
lr.glm.spk.base <- glm(cbind(eh, no.eh) ~ 1, data = mydata.by.spk, family = binomial)
correct.b <- sum(mydata.by.spk$eh * (predict(lr.glm.spk.base, type = "response") >= 0.5)) + sum(mydata.by.spk$no.eh * (predict(lr.glm.spk.base, type="response") < 0.5))
tot.b <- sum(mydata.by.spk$eh) + sum(mydata.by.spk$no.eh)
predict.acc.base <- (correct.b/tot.b)*100
predict.acc.base

# 66.28% accuracy - hm... why that? because both models always preditc no.eh(!)
# because the probability of eh occurring is so low!
# test this with:
#which(lr.glm.spk$fitted > .5)
#which(lr.glm.spk.base$fitted > .5)
##########################################################################
# plot effects using the visreg package (cf. Breheny & Burchett 2013)
par(mfrow = c(2, 2))
visreg(lr.glm, "age", xlab = "Age", ylab = "Log odds (eh)", ylim = c(-7, 0))
visreg(lr.glm, "sex", xlab = "Sex", ylab = "Log odds (eh)", ylim = c(-7, 0))

# visualize effects using sjPlot
# (cf. http://strengejacke.wordpress.com/2013/03/22/plotting-lm-and-glm-models-with-ggplot-rstats/)
sjp.glm(lr.glm, axisLabels.y = c("Sex:Male", "Age:Old"), gridBreaksAt = 0.5)
par(mfrow = c(1, 1))
##########################################################################
### --- model diagnostics
##########################################################################
# checking for multicollinearity
# checking multicolliniarity:
# extract variance inflation factors (VIF) (values greater 10 should/must be
# excluded; cf Myers 1990)
#
# "generally, VIF > 10 ! absence of absolute collinearity in the model cannot
# be claimed.
#
# VIF > 4 are usually already problematic but,
# for large data sets, even VIFs > 2 can lead inflated standard errors"
# (Jaeger 2013:http://wiki.bcs.rochester.edu/HlpLab/LSA2013Regression?action=AttachFile&do=view&target=LSA13-Lecture6-CommonIssuesAndSolutions.pdf).
vif(lr.glm)

# checking multicolliniarity: tolerance is 1/VIF
# values smaller than .01 should/must be excluded
# Menard 1995 states that values smaller than .2 are problematic
1/vif(lr.glm)

# mean VIF: should not be greater than 1 (Bowerman & O'Connell 1990)
mean(vif(lr.glm))

##########################################################################
# add diagnostic parameters to mydata
# we add some informational columns to our data set
# extract case wise diagnostics and add them to the data set
infl <- influence.measures(lr.glm)

# add influence statistics to data
mydata <- data.frame(mydata, infl[[1]], infl[[2]])
head(mydata)

##########################################################################
# continue diagnostics
# checking sample size (Green 1991)
# if you are interested in the overall model: 50 + 8k (k = number of predictors)
# if you are interested in individual predictors: 104 + k
# if you are interesetd in both: take the higher value!
smplesz <- function(x) {
  ifelse((length(x$fitted) < (104 + ncol(summary(x)$coefficients)-1)) == TRUE,
    return(
      paste("Sample too small: please increase your sample by ",
      104 + ncol(summary(x)$coefficients)-1 - length(x$fitted),
      " data points", collapse = "")),
    return("Sample size sufficient")) }
smplesz(lr.glm)

###########################################################################
# summarize regression analysis
blrm.summary(lr.glm, lr.lrm, predict.acc)

# EXPLANATION of MODEL FIT PARAMETERS

# R2 (Hosmer & Lemeshow)
# "Rt is the proportional reduction in the absolute value of the log-likelihood
# measure and as such it is a measure of how much the badness of fit improves
# as a result of the inclusionof the predictor variables. It can vary between 0
#(indicating that the predictors are useless at predicting the outcome variable)
# and 1 (indicating that the model predicts the outcome variable perfectly)"
# (Field, Miles & Field 2012:317).

# R2 (Cox & Snell)
# "Cox and Snell's R~s (1989) is based on the deviance of the model (-2LL(new»)
# and the deviance of the baseline model (-2LL(baseline), and the sample size,
# n [...]. However, this statistic never reaches its theoretical maximum of 1.

# R2 (Nagelkerke)
# Since R2 (Cox & Snell) never reaches its theoretical maximum of 1,
# Nagelkerke (1991) suggested Nagelkerke's R^2. (Field, Miles & Field 2012:317-318).

# Somers’ Dxy
# Somers’ Dxy is a rank correlation between predicted probabilities and observed
# responses ranges between 0 (randomness) and 1 (perfect prediction). (Baayen 2008:204)

# C
# C is an index of concordance between the predicted probability and the
# observed response. When C takes the value 0.5, the predictions are random,
# when it is 1, prediction is perfect. A value above 0.8 indicates that the
# model may have some real predictive capacity. (Baayen 2008:204)

# AIC
# Akaike information criteria (AlC = -2LL + 2k): "contains more predictor variables.
# You can think of this as the price you pay for something: you get a better
# value of R2, but you pay a higher price, and was that higher price worth it?
# These information criteria help you to decide.model. The BIC is the same as
# the AIC but adjusts the penalty included in the AlC (i.e., 2k) by the number
# of cases: BlC = -2LL + 2k x log(n) in which n is the number of cases in the
# model." (Field, Miles & Field 2012:318).

###########################################################################
### --- Mixed Effects Binomial Logistic Regression
###########################################################################
# Initiate the packages
library(Hmisc)
library(RLRsim)
library(sjPlot)
library(visreg)
library(mlogit)
library(plyr)
library(rms)
library(ggplot2)
library(effects)
library(lme4)
library(languageR)
#library(nlme)
source("RSkripte/multiplot_ggplot2.R")
source("RSkripte/PseudoR2lmerBinomial.R")
source("RSkripte/meblr.summary.R")
###########################################################################
### Load and manipulate data
###########################################################################
# set options
options("scipen" = 100, "digits" = 4)
# read in existing s´data set mblrdata.txt
mydata <- read.table("blrdata.txt", comment.char = "", quote = "", sep = "\t", header = T)
# convert age, sex, and ethnicity into factors
mydata$age <- as.factor(mydata$age)
mydata$sex <- as.factor(mydata$sex)
mydata$ethnicity <- as.factor(mydata$ethnicity)
mydata$suf.eh <- as.numeric(mydata$suf.eh)
# relevel factors age & ethnicity
mydata$age <- relevel(mydata$age, "young")
mydata$ethnicity <- relevel(mydata$ethnicity, "Pakeha")
# provide an overview of the data
head(mydata); str(mydata); summary(mydata)

###########################################################################
p1 <- ggplot(mydata, aes(sex, suf.eh, color = sex)) +
  scale_fill_brewer() +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_set(theme_bw(base_size = 10)) +
  coord_cartesian(ylim = c(0, 0.5)) +
  labs(x = "Sex", y = "Mean frequency of EH")
p2 <- ggplot(mydata, aes(age, suf.eh, color = age)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Age", y = "Mean frequency of EH") +
  scale_color_manual(values = c("darkblue", "lightblue"))
p3 <- ggplot(mydata, aes(ethnicity, suf.eh, colour = ethnicity)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Ethnicity", y = "Mean frequency of EH", colour = "ethnicity") +
  scale_color_manual(values = c("darkgreen", "lightgreen"))
p4 <- ggplot(mydata, aes(ethnicity, suf.eh, colour = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Ethnicity", y = "Mean frequency of EH", colour = "sex")
p5 <- ggplot(mydata, aes(sex, suf.eh, colour = age)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= age)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Sex", y = "Mean frequency of EH", colour = "age") +
  scale_color_manual(values = c("darkblue", "lightblue"))
p6 <- ggplot(mydata, aes(age, suf.eh, colour = ethnicity)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= ethnicity)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Age", y = "Mean frequency of EH", colour = "ethnicity") +
  scale_color_manual(values = c("darkgreen", "lightgreen"))
# display the plots
multiplot(p1, p4, p2, p5, p3, p6, cols = 3)

###########################################################################
p7 <- ggplot(mydata, aes(age, suf.eh, colour = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_wrap(~ ethnicity, nrow = 1) +
  coord_cartesian(ylim = c(0, 0.75)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "", y = "Mean frequency of EH", colour = "sex")
p8 <- ggplot(mydata, aes(age, suf.eh, colour = ethnicity)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group = ethnicity)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_wrap(~ sex, nrow = 1) +
  coord_cartesian(ylim = c(0, 0.75)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "", y = "Mean frequency of EH", colour = "ethnicity") +
  scale_color_manual(values = c("darkgreen", "lightgreen"))
p9 <- ggplot(mydata, aes(ethnicity, suf.eh, colour = age)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group = age)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_wrap(~ sex, nrow = 1) +
  coord_cartesian(ylim = c(0, 0.75)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "", y = "Mean frequency of EH", colour = "age") +
  scale_color_manual(values = c("darkblue", "lightblue"))
# display the plots
multiplot(p7, p8, p9, cols = 1)

###########################################################################
# test if all cells are filled: none of the cells can hold values of 0!
table(mydata$suf.eh, mydata$age, mydata$sex, mydata$ethnicity)

###########################################################################
### --- Model Building
###########################################################################
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
mydata.dist <- datadist(mydata)
options(datadist = "mydata.dist")
# a few words on glm vs lrm: Baayen (2008:196-197) states that lrm should be
# the function of choice in cases where each row contains
# exactly 1 success OR failure (1 or 0) while glm is preferrable if there are two
# columns holding the number of successes and the number of failures
# respectively. i have tried it both ways and both functions work fine if
# each row contains exactly 1 success OR failure but only glm can handle the
# latter case.
# generate initial saturated regression model including
# all variables and their interactions
m0.glm = glm(suf.eh ~ 1, family = binomial, data = mydata) # baseline model glm
m0.lrm = lrm(suf.eh ~ 1, data = mydata, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

m0.lrm

###########################################################################
# create model with a random intercept for file.speaker.id
#m1.lmer <- lmer(suf.eh ~ (1|file.speaker.id), data = mydata, family = binomial)
# Baayen (2008:278-284) uses the call above but the this call is now longer
# up-to-date because the "family" parameter is deprecated
# we switch to glmer (suggested by R) instead but we will also
# create a lmer object of the final minimal adequate model as some functions
# will not (yet) work on glmer
m0.glmer = glmer(suf.eh ~ (1|file.speaker.id), data = mydata, family = binomial)

# results of the lmer object
print(m0.lmer, corr = F)

# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# the aic of the glmer object is smaller which shows that including the random
# intercepts is justified

# inspect results
summary(m0.glm)

summary(m0.glmer)

###########################################################################
# model fitting
# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use a step-wise step down procedure
# step-wise step up, i.e. forward, would be much easier but the problem with stepwise step up
# is that the likelyhood of type II errors is much higher than with step-wise
# step down, i.e. backward elimination(cf. Field, Miles & Field 2012:265)
# we need to add "control = glmerControl(optimizer = "bobyqa")" because otherwise R fails to converge
#	manual modelfitting
m0.glmer <- glmer(suf.eh ~ 1+ (1|file.speaker.id), family = binomial, data = mydata, control=glmerControl(optimizer="bobyqa"))
m1.glmer <- glmer(suf.eh ~ age+ (1|file.speaker.id), family = binomial, data = mydata, control=glmerControl(optimizer="bobyqa"))
m2.glmer <- glmer(suf.eh ~ age+sex+ (1|file.speaker.id), family = binomial, data = mydata, control=glmerControl(optimizer="bobyqa"))
m3.glmer <- glmer(suf.eh ~ age+sex+ethnicity+ (1|file.speaker.id), family = binomial, data = mydata, control = glmerControl(optimizer="bobyqa"))
m4.glmer <- glmer(suf.eh ~ age+sex+ethnicity+age:sex+ (1|file.speaker.id), family = binomial, data = mydata, control = glmerControl(optimizer = "bobyqa"))
m5.glmer <- glmer(suf.eh ~ age+sex+ethnicity+age:sex+age:ethnicity+ (1|file.speaker.id), family = binomial, data = mydata, control = glmerControl(optimizer = "bobyqa"))
m6.glmer <- glmer(suf.eh ~ age+sex+ethnicity+age:sex+age:ethnicity+sex:ethnicity+ (1|file.speaker.id), family = binomial, data = mydata, control = glmerControl(optimizer = "bobyqa"))
m7.glmer <- glmer(suf.eh ~ age+sex+ethnicity+age:sex+age:ethnicity+sex:ethnicity+age:sex:ethnicity+ (1|file.speaker.id), family = binomial, data = mydata, control = glmerControl(optimizer = "bobyqa"))

# test which models are the most adequate
# we compare all models because this way, we get an overview of model paramerets
# and can check which model has the lowerst AIC, BIC, and the highest X^2 value
anova(m0.glmer, m1.glmer, m2.glmer, m3.glmer, m4.glmer, m5.glmer, m6.glmer, m7.glmer, test = "Chi")

# based on the overview, we expext m5.glmer to be the best model.
# we confirm this hypothesis by testing if deleting a variable significantly
# decreases model fit (if the models differ significantly then the deletion is
# not permitted and the effect has to remain in the model
# we begin by comparing the model with all main effects and all interactions to
# a model without the three-way-interaction and continue to delete insig. interactions and finally main effects
anova(m0.glmer, m1.glmer, test = "Chi") # m1 is better than m0 and they do not differ significantly thus we proceed with m2

anova(m1.glmer, m2.glmer, test = "Chi") # m2 is better than m1 and they do not differ significantly thus we proceed with m2

anova(m2.glmer, m3.glmer, test = "Chi") # m3 is NOT better than m2 and they do not differ significantly thus we proceed with m2

anova(m2.glmer, m4.glmer, test = "Chi") # m4 is NOT better than m2 and they do not differ significantly thus we proceed with m2

anova(m2.glmer, m5.glmer, test = "Chi") # m5 is NOT better than m2 and they do not differ significantly thus we proceed with m2

anova(m2.glmer, m6.glmer, test = "Chi") # m6 is NOT better than m2 and they do not differ significantly thus we proceed with m2

anova(m2.glmer, m7.glmer, test = "Chi") # m7 is NOT better than m2 and they do not differ significantly thus we proceed with m2

# m2.glmer is our final model!
# rename final minimal adeqaute model
mlr.glmer <- m2.glmer

# test if the final minimal adequate model performs better than the base-line model
anova(mlr.glmer, m0.glmer, test = "Chi")

# inspect results of the final minimal adequate model
print(mlr.glmer, corr = F)

# alternative result display (anova)
anova(mlr.glmer)

# extract the parameters of the fixed effects for the report
# to do that, we compare the model with only the random effect to a model with
# the random effect and the fixed effect for age
anova(m0.glmer, m1.glmer, test = "Chi") # effect of age

# we now test the effect of sex by adding sex as a fixed effect
anova(m1.glmer, m2.glmer, test = "Chi") # effect of sex

###########################################################################
### --- extracting and calculating model fit parameters
# we now create a lmr object equivalent to the final minimal adequate model
# but without the random effect
mlr.lrm <- lrm(suf.eh ~ age + sex, data = mydata, x = T, y = T)
m1.glm = glm(suf.eh ~ age + sex, family = binomial, data = mydata) # baseline model glm
# we now create a lmer object equivalent to the final minimal adequate model
mlr.lmer <- lmer(suf.eh ~ age + sex+ (1|file.speaker.id), data = mydata, family = binomial)

# now we check if the fixed effects of the lrm and the lmer model correlate (cf Baayen 2008:281)
cor.test(coef(mlr.lrm), fixef(mlr.lmer))

# the fixed effects do not correlate strongly - this is not good as it suggests that
# the coefficient estimates are not very stable

# we activate the package Hmisc (if not already active)
library(Hmisc)
# we now extract model fit parameters (cf Baayen 2008:281)
probs = 1/(1+exp(-fitted(mlr.lmer)))
probs = binomial()$linkinv(fitted(mlr.lmer))
somers2(probs, as.numeric(mydata$suf.eh))

# the model fit values indicate a very good fit:
# C
# "The measure named C is an index of concordance between the predicted
# probability and the observed response. [...] When C takes the value 0.5, the
# predictions are random, when it is 1, prediction is perfect. A value above
# 0.8 indicates that the model may have some real predictive capacity."
# (Baayen 2008:204)
# "Somers’ Dxy,
# a rank correlation between predicted probabilities and observed responses, [...]
# ranges between 0 (randomness) and 1 (perfect prediction)." (Baayen 2008:204)

###########################################################################
###########################################################################
###########################################################################
# model diagnostics: plot fitted against residuals
plot(mlr.glmer)

# plot residuals against fitted
plot(mlr.glmer, form = resid(., type = "response") ~ fitted(.) | file.speaker.id, abline = 0, cex = .5,id = 0.05, adj = -0.3)

# diagnostic plot: examining residuals (Pinheiro & Bates 2000:175)
plot(mlr.glmer, file.speaker.id ~ resid(.), abline = 0 , cex = .5)

# summarize final model
meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, dpvar=mydata$suf.eh)

###################################################
###              CLUSTER ANALYSIS
###################################################
# save plot
#png("D:\\Uni\\Lehre\\00-Workshops\\CorpusLinguisticsWorkshop/trees.png") # save plot
x <- 1:10
y <- 1:10
plot(x, y, type = "n", ylim = c(-.5,10), xlim = c(0,5), axes = F, xlab = "", ylab = "")
text("Trees", x = 2.25, y = 10, cex = 1.5)
text("Conifers", x = .5, y = 6.5, cex = 1.5)
text("Broad leaf", x = 2.25, y = 6.5, cex = 1.5)
text("Palms", x = 4, y = 6.5, cex = 1.5)
text("Pine tree", x = .25, y = 2.5, srt=90, cex = 1.5)
text("Fir tree", x = .75, y = 2.5, srt=90, cex = 1.5)
text("Oak tree", x = 2, y = 2.5, srt=90, cex = 1.5)
text("Beech tree", x = 2.5, y = 2.5, srt=90, cex = 1.5)
text("Phoenix palm tree", x = 3.75, y = 1.5, srt=90, cex = 1.5)
text("Nikau palm tree", x = 4.25, y = 1.5, srt=90, cex = 1.5)
#
lines(x = c(.5, 1.75), y = c(7, 9), lwd = 2)
lines(x = c(2.25, 2.25), y = c(7, 9), lwd = 2)
lines(x = c(4, 2.75), y = c(7, 9), lwd = 2)
#
lines(x = c(.5, .5), y = c(6, 4.5), lwd = 2)
lines(x = c(2.25, 2.25), y = c(6, 4.5), lwd = 2)
lines(x = c(4, 4), y = c(6, 4.5), lwd = 2)
#
lines(x = c(.25, .75), y = c(4.5, 4.5), lwd = 2)
lines(x = c(2, 2.5), y = c(4.5, 4.5), lwd = 2)
lines(x = c(3.75, 4.25), y = c(4.5, 4.5), lwd = 2)
#
lines(x = c(.25, .25), y = c(4.5, 4), lwd = 2)
lines(x = c(.75, .75), y = c(4.5, 4), lwd = 2)
lines(x = c(2, 2), y = c(4.5, 4), lwd = 2)
lines(x = c(2.5, 2.5), y = c(4.5, 4), lwd = 2)
lines(x = c(3.75, 3.75), y = c(4.5, 4), lwd = 2)
lines(x = c(4.25, 4.25), y = c(4.5, 4), lwd = 2)
# end plot
#dev.off()
###################################################
y <- c(1, 3.1, 1.2, 2.3, 3.4, 2.5, 1.6, 2.7, 3.8, 2.9)
x <- c(1:10)
#png("C:\\01-University\\03-Lehre\\StatisticsForLinguists/similarity.png") # save plot
plot(x, y, type = "l", ylim = c(0,11), lwd = 2, ylab = "Frequency", xlab = "Time")
lines(x = 1:10, y = c(5, 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9), col = "blue", lwd = 2)
lines(x = 1:10, y = c(8, 10.1, 8.2, 9.3, 10.4, 9.5, 8.6, 9.7, 10.8, 9.9), col = "red", lwd = 2)
#dev.off()
# similarity
students <- matrix(c(2,  3,  2, 1,  3,  2, 1,  2,  1, 2,  4,  4, 3,  4,  3),
  nrow = 5, byrow = T)
students <- as.data.frame(students)
rownames(students) <- c("StudentA", "StudentB", "StudentC", "StudentD", "StudentE")
students

diststudents <- dist(students, method = "manhattan")
diststudents

###################################################
###          CLUSTER ANALYSIS (numeric data)
###################################################
# load libraries
library("cluster")
library("factoextra")
library("seriation")
library("NbClust")
library("pvclust")
clusterstudents <- hclust(diststudents, method="ward.D")    # create cluster object (ward.D linkage)
# plot result as dendrogram
#png("D:\\Uni\\Lehre\\00-Workshops\\CorpusLinguisticsWorkshop/cluststudents.png") # save plot
plot(clusterstudents, hang = 0)
#dev.off()
#
students2 <- matrix(c(1.5, 3, 2, 1,  2,  1, 2,  4,  4, 3,  4,  3),
  nrow = 4, byrow = T)
students2 <- as.data.frame(students2)
rownames(students2) <- c("Cluster1", "StudentC", "StudentD", "StudentE")
diststudents2 <- dist(students2, method = "manhattan")
diststudents2

#
students3 <- matrix(c(1.5, 3, 2, 1,  2,  1, 2.5, 4, 3.5),
  nrow = 3, byrow = T)
students3 <- as.data.frame(students3)
rownames(students3) <- c("Cluster1", "StudentC", "Cluster2")
diststudents3 <- dist(students3, method = "manhattan")
# inspect distance object
diststudents3


######################################################
# eucleadian distance plot
# determine target file
#png("D:\\Uni\\Lehre\\00-Workshops\\CorpusLinguisticsWorkshop/Distances.png")
# save plot
par(mar=c(1,1,1,1))
x <- c(1,5)
y <- c(1,5)
plot(x, y, pch = 20, cex = 2, axes = F, las = 1, xlab = "", ylab = "", xlim = c(0,7), ylim = c(0,10))
text(0.5, .5, "vowel", cex = 2)
text(5, 5.5, "target vowel", cex = 2)
lines(x = c(1, 5), y = c(1, 5), type = "l", lty = 3, lwd = 2, col = "red")
lines(x = c(1, 5), y = c(1, 1), type = "l", lty = 2, lwd = 2, col = "blue")
lines(x = c(5, 5), y = c(1, 5), type = "l", lty = 4, lwd = 2, col = "green")
lines(x = c(.9, 5), y = c(.9, .9), type = "l", lty = 4, lwd = 2, col = "green")
legend("topleft", inset=.05, title="", bty = "n", lty = c(3, 2, 4), lwd = 2,
   c("euclidean distance", "maximum distance", "manhatten distance"), col=c("red", "blue", "green"), horiz=F, cex = 2)
par(mar=c(5.1,4.1,4.1,2.1))
# end writing to file
#dev.off()

###################################################
# load data
ire <- round(sqrt((rnorm(10, 9.5, .5))^2), 3)
sce <- round(sqrt((rnorm(10, 9.3, .4))^2), 3)
bre <- round(sqrt((rnorm(10, 6.4, .7))^2), 3)
aus <- round(sqrt((rnorm(10, 6.6, .5))^2), 3)
nze <- round(sqrt((rnorm(10, 6.5, .4))^2), 3)
ame <- round(sqrt((rnorm(10, 4.6, .8))^2), 3)
can <- round(sqrt((rnorm(10, 4.5, .7))^2), 3)
jam <- round(sqrt((rnorm(10, 1.4, .2))^2), 3)
phi <- round(sqrt((rnorm(10, 1.5, .4))^2), 3)
ind <- round(sqrt((rnorm(10, 1.3, .5))^2), 3)
clus <- data.frame(ire, nze, can, ame, phi, jam, bre, sce, aus, ind)
# add row names
rownames(clus) <- c("nae_neg", "like", "clefts", "tags", "youse", "soitwas", "dt", "nsr", "invartag", "wh_cleft")
# inspect data
clus

str(clus)

summary(clus)

# clean data
clust <- t(clus)# transpose data
clust <- na.omit(clust)                       # remove missing values
#clusts <- scale(clust)                        # standardize variables
clusts <- as.matrix(clust)
# assess if data is clusterable (does the data include nonrandom structures)
# and how many clusters are optimal
get_clust_tendency(clusts, n = 8, gradient = list(low = "steelblue",  high = "white"))
# the Hopkins statistic informs how similar the data is to a random distribution
# a value higher than 0.5 would indicate that the data is random (no inherent clusters)
# if the Hopkins statistic is close to 0, then the data is clusterable

# create distance matrix
clustd <- dist(clusts, method = "euclidean")   # create a distance object with eucledian (!) distance
# display distance matrix
round(clustd, 2)

# distplot: if the plot shows different regions (non random, non uniform grey
# areas) then clustering is permitable
dissplot(clustd)

# create distance matrix
clustd <- dist(clusts, method = "euclidean") # create distance matrix (eucledian method: not good when dealing with many dimensions)
#clustd <- dist(clusts, method = "maximum")   # create distance matrix (maximum method: here the difference between points dominates)
#clustd <- dist(clusts, method = "manhattan") # create distance matrix (manhattan method: most popular choice)
#clustd <- dist(clusts, method = "canberra")  # create distance matrix (canberra method: for count data - focuses on small differences and neglects larger differences)
#clustd <- dist(clusts, method = "binary")    # create distance matrix (binary method: for binary data only!)
#clustd <- dist(clusts, method = "minkowski") # create distance matrix (minkowski method: is not a true distance measure)

# distance method for words: daisy(data, method = "euclidean") # other possible distances are "manhattan" and "gower"

#cd <- hclust(clustd, method="single")    # create cluster object (single linkage)    : cluster with nearest data point
#cd <- hclust(clustd, method="ward.D")    # create cluster object (ward.D linkage)
cd <- hclust(clustd, method="ward.D2")   # create cluster object (ward.D2 linkage)   : cluster in a way to achieve minimum variance
#cd <- hclust(clustd, method="average")   # create cluster object (average linkage)   : cluster with closest mean
#cd <- hclust(clustd, method="mcquitty")  # create cluster object (mcquitty linkage)
#cd <- hclust(clustd, method="median")    # create cluster object (median linkage)    : cluster with closest median
#cd <- hclust(clustd, method="centroid")  # create cluster object (centroid linkage)  : cluster with closest prototypical point of target cluster
#cd <- hclust(clustd, method="complete")  # create cluster object (complete linkage)  : cluster with nearest furthest data point of target cluster

# plot result as dendrogram
plot(cd, hang = -1)              # display dendogram

# determine optimal number of clusters
# silhouette width: shows internal similarity of clusters vs similarity between clusters
optclus <- sapply(2:8, function(x) summary(silhouette(cutree(cd, k = x), clustd))$avg.width)
# inspect results
optclus # values lower than .2 indicate that clustering is not appropriate (Levshina 2015: 311)
        # the silhouette values display the silhouette width of 2 to 8 clusters
        # the highest value 0.75 is provided for 4 clusters
# cut tree into 4 clusters
groups <- cutree(cd, k=4)
rect.hclust(cd, k=4, border="red") # draw red borders around clusters

# which factors are particularly important
celtic <- clusts[c(1,8),]
others <- clusts[-c(1,8),]
# calculate column means
celtic.cm <- colMeans(celtic)
others.cm <- colMeans(others)
# calcualte difference between celtic and other englishes
diff <- celtic.cm - others.cm
sort(diff, decreasing = F)

plot(sort(diff), 1:length(diff), type= "n", xlab ="cluster 2 (others) <-> cluster 1 (celtic)", yaxt = "n", ylab = "")
text(sort(diff), 1:length(diff), names(sort(diff)), cex = 1)

# which factors are particularly important
nam <- clusts[c(3,4),]
others <- clusts[-c(3,4),]
# calculate column means
nam.cm <- colMeans(nam)
others.cm <- colMeans(others)
# calcualte difference between celtic and other englishes
diff <- nam.cm - others.cm
sort(diff, decreasing = F)

plot(sort(diff), 1:length(diff), type= "n", xlab ="cluster 2 (others) <-> cluster 1 (nam)", yaxt = "n", ylab = "")
text(sort(diff), 1:length(diff), names(sort(diff)), cex = 1)

# we see that wh-clefts and the frequency of like is typical for other varieties
# and that the use of youse as 2nd pl pronoun and inveáriant tags are typical for
# celtic englishes

# validate clustering
# compute pvclust to check how reliable our clusters are
res.pv <- pvclust(clus, method.dist="euclidean", method.hclust="ward.D2", nboot = 100)

# plot (provides Approximately Unbiased p-value and Bootstrap Probability value, cf. Levshina 2015: 316)
plot(res.pv, cex = 1)
pvrect(res.pv)

# load package ape; to install type: install.packages("ape")
library(ape)
# plot basic tree
plot(as.phylo(cd), cex = 0.9, label.offset = 1)

# plot as unrooted tree
plot(as.phylo(cd), type = "unrooted")


###################################################
###          CLUSTER ANALYSIS (nominal data)
###################################################
# load data
ire <- c(1,1,1,1,1,1,1,1,1,1)
sce <- c(1,1,1,1,1,1,1,1,1,1)
bre <- c(0,1,1,1,0,0,1,0,1,1)
aus <- c(0,1,1,1,0,0,1,0,1,1)
nze <- c(0,1,1,1,0,0,1,0,1,1)
ame <- c(0,1,1,1,0,0,0,0,1,0)
can <- c(0,1,1,1,0,0,0,0,1,0)
jam <- c(0,0,1,0,0,0,0,0,1,0)
phi <- c(0,0,1,0,0,0,0,0,1,0)
ind <- c(0,0,1,0,0,0,0,0,1,0)
clus <- data.frame(ire, nze, can, ame, phi, jam, bre, sce, aus, ind)
# add row names
rownames(clus) <- c("nae_neg", "like", "clefts", "tags", "youse", "soitwas", "dt", "nsr", "invartag", "wh_cleft")
# convert into factors
clus <- apply(clus, 1, function(x){
  x <- as.factor(x) })
# inspect data
clus

# clean data
clusts <- as.matrix(clus)
# create distance matrix
clustd <- dist(clusts, method = "binary")   # create a distance object with binary (!) distance
# display distance matrix
round(clustd, 2)

# create cluster object (ward.D2 linkage)   : cluster in a way to achieve minimum variance
cd <- hclust(clustd, method="ward.D2")
# plot result as dendrogram
plot(cd, hang = -1)              # display dendogram
# create factor with celtic varieties on one hand and other varieties on other
cluster <- as.factor(ifelse(as.character(rownames(clusts)) == "ire", "1",
  ifelse(as.character(rownames(clusts)) == "sce", "1", "0")))
# load library
library(vcd)
clsts.df <- as.data.frame(clusts)
# determine significance

pfish <- fisher.exact(table(cluster, clsts.df$youse))
pfish[[1]]

# determine effect size
assocstats(table(cluster, clsts.df$youse))

assocstats(table(cluster, clsts.df$like))

#############################################
###       KOLLOKATIONSANALYSE
#############################################
# load function(s)
source("RSkripte/collana.r")
# define path and load data
path <- "data05.txt"
coldata <- read.table(path, sep = "\t", header = T, skipNul = T, comment.char = "", quote = "")
# inspect data
head(coldata)

str(coldata)

# create table
t1 <- tapply(coldata$int, list(coldata$adjs, coldata$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
# inspect data
head(t3)

str(t3)

# apply collex function (ints >= 5: colnames(t3)[which(colSums(t3) >= 5)]
# the collex function takes the a term document matrix as first and the column
# with the collocate as second argument
absolutely <- collex(data = t3, cv1 = 2)
actually  <- collex(data = t3, cv1 = 3)
completely  <- collex(data = t3, cv1 = 11)
extremely  <- collex(data = t3, cv1 = 21)
highly  <- collex(data = t3, cv1 = 25)
incredibly  <- collex(data = t3, cv1 = 28)
particularly  <- collex(data = t3, cv1 = 33)
pretty  <- collex(data = t3, cv1 = 36)
real  <- collex(data = t3, cv1 = 39)
really  <- collex(data = t3, cv1 = 40)
so  <- collex(data = t3, cv1 = 45)
totally  <- collex(data = t3, cv1 = 52)
very  <- collex(data = t3, cv1 = 55)
well  <- collex(data = t3, cv1 = 57)
# extract informaltion
absolutely <- matrix(unlist(absolutely),ncol=4,byrow=TRUE)
actually <- matrix(unlist(actually),ncol=4,byrow=TRUE)
completely <- matrix(unlist(completely),ncol=4,byrow=TRUE)
extremely <- matrix(unlist(extremely),ncol=4,byrow=TRUE)
highly <- matrix(unlist(highly),ncol=4,byrow=TRUE)
incredibly <- matrix(unlist(incredibly),ncol=4,byrow=TRUE)
particularly <- matrix(unlist(particularly),ncol=4,byrow=TRUE)
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
totally <- matrix(unlist(totally),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
well <- matrix(unlist(well),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(absolutely, actually, completely, extremely, highly, incredibly,
  particularly, pretty, real, really, so, totally, very, well)
# convert into data frame
collexdf <- as.data.frame(collextab)
# add colnames
colnames(collexdf) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# inspect results before correction
head(collexdf)

# convert collexdf$p into numeric
collexdf$p <- as.numeric(levels(collexdf$p))[collexdf$p]

# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf)
collexdf$corr05 <- rep(corr05, nrow(collexdf))
corr01 <- 0.01/nrow(collexdf)
collexdf$corr01 <- rep(corr01, nrow(collexdf))
corr001 <- 0.001/nrow(collexdf)
collexdf$corr001 <- rep(corr001, nrow(collexdf))
# calculate corrected significance status
collexdf$sig <- as.vector(unlist(sapply(collexdf$p, function(x){
  x <- ifelse(x <= corr001, "p<.001",
    ifelse(x <= corr01, "p<.01",
    ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf <- collexdf[collexdf$p <= .05, ]
corrsigcollexdf <- collexdf[collexdf$sig != "n.s.", ]
# inspect results
head(sigcollexdf)

# inspect table with significant collocations after Bonferroni correction
head(corrsigcollexdf)

#############################################
###            BEISPIEL: KORPUSANALYSE
#############################################
# manuelles installieren
# If you don't already have devtools installed
#install.packages("devtools")
# Install development version of data.table
#library(devtools)
#install_github("Rdatatable/data.table", build_vignettes = FALSE)
# Initiate the packages
library(tm)
library(data.table)
library(plyr)
library(stringr)
library(NLP)
# load self written functions
source("RSkripte/ConcR.r")
bio <- read.table("data01.txt", header = T, sep = "\t")
# setting options
options(stringsAsFactors = F)
options(scipen=999)
#############################################
# search for swear words
# WARNING: ADAPT PATH!!!
pathname <- "Korpora\\IceIrelandSample"
context <- 20
all.pre = T
# define search strings
search.pattern1 <-  c("[A|a]rse[a-z]{0,}")
search.pattern2 <-  c("[F|f]uck[a-z]{0,}")
search.pattern3 <-  c("[S|s]hit[a-z]{0,}")
search.pattern4 <-  c("[C|c]ock[a-z]{0,}")
search.pattern5 <-  c("[W|w]hore[a-z]{0,}")
search.pattern6 <-  c("[A|a]ss")
search.pattern7 <-  c("[D|d]ick[a-z]{0,}")
search.pattern8 <-  c("[W|w]anker[a-z]{0,}")
search.pattern9 <-  c("[C|c]rap[a-z]{0,}")
search.pattern10 <-  c("[B|b]itch[a-z]{0,}")
search.pattern11 <-  c("[D|d]amn[a-z]{0,}")
# start search
sw1 <- ConcR(pathname, search.pattern1, context, all.pre = T)
sw2 <- ConcR(pathname, search.pattern2, context, all.pre = T)
sw3 <- ConcR(pathname, search.pattern3, context, all.pre = T)
sw4 <- ConcR(pathname, search.pattern4, context, all.pre = T)
sw5 <- ConcR(pathname, search.pattern5, context, all.pre = T)
sw6 <- ConcR(pathname, search.pattern6, context, all.pre = T)
sw7 <- ConcR(pathname, search.pattern7, context, all.pre = T)
sw8 <- ConcR(pathname, search.pattern8, context, all.pre = T)
sw9 <- ConcR(pathname, search.pattern9, context, all.pre = T)
sw10 <- ConcR(pathname, search.pattern10, context, all.pre = T)
sw11 <- ConcR(pathname, search.pattern11, context, all.pre = T)
# combine results
swire <- rbind(sw1[[2]], sw2[[2]], sw3[[2]], sw4[[2]], sw5[[2]], sw6[[2]],
  sw7[[2]], sw8[[2]], sw9[[2]],  sw10[[2]], sw11[[2]])
# convert matrix into a data frame
swire <- as.data.frame(swire)
# convert tokens to lower case
swire$token <- tolower(swire$token)
# inspect tokens
table(swire$token)

# clean data frame
swire$remove <- gsub(".+ass.+", "remove", swire$token, perl = T)
# remove items that are not swear words
swire <- swire[swire$remove != "remove", ]
# clean data frame
swire$remove <- gsub("ass.+", "remove", swire$token, perl = T)
# remove items that are not swear words
swire <- swire[swire$remove != "remove", ]
# clean data frame
swire$remove <- gsub(".+ass", "remove", swire$token, perl = T)
# remove items that are not swear words
swire <- swire[swire$remove != "remove", ]
# remove items that are not swear words
swire <- swire[swire$token != "arsed",]
swire <- swire[swire$token != "arsenal",]
swire <- swire[swire$token != "arsenals",]
swire <- swire[swire$token != "coarser",]
swire <- swire[swire$token != "cocked",]
swire <- swire[swire$token != "cockpit",]
swire <- swire[swire$token != "cocktail",]
swire <- swire[swire$token != "Cocktail",]
swire <- swire[swire$token != "cocktails",]
swire <- swire[swire$token != "cocktailyish",]
swire <- swire[swire$token != "hearse",]
swire <- swire[swire$token != "marseille",]
swire <- swire[swire$token != "peacock",]
swire <- swire[swire$token != "peacocks",]
swire <- swire[swire$token != "pearse",]
swire <- swire[swire$token != "rehearsed",]
swire <- swire[swire$token != "sparse",]
swire <- swire[swire$token != "alcock",]
swire <- swire[swire$token != "dickens",]
swire <- swire[swire$token != "dickensian",]
swire <- swire[swire$token != "dickson",]
swire <- swire[swire$token != "scrap",]
swire <- swire[swire$token != "scrape",]
swire <- swire[swire$token != "scraping",]
swire <- swire[swire$token != "scrapped",]
# inspect tokens
#table(swire$token)

# extract speaker
swire$spk <- gsub(".*<S", "<S", swire[,3])
swire$spk <- gsub(" .*", "", swire$spk)
# inspect data
head(swire)

##########################################################
# determine how many swearwords a speaker has used
swirespk <- table(swire$spk)
swirespk <- data.frame(swirespk)
colnames(swirespk) <- c("file.speaker.id", "freq")
# inspect data
head(swirespk)

##########################################################
# clean bio data: extract only relevant columns
bio$file.speaker.id <- as.vector(unlist(apply(bio, 1, function(x){
  x <- paste("<", x[2], "$", x[4], ">", sep = "")
  x <- gsub(" ", "", x) } )))
bio <- data.frame(bio$file.speaker.id, bio$sex, bio$age, bio$word.count)
colnames(bio) <- c("file.speaker.id", "sex", "age", "wc")
# inspect data
head(bio)

##########################################################
# combine frequencies and biodata
swire <- join(bio, swirespk, by = c("file.speaker.id"), type = "left")
# clean data
swire <- swire[is.na(swire$sex) == F, ]
swire <- swire[is.na(swire$age) == F, ]
swire$freq <- sapply(swire$freq, function(x){
  ifelse(is.na(x) == T, 0, x) } )
swire <- swire[swire$wc != 0, ]
# calculate per-1,000-words frequency
swire$ptw <- round(swire$freq/swire$wc*1000)
# select only files with headers S1A
swire$txttype <- gsub("<", "", swire$file.speaker.id)
swire$txttype <- gsub("-.*", "", swire$txttype)
swire <- swire[swire$txttype == "S1A", ]
swire <- swire[swire$wc > 99, ]
# inspect data
head(swire)

#############################################
#            GRAPHIKEN
# plot data (sex)
boxplot(swire$ptw ~ swire$sex, ylim = c(-5, 20),
  main = "Use of swear words by gender in Irish English",
  col = c("lightgreen", "lightblue"), notch = T)
grid()
text(1:2, tapply(swire$ptw, swire$sex, mean), "+")
text(1:2, c(-3.5, -3.5), paste("mean=\n", round(tapply(swire$ptw, swire$sex, mean), 3), sep = ""))
# include statz in graph
text(.75, 20, "Wilcox Test")
text(.75, 19, paste("W=", wilcox.test(swire$ptw ~ swire$sex)[1], sep = ""))
text(.75, 18, paste("p=", round(wilcox.test(swire$ptw ~ swire$sex)[[3]], 4), sep = ""))

# plot data (sex)
combined <- round(tapply(swire$ptw, swire$age, mean), 3)
m <- swire[swire$sex == "male", ]
male <- round(tapply(m$ptw, m$age, mean), 3)
f <- swire[swire$sex == "female", ]
female <- round(tapply(f$ptw, f$age, mean), 3)
# remove first item
combined <- combined[2:length(combined)]
male <- male[2:length(male)]
female <- female[2:length(female)]
# start plotting
plot(female, # plot the vector called "female"
  type = "o", # use plot type "o" (overplotted points and lines)
  lwd = 2, # use double line width (4 for 4 times the normal line width)
  lty = 1, # use line type 1 (striaght line)
  pch = 19, #  use point character 19 (filled circle)
  ylim = c(0, 5), # the y-axis should be drawn from 0 to 3
  ylab = "Relative Frequency", # the y-axis label should read "Relative Frequency"
  xlab = "Age",  # the x-axis label should read "Stages"
  axes = F, # don't draw axes yet!
  cex = 1) # all writing should be of normal size (.5 for half size)
# add aline for the data points in vector male (pch = 0: use empty squares as point characters)
lines(male, type = "o", lwd = 2,  lty = 1, pch = 0,  cex = 1)
# add aline for the data points in vector combined
# (lty = 3: use a dotted line instead of a straight line and
# pch = 4: use x marks as point characters)
lines(combined, type = "o", lwd = 2,  lty = 3, pch = 4,  cex = 1)
# add x-axes with specified labels at specified intervals
axis(1, at = 0:5, lab = c("", "19-25", "26-33", "34-41", "42-49", "50+"))
# add y-axes with specified labels at specified intervals
axis(2, at = seq(0, 5, .5), las = 1, lab = seq(0, 5, .5))
# create a legend
# define vector with linetypes
linetype <- c(1, 1, 3)
# define vector with point charaters
plotchar <- c(19, 0, 4)
# set up legend
legend("topright", inset = .05, c("female", "male", "both genders combined"),
  horiz = F,  pch = plotchar, lty = linetype)
# create a box around the plot
box()
# create a grid in the plot
grid()

###################################################
###                   END
###################################################

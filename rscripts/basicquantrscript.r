
# "Basics of Quantitative Reasoning"
# "UQ SLC Digital Team"
#
library(knitr)
variabletb <- matrix(c("Nominal Scaled / Categorical Variables", "Gender, Nationality, Part of Speech", "Ordinal Scaled Variables", "Graduation, School Grades, Military Rank", "Interval Scaled Variables", "Temperature, Acceptability Judgments", "Ratio-scaled Variables", "Age, Duration, Number of Syllables"), nrow = 4, byrow = T)
# add column and row names
colnames(variabletb) <- c("Variable Type", "Variable Level")
kable(variabletb, caption = "Variable types/Variable scales with examples.")
library(knitr)
variableextb <- matrix(c("Carl Lewis", "Ben Johnson", "Steve Davis", "1", "2", "3", "9.86", "9.97", "10.06"), nrow = 3, byrow = F)
# add column and row names
colnames(variableextb) <- c("Name", "Rank", "Time")
kable(variableextb, caption = "Results of a 100 meter dash.")
library(knitr)
numbers <- matrix(c("Head" , "Head" , "Head" , "3" , "0" , "0.125", "Head" , "Head" , "Tails" , "2" , "1" , "0.125", "Head" , "Tails" , "Head" , "2" , "1" , "0.125", "Tails" , "Head" , "Head" , "2" , "1" , "0.125", "Head" , "Tails" , "Tails" , "1" , "2" , "0.125", "Tails" , "Head" , "Tails" , "1" , "2" , "0.125", "Tails" , "Tails" , "Head" , "1" , "2" , "0.125", "Tails" , "Tails" , "Tails" , "0" , "3" , "0.125"), byrow = T, nrow = 8)
colnames(numbers) <- c("1^st^ Toss","2^nd^ Toss","3^rd^ Toss","Heads", "Tails", "Probabilty")
kable(numbers, caption = "Probabilities of Heads and Tails in 3 Coin Tosses.", align=c(rep('c',times=4)))
Type <- c(rep("NormalCoin", 500000), 
            rep("ManipulatedCoin", 500000))
Tosser <- rep(paste("Tosser", 1:500000, sep = ""), 2) 
Frequency <- c(rnorm(500000, 50, 3),
               rnorm(500000, 53, 3))
particletable <- data.frame(Type, Tosser, Frequency)
library(ggplot2)
ggplot(particletable,aes(x=Frequency, fill = Type))+
  geom_histogram(aes(y=..density..), alpha=.2, , position = "identity")+
  theme_bw()+
  labs(y = "Density", x = "Distribution of <heads> in coin tosses with a normal coin and a manipulated coin.") + 
# geom_density(alpha=.2) +  
# theme(legend.position = "none") +         # surpress legend
  scale_fill_manual(values = c('gray10', 'gray50')) # define colours
probdata1 <- matrix(c(0.125, 0.375, 0.375, 0.125), byrow = T, nrow = 1)
colnames(probdata1) <- c("0 Heads", "1 Time Head", "2 Times Heads", "3 Times Heads")
kable(probdata1, caption = "", align=c(rep('c',times=4)))
probdata1 <- matrix(c(0.125, 0.375, 0.375, 0.125), byrow = T, nrow = 1)
colnames(probdata1) <- c("0 Heads", "1 Head", "2 Heads", "3 Heads")
x <- barplot(probdata1, ylim = c(0, 0.5), col = "lightgrey", main = "Probabilities of Frequency Counts of Heads in 3 Coin Tosses.")
text(seq(0.7, 4.3, 1.2), probdata1+0.05, probdata1)
# probabilies of  0, 1, 2 and 3 times head in 3 coin tosses
dbinom(0:3, 3, 0.5)
# probabilies of  2 or 3 times head in 3 coin tosses
sum(dbinom(2:3, 3, 0.5))
# probabily of  100 times head in 100 coin tosses
dbinom(100, 100, 0.5)
# probabily of  58 to a 100 times head in 100 coin tosses
sum(dbinom(58:100, 100, 0.5))
# probabily of  59 to a 100 times head in 100 coin tosses
sum(dbinom(59:100, 100, 0.5))
# at which point does the probability of getting head 
# dip below 5 percent in 100 coin tosses?
qbinom(0.05, 100, 0.5, lower.tail=FALSE)
# at which point does the probability of getting head 
# dip below 5 percent in 100 coin tosses?
qnorm(0.05, lower.tail=TRUE)
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
barplot(wtb[2,], ylim = c(0, 0.1), ylab = "Probability", xlab = "Frequency Count (Head)",
  axes = F, axisnames = F, col = "lightgrey")
axis(1, at= xseq, labels= c("0", "20", "40", "60", "80", "100"))
axis(2, at= seq(0.0, 0.1, 0.05), labels= c("0.00", "0.05", "0.10"), cex = .75)
box()
# summ probs
sp <- cumsum(wtb[2,])
ttsp <- as.vector(unlist(sapply(sp, function(x){
  x <- ifelse(x <= 0.025, TRUE,
    ifelse(x >= 0.975, TRUE, FALSE))
    })))
barplot(wtb[2,], ylim = c(0, 0.1), ylab = "Probability", xlab = "Frequency Count (Head)",
  axes = F, axisnames = F, , col = ifelse(ttsp == T, "red", "lightgrey"))
axis(1, at= xseq, labels= c("0", "20", "40", "60", "80", "100"))
axis(2, at= seq(0.0, 0.1, 0.05), labels= c("0.00", "0.05", "0.10"), cex = .75)
box()
#grid()
text(75, 0.06, expression(paste(mu^1 !=  mu^2, sep = "")))
ttsp <- as.vector(unlist(sapply(sp, function(x){
  x <- ifelse(x <= 0.95, TRUE, FALSE)
    })))
barplot(wtb[2,], ylim = c(0, 0.1), ylab = "Probability", xlab = "Frequency Count (Head)",
  axes = F, axisnames = F, col = ifelse(ttsp == T, "lightgrey", "red"))
axis(1, at= xseq, labels= c("0", "20", "40", "60", "80", "100"))
axis(2, at= seq(0.0, 0.1, 0.05), labels= c("0.00", "0.05", "0.10"), cex = .75)
box()
#grid()
text(75, 0.06, expression(paste(mu^1 >=  mu^2, sep = "")))
# load packages
library(grDevices)
library(graphics)
### normal distribution: Mu=0, Sigma=1: Standard normal
library(graphics)
plot(dnorm, -4, 4, axes = F, xlab = "Standard Deviations", ylab = "Probability")
axis(1, at = seq(-4, 4, 1), labels = seq(-4, 4, 1))
axis(2, at= c(0.0, 0.2, 0.4), labels= c("0.0", "0.2", "0.4"))
text(3, 0.2, expression(paste(mu, "=0, ", sigma^2, "=1", sep = "")))
box()
lines(c(0, 0), c(-1, 0.5), col = "lightgrey")
lines(c(-0.675, 0.675), c(rep(0.3, 2)), col="red", lty=2)
text(0, 0.35, "50.0%")
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
plot(dnorm, -4, 4, axes = F, xlab = "Standard Deviations", ylab = "Probability")
axis(1, at = seq(-4, 4, 1), labels = seq(-4, 4, 1))
axis(2, at= c(0.0, 0.2, 0.4), labels= c("0.0", "0.2", "0.4"))
text(3, 0.2, expression(paste(mu, "=0, ", sigma^2, "=1", sep = "")))
box()
lines(c(1.96, 1.96), c(0, 0.5), col="lightgrey", lty=2)
text(3, 0.15, "2.5%")
text(3, 0.085, "(sd=1.96)")
lines(c(-1.96, -1.96), c(0, 0.5), col="lightgrey", lty=2)
text(-3, 0.15, "2.5%")
text(-3, 0.085, "(sd=-1.96)")
plot(dnorm, -4, 4, axes = F, xlab = "Standard Deviations", ylab = "Probability")
axis(1, at = seq(-4, 4, 1), labels = seq(-4, 4, 1))
axis(2, at= c(0.0, 0.2, 0.4), labels= c("0.0", "0.2", "0.4"))
text(3, 0.2, expression(paste(mu, "=0, ", sigma^2, "=1", sep = "")))
box()
lines(c(1.64, 1.64), c(0, 0.5), col="lightgrey", lty=2)
text(3, 0.15, "5% (sd=1.64)")
sum(dbinom(4, 7, 0.5))
sum(dbinom(3, 7, 0.5))
sum(dbinom(c(2, 5), 7, 0.5))
sum(dbinom(5:7, 7, 0.5))
sum(dbinom(3:6, 7, 0.5))
# load packages
library(grDevices)
# set up data
p50 <- dbinom(0:50, 50, 0.1)
w50 <- c(0:50)
wtb <- rbind(w50, p50)
colnames(wtb) <- c(0:50)
rownames(wtb) <- c("Likeuser", "prob")
xseq <- seq(0.7, 60.7, 1.2)
xseq <- xseq[c(1, 20, 40, 60, 80, 100)]
x <- barplot(wtb[2,], 
             ylim = c(0, 0.2), 
             ylab = "Probability", 
             xlab = "Frequency Count (discourse like users)", 
             axes = F, 
             axisnames = F, 
             col = "lightgrey")
axis(1, at= seq(0.7, 60.7, (60.7-0.7)/10), labels= seq(0, 50, 5))
axis(2, at= seq(0.0, 0.2, 0.05), labels= seq(0.0, 0.2, 0.05), cex = .75)
box()
# summ probs
sp <- cumsum(wtb[2,])
ttsp <- as.vector(unlist(sapply(sp, function(x){
  x <- ifelse(x <= 0.025, TRUE,
    ifelse(x >= 0.975, TRUE, FALSE))
    })))
barplot(wtb[2,], 
        ylim = c(0, 0.2), 
        ylab = "Probability", 
        xlab = "Frequency Count (discourse like users)",
        axes = F, 
        axisnames = F, , 
        col = ifelse(ttsp == T, "red", "lightgrey"))
axis(1, at= seq(0.7, 60.7, (60.7-0.7)/10), labels= seq(0, 50, 5))
axis(2, at= seq(0.0, 0.2, 0.05), labels= seq(0.0, 0.2, 0.05))
box()
#grid()
text(75, 0.06, expression(paste(mu^1 !=  mu^2, sep = "")))
#install.packages("knitr")      # install library (remove # to activate)
#install.packages("kableExtra") # install library (remove # to activate)
library(knitr)
library(kableExtra)
errortb <- matrix(c("", "alpha-error" , "beta-error", ""), byrow = F, nrow = 2)
colnames(errortb) <- c("Correlation", "No Correlation")
rownames(errortb) <- c("**Correlation**", "**No Correlation**")
library(knitr)                 # activate library for tabulating
kable(errortb, caption = "Alpha- and beta-errors.", booktabs = T)%>%
  kable_styling("striped", "bordered") %>%
  add_header_above(c("Reality" = 1, "Test" = 2))
Corpus <- c(rep("Corpus1", 100), 
            rep("Corpus2", 100))
Speaker <- rep(paste("Speaker", 1:50, sep = ""), 2) 
Frequency <- c(rnorm(100, 15, 3),
               rnorm(80, 15, 3),
               rnorm(20, 30, 3))
particletable <- data.frame(Corpus, Speaker, Frequency)
library(ggplot2)
ggplot(particletable,aes(x=Frequency, fill = Corpus))+
  geom_histogram(aes(y=..density..))+
  facet_grid(~Corpus)+
  theme_bw()+
  geom_density(alpha=.5) +  
  theme(legend.position = "none") +         # surpress legend
  scale_fill_manual(values = c('indianred4', 'gold')) # define colours
# test for normality
# anmerkungen
c1 <- particletable$Frequency[particletable$Corpus == "Corpus1"]
c2 <- particletable$Frequency[particletable$Corpus == "Corpus2"]
shapiro.test(c1) # p > .05 = normal
shapiro.test(c2) # p < .05 = non-normal
summary(c2)                       # summary for non-normal data
quantile(c2, c(0.25, 0.5, 0.75))  # quantiles
range(c2)                         # extract range (lowest/highest value)
mad(c2, constant = 1)             # median absolute deviation (median distance from median)
IQR(c2)                           # interquartile range
# möglichkeit 1
mydata <- read.delim("data/data01.txt", sep="\t", header = T)
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
data01 <- mydata # save data fro later use
library(knitr)
bayestable <- matrix(c("Correct","False positive","False negative","Correct"), byrow = T, nrow = 2)
colnames(bayestable) <- c("Reality: Cancer", "Reality: NoCancer")
rownames(bayestable) <- c("Test: Cancer", "Test: NoCancer")
kable(bayestable, caption = "", align=c(rep('c',times=4)))
library(knitr)
bayestable <- matrix(c("80%","9.6%","20%","90.4%"), byrow = T, nrow = 2)
colnames(bayestable) <- c("Reality: Cancer", "Reality: NoCancer")
rownames(bayestable) <- c("Test: Cancer", "Test: NoCancer")
kable(bayestable, caption = "", align=c(rep('c',times=4)))
The table above can be read as follows:
* 1% of people have cancer
* If the patient actually has cancer, the patient is in the first column. The test will detect cancer with a 80% accuracy, i.e. the test reports that the patient has cancer in 80% of cases. But in 20% of cases, the test will report "no cancer"" although the patient does, in fact, have cancer.
* If the patient does not have cancer, the patient is in the second column. There’s a 9.6% chance that the test will report that you have cancer, and a 90.6% chance that the test will report a negative result.
Imagine now, that the test reports that a patient has cancer. What is probability that the patient actually has cancer?
Because the test reports cancer, the patient is in the top row of the table. However, it could be a true positive or a false positive. The chances of a true positive (the patient has in deed cancer and the tests reports it) are = 1% \times 80% = .008% (0.8%). The chances of a false positive (the patient does not have cancer but the test reports that the patient has cancer) are = 99% \times 9.6% = 0.09504 (9.5%). 
# References


# "Descriptive Statistics"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)
# install libraries
install.packages(c("stringr", "VGAM", "fGarch"))
library(knitr)
Means <- c("(Arithmetic) mean (average)", "Median (middle value)", "Mode (most frequent value)", "Geometric mean (average factor)", "Harmonic mean (average rate)")
Use <- c("Description of normally dsitributed numeric variables (most common measure of central tendency)", "Description of non-normal numeric variables or ordinal variables (skewed data or influential outliers)", "Description of nominal and categorical variables", "Description of dynamic processes such as growth rates", "Description of dynamic processes such as velocities")
df <- data.frame(Means, Use)
kable(df, caption = "Measures of central tendency and their use.")r echo = F, results = 'asis'}
par(mfrow=c(1,2))
x1 <- c(2, 8, 4, 6)
x2 <- c(5, 5, 5, 5)
barplot(x1, ylim = c(0,10), axes = F)
text(seq(.7, 4.3, 1.2), x1+1, x1)
barplot(x2, ylim = c(0,10), axes = F, col = rep("red", 4))
text(seq(.7, 4.3, 1.2), x2+1, x2)
mtext("(Arithmetic) Mean", 3,1,at=2.5)
par(mfrow=c(1,1))r echo = F, results = 'asis'}
library(knitr)
Sentences <- c("Call me Ishmael", "Some years ago -- never mind how long precisely -- having little or no money in my purse, and nothing particular to interest me on shore, I thought I would sail about a little and see the watery part of the world.", "It is a way I have of driving off the spleen, and regulating the circulation.", "Whenever I find myself growing grim about the mouth; whenever it is a damp, drizzly November in my soul; whenever I find myself involuntarily pausing before coffin warehouses, and bringing up the rear of every funeral I meet; and especially whenever my hypos get such an upper hand of me, that it requires a strong moral principle to prevent me from deliberately stepping into the street, and methodically knocking people's hats off--then, I account it high time to get to sea as soon as I can.")
Words <- c(3, 40, 15, 87)
df <- data.frame(Sentences, Words)
kable(df, caption = "Sentences of the first paragraph of Herman Melville's *Moby Dick* and the number of words in each sentence.")
# create numeric vector
frequencies <- c(3, 40, 15, 87)
# calculate mean
mean(frequencies)r echo = F, results = 'asis'}
par(mfrow=c(1,2))
x1 <- c(5, 2, 9, 7, 1, 3, 8, 4, 6)
x2 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
barplot(x1, ylim = c(0,11), axes = F)
text(seq(.7, (.7+9*1.2), 1.2), x1+1, x1)
barplot(x2, ylim = c(0,11), axes = F, col = c(rep("grey", 4), "red", rep("grey", 4)))
text(seq(.7, (.7+9*1.2), 1.2), x2+1, x2)
mtext("Median value", 3, 1, at =(.7+4*1.2))
par(mfrow=c(1,1))r echo = F, results = 'asis'}
library(knitr)
Age <- c("0-10", "19-25", "26-33", "34-41", "42-49", "50+")
Counts <- c(9, 160, 70, 15, 9, 57)
df <- data.frame(Age, Counts)
kable(df, caption = "Number of speakers across age groups in the private dialogue section of the Irish component of the  *International Corpus of English* (ICE)")
Age <- c("0-18", "19-25", "26-33", "34-41", "42-49", "50+")
Counts <- c(9, 160, 70, 15, 9, 57)
df <- data.frame(Age, Counts)
barplot(df$Counts,
        ylim = c(0,200),
        ylab = "Frequency",
        xlab = "Age")
mtext(df$Age, 1, 1, at = seq(.7, 6.7, 1.2))
text(seq(.7, 6.7, 1.2), df$Counts +10, df$Counts)
box()r median}
# create a vector consisting out of ranks
ranks <- c(rep(1, 9), rep(2, 160), rep(3, 70), rep(4, 15), rep(5, 9), rep(6, 57))
# calculate median
median(ranks)r echo = F, results = 'asis'}
par(mfrow=c(1,2))
x1 <- c(5, 2, 9, 7, 1, 3, 8, 4, 6)
x2 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
barplot(x1, ylim = c(0,11), axes = F)
text(seq(.7, (.7+9*1.2), 1.2), x1+1, x1)
barplot(x2, ylim = c(0,11), axes = F, col = c(rep("grey", 8), "red"))
text(seq(.7, (.7+9*1.2), 1.2), x2+1, x2)
mtext("Mode", 3, 1, at =(.7+8*1.2))
par(mfrow=c(1,1))r echo = F, results = 'asis'}
library(knitr)
CurrentResidence <- c("Belfast", "Down", "Dublin (city)", "Limerick", "Tipperary")
Speakers <- c(98, 20, 110, 13, 19)
df <- data.frame(CurrentResidence, Speakers)
kable(df, caption = "Number of speakers across counties of current residency in the private dialogue section of the Irish component of the  *International Corpus of English* (ICE)")
barplot(df$Speakers,
        ylim = c(0,200),
        ylab = "Frequency",
        xlab = "Age")
mtext(df$CurrentResidence, 1, 1, at = seq(.7, 5.5, 1.2))
text(seq(.7, 5.5, 1.2), df$Speakers +10, df$Speakers)
box()r mode}
# create a factor with the current residence of speakers
CurrentResidence <- c(rep("Belfast", 98),         # repeat "Belfast" 98 times
                      rep("Down", 20),            # repeat "Down" 20 times
                      rep("Dublin (city)", 110),  # repeat "Dublin (city)" 110 times
                      rep("Limerick", 13),        # repeat "Limerick" 13 times
                      rep("Tipperary", 19))       # repeat "Tipperary" 19 times
# calculate mode
names(which.max(table(CurrentResidence)))         # extract which level occurs most frequentlyr echo = F, results = 'asis'}
library(knitr)
Year <- c("Year 1", "Year 2", "Year 3", "Year 4")
Package1 <- c("+5%", "-5%", "+5%", "-5%")
Package2 <- c("+20%", "-20%", "+20%", "-20%")
df <- data.frame(Year, Package1, Package2)
kable(df, caption = "Performance of two stock packages.")
library(knitr)
library(dplyr)
Corpus <- c(rep("C1", 5), 
            rep("C2", 5))
Speaker <- rep(c("A", "B", "C", "D", "E"), 2)  
Frequency <- c(5.2, 11.4, 27.1, 13.7, 9.6, 0.2, 0.0, 1.1, 93.7, 0.4)  
particletable <- data.frame(Corpus, Speaker, Frequency)
A <- c(5.2, 0.2)
B <- c(11.4, 0.0)
C <- c(27.1, 0.1)
D <- c(13.7, 93.7)
E <- c(9.6, 0.4)
barplotdata <- data.frame(A, B, C, D, E)
barplotdata <- as.matrix(barplotdata)
rownames(barplotdata) <- c("Corpus1", "Corpus2")
kable(particletable, 
      caption = "Relative frequencies of discourse partciles per speaker in two corpora")r echo = F, results = 'asis'}
barplot(barplotdata, 
        main = "Use of discourse particles across two corpora", # add title
        xlab="Speaker",                    # add x-axis label
        ylab="Relative Frequency",         # add y-axis label
        ylim = c(0,110),                   # define y-axis range
        col=c("darkblue","red"),           # define colors
        legend = rownames(barplotdata),    # add a legend
        beside=T,                          # stack the bars
        las = 1                            # tick label perpensicular
        )                                  # end barplot
box()                                      # create a box around panel
text(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5, 13.5,14.5), barplotdata+5, barplotdata) r echo = F, results = 'asis'}
library(knitr)
Month <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "Mean")
CityA <- c(-5, -12, 5, 12, 15, 18, 22, 23, 20, 16, 8, 1, 10.25)  
CityB <- c(7, 7, 8, 9, 10, 13, 15, 15, 13, 11, 8, 7, 10.25)  
temprature <- data.frame(Month, CityA, CityB)
kable(temprature, caption = "Average temperature in two cities by month")
January <- c(-5.00,  7.00)
February <- c(-12.00,  7.00)
March <- c(5.00,  8.00)
April <- c(12.00,  9.00)
May <- c(15.00, 10.00)
June <- c(18.00, 13.00)
July <- c(22.00, 15.00)
August <- c(23.00, 15.00)
September <- c(20.00, 13.00)
October <- c(16.00, 11.00)
November <- c(8.00,  8.00)
December <- c(1.00,  7.00)
lineplotdata <- data.frame(January, February, March, April, May, June, July, August, September, October, November, December)
lineplotdata <- as.matrix(lineplotdata)
rownames(lineplotdata) <- c("CityA", "City")r echo = F, results = 'asis'}
plot(lineplotdata[1,], 
     type = "l", 
     col = "red", 
     axes = F, 
     ylab = "Temperture", 
     xlab = "Month",
     ylim = c(-15, 30),
     lty = 1)
lines(lineplotdata[2,], 
      col = "darkblue",
      lty = 2)
axis(1, 1:12, colnames(lineplotdata), cex = .75, las = 2)
axis(2, seq(-15, 30, 5), seq(-15, 30, 5), las = 2)
box()
legend("topright", 
       lty = c(1,2), 
       col = c("red", "darkblue"), 
       legend = c("City A", "City B"), 
       bty = "n")r range}
# create a numeric vector
cityA <- c(-5, -12, 5, 12, 15, 18, 22, 23, 20, 16, 8, 1)
min(cityA); max(cityA) # extract ranger iqr}
summary(cityA) # extract IQRr variance}
sd(cityA)^2r sd}
sd(CityA) # calculate standard deviation
library(stringr)                          # activate stringr library
# define path to corpus
corpusfiles = list.files(path = "data/ICEIrelandSample", pattern = NULL, all.files = T, full.names = T, recursive = T, ignore.case = T, include.dirs = T)
# load and clean corpus
corpus <- lapply(corpusfiles, function(x) {
  scan(x, what = "char", sep = "", quote = "", quiet = T, skipNul = T)})
corpus <- unlist(corpus)                  # unlist corpus
corpus <- paste(corpus, sep = " ", collapse = " ")    # paste corpus together in one element
corpus <- gsub(" {2,}", " ", corpus)      # remove superfluous white spaces
corpus <- str_replace_all(corpus, ">.{1,20}</", "><") # clean corpus
corpus <- str_replace_all(corpus, "<.{1,50}>", "")    # clean corpus
corpus <- gsub(" {2,}", " ", corpus)      # remove superfluous white spaces
corpus <- str_trim(corpus, side = "both") # remove superfluous white spaces at string margins
corpus <- tolower(corpus)                 # convert to lower case
corpuswords <- strsplit(corpus, " ")      # split corpus into words
words <- table(corpuswords)[order(table(corpuswords), decreasing = T)][1:20]  # inspect frequency of first 20 elements
pos <- c("PPR", "ART", "PPR", "CON", "OTH", "PPR", "OTH", "ART", "V", "DPR", "PRP", "PRP", "PPR", "V", "V", "PPR", "PPR", "CON", "DPR", "ART")
wordstb <- data.frame(names(words), pos, as.vector(words))
colnames(wordstb) <- c("WordType", "POS", "Frequency")
library(knitr)
kable(wordstb, caption = "20 most frequent words in the sample corpus of ICE Ireland.")
sd(wordstb$Frequency, na.rm=TRUE) /  
   sqrt(length(wordstb$Frequency[!is.na(wordstb$Frequency)]))  
#install.packages("psych")   # install psych library (remove # to activate)
library(psych)               # activate psych library
describe(wordstb$Frequency,  # vector to be described
               type=2)       # determine of skew and kurtosis
library(stringr)                          # activate stringr library
# define path to corpus
corpusfiles = list.files(path = "data/ICEIrelandSample", pattern = NULL, all.files = T, full.names = T, recursive = T, ignore.case = T, include.dirs = T)
# load and clean corpus
corpus <- lapply(corpusfiles, function(x) {
  scan(x, what = "char", sep = "", quote = "", quiet = T, skipNul = T)})
corpus <- unlist(corpus)                  # unlist corpus
corpus <- paste(corpus, sep = " ", collapse = " ")    # paste corpus together in one element
corpus <- gsub(" {2,}", " ", corpus)      # remove superfluous white spaces
corpus <- str_replace_all(corpus, ">.{1,20}</", "><") # clean corpus
corpus <- str_replace_all(corpus, "<.{1,50}>", "")    # clean corpus
corpus <- gsub(" {2,}", " ", corpus)      # remove superfluous white spaces
corpus <- str_trim(corpus, side = "both") # remove superfluous white spaces at string margins
corpus <- tolower(corpus)                 # convert to lower case
corpuswords <- strsplit(corpus, " ")      # split corpus into words
words <- table(corpuswords)[order(table(corpuswords), decreasing = T)][1:20]  # inspect frequency of first 20 elements
pos <- c("PPR", "ART", "PPR", "CON", "OTH", "PPR", "OTH", "ART", "V", "DPR", "PRP", "PRP", "PPR", "V", "V", "PPR", "PPR", "CON", "DPR", "ART")
wordstb <- data.frame(names(words), pos, as.vector(words))
colnames(wordstb) <- c("WordType", "POS", "Frequency")
library(knitr)
kable(wordstb, caption = "20 most frequent words in the sample corpus of ICE Ireland.")
t.test(wordstb$Frequency, conf.level=0.95)  # extract mean and confidence intervals
#install.packages("Rmisc") # install Rmisc library (remove # to activate)
library(Rmisc)            # activate Rmisc library
CI(wordstb$Frequency, ci=0.95)   # extract mean and confidence intervals
#install.packages("DescTools") # install Rmisc library (remove # to activate)
library(DescTools)            # activate Rmisc library
MeanCI(wordstb$Frequency, conf.level=0.95)   # extract mean and confidence intervals
#install.packages("DescTools") # install Rmisc library (remove # to activate)
library(DescTools)            # activate Rmisc library
MeanCI(wordstb$Frequency, method="boot", type="norm", R=1000)
#install.packages("DescTools") # install Rmisc library (remove # to activate)
library(DescTools)            # activate Rmisc library
MeanCI(wordstb$Frequency, method="boot", type="norm", R=1000)
#install.packages("Rmisc") # install Rmisc library (remove # to activate)
library(Rmisc)                    # activate Rmisc library
summarySE(data=wordstb,           # apply summarySE function to data
          measurevar="Frequency", # define variable representing ferquencies
          groupvars="POS",        # define grouping variabel
          conf.interval = 0.95)   # extract standard deviation, standard error, and confidence intervals
#install.packages("boot") # install boot library (remove # to activate)
library(boot)                    # activate boot library
BootFunction = function(x, index) {                        # function to extract values
                  return(c(mean(x[index]),
                           var(x[index]) / length(index)))
                    }
Bootstrapped = boot(data=wordstb$Frequency,                # apply function to data
            statistic=BootFunction,
            R=1000)
mean(Bootstrapped$t[,1])                                   # extract values
boot.ci(Bootstrapped, conf=0.95)                           # alternative to extract values
binom.test(2, 20, 0.5,              # binom.test(x, n, p = 0.5, ...)
           alternative="two.sided", # define sidedness
           conf.level=0.95)         # define confidence level
Input =("
 Paw
 right
 left
 right
 right
 right
 right
 left
 right
 right
 right  
")
Gus = read.table(textConnection(Input),header=TRUE)
Successes = sum(Gus$ Paw == "left")      # Note the == operator
Failures  = sum(Gus$ Paw == "right")
Total = Successes + Failures
Expected = 0.5
binom.test(Successes, Total, Expected,
           alternative="two.sided",
           conf.level=0.95)
#install.packages("DescTools")        # install DescTools library (remove # to activate)
library(DescTools)                    # activate DescTools library
BinomCI(2, 20,                        # apply BinomCI function
        conf.level = 0.95,            # define ci
        method = "modified wilson")   # define method for ci extraction
#install.packages("DescTools")        # install DescTools library (remove # to activate)
library(DescTools)                    # activate DescTools library
observed = c(35,74,22,69)             # define multinominal vector
MultinomCI(observed,                  # apply MultinomCI function
           conf.level=0.95,           # define ci
           method="goodman")          # define method for ci extraction
# References
 

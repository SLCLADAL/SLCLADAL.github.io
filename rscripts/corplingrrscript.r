
# "Corpus Linguistics with R - Case Studies"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("tm", "dplyr","stringr", "visreg"))
# manual installation
install.packages("devtools")
# load devtools and install development version of data.table
library(devtools)
install_github("Rdatatable/data.table", build_vignettes = FALSE)
# define path to corpus
corpuspath <- "https://slcladal.github.io/data/ICEIrelandSample/"
# define corpusfiles
files <- paste(corpuspath, "S1A-00", 1:20, ".txt", sep = "")
files <- gsub("[0-9]([0-9][0-9][0-9])", "\\1", files)
# load corpus files
corpus <- sapply(files, function(x){
  x <- readLines(x)
  x <- paste(x, collapse = " ")
})
# inspect corpus
str(corpus)
# define path to corpus
# WARNING: you need to include your own path!
corpuspath <- "D:\\Uni\\UQ\\LADAL\\SLCLADAL.github.io\\data\\ICEIrelandSample"
# define corpusfiles
files <- list.paste(corpuspath, all.names = T)
# load corpus files
corpus <- sapply(files, function(x){
  x <- scan(x, what = "char", sep = "", quote = "", skipNul = T)
  x <- paste(x, sep = " ", collapse = " ")
})
# inspect corpus
str(corpus)
# load concordancing function
source("https://slcladal.github.io/rscripts/ConcR_2.5_LoadedFiles.R")
# define surrounding context for KWIC display
context <- 20
# define that we want everything that preceeds a match for the serach pattern 
all.pre = T
# define search patterns
search.pattern1 <- c("[A|a]rse[a-z]{0,}")
search.pattern2 <-  c("[F|f]uck[a-z]{0,}")
search.pattern3 <-  c("[S|s]hit[a-z]{0,}")
search.pattern4 <-  c("[C|c]ock[a-z]{0,}")
search.pattern5 <-  c("[W|w]hore[a-z]{0,}")
search.pattern6 <-  c("[A|a]ss[holes]{0,5}")
search.pattern7 <-  c("[D|d]ick[a-z]{0,}")
search.pattern8 <-  c("[W|w]anker[a-z]{0,}")
search.pattern9 <-  c("[C|c]rap[a-z]{0,}")
search.pattern10 <-  c("[B|b]itch[a-z]{0,}")
search.pattern11 <-  c("[D|d]amn[a-z]{0,}")
# start search
sw1 <- ConcR(corpus, search.pattern1, context, all.pre = T)
sw2 <- ConcR(corpus, search.pattern2, context, all.pre = T)
sw3 <- ConcR(corpus, search.pattern3, context, all.pre = T)
sw4 <- ConcR(corpus, search.pattern4, context, all.pre = T)
#sw5 <- ConcR(corpus, search.pattern5, context, all.pre = T)
sw6 <- ConcR(corpus, search.pattern6, context, all.pre = T)
#sw7 <- ConcR(corpus, search.pattern7, context, all.pre = T)
sw8 <- ConcR(corpus, search.pattern8, context, all.pre = T)
sw9 <- ConcR(corpus, search.pattern9, context, all.pre = T)
sw10 <- ConcR(corpus, search.pattern10, context, all.pre = T)
#sw11 <- ConcR(corpus, search.pattern11, context, all.pre = T)
# combine search results
swire <- rbind(sw1, sw2, sw3, sw4, sw6, sw8, sw9, sw10)
# convert matrix into a data frame
swire <- as.data.frame(swire)
# inspect structure of the data
str(swire)
# clean file names
swire$OriginalString <- gsub(".*/", "", swire$OriginalString)
swire$OriginalString <- gsub("\\..*", "", swire$OriginalString)
# store file names
files <- names(table(swire$OriginalString))
# inspect result
names(table(swire$OriginalString))
# convert tokens to lower case
swire$Token <- tolower(swire$Token)
# inspect tokens
names(table(swire$Token))
# create vector with non-swear words
nonswearwords <- c("cocktails", "crape")
# check number of rows in current data
nrow(swire)
# remove rows containing nonswearwords
swire <- swire[!swire$Token %in% nonswearwords,]
# check number of rows of data after removal
nrow(swire)
# create new variable
swire$KWIC <- paste(swire$PreContext, " << ", swire$Token, " >> ", swire$PostContext, sep = "")
# inspect first lines
head(swire$KWIC)
# save results on disc 
# WARNING: you need to include your own path!
write.table(swire$KWIC, "D:\\Uni\\UQ\\LADAL\\SLCLADAL.github.io\\data/swearwordkwic.txt", sep = "\t", quote = F)
library(png)
ima <- readPNG("images/excelkwic.png")
plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "", xaxt = "n",yaxt = "n")
lim <- par()
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
# clean data frame
swire$remove <- ifelse(swire$Token == "ass" & nchar(gsub(".* ", " ", swire$PreContext) > 1), "remove", "keep")
# remove items that are not swear words
swire <- swire[swire$remove != "remove", ]
# create vector with non-swear word uses
nonswearworduses <- c("bitchy", "asses", "assle", "asso")
# remove non-swear word uses
swire <- swire[!swire$Token %in% nonswearworduses, ]
# inspect results
names(table(swire$Token))
# remove columns
swire$remove <- NULL
swire$KWIC <- NULL
# extract speaker
swire$EntirePreContext <- gsub(".*<S", "<S", swire$EntirePreContext)
swire$EntirePreContext <- gsub(" .*", "", swire$EntirePreContext)
# rename columns
colnames(swire) <- ifelse(colnames(swire) == "EntirePreContext", "Speaker",
                          ifelse(colnames(swire) == "OriginalString", "File",colnames(swire)))
# inspect data
head(swire)
# tabulate speaker and swear word frequency
swirespk <- table(swire$Speaker)
swirespk <- data.frame(swirespk)
colnames(swirespk) <- c("Speaker", "Swearwords")
# inspect data
head(swirespk)
# load bio data
bio <- read.table("https://slcladal.github.io/data/data01.txt", 
                  header = T, sep = "\t")
# create new speaker id
bio$file.speaker.id <- paste("<", bio$text.id, "$", bio$spk.ref, ">", sep = "")
# determine file
bio$File <- bio$text.id
# create shorter table
bio <- data.frame(bio$File, bio$file.speaker.id, bio$sex, bio$age, bio$word.count)
# add column names
colnames(bio) <- c("File", "Speaker", "Gender", "Age", "Words")
# inspect data
head(bio)
# remove speakers of files that are not in the sample corpus
bio <- bio[bio$File %in% files,]
# combine frequencies and biodata
swire <- join(bio, swirespk, by = c("Speaker"), type = "left")
# replave NA with 0
swire$Swearwords <- ifelse(is.na(swire$Swearwords), 0, swire$Swearwords)
# inspect data
head(swire); table(swire$File)
# clean data
swire <- swire[is.na(swire$Gender) == F, ]
swire <- swire[is.na(swire$Age) == F, ]
swire <- swire[swire$Words != 0, ]
# calculate per-1,000-words frequency
swire$RelativeFrequency <- round(swire$Swearwords/swire$Words*1000)
# inspect data
head(swire)
# plot swear word use by gender
boxplot(swire$RelativeFrequency ~ swire$Gender, 
        ylim = c(-5, 20),
  main = "Use of swear words by gender in Irish English",
  col = c("orange", "darkgrey"), 
  notch = F)
grid()
# add text
# add + where mean values are
text(1:2, tapply(swire$RelativeFrequency, swire$Gender, mean), "+")
# add mean value below box
text(1:2, c(-3.5, -3.5), paste("mean=\n", round(tapply(swire$RelativeFrequency, swire$Gender, mean), 3), sep = ""))
# include statz in graph
# add results of Wilcox Test
text(.75, 20, "Wilcox Test")
text(.75, 18, paste("W=", as.vector(unlist(wilcox.test(swire$RelativeFrequency ~ swire$Gender)[1])), sep = ""))
text(.75, 16, paste("p=", round(wilcox.test(swire$RelativeFrequency ~ swire$Gender)[[3]], 4), sep = ""))
# create interaction table
interactiontb <- as.data.frame(tapply(swire$RelativeFrequency , 
                        list(swire$Age, swire$Gender), 
                        mean))
# inspect table
interactiontb
plot(interactiontb$female, 
     type = "b", lwd = 2,  
     lty = 1, 
     pch = 0,  
     cex = 1, 
     ylim = c(0,10),
     col = "orange",
     axes = F, 
     xlab = "Age",
     ylab = "Relative frequency (per 1,000 words)")
lines(interactiontb$male, 
      type = "o", 
      lwd = 2,  
      lty = 2, 
      pch = 1,
      col = "darkgrey",
      cex = 1)
axis(1, at = 0:5, lab = c("", "19-25", "26-33", "34-41", "42-49", "50+"))
# add y-axes with specified labels at specified intervals
axis(2, at = seq(0, 10, 2), las = 1, lab = seq(0, 10, 2))
# add legend
legend("topright", inset = .05, c("female", "male"),
  horiz = F,  pch = c(0,1), lty = c(1,2), col = c("orange", "darkgrey"))
# create a box around the plot
box()
# add grid
grid()

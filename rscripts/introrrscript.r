
# "A Basic Introduction To R"
# "UQ SLC Digital Team"
#
library(png)
ima <- readPNG("images/RStudioscreenshot.png")
plot(c(100, 250), c(300, 450), type = "n", xlab = "", ylab = "", xaxt = "n",yaxt = "n")
lim <- par()
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
Year <- c(1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000)
Frequency <- c(2.7, 3.5, 4.3, 3.6, 4.6, 4.9, 4.0, 5.1)
plot(Year, Frequency, type = "l")
plot(Year, Frequency, type = "l", col = "red")
1 + 2      # addition
sqrt(9)    # square root function
x <- 1     # assignment
1:10       # sequence
# install.packages("tm") 
library("tm")
require("tm")
# help
#help(require) 
#?require
apropos("nova")
# set your working directory
setwd("D:\\Uni\\UQ\\LADAL\\SLCLADAL.github.io") 
x <- 10.5
typeof(x)
class(x)
as.integer(x)
is.integer(x)
is.integer(as.integer(x))
x <- "3.14"
typeof(x)
x <- as.double(x)
1:3 == c(1, 2, 3)
options(stringsAsFactors = F)
myvector <- c(1, 2, 3)
names(myvector) <- c("one", "two", "three")
print(myvector)
print(myvector[1:2])
print(myvector[-1])
sum(myvector)
mean(myvector)
mymatrix <- matrix(0, nrow=3, ncol=4)
rownames(mymatrix) <- c("one", "two", "three")
colnames(mymatrix) <- c("house", "sun", "tree", ".")
mymatrix[, 1] <- 12
mymatrix[, "sun"] <- 4
mymatrix[3, 4] <- 5
mymatrix[2, 3:4] <- 9
colSums(mymatrix)
rowSums(mymatrix)
colMeans(mymatrix)
mydatdaframe <- data.frame(v = c(1, 2, 3), c = as.character(myvector), n = c("one", "two", "three"))
mydatdaframe$v
mydatdaframe$c
mydatdaframe[, "c"]
mydatdaframe[1, ]
#rowSums(mydatdaframe)
sort(mydatdaframe$n)
sort(mydatdaframe$n, decreasing = T)
c(myvector, myvector)
is.vector(myvector)
is.matrix(mymatrix)
is.matrix(myvector)
mymatrix / 2
mymatrix / myvector
t(mymatrix)
mymatrix %*% t(mymatrix)
cbind(mymatrix, myvector)
rbind(mymatrix, myvector)
data(USArrests)
#View(USArrests)
dim(USArrests)
nrow(USArrests)
ncol(USArrests)
length(USArrests)
max(USArrests)
which.max(USArrests[, "Murder"])
o <- order(USArrests[, "Murder"], decreasing = T)
USArrests[o, ]
murderRates <- USArrests[o[1:5], "Murder"]
names(murderRates) <- rownames(USArrests)[o[1:5]]
barplot(murderRates)

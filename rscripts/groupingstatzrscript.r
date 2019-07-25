
# "Classification"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("cluster", "factoextra", "cluster", 
                   "seriation", "pvclust", "ape", "vcd", 
                   "exact2x2", "factoextra", "seriation", 
                   "NbClust", "pvclust"))
x <- 1:10
y <- 1:10
plot(x, y, type = "n", ylim = c(-.5,10), xlim = c(0,5), axes = F, xlab = "", ylab = "")
text("Trees", x = 2.25, y = 10, cex = 1.5)
text("Conifers", x = .5, y = 6.5, cex = 1.5)
text("Broad leaf", x = 2.25, y = 6.5, cex = 1.5)
text("Palms", x = 4, y = 6.5, cex = 1.5)
text("Pine tree", x = .25, y = 1.5, srt=90, cex = 1.5)
text("Fir tree", x = .75, y = 1.5, srt=90, cex = 1.5)
text("Oak tree", x = 2, y = 1.5, srt=90, cex = 1.5)
text("Beech tree", x = 2.5, y = 1.5, srt=90, cex = 1.5)
text("Phoenix palm", x = 3.75, y = 1.75, srt=90, cex = 1.5)
text("Nikau palm", x = 4.25, y = 1.5, srt=90, cex = 1.5)
#
lines(x = c(.5, 1.75), y = c(7, 9), lwd = 2)
lines(x = c(2.25, 2.25), y = c(7, 9), lwd = 2)
lines(x = c(4, 2.75), y = c(7, 9), lwd = 2)
#
lines(x = c(.5, .5), y = c(6, 4.5), lwd = 2)
lines(x = c(2.25, 2.25), y = c(6, 4.5), lwd = 2)
lines(x = c(4, 4), y = c(6, 4.75), lwd = 2)
#
lines(x = c(.25, .75), y = c(4.5, 4.5), lwd = 2)
lines(x = c(2, 2.5), y = c(4.5, 4.5), lwd = 2)
lines(x = c(3.75, 4.25), y = c(4.75, 4.75), lwd = 2)
#
lines(x = c(.25, .25), y = c(4.5, 4), lwd = 2)
lines(x = c(.75, .75), y = c(4.5, 4), lwd = 2)
lines(x = c(2, 2), y = c(4.5, 4), lwd = 2)
lines(x = c(2.5, 2.5), y = c(4.5, 4), lwd = 2)
x <- 1:10
y <- 1:10
plot(x, y, type = "n", ylim = c(-.5,15), xlim = c(0,5), axes = F, xlab = "", ylab = "")
text("Trees", x = 2.25, y = 15, cex = 1)
text("Conifers", x = .5, y = 6.5, cex = 1)
text("Broad leaf", x = 2.25, y = 6.5, cex = 1)
text("Palm Trees", x = 3.5, y = 10, cex = 1)
text("Pine tree", x = .25, y = 1.5, srt=90, cex = 1)
text("Fir tree", x = .75, y = 1.5, srt=90, cex = 1)
text("Oak tree", x = 2, y = 1.5, srt=90, cex = 1)
text("Beech tree", x = 2.5, y = 1.5, srt=90, cex = 1)
text("Phoenix palm", x = 3.25, y = 1.75, srt=90, cex = 1)
text("Nikau palm", x = 3.75, y = 1.5, srt=90, cex = 1)
#
lines(x = c(1.5, 2.15), y = c(11, 13.5), lwd = 2)
lines(x = c(3.5, 2.5), y = c(11, 13.5), lwd = 2)
lines(x = c(.5, 1.5), y = c(7.25, 11), lwd = 2)
lines(x = c(1.5, 2.25), y = c(11, 7.25), lwd = 2)
#
lines(x = c(.5, .5), y = c(6, 4.5), lwd = 2)
lines(x = c(2.25, 2.25), y = c(6, 4.5), lwd = 2)
lines(x = c(3.5, 3.5), y = c(8.75, 6.25), lwd = 2)
#
lines(x = c(.25, .75), y = c(4.5, 4.5), lwd = 2)
lines(x = c(2, 2.5), y = c(4.5, 4.5), lwd = 2)
lines(x = c(3.25, 3.75), y = c(6.25, 6.25), lwd = 2)
#
lines(x = c(.25, .25), y = c(4.5, 4), lwd = 2)
lines(x = c(.75, .75), y = c(4.5, 4), lwd = 2)
lines(x = c(2, 2), y = c(4.5, 4), lwd = 2)
lines(x = c(2.5, 2.5), y = c(4.5, 4), lwd = 2)
# generate data
y <- c(1, 3.1, 1.2, 2.3, 3.4, 2.5, 1.6, 2.7, 3.8, 2.9)
x <- c(1:10)
plot(x, y, 
     type = "l", 
     ylim = c(0,11), 
     xaxt='n', 
     yaxt='n', 
     ann=FALSE, 
     lwd = 2, 
     ylab = "", 
     xlab = "")
lines(x = 1:10, y = c(5, 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9), col = "blue", lwd = 2)
lines(x = 1:10, y = c(8, 10.1, 8.2, 9.3, 10.4, 9.5, 8.6, 9.7, 10.8, 9.9), col = "red", lwd = 2)
# similarity
students <- matrix(c(2,  3,  2, 1,  3,  2, 1,  2,  1, 2,  4,  4, 3,  4,  3),
  nrow = 5, byrow = T)
students <- as.data.frame(students)
colnames(students) <- c("Math", "Music", "Biology")
rownames(students) <- c("StudentA", "StudentB", "StudentC", "StudentD", "StudentE")
library(knitr)
kable(students, caption = "Sample of five students and their grades in math, music, and biology")
diststudents <- dist(students, method = "manhattan") # create a distance matrix
library(knitr)
diststudentstb <- matrix(c("1", "3", "3","3", "", "2", "4", "4","", "", "6", "6", "", "", "", "2"), nrow = 4, byrow = F)
# add column and row names
colnames(diststudentstb) <- c("StudentA", "StudentB", "StudentC", "StudentD")
rownames(diststudentstb) <- c("StudentB", "StudentC", "StudentD", "StudentE")
kable(diststudentstb, caption = "Distance matrix based of students based on grades in math, music, and biology.")
library("cluster")    # activate library
clusterstudents <- hclust( # hierarchical cluster object
  diststudents,       # use data diststudents
  method="ward.D")    # ward.D as linkage method
plot(clusterstudents, # plot result as dendrogram
     hang = 0)        # labels at split
students2 <- matrix(c(1.5, 3, 2, 1,  2,  1, 2,  4,  4, 3,  4,  3),
  nrow = 4, byrow = T)
students2 <- as.data.frame(students2)
rownames(students2) <- c("Cluster1", "StudentC", "StudentD", "StudentE")
diststudents2 <- dist(students2, method = "manhattan")r cl10, eval = T, echo = F, results = 'asis'}
library(knitr)
diststudentstb <- matrix(c("2.5","3.5","3.5","","6.0","6.0","","","2.0"), 
                         nrow = 3, byrow = F)
# add column and row names
colnames(diststudentstb) <- c("Cluster 1", "Student C", "Student D")
rownames(diststudentstb) <- c("Student C", "Student D", "Student E")
kable(diststudentstb, 
      caption = "Distance matrix of students based on grades in math, music, and biology.")
students3 <- matrix(c(1.5,3,2,1,2,1,2.5,4,3.5),
  nrow = 3, byrow = T)
students3 <- as.data.frame(students3)
rownames(students3) <- c("Cluster1", "StudentC", "Cluster2")
diststudents3 <- dist(students3, 
                      method = "manhattan")
library(knitr)
diststudentstb <- matrix(c("2.5", "3.5", "", "6.0"), nrow = 2, byrow = F)
# add column and row names
colnames(diststudentstb) <- c("Cluster 1", "Student C")
rownames(diststudentstb) <- c("Student C", "Cluster 2")
kable(diststudentstb, caption = "Distance matrix based of students based on grades in math, music, and biology.")
par(mar=c(1,1,1,1))  # define margine width of the plot
x <- c(1,5)          # define an x value
y <- c(1,5)          # define a y value
plot(x, y, 
     pch = 20, 
     cex = 1, 
     axes = F, 
     las = 1, 
     xlab = "", 
     ylab = "", 
     xlim = c(0,7), 
     ylim = c(0,10))
text(0.5, .5, "Point A", cex = 1)
text(5, 5.5, "Point B", cex = 1)
lines(x = c(1, 5), y = c(1, 5), type = "l", lty = 3, lwd = 2, col = "red")
lines(x = c(1, 5), y = c(1, 1), type = "l", lty = 2, lwd = 2, col = "blue")
lines(x = c(5, 5), y = c(1, 5), type = "l", lty = 4, lwd = 2, col = "green")
lines(x = c(.9, 5), y = c(.9, .9), type = "l", lty = 4, lwd = 2, col = "green")
legend("topleft", inset=.05, title="", bty = "n", lty = c(3, 2, 4), lwd = 2,
   c("euclidean distance", "maximum distance", "manhatten distance"), col=c("red", "blue", "green"), horiz=F, cex = 1)
par(mar=c(5.1,4.1,4.1,2.1))
# generate data
IrishEnglish <- round(sqrt((rnorm(10, 9.5, .5))^2), 3)
ScottishEnglish <- round(sqrt((rnorm(10, 9.3, .4))^2), 3)
BritishEnglish <- round(sqrt((rnorm(10, 6.4, .7))^2), 3)
AustralianEnglish <- round(sqrt((rnorm(10, 6.6, .5))^2), 3)
NewZealandEnglish <- round(sqrt((rnorm(10, 6.5, .4))^2), 3)
AmericanEnglish <- round(sqrt((rnorm(10, 4.6, .8))^2), 3)
CanadianEnglish <- round(sqrt((rnorm(10, 4.5, .7))^2), 3)
JamaicanEnglish <- round(sqrt((rnorm(10, 1.4, .2))^2), 3)
PhillipineEnglish <- round(sqrt((rnorm(10, 1.5, .4))^2), 3)
IndianEnglish <- round(sqrt((rnorm(10, 1.3, .5))^2), 3)
clus <- data.frame(IrishEnglish, ScottishEnglish, BritishEnglish, 
                   AustralianEnglish, NewZealandEnglish, AmericanEnglish, 
                   CanadianEnglish, JamaicanEnglish, PhillipineEnglish, IndianEnglish)
# add row names
rownames(clus) <- c("nae_neg", "like", "clefts", "tags", "youse", "soitwas", "dt", "nsr", "invartag", "wh_cleft")
summary(clus) # inspect results
# clean data
clust <- t(clus)            # transpose data
clust <- na.omit(clust)     # remove missing values
clusts <- scale(clust)      # standardize variables
clusts <- as.matrix(clusts) # convert into matrix
clust
library("factoextra")         # load library to extract cluster tendency
clusttendency <- get_clust_tendency(clusts,    # apply get_clust_tendency to cluster object
                   n = 9,     # define number of points from sampe speace 
                   gradient = list(low = "steelblue",  # define color for low values 
                                   high = "white"))    # define color for high values
clusttendency[1]
clustd <- dist(clusts,                 # create distance matrix
               method = "euclidean")   # use eucledian (!) distance
round(clustd, 2)                       # display distance matrix
# create distance matrix (eucledian method: not good when dealing with many dimensions)
clustd <- dist(clusts, method = "euclidean")
# create distance matrix (maximum method: here the difference between points dominates)
clustd_maximum <- round(dist(clusts, method = "maximum"), 2)
# create distance matrix (manhattan method: most popular choice)
clustd_manhatten <- round(dist(clusts, method = "manhattan"), 2) 
# create distance matrix (canberra method: for count data only - focuses on small differences and neglects larger differences)
clustd_canberra <- round(dist(clusts, method = "canberra"), 2)
# create distance matrix (binary method: for binary data only!)
clustd_binary <- round(dist(clusts, method = "binary"), 2) 
# create distance matrix (minkowski method: is not a true distance measure)
clustd_minkowski <- round(dist(clusts, method = "minkowski"), 2) 
# distance method for words: daisy (other possible distances are "manhattan" and "gower")
library(cluster)
clustd_daisy <- round(daisy(clusts, metric = "euclidean"), 2) 
clustd_maximum 
library(seriation)
dissplot(clustd)  # create distance plot
cd <- hclust(clustd,             # create cluster object
             method="ward.D2")   # ward.D2 linkage (minimum variance)
plot(cd, hang = -1)              # display dendogram
# single linkage: cluster with nearest data point
cd_single <- hclust(clustd, method="single") 
# create cluster object (ward.D linkage)
cd_wardd <- hclust(clustd, method="ward.D")
# create cluster object (ward.D2 linkage): 
# cluster in a way to achieve minimum variance
cd_wardd2 <- hclust(clustd, method="ward.D2")
# average linkage: cluster with closest mean
cd_average <- hclust(clustd, method="average") 
# mcquitty linkage
cd_mcquitty <- hclust(clustd, method="mcquitty") 
# median linkage: cluster with closest median
cd_median <- hclust(clustd, method="median")
# centroid linkage: cluster with closest prototypical point of target cluster
cd_centroid <- hclust(clustd, method="centroid") 
# complete linkage: cluster with nearest/furthest data point of target cluster
cd_complete <- hclust(clustd, method="complete")  
optclus <- sapply(2:8, function(x) summary(silhouette(cutree(cd, k = x), clustd))$avg.width)
optclus # inspect results
optnclust <- which(optclus == max(optclus)) # determine optimal number of clusters
groups <- cutree(cd, k=optnclust) # cut tree into optimal number of clusters
groups <- cutree(cd, k=optnclust)          # cut tree into optimal clusters
plot(cd, hang = -1, cex = .75)             # plot result as dendrogram
rect.hclust(cd, k=optnclust, border="red") # draw red borders around clusters
# which factors are particularly important
celtic <- clusts[c(1,2),]
others <- clusts[-c(1,2),]
# calculate column means
celtic.cm <- colMeans(celtic)
others.cm <- colMeans(others)
# calcualte difference between celtic and other englishes
diff <- celtic.cm - others.cm
sort(diff, decreasing = F)
plot(                   # start plot
  sort(diff),           # y-values
  1:length(diff),       # x-values 
  type= "n",            # plot type (empty)
  cex.axis = .75,       # axis font size
  cex.lab = .75,        # label font size
  yaxt = "n",           # no y-axis tick marks
  ylab = "")            # no y-axis label
text(sort(diff), 1:length(diff), names(sort(diff)), cex = .75) # plot text into plot
Outer <- clusts[c(6:8),]     # data of outer circle varieties
Inner <- clusts[-c(6:8),]    # data of inner circle varieties
Outer.cm <- colMeans(Outer)  # column means for outer circle
Inner.cm <- colMeans(Inner)  # column means for inner circle
diff <- Outer.cm - Inner.cm  # difference between inner and outer circle
sort(diff, decreasing = F)   # order difference between inner and outer circle
plot(                   # start plot
  sort(diff),           # y-values
  1:length(diff),       # x-values 
  type= "n",            # plot type (empty)
  cex.axis = .75,       # axis font size
  cex.lab = .75,        # label font size
  yaxt = "n",           # no y-axis tick marks
  ylab = "")            # no y-axis label
text(sort(diff), 1:length(diff), names(sort(diff)), cex = .75) # plot text into plot
library(pvclust) # activate library
res.pv <- pvclust(clus,                     # apply pvclust method to clus data
                  method.dist="euclidean",  # use eucledian distance
                  method.hclust="ward.D2",  # use ward.d2 linkage
                  nboot = 100)              # use 100 bootstrap runs
plot(res.pv, 
     cex = .75)
pvrect(res.pv)
library(ape)            # load package ape
plot(as.phylo(cd),      # plot cluster object
     cex = 0.75,        # .75 font size
     label.offset = .5) # .5 label offset
# plot as unrooted tree
plot(as.phylo(cd),      # plot cluster object
     type = "unrooted", # plot as unrooted tree
     cex = .75,         # .75 font size
     label.offset = 1)  # .5 label offset
# generate data
IrishEnglish <- c(1,1,1,1,1,1,1,1,1,1)
ScottishEnglish <- c(1,1,1,1,1,1,1,1,1,1)
BritishEnglish <- c(0,1,1,1,0,0,1,0,1,1)
AustralianEnglish <- c(0,1,1,1,0,0,1,0,1,1)
NewZealandEnglish <- c(0,1,1,1,0,0,1,0,1,1)
AmericanEnglish <- c(0,1,1,1,0,0,0,0,1,0)
CanadianEnglish <- c(0,1,1,1,0,0,0,0,1,0)
JamaicanEnglish <- c(0,0,1,0,0,0,0,0,1,0)
PhillipineEnglish <- c(0,0,1,0,0,0,0,0,1,0)
IndianEnglish <- c(0,0,1,0,0,0,0,0,1,0)
clus <- data.frame(IrishEnglish, ScottishEnglish, BritishEnglish, 
                   AustralianEnglish, NewZealandEnglish, AmericanEnglish, 
                   CanadianEnglish, JamaicanEnglish, PhillipineEnglish, IndianEnglish)
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
cluster <- as.factor(ifelse(as.character(rownames(clusts)) == "IrishEnglish", "1",
  ifelse(as.character(rownames(clusts)) == "ScottishEnglish", "1", "0")))
# load library
library(vcd)
clsts.df <- as.data.frame(clusts)
# determine significance
library(exact2x2)
pfish <- fisher.exact(table(cluster, clsts.df$youse))
pfish[[1]]
# determine effect size
assocstats(table(cluster, clsts.df$youse))
assocstats(table(cluster, clsts.df$like))
library("factoextra")
library("seriation")
library("NbClust")
library("pvclust")

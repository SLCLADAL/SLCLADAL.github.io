knitr::include_graphics("https://slcladal.github.io/images/uq1.jpg")

## # set options
## options(stringsAsFactors = F)         # no automatic data transformation
## options("scipen" = 100, "digits" = 4) # suppress math annotation
## # install packages
## install.packages("cluster")
## install.packages("factoextra")
## install.packages("cluster")
## install.packages("seriation")
## install.packages("pvclust")
## install.packages("ape")
## install.packages("vcd")
## install.packages("exact2x2")
## install.packages("factoextra")
## install.packages("seriation")
## install.packages("NbClust")
## install.packages("pvclust")
## install.packages("flextable")
## install.packages("tidyverse")
## install.packages("tibble")
## install.packages("gplots")
## # install klippy for copy-to-clipboard button in code chunks
## install.packages("remotes")
## remotes::install_github("rlesur/klippy")

# load packages
library(cluster)
library(factoextra)
library(cluster)
library(seriation)
library(pvclust)
library(ape)
library(vcd)
library(exact2x2)
library(factoextra)
library(seriation)
library(NbClust)
library(pvclust)
library(flextable)
library(tidyverse)
library(tibble)
library(gplots)
# activate klippy for copy-to-clipboard button
klippy::klippy()

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

students %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Student") %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Sample of five students and their grades in math, music, and biology.")  %>%
  flextable::border_outer()

diststudents <- dist(students, method = "manhattan") # create a distance matrix

diststudentstb <- matrix(c("1", "3", "3","3", "", "2", "4", "4","", "", "6", "6", "", "", "", "2"), nrow = 4, byrow = F)
# add column and row names
colnames(diststudentstb) <- c("StudentA", "StudentB", "StudentC", "StudentD")
rownames(diststudentstb) <- c("StudentB", "StudentC", "StudentD", "StudentE")
diststudentstb %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Student") %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Distance matrix based of students based on grades in math, music, and biology.")  %>%
  flextable::border_outer()

# create hierarchical cluster object with ward.D as linkage method
clusterstudents <- hclust(diststudents, method="ward.D")
# plot result as dendrogram
plot(clusterstudents, hang = 0)

## students2 <- matrix(c(1.5, 3, 2, 1,  2,  1, 2,  4,  4, 3,  4,  3),
##   nrow = 4, byrow = T)
## students2 <- as.data.frame(students2)
## rownames(students2) <- c("Cluster1", "StudentC", "StudentD", "StudentE")
## diststudents2 <- dist(students2, method = "manhattan")

diststudentstb <- matrix(c("2.5","3.5","3.5","","6.0","6.0","","","2.0"), 
                         nrow = 3, byrow = F)
# add column and row names
colnames(diststudentstb) <- c("Cluster 1", "Student C", "Student D")
rownames(diststudentstb) <- c("Student C", "Student D", "Student E")
diststudentstb %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Student") %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Distance matrix of students based on grades in math, music, and biology.")  %>%
  flextable::border_outer()

## students3 <- matrix(c(1.5,3,2,1,2,1,2.5,4,3.5),
##                     nrow = 3, byrow = T)
## students3 <- as.data.frame(students3)
## rownames(students3) <- c("Cluster1", "StudentC", "Cluster2")
## diststudents3 <- dist(students3,
##                       method = "manhattan")

diststudentstb <- matrix(c("2.5", "3.5", "", "6.0"), nrow = 2, byrow = F)
# add column and row names
colnames(diststudentstb) <- c("Cluster 1", "Student C")
rownames(diststudentstb) <- c("Student C", "Cluster 2")
diststudentstb %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Student") %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Distance matrix based of students based on grades in math, music, and biology.")  %>%
  flextable::border_outer()

par(mar=c(1,1,1,1))  # define margin width of the plot
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
   c("euclidean distance", "maximum distance", "manhatten distance"), col=c("red", "blue", "green"), horiz=F, cex = 1); par(mar=c(5.1,4.1,4.1,2.1))

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
rownames(clus) <- c("nae_neg", "like", "clefts", "tags", "youse", "soitwas", 
                    "dt", "nsr", "invartag", "wh_cleft")

 clus %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Feature") %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Frequencies of on-standard features across selected varieties of English.")  %>%
  flextable::border_outer()

# clean data
clusm <- as.matrix(clus)
clust <- t(clusm)            # transpose data
clust <- na.omit(clust)     # remove missing values
clusts <- scale(clust)      # standardize variables
clusts <- as.matrix(clusts) # convert into matrix

 clust %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variety") %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Scaled frequencies of on-standard features across selected varieties of English.")  %>%
  flextable::border_outer()

# apply get_clust_tendency to cluster object
clusttendency <- get_clust_tendency(clusts,    
                                    # define number of points from sample space
                                    n = 9,      
                   gradient = list(
                     # define color for low values
                     low = "steelblue",
                     # define color for high values
                     high = "white"))    
clusttendency[1]

clustd <- dist(clusts,                 # create distance matrix
               method = "euclidean")   # use euclidean (!) distance

 round(clustd, 2) %>%
  as.matrix() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variety") %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Distance matrix of scaled frequencies of non-standard features across selected varieties of English.")  %>%
  flextable::border_outer()

# create distance matrix (euclidean method: not good when dealing with many dimensions)
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
clustd_daisy <- round(daisy(clusts, metric = "euclidean"), 2) 

## clustd_maximum

clustd_maximum  %>%
  as.matrix() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variety") %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Distance matrix of selected varieties of English.")  %>%
  flextable::border_outer()

# create distance plot
dissplot(clustd) 

# create cluster object
cd <- hclust(clustd, method="ward.D2") 
# display dendrogram              
plot(cd, hang = -1)              

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
# calculate difference between celtic and other englishes
diff <- celtic.cm - others.cm
sort(diff, decreasing = F)

plot(sort(diff),           # y-values
  1:length(diff),       # x-values 
  type= "n",            # plot type (empty)
  cex.axis = .75,       # axis font size
  cex.lab = .75,        # label font size
  xlab ="Prototypical for Non-Celtic Varieties (Cluster 2) <-----> Prototypical for Celtic Varieties (Cluster 1)", # x-axis label
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
  xlab ="Prototypical for Inner Circle Varieties (Cluster 2) <-----> Prototypical for Outer Circle Varieties (Cluster 1)", # x-axis label
  yaxt = "n",           # no y-axis tick marks
  ylab = "")            # no y-axis label
text(sort(diff), 1:length(diff), names(sort(diff)), cex = .75) # plot text into plot

res.pv <- pvclust(clus,                     # apply pvclust method to clus data
                  method.dist="euclidean",  # use eucledian distance
                  method.hclust="ward.D2",  # use ward.d2 linkage
                  nboot = 100)              # use 100 bootstrap runs

plot(res.pv, cex = .75)
pvrect(res.pv)

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
rownames(clus) <- c("nae_neg", "like", "clefts", "tags", "youse", "soitwas", 
                    "dt", "nsr", "invartag", "wh_cleft")
# convert into factors
clus <- apply(clus, 1, function(x){
  x <- as.factor(x) })

clus  %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variety") %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Occurrence of non-standard features across selected varieties of English.")  %>%
  flextable::border_outer()

# clean data
clusts <- as.matrix(clus)
# create distance matrix
clustd <- dist(clusts, method = "binary")   # create a distance object with binary (!) distance

round(clustd, 2)  %>%
  as.matrix() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variety") %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Distance matrix of selected varieties of English.")  %>%
  flextable::border_outer()

# create cluster object (ward.D2 linkage)   : cluster in a way to achieve minimum variance
cd <- hclust(clustd, method="ward.D2")
# plot result as dendrogram
plot(cd, hang = -1)              # display dendogram


# create factor with celtic varieties on one hand and other varieties on other
cluster <- as.factor(ifelse(as.character(rownames(clusts)) == "IrishEnglish", "1",
  ifelse(as.character(rownames(clusts)) == "ScottishEnglish", "1", "0")))
# convert into data frame
clsts.df <- as.data.frame(clusts)
# determine significance
library(exact2x2)
pfish <- fisher.exact(table(cluster, clsts.df$youse))
pfish[[1]]
# determine effect size
assocstats(table(cluster, clsts.df$youse))

assocstats(table(cluster, clsts.df$like))

# load data
vsmdata  <- base::readRDS(url("https://slcladal.github.io/data/vsd.rda", "rb"))

vsmdata  %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Table showing amplification of English adjectives.")  %>%
  flextable::border_outer()

# simplify data
vsmdata_simp <- vsmdata %>%
  # remove non-amplifier adjectives
  dplyr::filter(Amplifier != 0,
         Adjective != "many",
         Adjective != "much") %>%
  # collapse infrequent amplifiers
  dplyr::group_by(Amplifier) %>%
  dplyr::mutate(AmpFreq = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Amplifier = ifelse(AmpFreq > 20, Amplifier, "other")) %>%
  # collapse infrequent adjectives
  dplyr::group_by(Adjective) %>%
  dplyr::mutate(AdjFreq = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Adjective = ifelse(AdjFreq > 10, Adjective, "other")) %>%
  dplyr::filter(Adjective != "other") %>%
  dplyr::select(-AmpFreq, -AdjFreq)

vsmdata_simp  %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Table showing amplification of English adjectives (simplified).")  %>%
  flextable::border_outer()

# 1. convert the data as a table
dt <- as.matrix(table(vsmdata_simp))
# 2. Graph
balloonplot(t(dt), main ="vsmdata_simp", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

chisq <- chisq.test(dt)
chisq

res.ca <- FactoMineR::CA(dt, graph = FALSE)
# inspect results of the CA
#print(res.ca)
eig.val <- get_eigenvalue(res.ca)
eig.val

# repel= TRUE to avoid text overlapping (slow if many point)
fviz_ca_biplot(res.ca, 
               repel = TRUE,
               col.row = "orange",
               col.col = "darkgray")

## # Principal Component Analysis
## 
## # inspect data
## data(iris)
## head(iris, 3)

## # log transform
## log.ir <- log(iris[, 1:4])
## ir.species <- iris[, 5]
## # apply PCA - scale. = TRUE is highly
## # advisable, but default is FALSE.
## ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)

## # print method
## print(ir.pca)

## # plot method
## plot(ir.pca, type = "l")

## # summary method
## summary(ir.pca)

## # predict PCs
## predict(ir.pca, newdata=tail(log.ir, 2))

## # load library
## library(devtools)
## # install library from github
## install_github("vqv/ggbiplot")
## # load installed library
## library(ggbiplot)
## # create plot
## g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1,
##               groups = ir.species, ellipse = TRUE,
##               circle = TRUE)
## g <- g + scale_color_discrete(name = '')
## g <- g + theme(legend.direction = 'horizontal',
##                legend.position = 'top')
## print(g)

## require(caret)
## trans = preProcess(iris[,1:4],
##                    method=c("BoxCox", "center",
##                             "scale", "pca"))
## PC = predict(trans, iris[,1:4])

## # inspect retained PCs
## head(PC, 3)
## 
## # inspect loadings
## trans$rotation

## # Multidimensional Scaling
## 
## 
## # Classical MDS
## # N rows (objects) x p columns (variables)
## # each row identified by a unique row name
## 
## d <- dist(clus) # Euclidean distances between the rows
## fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
## fit # view results
## 
## # plot solution
## x <- fit$points[,1]
## y <- fit$points[,2]
## plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
##   main="Metric MDS", type="n")
## text(x, y, labels = row.names(clus), cex=.7)

## # Nonmetric MDS
## # N rows (objects) x p columns (variables)
## # each row identified by a unique row name
## 
## library(MASS)
## d <- dist(clus) # Euclidean distances between the rows
## fit <- isoMDS(d, k=2) # k is the number of dim
## fit # view results
## 
## # plot solution
## x <- fit$points[,1]
## y <- fit$points[,2]
## plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
##   main="Nonmetric MDS", type="n")
## text(x, y, labels = row.names(clus), cex=.7)

sessionInfo()


# "Tree-Based Models"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("Boruta", "caret", "cowplot", "dplyr", 
                   "ggplot2", "Gmisc", "grid", "Hmisc", 
                   "knitr", "party", "partykit", "randomForest", 
                   "Rling"))
library(partykit)              # activate partykit library
library(dplyr)                 # activate dplyr library
options(stringsAsFactors = T)  # set options: do not convert strings
citdata <- read.delim("https://slcladal.github.io/data/treedata.txt", header = T, sep = "\t")
set.seed(111)        # set.seed
# apply bonferroni correction (1 minus alpha multiplied by n of predictors)
control = ctree_control(mincriterion = 1-(.05*ncol(citdata)-1))
# create initial conditional inference tree model
citd.ctree <- ctree(LikeUser ~ Age + Gender + Status,
                    data = citdata)
plot(citd.ctree, gp = gpar(fontsize = 8)) # plot final ctree
library(knitr)                 # activate library for tabulating
citdata <- read.delim("data/treedata.txt", header = T, sep = "\t")
attach(citdata)
kable(head(citdata), caption = "Example data set to illustrate conditional inference trees.")
library(partykit)              # activate partykit library
library(dplyr)                 # activate dplyr library
options(stringsAsFactors = T)  # set options: do not convert strings
set.seed(111)                  # set.seed
citdata <- read.delim("https://slcladal.github.io/data/treedata.txt", header = T, sep = "\t")
# tabulate data
table(citdata$LikeUser, citdata$Gender)
table(citdata$LikeUser, citdata$Age)
table(citdata$LikeUser, citdata$Status)
# Gini: men
gini_men <- 1-(42/(42+75))^2 - (75/(42+75))^2
# Gini: women
gini_women <- 1-(91/(91+43))^2 - (43/(91+43))^2
# Weighted Average of Gini: Gender
gini_gender <- 42/(42+75)* gini_men +  91/(91+43) * gini_women
gini_gender
gini_young <- 1-(92/(92+34))^2 - (34/(92+34))^2  # Gini: young
gini_old <- 1-(41/(41+84))^2 - (84/(41+84))^2    # Gini: old
# Weighted Average of Gini: Gender
gini_age <- 92/(92+34)* gini_young +  41/(41+84) * gini_old
gini_age
### Status
gini_high <- 1-(73/(33+73))^2 - (33/(33+73))^2   # Gini: high
gini_low <- 1-(60/(60+85))^2 - (85/(60+85))^2    # Gini: low
# Weighted Average of Gini: Status
gini_status <- 73/(33+73)* gini_high +  60/(60+85) * gini_low
gini_status
library(grid)
library(Gmisc)
grid.newpage()
# set some parameters to use repeatedly
leftx <- .25
midx <- .5
rightx <- .75
width <- .4
gp <- gpar(fill = "lightgrey")
# create boxes
(rando <- boxGrob("Age", 
 x=midx, y=.9, box_gp = gp, width = width))
# connect boxes like this
(g1 <- boxGrob("15-40\n NonLikeUsers  LikeUsers \n 34                  92", 
 x=leftx, y=.5, box_gp = gp, width = width))
(g2 <- boxGrob("41-80\n  NonLikeUsers  LikeUsers \n 84                 41", 
 x=rightx, y=.5, box_gp = gp, width = width))
connectGrob(rando, g1, "N")
connectGrob(rando, g2, "N")
### 2nd node: gender
young <- citdata[citdata$Age == "15-40",]
tbyounggender <- table(young$LikeUser, young$Gender)
tbyounggender
# Gini: men
gini_youngmen <- 1-(tbyounggender[2,2]/sum(tbyounggender[,2]))^2 - (tbyounggender[1,2]/sum(tbyounggender[,2]))^2
# Gini: women
gini_youngwomen <- 1-(tbyounggender[2,1]/sum(tbyounggender[,1]))^2 - (tbyounggender[1,1]/sum(tbyounggender[,1]))^2
# Weighted Average of Gini: Gender
gini_younggender <- sum(tbyounggender[,2])/sum(tbyounggender)* gini_youngmen +  sum(tbyounggender[,1])/sum(tbyounggender) * gini_youngwomen
gini_younggender
### 2nd node: status
young <- citdata[citdata$Age == "15-40",]
tbyoungstatus <- table(young$LikeUser, young$Status)
tbyoungstatus
# Gini: low
gini_younglow <- 1-(tbyoungstatus[2,2]/sum(tbyoungstatus[,2]))^2 - (tbyoungstatus[1,2]/sum(tbyoungstatus[,2]))^2
# Gini: high
gini_younghigh <- 1-(tbyoungstatus[2,1]/sum(tbyoungstatus[,1]))^2 - (tbyoungstatus[1,1]/sum(tbyoungstatus[,1]))^2
# Weighted Average of Gini: Gender
gini_youngstatus <- sum(tbyoungstatus[,2])/sum(tbyoungstatus)* gini_younglow +  sum(tbyoungstatus[,1])/sum(tbyoungstatus) * gini_younghigh
gini_youngstatus
### 3nd node: gender
younghigh <- citdata %>%
  filter(Age == "15-40") %>%
  filter(Status == "high")
tbyounghighgender <- table(younghigh$LikeUser, younghigh$Gender)
tbyounghighgender
# Gini: men
gini_younghighmen <- 1-(tbyounghighgender[2,2]/sum(tbyounghighgender[,2]))^2 - (tbyounghighgender[1,2]/sum(tbyounghighgender[,2]))^2
# Gini: women
gini_younghighwomen <- 1-(tbyounghighgender[2,1]/sum(tbyounghighgender[,1]))^2 - (tbyounghighgender[1,1]/sum(tbyounghighgender[,1]))^2
# Weighted Average of Gini: Gender
gini_younghighgender <- sum(tbyounghighgender[,2])/sum(tbyounghighgender)* gini_younghighmen +  sum(tbyounghighgender[,1])/sum(tbyounghighgender) * gini_younghighwomen
gini_younghighgender
# compare with younghigh
gini_younghigh
### 3rd node: gender
old <- citdata[citdata$Age == "41-80",]
tboldgender <- table(old$LikeUser, old$Gender)
tboldgender
# Gini: men
gini_oldmen <- 1-(tboldgender[2,2]/sum(tboldgender[,2]))^2 - (tboldgender[1,2]/sum(tboldgender[,2]))^2
# Gini: women
gini_oldwomen <- 1-(tboldgender[2,1]/sum(tboldgender[,1]))^2 - (tboldgender[2,1]/sum(tboldgender[,1]))^2
# Weighted Average of Gini: Gender
gini_oldgender <- sum(tboldgender[,2])/sum(tboldgender)* gini_oldmen +  sum(tboldgender[,1])/sum(tboldgender) * gini_oldwomen
gini_oldgender
### 3rd node: status
tboldstatus <- table(old$LikeUser, old$Status)
tboldstatus
# Gini: low
gini_oldlow <- 1-(tboldstatus[2,2]/sum(tboldstatus[,2]))^2 - (tboldstatus[2,1]/sum(tboldstatus[,2]))^2
# Gini: high
gini_oldhigh <- 1-(tboldstatus[2,1]/sum(tboldstatus[,1]))^2 - (tboldstatus[2,1]/sum(tboldstatus[,1]))^2
# Weighted Average of Gini: Gender
gini_oldstatus <- sum(tboldstatus[,2])/sum(tboldstatus)* gini_oldlow +  sum(tboldstatus[,1])/sum(tboldstatus) * gini_oldhigh
gini_oldstatus
# compare with gini gender
gini_oldgender
### 3nd node: gender
oldfemale <- citdata %>%
  filter(Age == "41-80") %>%
  filter(Gender == "female")
tboldfemalestatus <- table(oldfemale$LikeUser, oldfemale$Status)
tboldfemalestatus
# Gini: low
gini_oldfemalelow <- 1-(tboldfemalestatus[2,2]/sum(tboldfemalestatus[,2]))^2 - (tboldfemalestatus[1,2]/sum(tboldfemalestatus[,2]))^2
# Gini: high
gini_oldfemalehigh <- 1-(tboldfemalestatus[2,1]/sum(tboldfemalestatus[,1]))^2 - (tboldfemalestatus[1,1]/sum(tboldfemalestatus[,1]))^2
# Weighted Average of Gini: Status
gini_oldgenderstatus <- sum(tboldfemalestatus[,2])/sum(tboldfemalestatus)* gini_oldfemalelow +  sum(tboldfemalestatus[,1])/sum(tboldfemalestatus) * gini_oldfemalehigh
gini_oldgenderstatus   # gini of status for old plus female
gini_oldgender         # compare with younghigh
#install.packages(Rling)       # install Rling library (remove # to activate)
library(Rling)                 # activate Rling library
library(partykit)              # activate partykit library
library(dplyr)                 # activate dplyr library
options(stringsAsFactors = T)  # set options: do not convert strings
options(scipen = 999)          # set options: supress math. notation
options(max.prAmplified=10000) # set options
# load data
citdata <- read.delim("data/treedata.txt", header = T, sep = "\t")
head(citdata)
set.seed(111)        # set.seed
# apply bonferroni correction (1 minus alpha multiplied by n of predictors)
control = ctree_control(mincriterion = 1-(.05*ncol(citdata)-1))
# create initial conditional inference tree model
citd.ctree <- ctree(LikeUser ~ Age + Gender + Status,
                    data = citdata)
plot(citd.ctree, gp = gpar(fontsize = 8)) # plot final ctree
# test prediction accuracy
ptb <- table(predict(citd.ctree), citdata$LikeUser)
(((ptb[1]+ptb[4])+(ptb[2]+ptb[3]))/sum(table(predict(citd.ctree), citdata$LikeUser)))*100
# determine baseline
(table(citdata$LikeUser)[[2]]/sum(table(citdata$LikeUser)))*100
library(knitr)                 # activate library for tabulating
citdata <- read.delim("data/numerictreedata.txt", header = T, sep = "\t")
kable(citdata, caption = "Example data set to illustrate numeric splits in conditional inference trees.")
library(knitr)                 # activate library for tabulating
library(dplyr)                 # activate library for tabulating
citdata2 <- read.delim("data/numerictreedata.txt", header = T, sep = "\t")
citdata2 <- citdata2 %>%
  arrange(Age)
kable(citdata2, caption = "Ordered data set to illustrate numeric splits in conditional inference trees.")
library(knitr)                 # activate library for tabulating
library(dplyr)                 # activate library for tabulating
Age <- c(15, ((15+22)/2), 22, ((22+27)/2), 27, ((27+37)/2), 37, ((37+42)/2), 42, ((42+63)/2), 63)
LikeUser <- c("yes", "", "yes", "", "yes", "", "no", "", "yes", "", "no")
citdata3 <- data.frame(Age, LikeUser)
kable(citdata3, caption = "Ordered data set with mean age levels to illustrate numeric splits in conditional inference trees.")
# 18.5
1-(1/(1+0))^2 - (0/(1+0))^2
1-(2/(2+3))^2 - (3/(2+3))^2
1/6 * 0.0 +  5/6 * 0.48
# 24.4
1-(2/(2+0))^2 - (0/(2+0))^2
1-(3/(3+1))^2 - (2/(3+1))^2
2/6 * 0.0 +  4/6 * 0.1875
# 32
1-(3/(3+0))^2 - (0/(3+0))^2
1-(1/(1+2))^2 - (2/(1+2))^2
3/6 * 0.0 +  3/6 * 0.4444444
# 39.5
1-(3/(3+1))^2 - (1/(3+1))^2
1-(1/(1+1))^2 - (1/(1+1))^2
4/6 * 0.375 +  2/6 * 0.5
# 52.5
1-(4/(4+1))^2 - (1/(4+1))^2
1-(0/(0+1))^2 - (1/(0+1))^2
5/6 * 0.32 +  1/6 * 0.0
library(knitr)                 # activate library for tabulating
library(dplyr)                 # activate library for tabulating
AgeSplit <- c(((15+22)/2), ((22+27)/2), ((27+37)/2), ((37+42)/2), ((42+63)/2))
Gini <- c(0.4, 0.5,0.444, 0.41, 0.267)
citdata3 <- data.frame(AgeSplit, Gini)
kable(citdata3, caption = "Possible age splits and their associated Gini values.")
library(knitr)               # activate knitr library
library(dplyr)               # activate dplyr library
citdata <- read.delim("data/treedata.txt", header = T, sep = "\t")
citdata <- head(citdata)
citdata$ID <- 1:nrow(citdata)
citdata <- citdata %>%
  dplyr::select(ID, Age, Gender, Status, LikeUser)
kable(citdata, caption = "Example data set to illustrate Random Forests.")
citdata <- citdata[c(6, 3, 4, 1, 2, 2),]
rownames(citdata) <- NULL
kable(citdata, caption = "Bootstrapped data set to illustrate Random Forests.")
library(ggplot2)       # activate ggplot2 library for plotting
library(cowplot)       # activate cowplot library for modifying ggplot2
library(randomForest)  # activate randomForest library for random forests
# define path to data
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
# load data 
data <- read.csv(url, header=FALSE)
# inspect data
head(data) 
colnames(data) <- c(
  "age",
  "sex",# 0 = female, 1 = male
  "cp", # chest pain
          # 1 = typical angina,
          # 2 = atypical angina,
          # 3 = non-anginal pain,
          # 4 = asymptomatic
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar greater than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results
          # 1 = normal
          # 2 = having ST-T wave abnormality
          # 3 = showing probable or definite left ventricular hypertrophy
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment
          # 1 = upsloping
          # 2 = flat
          # 3 = downsloping
  "ca", # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan
          # 3 = normal (no cold spots)
          # 6 = fixed defect (cold spots during rest and exercise)
          # 7 = reversible defect (when cold spots only appear during exercise)
  "hd" # (the predicted attribute) - diagnosis of heart disease
          # 0 if less than or equal to 50% diameter narrowing
          # 1 if greater than 50% diameter narrowing
  )
head(data) # now we have data and column names
str(data) 
data[data == "?"] <- NA
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$ca <- as.integer(data$ca) 
data$ca <- as.factor(data$ca)  # ...then convert the integers to factor levels
data$thal <- as.integer(data$thal) # "thal" also had "?"s in it.
data$thal <- as.factor(data$thal)
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd) # Now convert to a factor
str(data) 
set.seed(42)
## impute any missing values in the training set using proximities
data.imputed <- rfImpute(hd ~ ., data = data, iter=6)
model <- randomForest(hd ~ ., data=data.imputed, proximity=TRUE)
model 
library(knitr)                 # activate library for tabulating
confusionmatrixtb <- matrix(c("", "Healthy", "Unhealthy", "Healthy", "Number of healthy people correctly called healthy by the forest.", "Number of healthy people incorectly called unhealthy by the forest", "Unhealthy", "Number of unhealthy people incorrectly called healthy by the forest", "Number of unhealthy people correctly called unhealthy by the forest"), ncol = 3, byrow = T)
kable(head(confusionmatrixtb), caption = "Confusion matrix for the random forest for Heart Disease data.")
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
    model$err.rate[,"Healthy"],
    model$err.rate[,"Unhealthy"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
# ggsave("oob_error_rate_500_trees.pdf")
model <- randomForest(hd ~ ., data=data.imputed, ntree=1000, proximity=TRUE)
model
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
    model$err.rate[,"Healthy"],
    model$err.rate[,"Unhealthy"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
# ggsave("oob_error_rate_1000_trees.pdf")
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(hd ~ ., data=data.imputed, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
distance.matrix <- dist(1-model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)
## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
  X=mds.values[,1],
  Y=mds.values[,2],
  Status=data.imputed$hd)
ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) +
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")
varImpPlot(model, main = "", pch = 20) 
library(knitr)         # activate knitr library
library(dplyr)         # activate dplyr library
library(randomForest)  # activate randomForest library
rfd <- read.delim("data/treedata.txt", header = T, sep = "\t")
set.seed(222)                       # set seed
id <- sample(2, nrow(rfd), replace = T, prob = c(.7, .3))
train <- rfd[id == 1, ]
test <- rfd[id == 2,]
like_rf1 <- randomForest(LikeUser~., data = train)
print(like_rf1) # inspect model
attributes(like_rf1)
# install package
#source("https://bioconductor.org/biocLite.R"); biocLite(); library(Biobase)
#install.packages("Biobase", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com", 
#                                     "http://cran.rstudio.com/", dependencies=TRUE))
#install.packages("dimRed", dependencies = TRUE)
#install.packages('caret', dependencies = TRUE)
# load caret library
library(caret) # because initially caret did not work, the libraries above had to be installed
ptrain1 <- predict(like_rf1, train) # extract prediction for training data
head(ptrain1); head(train$LikeUser)         # inspect predictions
confusionMatrix(ptrain1, train$LikeUser)  
ptest1 <- predict(like_rf1, test)
confusionMatrix(ptest1, test$LikeUser)
plot(like_rf1, main = "")
like_rf2 <- tuneRF(train[, !colnames(train)== "LikeUser"], 
                        train[, colnames(train)== "LikeUser"], 
                        stepFactor = 3, # for most values 3 appears to be optimal
                        plot = T,
                        ntreeTry = 200,
                        trace = T,
                        improve = .05
)
like_rf2 <- randomForest(LikeUser~., data = train, 
                              ntree = 200,
                              ntry = 6,
                              importance= T,
                              proximity = T)
print(like_rf2)   # inspect model
ptrain2 <- predict(like_rf2, train)
confusionMatrix(ptrain2, train$LikeUser)
ptest2 <- predict(like_rf2, test)
confusionMatrix(ptest2, test$LikeUser)
hist(treesize(like_rf2), main = "", col = "lightgray")
varImpPlot(like_rf2, main = "", pch = 20) 
importance(like_rf2)
varUsed(like_rf2)
partialPlot(like_rf2, train, Age)
getTree(like_rf2, 1, labelVar = T)
MDSplot(like_rf2, test$LikeUser)
# detach partykit
#detach("package:partykit", unload=TRUE)
# load package party
library(party)
# set seed
set.seed(333)
# create initial model
like.rf <- cforest(LikeUser ~ Age + Gender + Status,
                        data = rfd, controls = cforest_unbiased(ntree = 50, mtry = 3))
# determine importance of factors
like.varimp <- varimp(like.rf, conditional = T)
round(like.varimp, 3)
# plot result
dotchart(sort(like.varimp), pch = 20, main = "Conditional importance of variables")
# load library
library(Hmisc)
# evaluate random forst
like.rf.pred <- unlist(treeresponse(like.rf))[c(FALSE,TRUE)]
somers2(like.rf.pred, as.numeric(rfd$LikeUser) - 1)
# load library
library(party)
cf1 <- cforest(LikeUser ~ . , data= rfd, control=cforest_unbiased(mtry=2,ntree=100)) # fit the random forest
varimp(cf1) # get variable importance, based on mean decrease in accuracy
# conditional=True, adjusts for correlations between predict
varimp(cf1, conditional=TRUE) 
varimpAUC(cf1)  # more robust towards class imbalance.
par(mar = c(5, 8, 4, 2) + 0.1)
plot(y = 1:length(varimpAUC(cf1)), x = varimpAUC(cf1)[order(varimpAUC(cf1))], 
     axes = F, ann = F, pch = 20, xlim = c(-0.01, 0.2), main = "Predictor Importance")
axis(1, at = seq(-0.01, 0.2, 0.05), seq(-0.01, 0.2, 0.05))
axis(2, at = 1:length(varimpAUC(cf1)), names(varimpAUC(cf1))[order(varimpAUC(cf1))], las = 2)
grid()
box()
par(mar = c(5, 4, 4, 2) + 0.1)
#install.packages(Boruta)       # install Boruta library (remove # to activate)
library(Boruta)                # activate Boruta library
options(stringsAsFactors = T)  # set options: do not convert strings
options(scipen = 999)          # set options: supress math. notation
options(max.prAmplified=10000) # set options
# load data
borutadata <- read.delim("data/treedata.txt", header = T, sep = "\t")
head(borutadata)
# initial run
boruta1 <- Boruta(LikeUser~.,data=borutadata)
print(boruta1)
getConfirmedFormula(boruta1)
plot(boruta1, cex = .5)
plotImpHistory(boruta1)
getConfirmedFormula(boruta1)
par(mar = c(8, 8, 4, 2) + 0.1)
plot(boruta1, cex.axis=.75, las=2, xlab="", ylab = "", cex = .75, 
     col = c("grey50", "grey50", "grey50","grey90","grey90","grey90"))
abline(v = 3.5, lty = "dashed")
mtext("Predictors", 1, line = 6, at = 5, cex = 1)
mtext("Control", 1, line = 6, at = 2, cex = 1)
mtext("Importance", 2, line = 2.5, at = 20, cex = 1, las = 0)
par(mar = c(5, 4, 4, 2) + 0.1)
# References

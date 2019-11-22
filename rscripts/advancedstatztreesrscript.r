
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
citdata <- read.delim("https://raw.githubusercontent.com/MartinSchweinberger/coedlss2019materials/master/datatables/treedata.txt", header = T, sep = "\t")
set.seed(111)        # set.seed
# apply bonferroni correction (1 minus alpha multiplied by n of predictors)
control = ctree_control(mincriterion = 1-(.05*ncol(citdata)-1))
# create initial conditional inference tree model
citd.ctree <- ctree(LikeUser ~ Age + Gender + Status,
                    data = citdata)
plot(citd.ctree, gp = gpar(fontsize = 8)) # plot final ctree
library(knitr)                 # activate library for tabulating
citdata <- read.delim("https://raw.githubusercontent.com/MartinSchweinberger/coedlss2019materials/master/datatables/treedata.txt", header = T, sep = "\t")
attach(citdata)
kable(head(citdata), caption = "Example data set to illustrate conditional inference trees.")
# activate libraries
library(partykit)              
library(dplyr)  
library(grid)
library(Gmisc) 
library(Rling) 
library(ggplot2)       
library(cowplot)       
library(randomForest)
library(party)
library(Hmisc)
library(Boruta) 
library(RCurl)
# to install the caret library, it was necessary to go through the installation 
# process below - once caret is installed once, you do not need to go through 
# these steps again
# install caret library
#source("https://bioconductor.org/biocLite.R"); biocLite(); library(Biobase)
#install.packages("Biobase", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com", 
#                                     "http://cran.rstudio.com/", dependencies=TRUE))
#install.packages("dimRed", dependencies = TRUE)
#install.packages('caret', dependencies = TRUE)
# activate caret library
library(caret) 
# set options
options(stringsAsFactors = F)
options(scipen = 999)
options(max.print=10000)
# load data
citdata <- read.delim("https://raw.githubusercontent.com/MartinSchweinberger/coedlss2019materials/master/datatables/treedata.txt", header = T, sep = "\t")
# inspect data
head(citdata); str(citdata)
# factorize variables (cit require factors instead of character vectors)
fcts <- c("Age", "Gender", "Status", "LikeUser")
citdata[fcts] <- lapply(citdata[fcts], factor)
# inspect data
str(citdata)
# tabulate data
table(citdata$LikeUser, citdata$Gender)
table(citdata$LikeUser, citdata$Age)
table(citdata$LikeUser, citdata$Status)
# GENDER
# re-inspect gender distribution
tblikegender <- table(citdata$LikeUser, citdata$Gender)
tblikegender
chisq.test(tblikegender) # sig difference: data can be split!
# calculate Gini for men
gini_men <- 1-(42/(42+75))^2 - (75/(42+75))^2
# calculate Gini for women
gini_women <- 1-(91/(91+43))^2 - (43/(91+43))^2
# calculate weighted average of Gini for Gender
gini_gender <- 42/(42+75)* gini_men +  91/(91+43) * gini_women
gini_gender
# re-inspect age distribution
tblikeage <- table(citdata$LikeUser, citdata$Age)
tblikeage
chisq.test(tblikeage) # sig difference: data can be split!
# calculate Gini for age groups
gini_young <- 1-(92/(92+34))^2 - (34/(92+34))^2  # Gini: young
gini_old <- 1-(41/(41+84))^2 - (84/(41+84))^2    # Gini: old
# calculate weighted average of Gini for Age
gini_age <- 92/(92+34)* gini_young +  41/(41+84) * gini_old
gini_age
# re-inspect status distribution
tblikestatus <- table(citdata$LikeUser, citdata$Status)
tblikestatus
chisq.test(tblikestatus) # sig difference: data can be split!
gini_high <- 1-(73/(33+73))^2 - (33/(33+73))^2   # Gini: high
gini_low <- 1-(60/(60+85))^2 - (85/(60+85))^2    # Gini: low
# calculate weighted average of Gini for Status
gini_status <- 73/(33+73)* gini_high +  60/(60+85) * gini_low
gini_status
# compare age, gender, and status ginis
gini_age; gini_gender; gini_status
# plot tree we have so far!
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
(g1 <- boxGrob("41-80\n  NonLikeUsers  LikeUsers \n 84                 41", 
               x=leftx, y=.5, box_gp = gp, width = width))
(g2 <- boxGrob("15-40\n NonLikeUsers  LikeUsers \n 34                  92", 
               x=rightx, y=.5, box_gp = gp, width = width))
connectGrob(rando, g1, "N")
connectGrob(rando, g2, "N")
# 2ND NODE
# split data according to first split (only old data for now)
old <- citdata[citdata$Age == "41-80",]
# inspect distribution
tboldgender <- table(old$LikeUser, old$Gender)
tboldgender
chisq.test(tboldgender) # sig difference: data can be split!
# calculate Gini for Gender
# calculate Gini for men
gini_oldmen <- 1-(tboldgender[2,2]/sum(tboldgender[,2]))^2 - (tboldgender[1,2]/sum(tboldgender[,2]))^2
# calculate Gini for women
gini_oldwomen <- 1-(tboldgender[2,1]/sum(tboldgender[,1]))^2 - (tboldgender[1,1]/sum(tboldgender[,1]))^2
# # calculate weighted aAverage of Gini for Gender
gini_oldgender <- sum(tboldgender[,2])/sum(tboldgender)* gini_oldmen +  sum(tboldgender[,1])/sum(tboldgender) * gini_oldwomen
gini_oldgender
# calculate Gini for Status
# inspect distribution
tboldstatus <- table(old$LikeUser, old$Status)
tboldstatus
chisq.test(tboldstatus) # sig difference: data can be split!
# 3RD NODE
# split data according to first split (only old data for now)
oldmale <- citdata %>%
  dplyr::filter(Age == "41-80") %>%
  dplyr::filter(Gender == "male")
# inspect distribution
tboldmalestatus <- table(oldmale$LikeUser, oldmale$Status)
tboldmalestatus
chisq.test(tboldmalestatus) # no sig difference: no more splits!
# 4TH NODE
# split data according to first split (only old data for now)
oldfemale <- citdata %>%
  dplyr::filter(Age == "41-80") %>%
  dplyr::filter(Gender == "female")
# inspect distribution
tboldfemalestatus <- table(oldfemale$LikeUser, oldfemale$Status)
tboldfemalestatus
chisq.test(tboldfemalestatus) # no sig difference: no more splits!
# 5TH NODE
# split data according to first split (only young data)
young <- citdata[citdata$Age == "15-40",]
# inspect distribution
tbyounggender <- table(young$LikeUser, young$Gender)
tbyounggender
chisq.test(tbyounggender) # no sig difference: do not split!
# calculate Gini for Status
# inspect distribution
tbyoungstatus <- table(young$LikeUser, young$Status)
tbyoungstatus
chisq.test(tbyoungstatus) # sig difference: split!
# 6TH NODE
# split data according to first and second split (young and low status data)
younglow <- citdata %>%
  filter(Age == "15-40") %>%
  filter(Status == "low")
# inspect gender distribution
tbyounglowgender <- table(younglow$LikeUser, younglow$Gender)
tbyounglowgender
chisq.test(tbyounglowgender) # no sig difference: no more splits!
# 7TH node
# split data according to first and second split (young and high-status data)
younghigh <- citdata %>%
  filter(Age == "15-40") %>%
  filter(Status == "high")
# inspect gender distribution
tbyounghighgender <- table(younghigh$LikeUser, younghigh$Gender)
tbyounghighgender
chisq.test(tbyounghighgender) # no sig difference: no more splits!
# set.seed (to store random numbers and thus make results reproducible)
set.seed(2019120202) 
# apply bonferroni correction (1 minus alpha multiplied by n of predictors)
control = ctree_control(mincriterion = 1-(.05*(ncol(citdata)-1)))
# create initial conditional inference tree model
citd.ctree <- ctree(LikeUser ~ Age + Gender + Status, data = citdata)
plot(citd.ctree, gp = gpar(fontsize = 8)) # plot final ctree
citprediction <- predict(citd.ctree)
confusionMatrix(citprediction, citdata$LikeUser)
library(knitr)                 # activate library for tabulating
citdata2 <- read.delim("https://slcladal.github.io/data/numerictreedata.txt", header = T, sep = "\t")
kable(citdata2, caption = "Example data set to illustrate numeric splits in conditional inference trees.")
citdata2 <- citdata2 %>%
  arrange(Age)
kable(citdata2, caption = "Example data set to illustrate numeric splits in conditional inference trees.")
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
AgeSplit <- c(((15+22)/2), ((22+27)/2), ((27+37)/2), ((37+42)/2), ((42+63)/2))
Gini <- c(0.4, 0.5,0.444, 0.41, 0.267)
citdata3 <- data.frame(AgeSplit, Gini)
kable(citdata3, caption = "Possible age splits and their associated Gini values.")
citdata <- read.delim("data/treedata.txt", header = T, sep = "\t")
citdata <- head(citdata)
citdata$ID <- 1:nrow(citdata)
citdata <- citdata %>%
  dplyr::select(ID, Age, Gender, Status, LikeUser)
kable(citdata, caption = "Example data set to illustrate Random Forests.")
citdata <- citdata[c(6, 3, 4, 1, 2, 2),]
rownames(citdata) <- NULL
kable(citdata, caption = "Bootstrapped data set to illustrate Random Forests.")
# load random forest data
rfdata <- read.delim("https://raw.githubusercontent.com/MartinSchweinberger/coedlss2019materials/master/datatables/rfdata.txt", header = T, sep = "\t")
# inspect data
head(rfdata); str(rfdata)
# factorize variables (rf require factors instead of character vectors)
fcts <- c("Gender", "Age", "ConversationType", "Priming", "SUFLike")
rfdata[fcts] <- lapply(rfdata[fcts], factor)
# inspect data
str(rfdata)
# check for NAs
natest <- rfdata %>%
  na.omit()
nrow(natest) # no NAs present in data (same number of rows with NAs omitted)
# replacing NAs with estimates
data.imputed <- rfImpute(SUFLike ~ ., data = rfdata, iter=6)
# set.seed
set.seed(2019120204)
# create initial model
rfmodel1 <- cforest(SUFLike ~ .,  data = rfdata, 
                    controls = cforest_unbiased(ntree = 50, mtry = 3))
# evaluate random forest (model diagnostics)
rfmodel1_pred <- unlist(treeresponse(rfmodel1))[c(FALSE,TRUE)]
somers2(rfmodel1_pred, as.numeric(rfdata$SUFLike) - 1)
# extract variable importance based on mean decrease in accuracy
rfmodel1_varimp <- varimp(rfmodel1, conditional = T) 
# show variable importance
rfmodel1_varimp
# extract more robust variable importance 
rfmodel1_robustvarimp <- varimpAUC(rfmodel1)  
# plot result
dotchart(sort(rfmodel1_robustvarimp), pch = 20, main = "Conditional importance of variables")
# set.seed
set.seed(2019120205)
rfmodel2 <- randomForest(SUFLike ~ ., data=rfdata, proximity=TRUE)
# inspect model
rfmodel2 
# lower number of trees makes the computation much quicker
oob.error.data <- data.frame(
  Trees=rep(1:nrow(rfmodel2$err.rate), times=3),
  Type=rep(c("OOB", "no", "yes"), each=nrow(rfmodel2$err.rate)),
  Error=c(rfmodel2$err.rate[,"OOB"],
          rfmodel2$err.rate[,"no"],
          rfmodel2$err.rate[,"yes"]))
# plot error rates
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type)) 
# check what mtry is optimal
tuneRF(rfdata[, !colnames(rfdata)== "SUFLike"],
       rfdata[, colnames(rfdata)== "SUFLike"],
       stepFactor = 2, # 2 has the lowest value to be optimal
       plot = T, ntreeTry = 50, trace = T, improve = .05)
# set.seed (to store random numbers and thus make results reporducible)
set.seed(2019120206)
# create a new model with fewer trees and that takes 2 variables at a time
rfmodel3 <- randomForest(SUFLike ~ ., data=rfdata, ntree=30, mtry = 4, proximity=TRUE)
# inspect model
rfmodel3
# save what the model predicted in a new variable
rfdata$Prediction <- predict(rfmodel3, rfdata) 
# create confusion matrix to check accuracy
confusionMatrix(rfdata$Prediction, rfdata$SUFLike)  
# plot variable importance
varImpPlot(rfmodel3, main = "", pch = 20) 
# extract importance of individual variables
partialPlot(rfmodel3, rfdata, Age)
# load data
borutadata <- read.delim("https://raw.githubusercontent.com/MartinSchweinberger/coedlss2019materials/master/datatables/borutadata.txt", header = T, sep = "\t")
# inspect Boruta data
str(borutadata)
# factorize variables (boruta - like rf - require factors instead of character vectors)
fcts <- c("Age", "Adjective", "Function", "Priming", "Gender", "Occupation", 
          "ConversationType", "AudienceSize", "really", "Gradabilty", "SemanticCategory")
borutadata[fcts] <- lapply(borutadata[fcts], factor)
# inspect data
str(borutadata)
# set.seed 
set.seed(2019120207)
# initial run
boruta1 <- Boruta(really~.,data=borutadata)
print(boruta1)
# extract decision
getConfirmedFormula(boruta1)
plotImpHistory(boruta1)
# remove irrelevant variables
rejected <- names(boruta1$finalDecision)[which(boruta1$finalDecision == "Rejected")]
# update data for boruta
borutadata <- borutadata %>%
  dplyr::select(-rejected)
# set.seed (to store random numbers and thus make results reporducible)
set.seed(2019120208)
# 2nd run
boruta2 <- Boruta(really~.,data=borutadata)
print(boruta2)
# extract decision
getConfirmedFormula(boruta2)
par(mar = c(8, 8, 4, 2) + 0.1)
plot(boruta2, cex.axis=.75, las=2, xlab="", ylab = "", cex = .75, 
     col = c(rep("grey50", 7),rep("grey90", 3)))
abline(v = 3.5, lty = "dashed")
mtext("Predictors", 1, line = 7, at = 7, cex = 1)
mtext("Control", 1, line = 7, at = 2, cex = 1)
mtext("Importance", 2, line = 2.5, at = 2.5, cex = 1, las = 0)
par(mar = c(5, 4, 4, 2) + 0.1)
Of the remaining variables, adjective frequency and adjective type have the strongest effect and are confirmed as being important while syntactic function fails to perform better than the best shadow variable. All other variables have only a marginal effect on the use of really as an adjective amplifier.
# References

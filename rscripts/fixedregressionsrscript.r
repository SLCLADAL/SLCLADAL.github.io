
# "Fixed-Effects Regression"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("boot", "car", "caret", "effects", "foreign", "ggplot2", 
                   "Hmisc", "knitr", "MASS", "mlogit", "msm", "QuantPsyc", 
                   "reshape2", "rms", "sandwich", "sfsmisc", "sjPlot", 
                  "stringr",  "vcd", "visreg"))
# load libraries
library(boot)
library(car)
library(caret)
library(effects)
library(foreign)
library(ggplot2)
library(Hmisc)
library(knitr)
library(MASS)
library(mlogit)
library(msm)
library(plyr)
library(QuantPsyc)
library(reshape2)
library(rms)
library(sandwich)
library(sfsmisc)
library(sjPlot)
library(stringr)
library(vcd)
library(visreg)
# load functions
source("https://slcladal.github.io/rscripts/blrsummary.r")
source("https://slcladal.github.io/rscripts/multiplot.r")
source("https://slcladal.github.io/rscripts/mlinrsummary.r")
source("https://slcladal.github.io/rscripts/SampleSizeMLR.r")
source("https://slcladal.github.io/rscripts/ExpR.r")
# load data
mlrdata <- read.delim("https://slcladal.github.io/data/mlrdata.txt", header = TRUE)
head(mlrdata); str(mlrdata); summary(mlrdata) # inspect data
# create plots
p1 <- ggplot(mlrdata, aes(status, money)) +   # data + x/y-axes
  geom_boxplot(fill=c("grey30", "grey70")) + # def. col.
  theme_set(theme_bw(base_size = 8))+   # black and white theme
  labs(x = "") +                        # x-axis label
  labs(y = "Money spent on present (AUD)", cex = .75) +   # y-axis label
  coord_cartesian(ylim = c(0, 250)) +   # y-axis range
  guides(fill = FALSE) +                # no legend
  ggtitle("Status")                     # title
# plot 2
p2 <- ggplot(mlrdata, aes(attraction, money)) +
  geom_boxplot(fill=c("grey30", "grey70")) +
  theme_set(theme_bw(base_size = 8))+
  labs(x = "") +                              # x-axis label
  labs(y = "Money spent on present (AUD)") +  # y-axis label
  coord_cartesian(ylim = c(0, 250)) +
  guides(fill = FALSE) +
  ggtitle("Attraction")
# plot 3
p3 <- ggplot(mlrdata, aes(x = money)) +
  geom_histogram(aes(y=..density..),    # add density statistic
                 binwidth = 10,         # def. bin width
                 colour = "black",      # def. bar edge colour
                 fill = "white") +      # def. bar col.
    theme_bw() +                        # black-white theme
  geom_density(alpha=.2, fill = "gray50") + # def. col. of overlay
    labs(x = "Money spent on present (AUD)") +
  labs(y = "Density of frequency")
# plot 4
p4 <- ggplot(mlrdata, aes(status, money)) +
  geom_boxplot(notch = F, aes(fill = factor(status))) + # create boxplot
  scale_fill_manual(values = c("grey30", "grey70")) +   # def. col. palette
  facet_wrap(~ attraction, nrow = 1) +  # separate panels for attraction
  theme_set(theme_bw(base_size = 8)) +
  labs(x = "") +
  labs(y = "Money spent on present (AUD)") +
  coord_cartesian(ylim = c(0, 250)) +
  guides(fill = FALSE)
# show plots
multiplot(p1, p3, p2, p4, cols = 2)
m1.mlr = lm(                      # generate lm regression object
  money ~ 1 + attraction*status,  # def. rgression formula (1 = intercept)
  data = mlrdata)                 # def. data
m1.glm = glm(                     # generate glm regression object
  money ~ 1 + attraction*status,  # def. rgression formula (1 = intercept)
  family = gaussian,              # def. linkage function
  data = mlrdata)                 # def. data
# automated AIC based model fitting
step(m1.mlr, direction = "both")
m2.mlr = lm(                       # generate lm regression object
  money ~ (status + attraction)^2, # def. regression formula
  data = mlrdata)                  # def. data
m2.glm = glm(                      # generate glm regression object
  money ~ (status + attraction)^2, # def. regression formula
  family = gaussian,               # def. linkage function
  data = mlrdata)                  # def. data
# inspect final minimal model
summary(m2.mlr)
#intercept  Single  NotInterested  Single:NotInterested
99.15     + 57.69  + 0           + 0     # 156.8 single + interested
99.15     + 57.69  - 47.66       - 63.18 # 46.00 single + not interested
99.15     - 0      + 0           - 0     # 99.15 relationship + interested
99.15     - 0      - 47.66       - 0     # 51.49 relationship + not interested
# make prediction based on the model for original data
prediction <- predict(m2.mlr, newdata = mlrdata)
# inspect predictions
table(round(prediction,2))
# extract confidence intervals of the coefficients
confint(m2.mlr)
# create and compare baseline- and minimal adequate model
m0.mlr <- lm(money ~1, data = mlrdata)
anova(m0.mlr, m2.mlr)
# compare baseline- and minimal adequate model
Anova(m0.mlr, m2.mlr, type = "III")
# start plotting
par(mfrow = c(2, 2)) # display plots in 3 rows/2 columns
plot(m2.mlr); par(mfrow = c(1, 1)) # cerate plots and restore original settings
# determine a cutoff for data points that have D-values higher than 4/(n-k-1)
cutoff <- 4/((nrow(mlrdata)-length(m2.mlr$coefficients)-2))
# start plotting
par(mfrow = c(1, 2))           # display plots in 3 rows/2 columns
qqPlot(m2.mlr, main="QQ Plot") # create qq-plot
plot(m2.mlr, which=4, cook.levels = cutoff); par(mfrow = c(1, 1))
# extract influence statistics
infl <- influence.measures(m2.mlr)
# add infl. statistics to data
mlrdata <- data.frame(mlrdata, infl[[1]], infl[[2]])
# annotate too influential data points
remove <- apply(infl$is.inf, 1, function(x) {
  ifelse(x == TRUE, return("remove"), return("keep")) } )
# add annotation to data
mlrdata <- data.frame(mlrdata, remove)
# number of rows before removing outliers
nrow(mlrdata)
# remove outliers
mlrdata <- mlrdata[mlrdata$remove == "keep", ]
# number of rows after removing outliers
nrow(mlrdata)
# recreate regression models on new data
m0.mlr = lm(money ~ 1, data = mlrdata)
m0.glm = glm(money ~ 1, family = gaussian, data = mlrdata)
m1.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m1.glm = glm(money ~ status * attraction, family = gaussian,
             data = mlrdata)
# automated AIC based model fitting
step(m1.mlr, direction = "both")
# create new final models
m2.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m2.glm = glm(money ~ status * attraction, family = gaussian,
             data = mlrdata)
# inspect final minimal model
summary(m2.mlr)
# extract confidence intervals of the coefficients
confint(m2.mlr)
# compare baseline with final model
anova(m0.mlr, m2.mlr)
# compare baseline with final model
Anova(m0.mlr, m2.mlr, type = "III")
# start plotting
par(mfrow = c(2, 2)) # display plots in 2 rows/2 columns
plot(m2.mlr)         # plot fitted values
par(mfrow = c(1, 1)) # restore original settings
# determine a cutoff for data points that have
# D-values higher than 4/(n-k-1)
cutoff <- 4/((nrow(mlrdata)-length(m2.mlr$coefficients)-2))
# start plotting
par(mfrow = c(1, 2))           # display plots in 1 row/2 columns
qqPlot(m2.mlr, main="QQ Plot") # create qq-plot
plot(m2.mlr, which=4, cook.levels = cutoff) # plot cook*s distance
par(mfrow = c(1, 1))           # restore original settings
# add model diagnostics to the data
mlrdata$residuals <- resid(m2.mlr)
mlrdata$standardized.residuals <- rstandard(m2.mlr)
mlrdata$studentized.residuals <- rstudent(m2.mlr)
mlrdata$cooks.distance <- cooks.distance(m2.mlr)
mlrdata$dffit <- dffits(m2.mlr)
mlrdata$leverage <- hatvalues(m2.mlr)
mlrdata$covariance.ratios <- covratio(m2.mlr)
mlrdata$fitted <- m2.mlr$fitted.values
# plot 5
p5 <- ggplot(mlrdata,
             aes(studentized.residuals)) +
  theme(legend.position = "none") +
  theme_set(theme_bw(base_size = 8))+
  geom_histogram(aes(y=..density..),
                 binwidth = 1,
                 colour="black",
                 fill="white") +
  labs(x = "Studentized Residual", y = "Density") +
  stat_function(fun = dnorm,
                args = list(mean = mean(mlrdata$studentized.residuals, na.rm = TRUE),
                            sd = sd(mlrdata$studentized.residuals, na.rm = TRUE)),
                colour = "red", size = 1)
# plot 6
p6 <- ggplot(mlrdata, aes(fitted, studentized.residuals)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "Red")+
    theme_set(theme_bw(base_size = 8))+
  labs(x = "Fitted Values",
       y = "Studentized Residual")
# plot 7
p7 <- qplot(sample = mlrdata$studentized.residuals, stat="qq") +
    theme_set(theme_bw(base_size = 8))+
  labs(x = "Theoretical Values",
       y = "Observed Values")
multiplot(p5, p6, p7, cols = 3)
# 1: optimal = 0
# (aufgelistete datenpunkte sollten entfernt werden)
which(mlrdata$standardized.residuals > 3.29)
# 2: optimal = 1
# (listed data points should be removed)
stdres_258 <- as.vector(sapply(mlrdata$standardized.residuals, function(x) {
ifelse(sqrt((x^2)) > 2.58, 1, 0) } ))
(sum(stdres_258) / length(stdres_258)) * 100
# 3: optimal = 5
# (listed data points should be removed)
stdres_196 <- as.vector(sapply(mlrdata$standardized.residuals, function(x) {
ifelse(sqrt((x^2)) > 1.96, 1, 0) } ))
(sum(stdres_196) / length(stdres_196)) * 100
# 4: optimal = 0
# (listed data points should be removed)
which(mlrdata$cooks.distance > 1)
# 5: optimal = 0
# (data points should be removed if cooks distance is close to 1)
which(mlrdata$leverage >= (3*mean(mlrdata$leverage)))
# 6: checking autocorrelation:
# Durbin-Watson test (optimal: grosser p-wert)
dwt(m2.mlr)
# 7: test multicolliniarity 1
vif(m2.mlr)
# 8: test multicolliniarity 2
1/vif(m2.mlr)
# 9: mean vif should not exceed 1
mean(vif(m2.mlr))
# check if sample size is sufficient
smplesz(m2.mlr)
# check beta-error likelihood
expR(m2.mlr)
# tabulate regression results
mlrsummary <- mlinrsummary(m2.mlr, m2.glm, ia = T)
# remove columns with confidence intervals
mlrsummary[,-c(3:4)]
# tabulate regression results
mlrsummary <- mlinrsummary(m2.mlr, m2.glm, ia = T)
kable(mlrsummary, caption = "Overview of the final minimal adequate linear fixed-effects regression model.")
bodyheight=rnorm(20,175,20) # generates 20 values, with mean of 30 & s.d.=2
bodyheight=sort(bodyheight) # sorts these values in ascending order.
relationship=c(0,0,0,0,0,1,0,1,0,0,1,1,0,1,1,1,0,1,1,1) # assign 'survival' to these 20 individuals non-randomly... most mortality occurs at smaller body size
blrex=as.data.frame(cbind(bodyheight,relationship)) # saves data frame with two columns: body size & survival
kable(head(blrex), caption = "First six observations of an example data set representing the height and relationship status of a sample of men.")
# plot 1
p1 <- ggplot(blrex, aes(bodyheight, relationship)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, color = "red", size = .5) +
  labs(x = "Height") +
  labs(y = "Relationship", cex = .75) +
  theme_set(theme_bw(base_size = 8))+
  coord_cartesian(ylim = c(-0.2, 1.2), xlim = c(140, 220)) +
  scale_y_continuous(breaks=seq(0, 1, 1), labels = c("Not in Relationship", "In Relationship")) +
  guides(fill = FALSE)
# plot 2
p2 <- ggplot(blrex, aes(x=bodyheight, y=relationship)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = FALSE, color = "red", size = .5) +
  coord_cartesian(ylim = c(-0.2, 1.2), xlim = c(140, 220)) +
  labs(x = "Height") +
  scale_y_continuous(breaks=seq(0, 1, 1), labels = c("Not in Relationship", "In Relationship")) +
  labs(y = "Relationship status", cex = .75)
# draw two plots in one window
multiplot(p1, p2, cols = 2)
# load data
blrdata <- read.table("https://slcladal.github.io/data/blrdata.txt",
                      comment.char = "",  # data does not contain comments
                      quote = "",         # data does not contain quotes
                      sep = "\t",         # data is tab separetd
                      header = T)         # variables have headers
# inspect data
str(blrdata)
kable(head(blrdata), caption = "First six line of the blrdata data set.")
vrs <- c("Age", "Gender", "Ethnicity", "ID")  # define variables to be factorized
fctr <- which(colnames(blrdata) %in% vrs)     # define vector with variables
blrdata[,fctr] <- lapply(blrdata[,fctr], factor) # factorize variables
blrdata$Age <- relevel(blrdata$Age, "Young") # relevel Age (Young = Reference)
blrdata$Ethnicity <- relevel(                # relevel Ethnicity
  blrdata$Ethnicity, "Pakeha") # define Pakeha as Reference level)
ggplot(blrdata, aes(Age, EH, color = Gender)) +
  facet_wrap(~Ethnicity) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_set(theme_bw(base_size = 10)) +
  theme(legend.position = "top") +
  labs(x = "", y = "Observed Probabilty of eh") +
  scale_color_manual(values = c("gray20", "gray70"))
# set contrasts
options(contrasts  =c("contr.treatment", "contr.poly"))
# create distance matrix
blrdata.dist <- datadist(blrdata)
# include distance matrix in options
options(datadist = "blrdata.dist")
# baseline glm model
m0.glm = glm(EH ~ 1, family = binomial, data = blrdata)
# baseline lrm model
m0.lrm = lrm(EH ~ 1, data = blrdata, x = T, y = T)
# check incomplete information
ifelse(min(ftable(blrdata$Age, blrdata$EH)) == 0, "not possible", "possible")
# add age to the model
m1.glm = glm(EH ~ Age, family = binomial, data = blrdata)
# check multicollinearity (vifs should have values of 3 or lower for main effects)
ifelse(max(vif(m1.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
# check if adding Age significantly improves model fit
anova(m1.glm, m0.glm, test = "Chi")
ifelse(min(ftable(blrdata$Gender, blrdata$EH)) == 0, "not possible", "possible")
m2.glm <- update(m1.glm, . ~ . +Gender)
ifelse(max(vif(m2.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
anova(m2.glm, m1.glm, test = "Chi")
Anova(m2.glm, test = "LR")
ifelse(min(ftable(blrdata$Ethnicity, blrdata$EH)) == 0, "not possible", "possible")
m3.glm <- update(m2.glm, . ~ . +Ethnicity)
ifelse(max(vif(m3.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
anova(m3.glm, m2.glm, test = "Chi")
ifelse(min(ftable(blrdata$Age, blrdata$Gender, blrdata$EH)) == 0, "not possible", "possible")
m4.glm <- update(m2.glm, . ~ . +Age*Gender)
ifelse(max(vif(m4.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
anova(m4.glm, m2.glm, test = "Chi")
ifelse(min(ftable(blrdata$Age, blrdata$Ethnicity, blrdata$EH)) == 0, "not possible", "possible")
m5.glm <- update(m2.glm, . ~ . +Age*Ethnicity)
ifelse(max(vif(m5.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
anova(m5.glm, m2.glm, test = "Chi")
ifelse(min(ftable(blrdata$Gender, blrdata$Ethnicity, blrdata$EH)) == 0, "not possible", "possible")
m6.glm <- update(m2.glm, . ~ . +Gender*Ethnicity)
ifelse(max(vif(m6.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
anova(m6.glm, m2.glm, test = "Chi")
ifelse(min(ftable(blrdata$Age, blrdata$Gender, blrdata$Ethnicity, blrdata$EH)) == 0, "not possible", "possible")
m7.glm <- update(m2.glm, . ~ . +Gender*Ethnicity)
ifelse(max(vif(m7.glm)) <= 3,  "vifs ok", "WARNING: high vifs!") # VIFs ok
anova(m7.glm, m2.glm, test = "Chi")
m2.lrm <- lrm(EH ~ Age+Gender, data = blrdata, x = T, y = T, linear.predictors = T)
m2.lrm
anova(m2.lrm)
# model validation (remove # to activate: output too long for website)
m7.lrm <- lrm(EH ~ (Age+Gender+Ethnicity)^3, data = blrdata, x = T, y = T, linear.predictors = T)
#validate(m7.lrm, bw = T, B = 200)
pentrace(m2.lrm, seq(0, 0.8, by = 0.05)) # determine penalty
lr.glm <- m2.glm  # rename final minimal adequate glm model
lr.lrm <- m2.lrm  # rename final minimal adequate lrm model
modelChi <- lr.glm$null.deviance - lr.glm$deviance
chidf <- lr.glm$df.null - lr.glm$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob
anova(m0.glm, lr.glm, test = "Chi") # Model Likelihood Ratio Test
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
# extract the confidence intervals for the coefficients
confint(lr.glm)
exp(lr.glm$coefficients) # odds ratios
exp(confint(lr.glm))     # confidence intervals of the coefficients
blrdata_byspeaker <- table(blrdata$ID, blrdata$EH)
blrdata_byspeaker <- data.frame(rownames(blrdata_byspeaker), blrdata_byspeaker[, 1], blrdata_byspeaker[, 2])
names(blrdata_byspeaker) <- c("ID", "NOEH", "EH")
rownames(blrdata_byspeaker) <- 1:length(blrdata_byspeaker[,1])
blrdata_byspeaker <- plyr::join(blrdata_byspeaker,  # join by-speaker data and biodata
                          blrdata, by = "ID", # join by ID
                          type = "left",      # only speakers for which bio data is provided
                          match = "first")    #
blrdata_byspeaker$EH <- NULL                  # remove EH column
kable(head(blrdata_byspeaker), caption = "First six rows of the by-speaker data.")
# use by.spk data to fit another model which we will use to test the accuracy of the model
lr.glm.spk <- glm(cbind(EH, NOEH) ~ Age*Gender + Ethnicity + Age:Ethnicity, data = blrdata_byspeaker, family = binomial)
correct <- sum(blrdata_byspeaker$EH * (predict(lr.glm.spk, type = "response") >= 0.5)) + sum(blrdata_byspeaker$NOEH * (predict(lr.glm.spk, type="response") < 0.5))
tot <- sum(blrdata_byspeaker$EH) + sum(blrdata_byspeaker$NOEH)
predict.acc <- (correct/tot)*100
predict.acc
# extract prediction accuracy
lr.glm.spk.base <- glm(cbind(EH, NOEH) ~ 1, data = blrdata_byspeaker, family = binomial)
correct.b <- sum(blrdata_byspeaker$EH * (predict(lr.glm.spk.base, type = "response") >= 0.5)) + sum(blrdata_byspeaker$NOEH * (predict(lr.glm.spk.base, type="response") < 0.5))
tot.b <- sum(blrdata_byspeaker$EH) + sum(blrdata_byspeaker$NOEH)
predict.acc.base <- (correct.b/tot.b)*100
# inspect prediction accuracy
predict.acc.base
# compare preictions of final and base line model
which(lr.glm.spk$fitted > .5)
which(lr.glm.spk.base$fitted > .5)
# create variable with contains the prediction of the model
blrdata$Prediction <- predict(lr.glm, blrdata, type = "response")
blrdata$Prediction <- ifelse(blrdata$Prediction > .5, 1, 0)
# convert predicted and observed into factors with the same levels
blrdata$Prediction <- factor(blrdata$Prediction, levels = c("0", "1"))
blrdata$EH <- factor(blrdata$EH, levels = c("0", "1"))
# create a confusion matrix with compares observed against predicted values
caret::confusionMatrix(blrdata$Prediction, blrdata$EH)
# create plot
par(mfrow = c(1, 2))
visreg(lr.glm, "Age", xlab = "Age",
       ylab = "Logged Odds (EH)",
       ylim = c(-3, 0))
visreg(lr.glm, "Gender", xlab = "Gender",
       ylab = "Logged Odds (EH)",
       ylim = c(-3, 0)); par(mfrow = c(1, 1))
# extract predicted probabilities
blrdata$Predicted <- predict(lr.glm, blrdata, type = "response")
# plot
ggplot(blrdata, aes(Age, Predicted, color = Gender)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_set(theme_bw(base_size = 10)) +
  theme(legend.position = "top") +
    ylim(0, .75) +
  labs(x = "", y = "Predicted Probabilty of eh") +
  scale_color_manual(values = c("gray20", "gray70"))
vif(lr.glm)
mean(vif(lr.glm))
infl <- influence.measures(lr.glm) # calculate influence statistics
blrdata <- data.frame(blrdata, infl[[1]], infl[[2]]) # add influence statistics
# function to evaluate sample size
smplesz <- function(x) {
  ifelse((length(x$fitted) < (104 + ncol(summary(x)$coefficients)-1)) == TRUE,
    return(
      paste("Sample too small: please increase your sample by ",
      104 + ncol(summary(x)$coefficients)-1 - length(x$fitted),
      " data points", collapse = "")),
    return("Sample size sufficient")) }
# apply unction to model
smplesz(lr.glm)
blrsummary <- blrsummary(lr.glm, lr.lrm, predict.acc) # summarize regression analysis
blrsummary
blrsummary <- blrsummary(lr.glm, lr.lrm, predict.acc) # summarize regression analysis
kable(blrsummary, caption = "Summary of the final minimal adequate binomial logistic fixed-effects regression model which was fitted to predictors of eh in New Zealand English.")
# load data
ordata <- read.delim("https://slcladal.github.io/data/ordinaldata.txt", sep = "\t", header = T)
colnames(ordata) <- c("Recommend", "Internal", "Exchange", "FinalScore")
# inspect data
str(ordata)
# relevel data
ordata <- ordata %>%
dplyr::mutate(Recommend = factor(Recommend, 
                           levels=c("unlikely", "somewhat likely", "very likely"),
                           labels=c("unlikely",  "somewhat likely",  "very likely"))) %>%
  dplyr::mutate(Exchange = ifelse(Exchange == 1, "Exchange", "NoExchange")) %>%
  dplyr::mutate(Internal = ifelse(Internal == 1, "Internal", "External"))
## three way cross tabs (xtabs) and flatten the table
ftable(xtabs(~ Exchange + Recommend + Internal, data = ordata))
summary(ordata$FinalScore); sd(ordata$FinalScore)
# visualize data
ggplot(ordata, aes(x = Recommend, y = FinalScore)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(Exchange ~ Internal, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
## fit ordered logit model and store results 'm'
m <- polr(Recommend ~ Internal + Exchange + FinalScore, data = ordata, Hess=TRUE)
## view a summary of the model
summary(m)
## store table
(ctable <- coef(summary(m)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
# extract profiled confidence intervals
ci <- confint(m)
# calculate odds ratios and combine them with profiled CIs
exp(cbind(OR = coef(m), ci))
# plot poisson
Count <- 1:10
Lambda1 <- dpois(Count, 1)
Lambda2 <- dpois(Count, 2)
Lambda3 <- dpois(Count, 3)
Lambda4 <- dpois(Count, 4)
Lambda5 <- dpois(Count, 5)
pdata <- data.frame(Count, Lambda1, Lambda2, Lambda3, Lambda4, Lambda5)
pdata <- pdata %>%
  dplyr::group_by(Count) %>%
  tidyr::gather(Lambda, Value, Lambda1:Lambda5) %>%
  dplyr::mutate(Lambda = str_replace_all(Lambda, "Lambda", ""))   %>%
  dplyr::mutate(Lambda = factor(Lambda))
# plot poisson with different lambdas
ggplot(pdata, aes(x = Count, y = Value, color = Lambda)) +
  geom_smooth(alpha=.5, se = F) +
  guides(color=guide_legend(override.aes=list(fill=NA))) + 
  theme_set(theme_bw(base_size = 10)) +
  theme(legend.position="top") +
  scale_colour_manual(values=c("goldenrod2", "gray40", "blue",
                               "indianred4", "gray80"), name="Lambda") +
  labs(x = "Number of Instances")  + 
  scale_x_continuous(breaks=1:10, labels=1:10) +
  scale_y_continuous(name="Probability", limits=c(0, .4))
# load data
poissondata <- read.delim("https://slcladal.github.io/data/posdata.txt", sep = "\t", header = T, skipNul = T, quote = "")
# inspect data
summary(poissondata)
# process data
poissondata <- poissondata %>%
  mutate(Id = factor(Id, levels = 1:200, labels = 1:200))
# inspect data
summary(poissondata); str(poissondata)
# output the results
gf = goodfit(poissondata$Pauses,type= "poisson",method= "ML")
summary(gf)
plot(gf,main="Count data vs Poisson distribution")
# check homogeneity
leveneTest(poissondata$Pauses, poissondata$Language, center = mean)
# extract mean and standard devaiation
with(poissondata, tapply(Pauses, Language, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
# plot data
ggplot(poissondata, aes(Pauses, fill = Language)) +
  geom_histogram(binwidth=.5, position="dodge") +
  scale_fill_manual(values=c("gray30", "gray50", "gray70"))
# calculate poissant regression
m1.poisson <- glm(Pauses ~ Language + Alcohol, family="poisson", data=poissondata)
# inspect model
summary(m1.poisson)
cov.m1 <- vcovHC(m1.poisson, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1.poisson), "Robust SE" = std.err,
"Pr(>|z|)" = 2 * pnorm(abs(coef(m1.poisson)/std.err), lower.tail=FALSE),
LL = coef(m1.poisson) - 1.96 * std.err,
UL = coef(m1.poisson) + 1.96 * std.err)
# inspect data
r.est
with(m1.poisson, cbind(res.deviance = deviance, df = df.residual,
  p = pchisq(deviance, df.residual, lower.tail=FALSE)))
## update m1 model dropping prog
m2.poisson <- update(m1.poisson, . ~ . - Language)
## test model differences with chi square test
anova(m2.poisson, m1.poisson, test="Chisq")
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                                                coef(m1.poisson), cov.m1)
## exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s
rexp.est
# extract predicted values
(s1 <- data.frame(Alcohol = mean(poissondata$Alcohol),
  Language = factor(1:3, levels = 1:3, labels = names(table(poissondata$Language)))))
predict(m1.poisson, s1, type="response", se.fit=TRUE)
## calculate and store predicted values
poissondata$Predicted <- predict(m1.poisson, type="response")
## order by program and then by math
poissondata <- poissondata[with(poissondata, order(Language, Alcohol)), ]
## create the plot
ggplot(poissondata, aes(x = Alcohol, y = Predicted, colour = Language)) +
  geom_point(aes(y = Pauses), alpha=.5, 
             position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Alcohol (ml)", y = "Expected number of pauses") +
  scale_color_manual(values=c("gray30", "gray50", "gray70"))
# load data
robustdata <- read.delim("https://slcladal.github.io/data/mlrdata.txt", sep = "\t", header = T)
# inspect data
summary(robustdata)
# create model
slm <- lm(money ~ status+attraction, data = robustdata)
# inspect model
summary(slm)
# create model diagnost plots
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(slm, las = 1)
par(opar)
robustdata[c(52, 64, 83), 1:2]
CooksDistance <- cooks.distance(slm)
StandardizedResiduals <- stdres(slm)
a <- cbind(robustdata, CooksDistance, StandardizedResiduals)
a[CooksDistance > 4/100, ]
AbsoluteStandardizedResiduals <- abs(StandardizedResiduals)
a <- cbind(robustdata, CooksDistance, StandardizedResiduals, AbsoluteStandardizedResiduals)
asorted <- a[order(-AbsoluteStandardizedResiduals), ]
asorted[1:10, ]
# create robust regression model
rmodel <- rlm(money ~ status + attraction, data = robustdata)
# inspect model
summary(rmodel)
hweights <- data.frame(status = robustdata$status, resid = rmodel$resid, weight = rmodel$w)
hweights2 <- hweights[order(rmodel$w), ]
hweights2[1:15, ]
p_status <- f.robftest(rmodel, var = 2)
p_attraction <- f.robftest(rmodel, var = 3)
# inspect results
p_status; p_attraction
The output shows that both status and attraction are significant but, as we have seen above, the effect that really matters is the interaction between status and attraction. This was, however, not the focus of this sections as this section meerly served to introduce the concept of weights and how they can be used in the context of a robust linear regression.
# References

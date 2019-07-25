
# "Fixed-Effects Regression Models"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("boot", "car", "effects", "ggplot2",
                   "Hmisc", "languageR", "lme4", "mlogit",
                   "msm", "plyr", "QuantPsyc", "RLRsim", "rms",
                   "sandwich", "sjPlot", "visreg"))
# load libraries
library(ggplot2)
library(car)
library(QuantPsyc)
library(boot)
# load functions
source("rscripts/multiplot_ggplot2.r")
source("rscripts/mlinr.summary.r")
source("rscripts/SampleSizeMLR.r")
source("rscripts/ExpR.r")
# load data
mlrdata <- read.delim("https://slcladal.github.io/data/mlrdata.txt", header = TRUE)
head(mlrdata)    # inspect first 6 lines
str(mlrdata)     # inspect structure
summary(mlrdata) # summarize data
# create plot
# plot 1
p1 <- ggplot(mlrdata,                   # def. data
             aes(status, money)) +      # def. x/y-axes
  geom_boxplot(fill=c("gold", "indianred4")) + # def. col.
  theme_set(theme_bw(base_size = 8))+   # black and white theme
  labs(x = "") +                        # x-axis label
  labs(y = "Money spent on present (AUD)", cex = .75) +   # y-axis label
  coord_cartesian(ylim = c(0, 250)) +   # y-axis range
  guides(fill = FALSE) +                # no legend
  ggtitle("Status")                     # title
# plot 2
p2 <- ggplot(mlrdata,
             aes(attraction, money)) +
  geom_boxplot(fill=c("grey30", "grey70")) +
theme_set(theme_bw(base_size = 8))+
  labs(x = "") +                              # x-axis label
  labs(y = "Money spent on present (AUD)") +  # y-axis label
  coord_cartesian(ylim = c(0, 250)) +
  guides(fill = FALSE) +
  ggtitle("Attraction")
# plot 3
p3 <- ggplot(mlrdata,
             aes(x = money)) +
  geom_histogram(aes(y=..density..),    # add density statistic
                 binwidth = 10,         # def. bin width
                 colour = "black",      # def. bar edge colour
                 fill = "white") +      # def. bar col.
    theme_bw() +                        # black-white theme
  geom_density(alpha=.2, fill = "#FF6666") # def. col. of overlay
# plot 4
p4 <- ggplot(mlrdata, aes(status, money)) +
  geom_boxplot(notch = F, aes(fill = factor(status))) + # create boxplot
  scale_fill_brewer(palette="Set1") +   # def. col. palette
  facet_wrap(~ attraction, nrow = 1) +  # separate panels for attraction
theme_set(theme_bw(base_size = 8)) +
  labs(x = "") +
  labs(y = "Money spent on present (Euro)") +
  coord_cartesian(ylim = c(0, 250)) +
  guides(fill = FALSE)
# show plots
multiplot(p1, p3, p2, p4, cols = 2)
m0.mlr = lm(         # generate lm regression object
  money ~ 1,         # def. rgression formula (1 = intercept)
  data = mlrdata)    # def. data
m0.glm = glm(        # generate glm regression object
  money ~ 1,         # def. rgression formula (1 = intercept)
  family = gaussian, # def. linkage function
  data = mlrdata)    # def. data
m1.mlr = lm(         # generate lm regression object
  money ~ (status + attraction)^2, # def. rgression formula
  data = mlrdata)    # def. data
m1.glm = glm(        # generate glm regression object
  money ~ status * attraction,     # def. rgression formula
  family = gaussian, # def. linkage function
  data = mlrdata)    # def. data set
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
# compare baseline- and minimal adequate model
anova(m0.mlr, m2.mlr)
# compare baseline- and minimal adequate model
Anova(m0.mlr, m2.mlr, type = "III")
# start plotting
par(mfrow = c(1, 4)) # display plots in 3 rows/2 columns
plot(m2.mlr)         # plot fitted values
par(mfrow = c(1, 1)) # restore original settings
# determine a cutoff for data points that have D-values higher than 4/(n-k-1)
cutoff <- 4/((nrow(mlrdata)-length(m2.mlr$coefficients)-2))
# start plotting
par(mfrow = c(1, 2))           # display plots in 3 rows/2 columns
qqPlot(m2.mlr, main="QQ Plot") # create qq-plot
plot(m2.mlr, which=4, cook.levels = cutoff) # plot cook*s distance
par(mfrow = c(1, 1))           # restore original settings
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
mlrsummary <- mlr.summary(m2.mlr, m2.glm, ia = T)
# remove columns with confidence intervals
mlrsummary[,-c(4:5)]
bodyheight=rnorm(20,180,10) # generates 20 values, with mean of 30 & s.d.=2
bodyheight=sort(bodyheight) # sorts these values in ascending order.
relationship=c(0,0,0,0,0,1,0,1,0,0,1,1,0,1,1,1,0,1,1,1) # assign 'survival' to these 20 individuals non-randomly... most mortality occurs at smaller body size
blrex=as.data.frame(cbind(bodyheight,relationship)) # saves data frame with two columns: body size & survival
library(knitr)
kable(blrex, caption = "Example data set representing the height and relationship status of a sample of men.")
# load library
library(ggplot2)
# plot 1
p1 <- ggplot(blrex,
             aes(bodyheight, relationship)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Height") +
  labs(y = "Relationship", cex = .75) +
  theme_set(theme_bw(base_size = 8))+
  coord_cartesian(ylim = c(-0.2, 1.2), xlim = c(160, 200)) +
  scale_y_continuous(breaks=seq(0, 1, 1), labels = c("Not in Relationship", "In Relationship")) +
  guides(fill = FALSE)
# plot 2
p2 <- ggplot(blrex, aes(x=bodyheight, y=relationship)) +
  geom_point() +
  geom_smooth(method = "glm",
    method.args = list(family = "binomial"),
    se = FALSE) +
  coord_cartesian(ylim = c(-0.2, 1.2), xlim = c(160, 200)) +
  labs(x = "Height") +                        # x-axis label
  scale_y_continuous(breaks=seq(0, 1, 1), labels = c("Not in Relationship", "In Relationship")) +
  labs(y = "Relationship", cex = .75)
# draw two plots in one window
multiplot(p1, p2, cols = 2)
 # clean workspace
rm(list=ls(all=T))
 # set options
options("scipen" = 100, "digits" = 4)
# load libraries
library(effects)
library(ggplot2)
library(mlogit)
library(plyr)
library(rms)
library(sjPlot)
library(visreg)
# load functions
source("rscripts/multiplot_ggplot2.R")
source("rscripts/blr.summary.R")
# load data
blrdata <- read.table("data/blrdata.txt",
                      comment.char = "",  # data does not contain comments
                      quote = "",         # data does not contain quotes
                      sep = "\t",         # data is tab separetd
                      header = T)         # variables have headers
# inspect data
str(blrdata)
library(knitr)
kable(head(blrdata), caption = "First six line of the blrdata data set.")
vrs <- c("Age", "Gender", "Ethnicity", "ID")  # define variables to be factorized
fctr <- which(colnames(blrdata) %in% vrs)     # define vector with variables
blrdata[,fctr] <- lapply(blrdata[,fctr], factor) # factorize variables
blrdata$Age <- relevel(blrdata$Age, "Young") # relevel Age (Young = Reference)
blrdata$Ethnicity <- relevel(                # relevel Ethnicity
  blrdata$Ethnicity, "Pakeha") # define Pakeha as Reference level)
p1 <- ggplot(blrdata,
             aes(Gender, EH, color = Gender)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Gender", y = "Probability of EH")+
  guides(fill=FALSE, color=FALSE) +
  scale_color_manual(values = c("blue", "red"))
p2 <- ggplot(blrdata,
             aes(Age, EH, color = Age)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Age", y = "Probability of EH") +
  guides(fill=FALSE, color=FALSE) +
  scale_color_manual(values = c("darkblue", "lightblue"))
p3 <- ggplot(blrdata,
             aes(Ethnicity, EH, colour = Ethnicity)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Ethnicity", y = "Probability of EH", colour = "Ethnicity") +
  guides(fill=FALSE, color=FALSE) +
  scale_color_manual(values = c("darkgreen", "lightgreen"))
p4 <- ggplot(blrdata,
             aes(Ethnicity, EH, colour = Gender)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Ethnicity", y = "Probability of EH", colour = "Gender")+
  scale_color_manual(values = c("blue", "red"))
p5 <- ggplot(blrdata,
             aes(Gender, EH, colour = Age)) +
  stat_summary(fun.y = mean, geom = "point",
               aes(group= Age)) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Sex", y = "Probability of EH", colour = "Age") +
  guides(fill = FALSE) +
  scale_color_manual(values = c("darkblue", "lightblue"))
p6 <- ggplot(blrdata,
             aes(Age, EH, colour = Ethnicity)) +
  stat_summary(fun.y = mean, geom = "point",
               aes(group= Ethnicity)) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot,
               geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Age", y = "Probability of EH", colour = "Ethnicity") +
  guides(fill = FALSE) +
  scale_color_manual(values = c("darkgreen", "lightgreen"))
# display the plots
multiplot(p1, p4, p2, p5, p3, p6, cols = 3)
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
m1.glm = glm(EH ~ Age*Gender*Ethnicity, family = binomial, data = blrdata)
m1.lrm = lrm(EH ~ Age*Gender*Ethnicity, data = blrdata, x = T, y = T)
# remove Age:Gender:Ethnicity
m2.glm <- update(m1.glm, . ~ . -Age:Gender:Ethnicity)
# check if removal significantly decreases model fit
anova(m1.glm, m2.glm, test = "Chi")
m3.glm <- update(m2.glm, . ~ . -Gender:Ethnicity)
anova(m2.glm, m3.glm, test = "Chi")
m4.glm <- update(m3.glm, . ~ . -Age:Gender)
anova(m3.glm, m4.glm, test = "Chi")
m5.glm <- update(m4.glm, . ~ . -Age:Ethnicity)
anova(m4.glm, m5.glm, test = "Chi")
m6.glm <- update(m5.glm, . ~ . -Ethnicity)
anova(m5.glm, m6.glm, test = "Chi")
m7.glm <- update(m6.glm, . ~ . -Gender)
anova(m7.glm, m6.glm, test = "Chi")
m8.glm <- update(m6.glm, . ~ . -Age)
anova(m8.glm, m6.glm, test = "Chi")
m6.lrm <- lrm(EH ~ Age+Gender, data = blrdata, x = T, y = T, linear.predictors = T)
m6.lrm
anova(m6.lrm)
# model validation
validate(m1.lrm, bw = T, B = 200)
pentrace(m6.lrm, seq(0, 0.8, by = 0.05)) # determine penalty
lr.glm <- m6.glm  # rename final minimal adeqaute glm model
lr.lrm <- m6.lrm  # rename final minimal adeqaute lrm model
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
blrdata_byspeaker <- join(blrdata_byspeaker,  # join by-speaker data and biodata
                          blrdata, by = "ID", # join by ID
                          type = "left",      # only speakers for which bio data is provided
                          match = "first")    #
blrdata_byspeaker$EH <- NULL                  # remove EH column
library(knitr)    # load library
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
# create plot
par(mfrow = c(1, 2))
visreg(lr.glm, "Age", xlab = "Age",
       ylab = "Logged Odds (EH)",
       ylim = c(-3, 0))
visreg(lr.glm, "Gender", xlab = "Gender",
       ylab = "Logged Odds (EH)",
       ylim = c(-3, 0))
par(mfrow = c(1, 1))
vif(lr.glm)
mean(vif(lr.glm))
infl <- influence.measures(lr.glm) # calculate influence statistics
blrdata <- data.frame(blrdata, infl[[1]], infl[[2]]) # add influence statistics
library(knitr)    # load library
kable(head(blrdata), caption = "First six rows of the data set with added influence statistics.")
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
blrmsummary <- blrm.summary(lr.glm, lr.lrm, predict.acc) # summarize regression analysis
blrmsummary[, -c(4:5)] # remove columns with confidence intervals
# load libraries
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
# load data
ordata <- read.delim("https://slcladal.github.io/data/ordinaldata.txt", sep = "\t", header = T)
colnames(ordata) <- c("Recommend", "Internal", "Exchange", "FinalScore")
# inspect data
head(ordata); nrow(ordata)
#
ordata$Recommend <- factor(ordata$Recommend, 
                           levels=c("unlikely", 
                                    "somewhat likely", 
                                    "very likely"),
                           labels=c("unlikely", 
                                    "somewhat likely", 
                                    "very likely"))
# one at a time, table apply, pared, and public
lapply(ordata[, c("Recommend", "Internal", "Exchange")], table)
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
# default method gives profiled CIs
(ci <- confint(m)) 
# CIs assuming normality
confint.default(m) 
## odds ratios
exp(coef(m))
## OR and CI
exp(cbind(OR = coef(m), ci))
# load libraries
library(ggplot2)
library(sandwich)
library(msm)
# load data
p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
# inspect data
summary(p)
# factorize id and prog
p$id <- factor(p$id)
p$prog <- factor(p$prog, levels=1:3, labels=c("General", "Academic", 
                                                     "Vocational"))
# inspect data
summary(p)
# extract mean and standard devaiation
with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
# plot data
ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")
# calculate poissant regression
poissantr <- glm(num_awards ~ prog + math, family="poisson", data=p)
# inspect model
summary(poissantr)
# 
cov.m1 <- vcovHC(poissantr, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(poissantr), "Robust SE" = std.err,
"Pr(>|z|)" = 2 * pnorm(abs(coef(poissantr)/std.err), lower.tail=FALSE),
LL = coef(poissantr) - 1.96 * std.err,
UL = coef(poissantr) + 1.96 * std.err)
# inspect data
r.est
with(poissantr, cbind(res.deviance = deviance, df = df.residual,
  p = pchisq(deviance, df.residual, lower.tail=FALSE)))
## update m1 model dropping prog
poissantr2 <- update(poissantr, . ~ . - prog)
## test model differences with chi square test
anova(poissantr2, poissantr, test="Chisq")
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                                                coef(poissantr), cov.m1)
## exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s
rexp.est
(s1 <- data.frame(math = mean(p$math),
  prog = factor(1:3, levels = 1:3, labels = levels(p$prog))))
predict(poissantr, s1, type="response", se.fit=TRUE)
## calculate and store predicted values
p$phat <- predict(poissantr, type="response")
## order by program and then by math
p <- p[with(p, order(prog, math)), ]
## create the plot
ggplot(p, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, 
             position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Math Score", y = "Expected number of awards")
# load libraries
library(foreign)
library(MASS)
library(sfsmisc)
# load data
robustdata <- read.delim("https://slcladal.github.io/data/robustdata.txt", sep = "\t", header = T)
# inspect data
summary(robustdata)
# create model
slm <- lm(crime ~ poverty + single, data = robustdata)
# inspect model
summary(slm)
# create model diagnost plots
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(slm, las = 1)
par(opar)
robustdata[c(9, 25, 51), 1:2]
d1 <- cooks.distance(slm)
r <- stdres(slm)
a <- cbind(robustdata, d1, r)
a[d1 > 4/51, ]
rabs <- abs(r)
a <- cbind(robustdata, d1, r, rabs)
asorted <- a[order(-rabs), ]
asorted[1:10, ]
# create robust regression model
rmodel <- rlm(crime ~ poverty + single, data = robustdata)
# inspect model
summary(rmodel)
hweights <- data.frame(state = robustdata$state, resid = rmodel$resid, weight = rmodel$w)
hweights2 <- hweights[order(rmodel$w), ]
hweights2[1:15, ]
rr.bisquare <- rlm(crime ~ poverty + single, data=robustdata, psi = psi.bisquare)
summary(rr.bisquare)
biweights <- data.frame(state = robustdata$state, 
                        resid = rr.bisquare$resid,
                        weight = rr.bisquare$w)
biweights2 <- biweights[order(rr.bisquare$w), ]
biweights2[1:15, ]
p_poverty <- f.robftest(rmodel, var = "poverty")
p_single <- f.robftest(rmodel, var = "single")
# inspect results
p_poverty; p_single

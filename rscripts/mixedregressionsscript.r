# Mixed-Effects Regression Models
# Introduction
# Preparation and session set up
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("RLRsim", "nlme", "lme4", "Hmisc", "RLRsim", "sjPlot", 
                   "visreg", "mlogit", "plyr", "rms", "ggplot2", "effects", 
                   "lme4", "languageR", "Hmisc"))
#################################################################
# Linear Mixed-Effects Regression Models \label{mem}
## Introduction 
# activate packages
library(RLRsim)
#library(car)
#library(QuantPsyc)
#library(boot)
library(nlme)
library(lme4)
#library(ez)
library(ggplot2)
# set options
options("scipen" = 100, "digits" = 4)      # supress scientific notation
options(stringsAsFactors = F)              # do not convert strings into factors
mydata <- read.delim("data/lmemdata.txt", header = TRUE) # read in data
mydata$date <- as.numeric(mydata$date)     # convert date into a numeric variable
head(mydata); nrow(mydata)                 # inspect updated data set

# visualize variables (2 plots per row)
# 3 plots in 1 window
def.par <- par(no.readonly = TRUE)
nf <- layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = T))
plot(mydata$pptw ~ mydata$date, ylab = "Frequency", xlab = "year of publication")
abline(lm(mydata$pptw ~ mydata$date), lty = 3, lwd = 2, col = "red")
# re-set margins to fit the labels
par(mar = c(7.2, 4, 1, 2) + 0.1)
# reorder genre by median
genrebymedian <- with(mydata, reorder(genre, -pptw, median))
#	generate plots
plot(mydata$pptw ~ genrebymedian,
  col = "lightgrey",
  ylab = "Frequency",
  xlab = "",
  las = 2,
  cex.axis = .7,
  cex = .5)
# re-set margins
par(mar = c(5, 4, 1, 2) + 0.1)
x = mydata$pptw
h = hist(mydata$pptw,
	ylim =c(0, 150),
	xlim = c(50, 200),
	xlab = "prepositions per text",
	col = "lightgrey",
	main = "")
xfit <- seq(min(mydata$pptw), max(mydata$pptw), length = 40)
yfit <- dnorm(xfit, mean = mean(mydata$pptw),sd = sd(mydata$pptw))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, lty = 2, lwd=2)
# restore original graphic's parameters
par(def.par)

# plot 8
p8 <- ggplot(mydata, aes(date, pptw)) +
  geom_point() +
  labs(x = "Year") +
  labs(y = "Prepositions per 1,000 words") +
  geom_smooth(method = "lm")  + 
  theme_set(theme_bw(base_size = 10))
# plot 9
p9 <- ggplot(mydata, aes(region, pptw)) +
  geom_boxplot() +
  labs(x = "Region") +
  labs(y = "Prepositions per 1,000 words") +
  geom_smooth(method = "lm") # with linear model smoothing!
# include genre (lowess)
multiplot(p8, p9, cols = 2)

ggplot(mydata, aes(date, pptw)) +
  geom_point() +
  facet_wrap(~ genre, nrow = 4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Year") +
  labs(y = "Prepositions per 1,000 words") +
  coord_cartesian(ylim = c(0, 220))

mydata$date <- scale(mydata$date, scale = F)
head(mydata)
str(mydata)

# generate a glm baseline model
m0.glm <- glm(pptw ~ 1, family = gaussian, data = mydata)
# generate a lm base-line model
m0.lm <- lm(pptw ~ 1, data = mydata)
# set up first lme model including only the random effect specifying the random intercepts
m0.lme = lme(pptw ~ 1, random = ~1|genre, data = mydata, method = "ML")
# set up first lmer model including only the random effect specifying the random intercepts
m0.lmer = lmer(pptw ~ 1 + (1|genre), data = mydata, REML = F)
## Testing Random Effects
x2 = -2*logLik(m0.lm, REML = T)+2*logLik(m0.lmer, REML = T)
x2 <- x2 <- x2[[1]]
list(x2, pchisq(x2, df=2, lower.tail=F))

m0.lmer1 <- lmer(pptw ~ (1|genre) + 1, data = mydata, REML = T)
m0.lmer2 <- lmer(pptw ~ (1|region) + 1, data = mydata, REML = T)
m0.lmer3 <- lmer(pptw ~ (1|genre/region) + 1, data = mydata, REML = T)
anova(m0.lmer1, m0.lmer2, m0.lmer3)
# the model with the random effect structure (1|genre/region) performs
# significantly better (also it has a much lower AIC and deviance)
# therefore, m0.lmer3 is our new m0 model
m0.lmer <- m0.lmer3
# test if including the random effect is permitted by applying a restricted likelihood ratio test
# WARNING: this test can only take simple random effect (1|genre) but not
# (1|genre/date)
exactRLRT(m0.lmer1)
# there is another way to compare model with and without random effects: see below!
# create a second model with date as a fixed effect
# m1.lme <- lme(m0.lme, .~. + date) # alternative way to update the model
m1.lme = lme(pptw ~ date, random = ~1|genre/region, data = mydata, method = "ML")
# set up m1 model but using the lmer function from the lme4 package
m1.lmer = lmer(pptw ~ (1|genre/region) + date, data = mydata, REML = F)
# compare the models to see if including date has improved the model
# the difference between the models is the effect (size) of date!
anova(m0.lme, m1.lme)
# m1.lme is the better model (sig. p-value & lower AIC)
# date correlates significantly with pptw (X2(1) = 8.81, p = .003);
# X2 = L.Ratio;
# df = subtract df smaller from df larger model
# inspect results
summary(m1.lme)
# alternative display of the results
anova(m1.lme)
# test if date is significant
anova(m0.lmer, m1.lmer)
# extract estimates and sd for fixed and random effects
intervals(m1.lme)

## Model Diagnostics
plot(m1.lme, genre ~ resid(.), abline = 0 ) # generate diagnostic plots

plot(m1.lme, resid(., type = "p") ~ fitted(.) | genre, id = 0.05, adj = -0.3)

m2.lme <- update(m1.lme, weights = varIdent(form = ~ 1 | genre))
# test if m2.lme is more appropriate for the data than m1.lme
anova(m1.lme, m2.lme)

summary(m2.lme)        # inspect results

anova(m2.lme)          # ANOVA display of the results

anova(m0.lme, m2.lme)  # test if date is significant

intervals(m2.lme)      # extract estimates and sd for fixed and random effects

## Effect Sizes
ef.lme <- function(x) {
  df <- summary(x)[[20]][6]
  t <-  summary(x)[[20]][8]
  #df <- summary(x)$tTable[, 3]
  #t <- summary(x)$tTable[, 4]
  r <- sqrt((t^2)/((t^2)+df))
  return(paste("Pearson's r = ", round(r, 3)))
  }
ef.lme(m2.lme)

m2.lmer = lmer(pptw ~ (1|genre/region) + date, data = mydata, REML = F)
summary(m2.lmer)

m2.lmer = lmer(pptw ~ (1|genre/region) + date, data = mydata, REML = F)
2*pchisq(2*as.numeric(logLik(m2.lmer)-logLik(m0.glm)), 2, lower.tail = FALSE)

## Rerun Model Diagnostics 
# start plotting
par(mfrow = c(2, 2))           # display plots in 2 rows and 2 columns
plot(m2.lme)
par(mfrow = c(1, 1))

# diagnostic plot (Pinheiro & Bates 2000:21)
plot(m2.lme, form = resid(., type = "p") ~ fitted(.) | genre, abline = 0, cex = .5)

# diagnostic plot: residuals of fitted values against observed values (cf. Pinheiro & Bates 2000:182)
qqnorm(m2.lme)

# normal plot of the estimated date %in% genre random effects
qqnorm(m2.lme, ~ranef(., level = 2), id = 0.05, cex = 0.7, xlim = c(-40, 40))

# diagnostic plot: normal plots of the residuals by genre (cf. Pinheiro & Bates 2000:22, 179)
qqnorm(m2.lme, ~resid(.) | genre )

# inspect the observed responses versus the within-group fitted values
# (cf. Pinheiro & Bates 2000:178)
plot(m2.lme, pptw ~ fitted(.), id = 0.05, adj = -0.3, xlim = c(80, 220), cex = .8)

## Reporting Results 
summary(m2.lmer)

################################################################
# Mixed-Effects Binomial Logistic Regression Models
## Introduction 
## Example: Discourse LIKE in Irish English
rm(list=ls(all=T))  # clean current workspace
options("scipen" = 100, "digits" = 4)     # set options
library(Hmisc)      # activate library
library(RLRsim)     # activate library
library(sjPlot)     # activate library
library(visreg)     # activate library
library(mlogit)     # activate library
library(plyr)       # activate library
library(rms)        # activate library
library(ggplot2)    # activate library
library(effects)    # activate library
library(lme4)       # activate library
library(languageR)  # activate library
source("rscripts/multiplot_ggplot2.R")    # load multiplot function
source("rscripts/PseudoR2lmerBinomial.R") # load pseudor2 function
source("rscripts/meblr.summary.R")        # load summary function
# read in mblrdata.txt
mblrdata <- read.table("data/mblrdata.txt", # load data
                       comment.char = "",   # data does not contain comments
                       quote = "",          # data does not contain quotes
                       sep = "\t",          # data is tab separated
                       header = T)          # data has column names
str(mblrdata)                               # inspect data structure

vrs <- c("ID", "Age", "Gender", "ConversationType", "Priming")  # define variables to be factorized
fctr <- which(colnames(mblrdata) %in% vrs)     # define vector with variables
mblrdata[,fctr] <- lapply(mblrdata[,fctr], factor) # factorize variables
mblrdata$Age <- relevel(mblrdata$Age, "Young") # relevel Age (Young = Reference)

plot(table(mblrdata$ID)[order(table(mblrdata$ID), decreasing = T)],
     ylim = c(0,150),
      cex = .5)

collapsespeaker <- table(mblrdata$ID)[which(table(mblrdata$ID) < 21)]
mblrdata$ID <- ifelse(mblrdata$ID %in% collapsespeaker, "Other", mblrdata$ID)

p1 <- ggplot(mblrdata, aes(Gender, SUFlike, color = Gender)) +
  scale_fill_brewer() +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_set(theme_bw(base_size = 10)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Sex", y = "Mean frequency of discourse like") +
    guides(fill=FALSE, color=FALSE) +              # supress legend
  scale_color_manual(values = c("blue", "red"))
p2 <- ggplot(mblrdata, aes(Age, SUFlike, color = Age)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Age", y = "Mean frequency of discourse like") +
    guides(fill=FALSE, color=FALSE) +              # supress legend
  scale_color_manual(values = c("darkblue", "lightblue"))
p3 <- ggplot(mblrdata, aes(ConversationType, SUFlike, colour = ConversationType)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "ConversationType", y = "Mean frequency of discourse like", colour = "ConversationType") +
    guides(fill=FALSE, color=FALSE) +              # supress legend
  scale_color_manual(values = c("darkgreen", "lightgreen"))
p4 <- ggplot(mblrdata, aes(Priming, SUFlike, colour = Priming)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Priming", y = "Mean frequency of discourse like", colour = "Priming") +
    guides(fill=FALSE, color=FALSE) +              # supress legend
  scale_color_manual(values = c("grey30", "grey60"))
p5 <- ggplot(mblrdata, aes(Age, SUFlike, colour = Gender)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= Gender)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_set(theme_bw(base_size = 10)) +
    scale_color_manual(values = c("blue", "red")) +
  labs(x = "Age", y = "Mean frequency of discourse like", colour = "Gender")
p6 <- ggplot(mblrdata, aes(Gender, SUFlike, colour = ConversationType)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= ConversationType)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_set(theme_bw(base_size = 10)) +
  labs(x = "Sex", y = "Mean frequency of discourse like", colour = "Age") +
  scale_color_manual(values = c("darkgreen", "lightgreen"))
# display the plots
multiplot(p1, p3, p5, p2, p4, p6, cols = 2)

## Model Building
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
mblrdata.dist <- datadist(mblrdata)
options(datadist = "mblrdata.dist")
m0.glm = glm(SUFlike ~ 1, family = binomial, data = mblrdata) # baseline model glm
m0.lrm = lrm(SUFlike ~ 1, data = mblrdata, x = T, y = T) # baseline model lrm
m0.glmer = glmer(SUFlike ~ (1|ID), data = mblrdata, family = binomial) # base-line mixed-model
## Testing the Random Effect
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# test random effects
null.id = -2 * logLik(m0.glm) + 2 * logLik(m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F) # sig m0.glmer better than m0.glm

## Model Fitting
m0.glmer <- glmer(SUFlike ~ 1+ (1|ID), family = binomial, data = mblrdata, control=glmerControl(optimizer="bobyqa"))
# add Priming
ifelse(min(ftable(mblrdata$Priming, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m1.glm <- update(m0.glm, .~.+Priming)
m1.glmer <- update(m0.glmer, .~.+Priming)
anova(m1.glmer, m0.glmer, test = "Chi") # SIG (p<.001***) 

# add Age
ifelse(min(ftable(mblrdata$Age, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m2.glm <- update(m1.glm, .~.+Age)
ifelse(max(vif(m2.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs ok
m2.glmer <- update(m1.glmer, .~.+Age)
anova(m2.glmer, m1.glmer, test = "Chi") #mar sig (p=.0.61) BUT BIC inflation  

# add Gender
ifelse(min(ftable(mblrdata$Gender, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m3.glm <- update(m1.glm, .~.+Gender)
ifelse(max(vif(m3.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs ok
m3.glmer <- update(m1.glmer, .~.+Gender)
anova(m3.glmer, m1.glmer, test = "Chi") # SIG (p<.001***)  

# add ConversationType
ifelse(min(ftable(mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m4.glm <- update(m3.glm, .~.+ConversationType)
ifelse(max(vif(m4.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs ok
m4.glmer <- update(m3.glmer, .~.+ConversationType)
anova(m4.glmer, m3.glmer, test = "Chi") # SIG (p<.001***)  

# add Priming*Age
ifelse(min(ftable(mblrdata$Priming, mblrdata$Age, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m5.glm <- update(m4.glm, .~.+Priming*Age)
ifelse(max(vif(m5.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs ok
m5.glmer <- update(m4.glmer, .~.+Priming*Age)
anova(m5.glmer, m4.glmer, test = "Chi") # not sig (p=0.6)  

# add Priming*Gender
ifelse(min(ftable(mblrdata$Priming, mblrdata$Gender, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m6.glm <- update(m4.glm, .~.+Priming*Gender)
ifelse(max(vif(m6.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs unacceptable

# add Priming*ConversationType
ifelse(min(ftable(mblrdata$Priming, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m7.glm <- update(m4.glm, .~.+Priming*ConversationType)
ifelse(max(vif(m7.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs unacceptable

# add Age*Gender
ifelse(min(ftable(mblrdata$Age, mblrdata$Gender, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m8.glm <- update(m4.glm, .~.+Age*Gender)
ifelse(max(vif(m8.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs unacceptable

# add Age*ConversationType
ifelse(min(ftable(mblrdata$Age, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m9.glm <- update(m4.glm, .~.+Age*ConversationType)
ifelse(max(vif(m9.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs ok
m9.glmer <- update(m4.glmer, .~.+Age*ConversationType)
anova(m9.glmer, m4.glmer, test = "Chi") # not sig (p=0.3)  

# add Gender*ConversationType
ifelse(min(ftable(mblrdata$Gender, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m10.glm <- update(m4.glm, .~.+Gender*ConversationType)
ifelse(max(vif(m10.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs unacceptable

# add Priming*Age*Gender
ifelse(min(ftable(mblrdata$Priming,mblrdata$Age, mblrdata$Gender, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m11.glm <- update(m4.glm, .~.+Priming*Age*Gender)
ifelse(max(vif(m11.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs unacceptable

# add Priming*Age*ConversationType
ifelse(min(ftable(mblrdata$Priming,mblrdata$Age, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m12.glm <- update(m4.glm, .~.+Priming*Age*ConversationType)
ifelse(max(vif(m12.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs unacceptable

# add Priming*Gender*ConversationType
ifelse(min(ftable(mblrdata$Priming,mblrdata$Gender, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m13.glm <- update(m4.glm, .~.+Priming*Gender*ConversationType)
ifelse(max(vif(m13.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs unacceptable

# add Age*Gender*ConversationType
ifelse(min(ftable(mblrdata$Age,mblrdata$Gender, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m14.glm <- update(m4.glm, .~.+Age*Gender*ConversationType)
ifelse(max(vif(m14.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs unacceptable

# add Priming*Age*Gender*ConversationType
ifelse(min(ftable(mblrdata$Priming,mblrdata$Age,mblrdata$Gender, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m15.glm <- update(m4.glm, .~.+Priming*Age*Gender*ConversationType)
ifelse(max(vif(m15.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") # VIFs unacceptable

# comparisons of glmer objects
m1.m0 <- anova(m1.glmer, m0.glmer, test = "Chi") 
m2.m1 <- anova(m2.glmer, m1.glmer, test = "Chi") 
m3.m1 <- anova(m3.glmer, m1.glmer, test = "Chi") 
m4.m3 <- anova(m4.glmer, m3.glmer, test = "Chi") 
m5.m4 <- anova(m5.glmer, m4.glmer, test = "Chi") 
m9.m4 <- anova(m9.glmer, m4.glmer, test = "Chi") 
# create a list of the model comparisons
mdlcmp <- list(m1.m0, m2.m1, m3.m1, m4.m3, m5.m4, m9.m4)
# load function for summary
source("rscripts/ModelFittingSummarySWSU.R") # for GLMEM (step-wise step-up)
mdlft <- mdl.fttng.swsu(mdlcmp)
mdlft <- mdlft[,-2]

mlr.glmer <- m4.glmer # rename final minimal adequate model
mlr.glm <- m4.glm # rename final minimal adequate fixed-effects model
anova(mlr.glmer, m0.glmer, test = "Chi") # final model better than base-line model
print(mlr.glmer, corr = F) # inspect final minimal adequate model

anova(mlr.glmer)  # ANOVA summary

anova(m1.glmer, m0.glmer, test = "Chi") #  Priming effect

anova(m3.glmer, m1.glmer, test = "Chi") # Gender effect

anova(m4.glmer, m3.glmer, test = "Chi") #  ConversationType effect

## Extracting Model Fit Parameters
mlr.lrm <- lrm(SUFlike ~ Priming + Gender + ConversationType, data = mblrdata, x = T, y = T)
m1.glm = glm(SUFlike ~ Priming + Gender + ConversationType, family = binomial, data = mblrdata) # baseline model glm
# we now create a lmer object equivalent to the final minimal adequate model
mlr.lmer <- lmer(SUFlike ~ Age + Gender + ConversationType + (1|ID), data = mblrdata, family = binomial)
cor.test(coef(mlr.lrm), fixef(mlr.lmer))

library(Hmisc)   # activate Hmisc library
probs = 1/(1+exp(-fitted(mlr.lmer)))
probs = binomial()$linkinv(fitted(mlr.lmer))
somers2(probs, as.numeric(mblrdata$SUFlike))

## Model Diagnostics
plot(mlr.glmer)

# plot residuals against fitted
stripParams <- list(cex=.3, lines=1.5)
plot(mlr.glmer, form = resid(., type = "response") ~ fitted(.) | ID, abline = 0, par.strip.text = stripParams,cex = .3,id = 0.05, adj = -0.3)

# diagnostic plot: examining residuals (Pinheiro & Bates 2000:175)
plot(mlr.glmer, ID ~ resid(.), abline = 0 , cex = .5)

# summarize final model
mblrmtb <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, dpvar=mblrdata$SUFlike)
mblrmtb <- mblrmtb[, -c(4:5)]

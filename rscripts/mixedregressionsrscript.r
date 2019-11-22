
# "Mixed-Effects Regression Models"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("car", "dplyr", "effects", "ggplot2", "Hmisc", "knitr",
                   "languageR", "lme4", "mlogit", "nlme", "RLRsim", "rms", 
                   "sjPlot", "visreg"))
# random intercepts and random slops
x <- 0:10
y = 0:10
# start plot
par(mfrow = c(1, 4))
# intercepts
plot(x, y, type = "n", xaxt='n', yaxt='n', ylab='Weight', xlab = "Height", xlim = c(0, 10), ylim = c(-5, 10))
axis(2, seq(-5,10, 5), seq(50, 110, 20))
abline(0, 1, lty = 1, col ="black")
mtext("Fixed-Effects Model:\n1 Intercept + 1 Slope", 1, 2, cex = .6)
box()
# random intercepts
plot(x, y, type = "n", xaxt='n', yaxt='n', ann=FALSE, xlim = c(0, 10), ylim = c(-5, 10))
abline(4, 1, col ="black")
abline(2, 1, col ="black")
abline(2, 1, col ="black")
abline(0, 1, col ="black")
abline(-1, 1, col ="black")
abline(-2, 1, col ="black")
abline(-4, 1, col ="black")
mtext("Mixed-Effects Model:\n1 Intercept per Random Effect Level\n+ 1 Slope", 1, 3, cex = .6)
box()
# random slopes
plot(x, y, type = "n", xaxt='n', yaxt='n', ann=FALSE, xlim = c(0, 10), ylim = c(-5, 10))
abline(0, 1.75, col ="black")
abline(0, 1.5, col ="black")
abline(0, 1.25, col ="black")
abline(0, 0, col ="black")
abline(0, -.25, col ="black")
abline(0, -.5, col ="black")
abline(0, -.75, col ="black")
mtext("Mixed-Effects Model:\n1 Intercept\n+ 1 Slope per Random Effect Level", 1, 3, cex = .6)
box()
# random slopesund random intercepts
plot(x, y, type = "n", xaxt='n', yaxt='n', ann=FALSE, xlim = c(0, 10), ylim = c(-5, 10))
abline(2, 1.75, col ="black")
abline(-1, 1.5, col ="black")
abline(1, 1.25, col ="black")
abline(4, 0, col ="black")
abline(-4, -.25, col ="black")
abline(0, -.5, col ="black")
abline(-1, -.75, col ="black")
mtext("Mixed-Effects Model:\n1 Intercept per Random Effect Level\n+ 1 Slope per Random Effect Level", 1, 3, cex = .6)
box()
# restore original graphic's parameters
par(mfrow = c(1, 1))
Height <- c(169, 176, 164, 160, 158, 173, 166, 161, 180, 187, 170, 177, 163, 161, 157)
Weight <- c(68, 72, 65, 62, 60, 80, 75, 70, 85, 92, 88, 92, 85, 82, 80) # plot scatterplot and the regression line
z <- c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "c", "c", "c", "c", "c")
tb <- data.frame(Height,Weight, z)
a <- tb[z == "a", ]
a <- a[, 1:2]
b <- tb[z == "b", ]
b <- b[, 1:2]
c <- tb[z == "c", ]
c <- c[, 1:2]
d <- tb[, 1:2]
# plot
par(mfrow = c(1, 3))
# plot 1
plot(a, xlim = c(150, 200), ylim = c(50, 100))
text(b[,1], b[,2], "+")
text(c[,1], c[,2], "*")
# plot 2
plot(a, xlim = c(150, 200), ylim = c(50, 100))
mod0 <- lm(d$Weight ~ d$Height, data = d)
abline(mod0, lty=1, col = "black")
text(b[,1], b[,2], "+")
text(c[,1], c[,2], "*")
# plot 3
plot(a, xlim = c(150, 200), ylim = c(50, 100))
grid()
mod1 <- lm(a$Weight ~ a$Height, data = a)
abline(mod0, lty=1, col = "black")
abline(mod0[[1]][[1]]+10, mod0[[1]][[2]], lty = 2, col = "red")
abline(mod0[[1]][[1]]-10, mod0[[1]][[2]], lty = 3, col = "blue")
abline(mod0[[1]][[1]]-1, mod0[[1]][[2]], lty = 4, col = "green")
text(b[,1], b[,2], "+")
text(c[,1], c[,2], "*")
par(mfrow = c(1, 1))
# activate packages
library(Boruta)
library(car)
library(dplyr)
library(FSA)
library(effects)
library(ggplot2)
library(Hmisc)
library(knitr)
library(languageR)
library(lme4)
library(MASS)
library(mlogit)
library(nlme)
library(RLRsim)
library(rms)
library(sjPlot)
library(stringr)
library(tidyr)
library(vcd)
library(visreg)
# load functions
source("https://slcladal.github.io/rscripts/multiplot_ggplot2.r")   
source("https://slcladal.github.io/rscripts/PseudoR2lmerBinomial.r") 
source("https://slcladal.github.io/rscripts/meblr.summary.r")  
source("https://slcladal.github.io/rscripts/eflme.r")
source("https://slcladal.github.io/rscripts/ModelFittingSummarySWSU.r") 
# supress scientific notation
options("scipen" = 100, "digits" = 4)      
# do not convert strings into factors
options(stringsAsFactors = F)              
# read in data
lmmdata <- read.delim("https://slcladal.github.io/data/lmmdata.txt", header = TRUE) %>%
# convert date into a numeric variable
    dplyr::mutate(Date = as.numeric(Date))
# inspect updated data set
head(lmmdata); nrow(lmmdata) 
# visualize variables (2 plots per row)
# 3 plots in 1 window
def.par <- par(no.readonly = TRUE)
nf <- layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = T))
plot(lmmdata$Prepositions ~ lmmdata$Date, ylab = "Frequency", xlab = "Year of publication", ylim = c(0, 200))
abline(lm(lmmdata$Prepositions ~ lmmdata$Date), lty = 3, lwd = 2, col = "red")
# re-set margins to fit the labels
par(mar = c(7.2, 4, 1, 2) + 0.1)
# reorder Genre by median
Genrebymedian <- with(lmmdata, reorder(Genre, -Prepositions, median))
#	generate plots
plot(lmmdata$Prepositions ~ Genrebymedian,
  col = "lightgrey",
  ylab = "Frequency",
  xlab = "",
  las = 2,
  cex.axis = .7,
  cex = .5,
  ylim = c(0,200))
# re-set margins
par(mar = c(5, 4, 1, 2) + 0.1)
x = lmmdata$Prepositions
h = hist(lmmdata$Prepositions,
	ylim =c(0, 200),
	xlim = c(50, 200),
	xlab = "Prepositions per text",
	col = "lightgrey",
	main = "")
xfit <- seq(min(lmmdata$Prepositions), max(lmmdata$Prepositions), length = 40)
yfit <- dnorm(xfit, mean = mean(lmmdata$Prepositions),sd = sd(lmmdata$Prepositions))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, lty = 2, lwd=2); par(def.par)# restore original graphic's parameters
# plot 8
p8 <- ggplot(lmmdata, aes(Date, Prepositions)) +
  geom_point() +
  labs(x = "Year") +
  labs(y = "Prepositions per 1,000 words") +
  geom_smooth(method = "lm")  + 
  theme_set(theme_bw(base_size = 10))
# plot 9
p9 <- ggplot(lmmdata, aes(Region, Prepositions)) +
  geom_boxplot() +
  labs(x = "Region") +
  labs(y = "Prepositions per 1,000 words") +
  geom_smooth(method = "lm") # with linear model smoothing!
# include genre (lowess)
multiplot(p8, p9, cols = 2)
ggplot(lmmdata, aes(Date, Prepositions)) +
  geom_point() +
  facet_wrap(~ Genre, nrow = 4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Date of composition") +
  labs(y = "Prepositions per 1,000 words") +
  coord_cartesian(ylim = c(0, 220))
lmmdata$DateUnscaled <- lmmdata$Date
lmmdata$Date <- scale(lmmdata$Date, scale = F)
# inspect data
head(lmmdata); str(lmmdata)
# generate models
m0.glm <- glm(Prepositions ~ 1, family = gaussian, data = lmmdata)
m0.glmer = glmer(Prepositions ~ 1 + (1|Genre), data = lmmdata, family = "gaussian", REML = F)
x2 = -2*logLik(m0.glm, REML = T)+2*logLik(m0.glmer, REML = T)
x2 <- x2 <- x2[[1]]
list(x2, pchisq(x2, df=2, lower.tail=F))
m1.glmer <- glmer(Prepositions ~ (1|Genre) + Date, data = lmmdata, REML = T)
anova(m1.glmer, m0.glmer, test = "Chi")
m2.glmer <- update(m1.glmer, .~.+Region)
# compare models                
anova(m2.glmer, m1.glmer, test = "Chi")
m3.glmer <- update(m1.glmer, .~.+Region*Date)
# compare models                
anova(m3.glmer, m1.glmer, test = "Chi")
# inspect results
summary(m1.glmer)
plot(m1.glmer, Genre ~ resid(.), abline = 0 ) # generate diagnostic plots
plot(m1.glmer, resid(., type = "pearson") ~ fitted(.) | Genre, id = 0.05, 
     adj = -0.3, pch = 20, col = "gray40")
m4.lme = lme(Prepositions ~ Date, random = ~1|Genre, data = lmmdata, method = "ML")
m5.lme <- update(m4.lme, weights = varIdent(form = ~ 1 | Genre))
# compare models
anova(m5.lme, m4.lme)
# inspect results
summary(m5.lme)        
anova(m5.lme)          
# creat base-line model
m0.lme = lme(Prepositions ~ 1, random = ~1|Genre, data = lmmdata, method = "ML", weights = varIdent(form = ~ 1 | Genre))
anova(m5.lme, m0.lme)  # test if date is significant
# extract estimates and sd for fixed and random effects
intervals(m5.lme)      
# calculate effect size 
ef.lme(m5.lme)
# extract predicted values
lmmdata$Predicted <- predict(m5.lme, lmmdata)
# plot predicted values
ggplot(lmmdata, aes(DateUnscaled, Predicted)) +
  facet_wrap(~Genre) +
  geom_point(aes(x = DateUnscaled, y = Prepositions), color = "gray80", size = .5) +
  geom_smooth(aes(y = Predicted), color = "gray20", linetype = "solid", 
              se = T, method = "lm") +
  guides(color=guide_legend(override.aes=list(fill=NA))) +  
  theme_set(theme_bw(base_size = 10)) +
  theme(legend.position="top", legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  xlab("Date of composition")
# start plotting
par(mfrow = c(2, 2))           # display plots in 2 rows and 2 columns
plot(m5.lme, pch = 20, col = "black", lty = "dotted")
par(mfrow = c(1, 1))
# fitted values by Genre
plot(m5.lme, form = resid(., type = "p") ~ fitted(.) | Genre, abline = 0, 
     cex = .5, pch = 20, col = "black")
# residuals of fitted values against observed
qqnorm(m5.lme, pch = 20, col = "black")
# residuals by genre
qqnorm(m5.lme, ~resid(.) | Genre, pch = 20, col = "black" )
# observed responses versus the within-group fitted values
plot(m5.lme, Prepositions ~ fitted(.), id = 0.05, adj = -0.3, 
     xlim = c(80, 220), cex = .8, pch = 20, col = "blue")
summary(m5.lme)
x1 <- c(62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 72.5, 73.5, 74.5, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86)
x2 <- x1-2
x3 <- x2-2
x4 <- x3-2
x5 <- x1+2
x6 <- x5+2
x7 <- x6+2
x11 <- x1-(mean(x1)-x1)
x12 <- x1-(mean(x1)-x1)*1.5
x13 <- x1-(mean(x1)-x1)*3
x14 <- x1-(mean(x1)-x1)^1.5
x15 <- x1-(mean(x1)-x1)^1.75
x16 <- x1-(mean(x1)-x1)^.9
x17 <- x1-(mean(x1)-x1)^.5
x21 <- x1-(mean(x1)-x1)
x22 <- x1-(mean(x1)-x1)*1.5
x23 <- x1-(mean(x1)-x1)*3
x24 <- x1-(mean(x1)-x1)*1.5
x25 <- x1-(mean(x1)-x1)*2
x26 <- x1-(mean(x1)-x1)*.9
x27 <- x1-(mean(x1)-x1)*.5
y <- c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B")
yn <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) 
logd <- data.frame(x1, x2, x3, x4, x5, x6, x7, y, yn)
colnames(logd) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "y", "yn")
m1 = glm(yn ~ x1, data=logd, family = binomial(link="logit"))
m2 = glm(yn ~ x2, data=logd, family = binomial(link="logit"))
m3 = glm(yn ~ x3, data=logd, family = binomial(link="logit"))
m4 = glm(yn ~ x4, data=logd, family = binomial(link="logit"))
m5 = glm(yn ~ x5, data=logd, family = binomial(link="logit"))
m6 = glm(yn ~ x6, data=logd, family = binomial(link="logit"))
m7 = glm(yn ~ x7, data=logd, family = binomial(link="logit"))
m11 = glm(yn ~ x11, data=logd, family = binomial(link="logit"))
m12 = glm(yn ~ x12, data=logd, family = binomial(link="logit"))
m13 = glm(yn ~ x13, data=logd, family = binomial(link="logit"))
m14 = glm(yn ~ x14, data=logd, family = binomial(link="logit"))
m15 = glm(yn ~ x15, data=logd, family = binomial(link="logit"))
m16 = glm(yn ~ x16, data=logd, family = binomial(link="logit"))
m17 = glm(yn ~ x17, data=logd, family = binomial(link="logit"))
m21 = glm(yn ~ x21, data=logd, family = binomial(link="logit"))
m22 = glm(yn ~ x22, data=logd, family = binomial(link="logit"))
m23 = glm(yn ~ x23, data=logd, family = binomial(link="logit"))
m24 = glm(yn ~ x24, data=logd, family = binomial(link="logit"))
m25 = glm(yn ~ x25, data=logd, family = binomial(link="logit"))
m26 = glm(yn ~ x26, data=logd, family = binomial(link="logit"))
m27 = glm(yn ~ x27, data=logd, family = binomial(link="logit"))
par(mfrow = c(2, 2))
plot(yn  ~ x1, type = "n", xaxt='n', yaxt='n', ann=FALSE, data = logd, xlab="x1", ylab="yn", pch=19)       
axis(2, seq(0,1,1), seq(0,1,1))
curve(predict(m1,data.frame(x1=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
mtext("Fixed-Effects Model:\n1 Intercept + 1 Slope", 1, 2, cex = .6)
mtext("Probability", 2, 2, cex = .6)
plot(yn  ~ x1, type = "n", xaxt='n', yaxt='n', ann=FALSE, data = logd, xlab="x1", ylab="yn", pch=19)     
mtext("Mixed-Effects Model:\n1 Intercept per Random Effect Level\n+ 1 Slope", 1, 3, cex = .6)
curve(predict(m1,data.frame(x1=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m2,data.frame(x2=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m3,data.frame(x3=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m4,data.frame(x4=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m5,data.frame(x5=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m6,data.frame(x6=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m7,data.frame(x7=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
plot(yn  ~ x11, type = "n", xaxt='n', yaxt='n', ann=FALSE, data = logd, xlim=c(50,100), ylab="yn", pch=19) 
mtext("Mixed-Effects Model:\n1 Intercept\n+ 1 Slope per Random Effect Level", 1, 3, cex = .6)
mtext("Probability", 2, 2, cex = .6)
curve(predict(m21,data.frame(x21=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m22,data.frame(x22=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m23,data.frame(x23=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m24,data.frame(x24=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m25,data.frame(x25=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m26,data.frame(x26=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m27,data.frame(x27=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
plot(yn  ~ x11, type = "n", xaxt='n', yaxt='n', ann=FALSE, data = logd, xlim=c(50,100), ylab="yn", pch=19) 
mtext("Mixed-Effects Model:\n1 Intercept per Random Effect Level\n+ 1 Slope per Random Effect Level", 1, 3, cex = .6)
curve(predict(m11,data.frame(x11=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m12,data.frame(x12=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m13,data.frame(x13=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m14,data.frame(x14=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m15,data.frame(x15=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m16,data.frame(x16=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
curve(predict(m17,data.frame(x17=x),type="response"), lty=1, lwd=1, col="darkgrey", add=TRUE)
par(mfrow = c(1, 1))
# load data
mblrdata <- read.table("https://slcladal.github.io/data/mblrdata.txt", 
                       comment.char = "",# data does not contain comments
                       quote = "",       # data does not contain quotes
                       sep = "\t",       # data is tab separated
                       header = T)       # data has column names
# inspect data structure
str(mblrdata)
# def. variables to be factorized
vrs <- c("ID", "Age", "Gender", "ConversationType", "Priming")
# def. vector with variables
fctr <- which(colnames(mblrdata) %in% vrs)     
# factorize variables
mblrdata[,fctr] <- lapply(mblrdata[,fctr], factor)
# relevel Age (Young = Reference)
mblrdata$Age <- relevel(mblrdata$Age, "Young")
# order data by ID
mblrdata <- mblrdata %>%
  dplyr::arrange(ID)
kable(head(mblrdata), caption = "First six rows of the data set.")
ggplot(mblrdata, aes(Gender, SUFlike, color = Priming)) +
  facet_wrap(Age~ConversationType) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_set(theme_bw(base_size = 10)) +
  theme(legend.position = "top") +
  labs(x = "", y = "Observed Probabilty of discourse like") +
  scale_color_manual(values = c("gray20", "gray70"))
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
mblrdata.dist <- datadist(mblrdata)
options(datadist = "mblrdata.dist")
# baseline model glm
m0.glm = glm(SUFlike ~ 1, family = binomial, data = mblrdata) 
# baseline model lrm
m0.lrm = lrm(SUFlike ~ 1, data = mblrdata, x = T, y = T) 
# base-line mixed-model
m0.glmer = glmer(SUFlike ~ (1|ID), data = mblrdata, family = binomial) 
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm
# test random effects
null.id = -2 * logLik(m0.glm) + 2 * logLik(m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F) 
# sig m0.glmer better than m0.glm
m0.glmer <- glmer(SUFlike ~ 1+ (1|ID), family = binomial, data = mblrdata, control=glmerControl(optimizer="bobyqa"))
# add Priming
ifelse(min(ftable(mblrdata$Priming, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m1.glm <- update(m0.glm, .~.+Priming)
m1.glmer <- update(m0.glmer, .~.+Priming)
anova(m1.glmer, m0.glmer, test = "Chi") 
# add Age
ifelse(min(ftable(mblrdata$Age, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m2.glm <- update(m1.glm, .~.+Age)
ifelse(max(vif(m2.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") 
m2.glmer <- update(m1.glmer, .~.+Age)
anova(m2.glmer, m1.glmer, test = "Chi")   
Anova(m2.glmer, test = "Chi")
# add Gender
ifelse(min(ftable(mblrdata$Gender, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m3.glm <- update(m1.glm, .~.+Gender)
ifelse(max(vif(m3.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") 
m3.glmer <- update(m1.glmer, .~.+Gender)
anova(m3.glmer, m1.glmer, test = "Chi")
Anova(m3.glmer, test = "Chi")
# add ConversationType
ifelse(min(ftable(mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m4.glm <- update(m3.glm, .~.+ConversationType)
ifelse(max(vif(m4.glm)) <= 3,  "VIFs okay", "VIFs unacceptable") 
m4.glmer <- update(m3.glmer, .~.+ConversationType)
anova(m4.glmer, m3.glmer, test = "Chi") 
Anova(m4.glmer, test = "Chi")
# add Priming*Age
ifelse(min(ftable(mblrdata$Priming, mblrdata$Age, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m5.glm <- update(m4.glm, .~.+Priming*Age)
ifelse(max(vif(m5.glm)) <= 20,  "VIFs okay", "WARNING: high VIFs!") # VIFs ok
m5.glmer <- update(m4.glmer, .~.+Priming*Age)
anova(m5.glmer, m4.glmer, test = "Chi") 
# add Priming*Gender
ifelse(min(ftable(mblrdata$Priming, mblrdata$Gender, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m6.glm <- update(m4.glm, .~.+Priming*Gender)
m6.glmer <- update(m4.glmer, .~.+Priming*Gender)
anova(m6.glmer, m4.glmer, test = "Chi") 
Anova(m6.glmer, test = "Chi")
# add Priming*ConversationType
ifelse(min(ftable(mblrdata$Priming, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m7.glm <- update(m6.glm, .~.+Priming*ConversationType)
ifelse(max(vif(m7.glm)) <= 20,  "VIFs okay", "WARNING: high VIFs!")
m7.glmer <- update(m6.glmer, .~.+Priming*ConversationType)
anova(m7.glmer, m6.glmer, test = "Chi")
# add Age*Gender
ifelse(min(ftable(mblrdata$Age, mblrdata$Gender, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m8.glm <- update(m6.glm, .~.+Age*Gender)
ifelse(max(vif(m8.glm)) <= 20,  "VIFs okay", "WARNING: high VIFs!")
m8.glmer <- update(m6.glmer, .~.+Age*Gender)
anova(m8.glmer, m6.glmer, test = "Chi") 
# add Age*ConversationType
ifelse(min(ftable(mblrdata$Age, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m9.glm <- update(m6.glm, .~.+Age*ConversationType)
ifelse(max(vif(m9.glm)) <= 20,  "VIFs okay", "WARNING: high VIFs!") 
m9.glmer <- update(m6.glmer, .~.+Age*ConversationType)
anova(m9.glmer, m6.glmer, test = "Chi") 
# add Gender*ConversationType
ifelse(min(ftable(mblrdata$Gender, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m10.glm <- update(m6.glm, .~.+Gender*ConversationType)
ifelse(max(vif(m10.glm)) <= 20,  "VIFs okay", "WARNING: high VIFs!")
m10.glmer <- update(m6.glmer, .~.+Gender*ConversationType)
anova(m10.glmer, m6.glmer, test = "Chi") 
# add Priming*Age*Gender
ifelse(min(ftable(mblrdata$Priming,mblrdata$Age, mblrdata$Gender, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m11.glm <- update(m6.glm, .~.+Priming*Age*Gender)
ifelse(max(vif(m11.glm)) <= 20,  "VIFs okay", "WARNING: high VIFs!")
m11.glmer <- update(m6.glmer, .~.+Priming*Age*Gender)
anova(m11.glmer, m6.glmer, test = "Chi") 
# add Priming*Age*ConversationType
ifelse(min(ftable(mblrdata$Priming,mblrdata$Age, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m12.glm <- update(m6.glm, .~.+Priming*Age*ConversationType)
ifelse(max(vif(m12.glm)) <= 20,  "VIFs okay", "WARNING: high VIFs!")
m12.glmer <- update(m6.glmer, .~.+Priming*Age*ConversationType)
anova(m12.glmer, m6.glmer, test = "Chi")
# add Priming*Gender*ConversationType
ifelse(min(ftable(mblrdata$Priming,mblrdata$Gender, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m13.glm <- update(m6.glm, .~.+Priming*Gender*ConversationType)
ifelse(max(vif(m13.glm)) <= 20,  "VIFs okay", "WARNING: high VIFs!")
vif(m13.glm)
m13.glmer <- update(m6.glmer, .~.+Priming*Gender*ConversationType)
anova(m13.glmer, m6.glmer, test = "Chi")
Anova(m13.glmer, test = "Chi")
# add Age*Gender*ConversationType
ifelse(min(ftable(mblrdata$Age,mblrdata$Gender, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m14.glm <- update(m13.glm, .~.+Age*Gender*ConversationType)
ifelse(max(vif(m14.glm)) <= 20,  "VIFs okay", "WARNING: high VIFs!")
vif(m14.glm)
# add Priming*Age*Gender*ConversationType
ifelse(min(ftable(mblrdata$Priming,mblrdata$Age,mblrdata$Gender, mblrdata$ConversationType, mblrdata$SUFlike)) == 0, "incomplete information", "okay")
m15.glm <- update(m13.glm, .~.+Priming*Age*Gender*ConversationType)
ifelse(max(vif(m15.glm)) <= 20,  "VIFs okay", "WARNING: high VIFs!") 
vif(m15.glm)
# comparisons of glmer objects
m1.m0 <- anova(m1.glmer, m0.glmer, test = "Chi") 
m2.m1 <- anova(m2.glmer, m1.glmer, test = "Chi")   
m3.m1 <- anova(m3.glmer, m1.glmer, test = "Chi")
m4.m3 <- anova(m4.glmer, m3.glmer, test = "Chi") 
m5.m4 <- anova(m5.glmer, m4.glmer, test = "Chi") 
m6.m4 <- anova(m6.glmer, m4.glmer, test = "Chi") 
m7.m6 <- anova(m7.glmer, m6.glmer, test = "Chi")
m8.m6 <- anova(m8.glmer, m6.glmer, test = "Chi") 
m9.m6 <- anova(m9.glmer, m6.glmer, test = "Chi") 
m10.m6 <- anova(m10.glmer, m6.glmer, test = "Chi") 
m11.m6 <- anova(m11.glmer, m6.glmer, test = "Chi") 
m12.m6 <- anova(m12.glmer, m6.glmer, test = "Chi")
m13.m6 <- anova(m13.glmer, m6.glmer, test = "Chi")
# create a list of the model comparisons
mdlcmp <- list(m1.m0, m2.m1, m3.m1, m4.m3, m5.m4, m6.m4, m7.m6, m8.m6, m9.m6, m10.m6, m11.m6, m12.m6, m13.m6)
# summary table for model fitting
mdlft <- mdl.fttng.swsu(mdlcmp)
mdlft <- mdlft[,-2]
kable(mdlft, caption = "Model fitting process summary.")
mlr.glmer <- m13.glmer # rename final minimal adequate model
mlr.glm <- m13.glm # rename final minimal adequate fixed-effects model
anova(mlr.glmer, m0.glmer, test = "Chi") # final model better than base-line model
print(mlr.glmer, corr = F) # inspect final minimal adequate model
anova(m1.glmer, m0.glmer, test = "Chi") 
# extract predicted values
mblrdata$Predicted <- predict(m13.glmer, mblrdata, type = "response")
# plot
ggplot(mblrdata, aes(Gender, Predicted, color = Priming)) +
  facet_wrap(~ConversationType) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_set(theme_bw(base_size = 10)) +
  theme(legend.position = "top") +
    ylim(0, .75) +
  labs(x = "", y = "Predicted Probabilty of discourse like") +
  scale_color_manual(values = c("gray20", "gray70"))
mlr.lrm <- lrm(SUFlike ~ Priming + Gender + ConversationType, data = mblrdata, x = T, y = T)
m1.glm = glm(SUFlike ~ Priming + Gender + ConversationType, family = binomial, data = mblrdata) # baseline model glm
# we now create a lmer object equivalent to the final minimal adequate model
mlr.lmer <- lmer(SUFlike ~ Age + Gender + ConversationType + (1|ID), data = mblrdata, family = binomial)
cor.test(coef(mlr.lrm), fixef(mlr.lmer))
probs = 1/(1+exp(-fitted(mlr.lmer)))
probs = binomial()$linkinv(fitted(mlr.lmer))
somers2(probs, as.numeric(mblrdata$SUFlike))
plot(mlr.glmer, pch = 20, col = "black", lty = "dotted")
# summarize final model
mblrmtb <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, dpvar=mblrdata$SUFlike)
kable(mblrmtb[, -c(4:5)], caption = "Summary of the final minimal adequate binomial logistic mixed-effects regression model which was fitted to predictors of discourse like in New Zealand English.")
# load data
countdata <- read.table("https://slcladal.github.io/data/countdata.txt", 
                       comment.char = "",quote = "", sep = "\t", header = T) 
# inspect data
str(countdata)
# factorize variables
countdata <- countdata %>%
  dplyr::select(-ID)
clfct <- c("Trial", "Language", "Gender")
countdata[clfct] <- lapply(countdata[clfct], factor)
 # inspect data
str(countdata); head(countdata)
p1d <- countdata %>%
  dplyr::select(Language, Shots) %>%
  dplyr::group_by(Language) %>%
  dplyr::mutate(Mean = round(mean(Shots), 1)) %>%
  dplyr::mutate(SD = round(sd(Shots), 1))
# start plot
ggplot(p1d, aes(Language, Shots, color = Language, fill = Language)) +
  geom_violin(trim=FALSE, color = "gray20")+ 
  geom_boxplot(width=0.1, fill="white", color = "gray20") +
  geom_text(aes(y=-4,label=paste("mean: ", Mean, sep = "")), size = 3, color = "black") +
  geom_text(aes(y=-5,label=paste("SD: ", SD, sep = "")), size = 3, color = "black") +
  scale_fill_manual(values=rep("grey90",4)) + 
  theme_set(theme_bw(base_size = 10)) +
  theme(legend.position="none", legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ylim(-5, 15) +
  labs(x = "Language", y = "Shots")
# perform variable selection
set.seed(20191220)
boruta <- Boruta(UHM ~.,data=countdata)
print(boruta)
# output the results
gf = goodfit(countdata$UHM,type= "poisson",method= "ML")
plot(gf,main="Count data vs Poisson distribution")
summary(gf)
# check homogeneity
leveneTest(countdata$UHM, countdata$Shots, center = mean)
# base-line mixed-model
m0.glmer = glmmPQL(UHM ~ 1, random = ~ 1 | Language, data = countdata, 
                   family = quasipoisson(link='log'))
# create glm base line model
m0.glm = glm(UHM ~ 1, data = countdata, family = quasipoisson(link='log'))
# add Shots
m1.glm <- update(m0.glm, .~.+ Shots)
m1.glmer <- update(m0.glmer, .~.+ Shots)
Anova(m1.glmer, test = "Chi")           # SIG! (p<0.0000000000000002 ***)
summary(m1.glmer)
#summary(m1.glm)
exp(coef(m1.glm))
# diagnostic plot
plot(m1.glmer, pch = 20, col = "black", lty= "dotted", ylab = "Pearson's residuals")
# generate diagnostic plots
plot(m1.glmer, Language ~ resid(.), abline = 0, fill = "gray70") 
# create effect plot for comparison purposes
plot(predictorEffects(m1.glmer)) 
# create table with predicted values for uhms by shots
eff_cf <- predictorEffects(m1.glmer)
eff_df <- data.frame(eff_cf)
# create data frame with effects
ef <- as.data.frame(effect("Shots", m1.glmer, xlevels=list(Shots=seq(0, 13, 1))))
ef$Type <- rep("Predicted", 14)
ef$lower <- NULL
ef$upper <- NULL
ef <- ef %>%
  dplyr::filter(Shots < 10) # remove predictions for 10 shots or more
# create table with observed values for uhms by shots
UHM_mean <- t(tapply(countdata$UHM, countdata$Shots, mean))
UHM_se <- t(tapply(countdata$UHM, countdata$Shots, se))
Shots <- c(seq(0,10,1), 12:13)  
df <- data.frame(Shots, UHM_mean[1,], UHM_se[1,])
colnames(df) <- c("Shots", "fit", "se")
df$Type <- rep("Observed", 13)
df <- df %>%
  dplyr::filter(Shots < 10) # remove predictions for 10 shots or more
# combine observed and predicted table
tbd <- rbind(ef, df)
# inspect data
head(tbd)
# plot observed and expected
ggplot(tbd, aes(Shots, fit, color = Type)) +
  geom_point() + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.5) +
  theme_set(theme_bw(base_size = 20)) +
  scale_color_manual(values = c("grey20", "gray70")) +
  theme(legend.position="none", legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  ylim(-2.5, 5) +
  labs(x = "Shots", y = "Number of uhm fillers")
The effect plot shows that the predicted number of shots incerases linearly with each shot, the observed number of shots plateus once a speaker had 5 shots (or more). We will now summarize the results as if the violations had NOT occurred(!) - again: thsi is onyl because we are practicing here - this would be absolutely unacceptable in a proper write-up of an analysis!
A mixed-effect pseudo-Poisson regression model which contained the trial as random effect was fit to to the data. Prior to the model fitting process, a Boruta analysis was applied to determine whether any of the predictors had a meaningful relationship with the dependent variable (instances of *uhm*). Since the Boruta analysis indicated that only the number of shots that speakers have had was important, only "Shots" was tested during model fitting. The final minimal adequate model showed that the number of *uhm* as fillers increases significantly, and near-linearly with the number of shots speakers had ($\chi$^2^(1):276.0, p <.0001, $\beta$: 1.2632). An inspection of the random effect structure conveyed that there was almost no variability between languages.
# References

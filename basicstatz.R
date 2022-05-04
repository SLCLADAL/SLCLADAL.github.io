knitr::include_graphics("https://slcladal.github.io/images/uq1.jpg")

knitr::include_graphics("https://slcladal.github.io/images/gy_chili.jpg")

## # install packages
## install.packages("tidyverse")
## install.packages("VGAM")
## install.packages("fGarch")
## install.packages("cfa")
## install.packages("gridExtra")
## install.packages("lawstat")
## install.packages("calibrate")
## install.packages("car")
## # install.packages("QuantPsyc")
## install.packages("flextable")
## install.packages("e1071")
## install.packages("effectsize")
## install.packages("tibble")
## install.packages("report")
## install.packages("here")
## # install klippy for copy-to-clipboard button in code chunks
## install.packages("remotes")
## remotes::install_github("rlesur/klippy")

# load packages
library(tidyverse)    # for data processing
library(tidyr)        # for data transformation
library(flextable)    # for creating tables
library(here)         # for generating easy paths
library(e1071)        # for checking assumptions
library(lawstat)      # for statistical tests
library(effectsize)   # for calculating effect sizes
library(fGarch)       # for statistical tests
library(gridExtra)    # for plotting
library(report)       # for reports
# activate klippy for copy-to-clipboard button
klippy::klippy()

# data for indep. t-test
itdata  <- base::readRDS(url("https://slcladal.github.io/data/itdata.rda", "rb"))
# data for paired t-test
ptdata  <- base::readRDS(url("https://slcladal.github.io/data/ptdata.rda", "rb"))
# data for fishers exact test
fedata  <- base::readRDS(url("https://slcladal.github.io/data/fedata.rda", "rb"))
# data for mann-whitney u-test
mwudata  <- base::readRDS(url("https://slcladal.github.io/data/mwudata.rda", "rb"))
# data for wilcox test
uhmdata  <- base::readRDS(url("https://slcladal.github.io/data/uhmdata.rda", "rb"))
# data for friedmann-test
frdata  <- base::readRDS(url("https://slcladal.github.io/data/frdata.rda", "rb"))
# data for x2-test
x2data  <- base::readRDS(url("https://slcladal.github.io/data/x2data.rda", "rb"))
# data for extensions of x2-test
x2edata  <- base::readRDS(url("https://slcladal.github.io/data/x2edata.rda", "rb"))
# multi purpose data
mdata <- base::readRDS(url("https://slcladal.github.io/data/mdata.rda", "rb")) 

ndata <- mdata %>%
  dplyr::rename(Gender = sex,
                Words = word.count) %>%
  dplyr::select(Gender, Words) %>%
  dplyr::filter(!is.na(Words),
                !is.na(Gender)) %>%
  dplyr::group_by(Gender) %>%
  dplyr::sample_n(100)

 ndata %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

ggplot(ndata, aes(x = Words)) +
  facet_grid(~Gender) +
  geom_histogram(aes(y=..density..)) +
  geom_density(aes(y=..density..), color = "red") +
  theme_bw() +
  labs(title = "Histograms with denisty of words uttered by men and women in a sample corpus")

ggplot(ndata, aes(sample = Words)) +
  facet_grid(~Gender) +
  geom_qq() +
  geom_qq_line(color = "red") +
  theme_bw() +
  labs(title = "QQ-plot of words uttered by men and women in a sample corpus", x = "", y = "")

skew <- data.frame(rbeta(1000000,10,2)) %>%
  mutate(pskew = rbeta(1000000,2,10),
         nrm = rbeta(1000000,10,10)) %>%
  dplyr::rename(nskew = colnames(.)[1],
                pskew = colnames(.)[2],
                nrm = colnames(.)[3])
# inspect
ggplot(skew, aes(x = nskew, alpha = .5)) +
  geom_density(fill = "lightgreen", alpha = .5, color = "lightgreen") +
  geom_density(aes(x = pskew), fill = "orange", alpha = .5, color = "orange") +
  geom_density(aes(x = nrm), fill = "lightgray", alpha = .5, color = "lightgray") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Positively skewed distribution (orange), \nnormal, non-skewed distribution (lightgray), \nnegatively skewed distribution (lightgreen)", x = "", y = "Density")

# extract word counts for one group
words_women <- ndata %>%
  dplyr::filter(Gender == "female") %>%
  dplyr::pull(Words)
# inspect
head(words_women)

summary(words_women)

skewness(words_women, type = 2)            

kur <- data.frame(x <- seq(-4, 4, length.out = 100)) %>%
  mutate(lep = dt(x, 1)^2*5,
         nrm = dnorm(x),
         thk = dt(x, 1)) %>%
  dplyr::rename(x = colnames(.)[1],
                lepto = colnames(.)[2],
                normal = colnames(.)[3],
                platy = colnames(.)[4])
ggplot(kur, aes(x, lepto)) + 
#  geom_line(color = "orange", size = 1.5, linetype = 2, alpha = .5) +
  geom_ribbon(aes(x = x, ymax = lepto), ymin=0, alpha=0.5, fill = "orange") +
  geom_ribbon(aes(x = x, ymax = normal), ymin=0, alpha=0.5, fill = "lightgray") +
  geom_ribbon(aes(x = x, ymax = platy), ymin=0, alpha=0.5, fill = "lightgreen") +
  theme_bw() + 
  labs(title = "Leptokurtic distribution (orange, spicky)\nMesokurtic distribution (gray, normal)\nPlatykurtic distribution (green, flattened)", x = "", y = "") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  coord_cartesian(xlim = c(-4,4), ylim = c(0,.5))

kurtosis(words_women)            

shapiro.test(words_women)

levene.test(mdata$word.count, mdata$sex)

Pretest <- c(78, 65, 71, 68, 76, 59)
Posttest <- c(71, 62, 70, 60, 66, 48)
ptd <- data.frame(Pretest, Posttest)

ptd %>%
  as.data.frame() %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

t.test(ptd$Pretest,
       ptd$Posttest,
       paired=TRUE,
       conf.level=0.95)

effectsize::cohens_d(x = ptd$Pretest, 
                     y = ptd$Posttest,
                     paired = TRUE)

EffectSize <- c("Very small", "Small", "Medium", "Large", "Very large", "Huge")
d <- c(0.01, 0.20, 0.50, 0.80, 1.20, 2.0)  
Reference <- c("Sawilowsky (2009)", "Cohen (1988)", "Cohen (1988)", "Cohen (1988)","Sawilowsky (2009)", "Sawilowsky (2009)")
dtb <- data.frame(EffectSize, d, Reference)
dtb %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

report::report(t.test(ptd$Pretest,ptd$Posttest, paired=TRUE, conf.level=0.95))

# load data
tdata <- base::readRDS(url("https://slcladal.github.io/data/d03.rda", "rb")) %>%
  dplyr::rename(NativeSpeakers = 1,
                Learners = 2) %>%
  tidyr::gather(Group, Score, NativeSpeakers:Learners) %>%
  dplyr::mutate(Group = factor(Group))

# inspect data
tdata %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

t.test(Score ~ Group, 
       var.equal = T,
       data = tdata)

effectsize::cohens_d(tdata$Score ~ tdata$Group,
                     paired = FALSE)

report::report(t.test(Score ~ Group, var.equal = T, data = tdata))

# generate data
coolmatrix <- matrix(c("truly", "very", 5, 17, 40, 41), ncol = 3, byrow = F)
colnames(coolmatrix) <- c("Adverb", "with cool", "with other adjectives")
# inspect data
coolmatrix %>%
  as.data.frame() %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# create table
coolmx <- matrix(
  c(5, 17, 40, 41),
  nrow = 2, # number of rows of the table
  # def. dimension names
  dimnames = list(
    Adverbs = c("truly", "very"),
    Adjectives = c("cool", "other adjective"))
)
# perform test
fisher.test(coolmx)

# create table
Rank <- c(1,3,5,6,8,9,10,11,17,19, 2,4,7,12,13,14,15,16,18,20)
LanguageFamily <- c(rep("Kovati", 10), rep("Urudi", 10))
lftb <- data.frame(LanguageFamily, Rank)

lftb %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

ggplot(lftb, aes(x = LanguageFamily, y = Rank, fill = LanguageFamily)) +
  geom_boxplot() +
  scale_fill_manual(values = c("orange", "darkgrey")) +
  theme_bw() +
  theme(legend.position = "none")

# inspect structure
str(lftb)

# perform test
wilcox.test(lftb$Rank ~ lftb$LanguageFamily) 

report::report(wilcox.test(lftb$Rank ~ lftb$LanguageFamily))

# generate non-normal skewed numeric data
frequency <- 1:100
frequency <- round(1/(frequency/20)*1000, 0)
normal_reaction <- round(rsnorm(100, 2000, 400), 1)
reaction_times <- order(normal_reaction)
reaction_times <- reaction_times*1:100
# combine into data frame
wxdata <- data.frame(frequency, normal_reaction, reaction_times) %>%
  dplyr::rename(Frequency = frequency,
                NormalizedReaction = normal_reaction,
                Reaction= reaction_times)

 wxdata %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# plot data
p1 <- ggplot(wxdata, aes(Frequency)) + # define data
  geom_density(fill = "orange", alpha = .5) + # define plot type (density)
  theme_bw() +                                # black + white background
  labs(y="Density", x = "Frequency") +        # axes titles
  coord_cartesian(ylim = c(0, .002),            # define y-axis coordinates
                  xlim = c(-5, 10000))           # define x-axis coordinates
p2 <- ggplot(wxdata, aes(Reaction)) +
  geom_density(fill = "lightgray", alpha = .5) +
  theme_bw() +
  labs(y="Density", x = "Reaction Time") +
  coord_cartesian(ylim = c(0, 0.00025), xlim = c(0, 10000))
grid.arrange(p1, p2, nrow = 1)             # 2 plots in one window

# perform test
wilcox.test(wxdata$Reaction, wxdata$Frequency) 

report::report(wilcox.test(wxdata$Reaction, wxdata$Frequency))

# create data
sober <- sample(0:9, 15, replace = T)
intoxicated <-  sample(3:12, 15, replace = T) 
# tabulate data
intoxtb <- data.frame(sober, intoxicated) 

intoxtb %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

intoxtb2 <- data.frame(c(rep("sober", nrow(intoxtb)),
                         rep("intoxicated", nrow(intoxtb))),
                       c(intoxtb$sober, intoxtb$intoxicated)) %>%
  dplyr::rename(State = 1,
                Errors = 2)
ggplot(intoxtb2, aes(State, Errors)) +
  geom_boxplot(fill = c("orange", "darkgrey"), width=0.5) +
  labs(y = "Number of errors", x = "State") +
  theme_bw()

# perform test
wilcox.test(intoxtb$intoxicated, intoxtb$sober, paired=T) 

report::report(wilcox.test(intoxtb$intoxicated, intoxtb$sober, paired=T) )

# create data
uhms <- c(15, 13, 10, 8, 37, 23, 31, 52, 11, 17)
Speaker <- c(rep("Learner", 5), rep("NativeSpeaker", 5))
# create table
uhmtb <- data.frame(Speaker, uhms)

uhmtb %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

ggplot(uhmtb, aes(Speaker, uhms)) +
  geom_boxplot(fill = c("orange", "darkgrey")) +
  theme_bw() +
  labs(x = "Speaker type", y = "Errors")

kruskal.test(uhmtb$Speaker~uhmtb$uhms) 

# create data
uhms <- c(7.2, 9.1, 14.6, 13.8)
Gender <- c("Female", "Male", "Female", "Male")
Age <- c("Young", "Young", "Old", "Old")
# create table
uhmtb2 <- data.frame(Gender, Age, uhms)

uhmtb2 %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

friedman.test(uhms ~ Age | Gender, data = uhmtb2)

chidata <- matrix(c(181, 655, 177, 67), nrow = 2, byrow = T)
# add column and row names
colnames(chidata) <- c("BrE", "AmE")
rownames(chidata) <- c("kindof", "sortof")
# inspect data
chidata %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Hedge") %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

chidata_extended <- matrix(c(181, 177, 358, 655, 67, 722, 836, 244, 1080), nrow = 3, byrow = F)
# add column and row names
colnames(chidata_extended) <- c("BrE", "AmE", "Total")
rownames(chidata_extended) <- c("kindof", "sortof", "Total")
# inspect data
chidata_extended %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Hedge") %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

chidata_expected <- matrix(c(277.1185, 80.88148, 358, 558.8815,163.11852, 722, 836, 244, 1080), nrow = 3, byrow = F)
# add column and row names
colnames(chidata_expected) <- c("BrE", "AmE", "Total")
rownames(chidata_expected) <- c("kindof", "sortof", "Total")
# inspect data
chidata_expected %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Hedge") %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

chidata_chi <- matrix(c(33.33869, 114.22602, 147.5647, 16.53082, 56.63839, 73.16921, 49.86951, 170.8644, 220.7339), nrow = 3, byrow = F)
# add column and row names
colnames(chidata_chi) <- c("BrE", "AmE", "Total")
rownames(chidata_chi) <- c("kindof", "sortof", "Total")
# inspect data
chidata_chi %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Hedge") %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

critval <- matrix(c(1, 3.84, 6.64, 10.83, 2, 5.99, 9.21, 13.82, 3, 7.82, 11.35, 16.27, 4, 9.49, 13.28, 18.47, 5, 11.07, 15.09, 20.52), ncol = 4, byrow = T)
# add column names
colnames(critval) <- c("DF", "p<.05", "p<.01", "p<.001")
# inspect data
critval %>%
  as.data.frame() %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# inspect data
chidata %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Hedge") %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

assocplot(as.matrix(chidata))   # association plot

mosaicplot(chidata, shade = TRUE, type = "pearson", main = "")  # mosaic plot

# perform chi square test without Yate's correction
chisq.test(chidata, corr = F)  

## # X2-test without Yate's correction
## chisq.test(chidata, correct=FALSE)

## format(chisq.test(chidata)$p.value, scientific=FALSE)

# calculate effect size
sqrt(chisq.test(chidata, corr = F)$statistic / sum(chidata) * (min(dim(chidata))-1))

ex1dat <- matrix(c("Young", 61, 43, 104, "Old", 42, 36, 78, "Total", 103, 79, 182), ncol = 4, byrow = T)
# add column names
colnames(ex1dat) <- c("Age", "1SGPN", "PN without 1SG", "Total")
# inspect data
ex1dat %>%
  as.data.frame() %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# generate data
ex1dat <- matrix(c(61, 43,  42, 36), ncol = 2, byrow = T)
# add column names
colnames(ex1dat) <- c("1SGPN", "PN without 1SG")
# perform x2-test
x2_ex1 <- chisq.test(ex1dat)
# inspect results
x2_ex1

critval <- matrix(c("whatever", 17, 55, 71, "other words", 345128, 916552, 1261680, "Total", 345145, 916607, 1261752), ncol = 4, byrow = T)
# add column names
colnames(critval) <- c("Item", "YoungMales", "YoungFemales", "Total")
# inspect data
critval %>%
  as.data.frame() %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# generate data
ex2dat <- matrix(c(17, 55,  345128, 916552), ncol = 2, byrow = T)
# add column names
colnames(ex2dat) <- c("YoungMales", "YoungFemales")
# perform x2-test
x2_ex2 <- chisq.test(ex2dat)
# inspect results
x2_ex2

critval <- matrix(c("kind of", 32.9927, 113.0407, 146.0335, "sort of", 16.3593, 56.0507, 72.4100, "Total", 49.3520, 169.0914, 218.4434), ncol = 4, byrow = T)
# add column names
colnames(critval) <- c("Variant", "BrE", "AmE", "Total")
# inspect data
critval %>%
  as.data.frame() %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

critval <- matrix(c("X-ray soft",  21, 14, 35, "X-ray hard", 18, 13, 31, "Beta-rays", 24, 12, 36, "Light", 13, 30, 43, "Total", 76, 69, 145), ncol = 4, byrow = T)
# add column names
colnames(critval) <- c("Treatment", "Mitosis not reached", "Mitosis reached", "Total")



# inspect data
critval %>%
  as.data.frame() %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# create data
wholetable <- matrix(c(21, 14, 18, 13, 24, 12, 13, 30), byrow = T, nrow = 4)
colnames(wholetable) <- c("reached", "notreached")           # add column names
rownames(wholetable) <- c("rsoft", "rhard", "beta", "light") # add row names

# inspect data
wholetable %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Treatment") %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

subtable <- wholetable[1:2,] # extract subtable

# inspect data
subtable %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Treatment") %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# simple x2-test
chisq.test(subtable, corr = F)

# load function for correct chi-square
source("https://slcladal.github.io/rscripts/x2.2k.r") 
x2.2k(wholetable, 1, 2)

critval <- matrix(c("chi-squared", 0.0255, 0.025, "p-value", 0.8732, 0.8744), ncol = 3, byrow = T)
# add column names
colnames(critval) <- c("Statistic", "chi-square" , "chi-square in 2*k-tables")
# inspect data
critval %>%
  as.data.frame() %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# create table
wholetable <- matrix(c(8, 31, 44, 36, 5, 14, 25, 38, 4, 22, 17, 12, 8, 11, 16, 24), ncol=4)
attr(wholetable, "dimnames")<-list(Register=c("acad", "spoken", "fiction", "new"),
Metaphor = c("Heated fluid", "Light", "NatForce", "Other"))

critval <- matrix(c("acad", 8, 5, 4, 8, "spoken", 31, 14, 22, 11, "fiction", 44, 25, 17, 16, "new", 36, 38, 12, 24), ncol = 5, byrow = T)
# add column names
colnames(critval) <- c("Register", "Heated fluid", "Light", "NatForce", "Other")
# inspect data
critval %>%
  as.data.frame() %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# create table
subtable <- matrix(c(14, 25, 22, 17), ncol=2)
chisq.results <- chisq.test(subtable, correct=FALSE) # WRONG!
phi.coefficient = sqrt(chisq.results$statistic / sum(subtable) * (min(dim(subtable))-1))
chisq.results
phi.coefficient

# load function for chi square test for subtables
source("https://slcladal.github.io/rscripts/sub.table.r") 
# apply test
results <- sub.table(wholetable, 2:3, 2:3, out="short")
# inspect results
results

# load package
library(cfa)
# load data
cfadata  <- base::readRDS(url("https://slcladal.github.io/data/cfd.rda", "rb"))

# inspect data
cfadata %>%  
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# define configurations
configs <- cfadata %>%
  dplyr::select(Variety, Age, Gender, Class)
# define counts
counts <- cfadata$Frequency

# perform cfa
cfa(configs,counts)

# define configurations
configs <- cfadata %>%
  dplyr::select(Variety, Age, Gender, Class)
# define counts
counts <- cfadata$Frequency

# perform cfa
hcfa(configs,counts)

sessionInfo()

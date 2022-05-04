knitr::include_graphics("https://slcladal.github.io/images/uq1.jpg")

## # install packages
## install.packages("dplyr")
## install.packages("ggplot2")
## install.packages("stringr")
## install.packages("boot")
## install.packages("DescTools")
## install.packages("psych")
## install.packages("Rmisc")
## install.packages("ggpubr")
## install.packages("flextable")
## # install klippy for copy-to-clipboard button in code chunks
## install.packages("remotes")
## remotes::install_github("rlesur/klippy")

# set options
options(stringsAsFactors = F)          # no automatic data transformation
options("scipen" = 100, "digits" = 12) # suppress math annotation
# activate packages
library(boot)
library(DescTools)
library(dplyr)
library(stringr)
library(ggplot2)
library(flextable)
library(psych)
library(Rmisc)
library(ggpubr)
# activate klippy for copy-to-clipboard button
klippy::klippy()

Means <- c("(Arithmetic) mean (average)", "Median (middle value)", "Mode (most frequent value)", "Geometric mean (average factor)", "Harmonic mean (average rate)")
Use <- c("Description of normally distributed numeric variables (most common measure of central tendency)", "Description of non-normal numeric variables or ordinal variables (skewed data or influential outliers)", "Description of nominal and categorical variables", "Description of dynamic processes such as growth rates", "Description of dynamic processes such as velocities")
df <- data.frame(Means, Use)

# inspect data
df %>%
  as.data.frame() %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Measures of central tendency and their use.")  %>%
  flextable::border_outer()

df <- data.frame(id = rep(1:4, 2),
           g = c(rep("x1", 4), rep("x2", 4)),
           freq = c(2, 8, 4, 6, 5, 5, 5, 5)) %>%
  dplyr::rename(id = 1, g = 2, freq = 3)
df %>%
  ggplot(aes(x = id, y = freq, label = freq, fill = g)) +
  geom_bar(stat = "identity") +
  facet_grid(~g) +
  theme_void() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  ggtitle("(Arithmetic) Mean") +
  geom_text(vjust=-1.6, color = "black") +
  coord_cartesian(ylim = c(0, 9)) +
  scale_fill_manual(breaks = "g", values = c(x1 = "gray80", x2 = "red"))

Sentences <- c("Call me Ishmael", "Some years ago -- never mind how long precisely -- having little or no money in my purse, and nothing particular to interest me on shore, I thought I would sail about a little and see the watery part of the world.", "It is a way I have of driving off the spleen, and regulating the circulation.", "Whenever I find myself growing grim about the mouth; whenever it is a damp, drizzly November in my soul; whenever I find myself involuntarily pausing before coffin warehouses, and bringing up the rear of every funeral I meet; and especially whenever my hypos get such an upper hand of me, that it requires a strong moral principle to prevent me from deliberately stepping into the street, and methodically knocking people's hats off--then, I account it high time to get to sea as soon as I can.")
Words <- c(3, 40, 15, 87)
df <- data.frame(Sentences, Words)

# inspect data
df %>%
  as.data.frame() %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "left") %>%
  flextable::set_caption(caption = "Sentences of the first paragraph of Herman Melville's *Moby Dick* and the number of words in each sentence.")  %>%
  flextable::border_outer()

# create numeric vector
frequencies <- c(3, 40, 15, 87)
# calculate mean
mean(frequencies)

(1 + 2 + 3 + 4 + 5 + 6)/6

mean(c(4, 3, 6, 2, 1, 5, 6, 8))

vec <- c(1, 5, 5, 9)
mean(vec)

data.frame(id = rep(1:9, 2),
           g = c(rep("x1", 9), rep("x2", 9)),
           freq = c(5, 2, 9, 7, 1, 3, 8, 4, 6, 1, 2, 3, 4, 5, 6, 7, 8, 9),
           clr = c(rep("g", 13), "r", rep("g", 4)))  %>%
  ggplot(aes(x = id, y = freq, label = freq, fill = clr)) +
  geom_bar(stat = "identity") +
  facet_grid(~g) +
  theme_void() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  ggtitle("Median") +
  scale_fill_manual(breaks = "clr", values = c(g = "gray80", r = "red")) +
  geom_text(vjust=-1.6, color = "black") +
  coord_cartesian(ylim = c(0, 10))

Age <- c("0-18", "19-25", "26-33", "34-41", "42-49", "50+")
Counts <- c(9, 160, 70, 15, 9, 57)
df <- data.frame(Age, Counts)
# inspect data
df  %>%
  as.data.frame() %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Number of speakers across age groups in the private dialogue section of the Irish component of the  *International Corpus of English* (ICE).")  %>%
  flextable::border_outer()

Age <- c("0-18", "19-25", "26-33", "34-41", "42-49", "50+")
Counts <- c(9, 160, 70, 15, 9, 57)
data.frame(Age, Counts) %>%
  ggplot(aes(Age, Counts, label = Counts)) +
  geom_bar(stat = "identity") +
  geom_text(vjust=-1.6, color = "black") +
  coord_cartesian(ylim = c(0, 200)) +
  theme_bw() +
  labs(x = "", y = "Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# create a vector consisting out of ranks
ranks <- c(rep(1, 9), rep(2, 160), rep(3, 70), rep(4, 15), rep(5, 9), rep(6, 57))
# calculate median
median(ranks)

(3 + 4)/2

median(c(4, 3, 6, 2, 1, 5, 6, 8))

vec <- c(1, 5, 5, 9)
median(vec)

data.frame(id = rep(1:9, 2),
           g = c(rep("x1", 9), rep("x2", 9)),
           freq = c(5, 2, 9, 7, 1, 3, 8, 4, 6, 1, 2, 3, 4, 5, 6, 7, 8, 9),
           clr = c(rep("g", 17), "r"))  %>%
  ggplot(aes(x = id, y = freq, label = freq, fill = clr)) +
  geom_bar(stat = "identity") +
  facet_grid(~g) +
  theme_void() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  ggtitle("Mode") +
  scale_fill_manual(breaks = "clr", values = c(g = "gray80", r = "red")) +
  geom_text(vjust=-1.6, color = "black") +
  coord_cartesian(ylim = c(0, 10))

CurrentResidence <- c("Belfast", "Down", "Dublin (city)", "Limerick", "Tipperary")
Speakers <- c(98, 20, 110, 13, 19)
df <- data.frame(CurrentResidence, Speakers) 
df  %>%
  as.data.frame() %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Number of speakers across counties of current residency in the private dialogue section of the Irish component of the  *International Corpus of English* (ICE).")  %>%
  flextable::border_outer()

df %>%
  ggplot(aes(CurrentResidence, Speakers, label = Speakers)) +
  geom_bar(stat = "identity") +
  geom_text(vjust=-1.6, color = "black") +
  coord_cartesian(ylim = c(0, 200)) +
  theme_bw() +
  labs(x = "", y = "Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# create a factor with the current residence of speakers
CurrentResidence <- c(rep("Belfast", 98),         # repeat "Belfast" 98 times
                      rep("Down", 20),            # repeat "Down" 20 times
                      rep("Dublin (city)", 110),  # repeat "Dublin (city)" 110 times
                      rep("Limerick", 13),        # repeat "Limerick" 13 times
                      rep("Tipperary", 19))       # repeat "Tipperary" 19 times
# calculate mode
names(which.max(table(CurrentResidence)))         # extract which level occurs most frequently

Year <- c("Year 1", "Year 2", "Year 3", "Year 4")
Package1 <- c("+5%", "-5%", "+5%", "-5%")
Package2 <- c("+20%", "-20%", "+20%", "-20%")
df <- data.frame(Year, Package1, Package2)
df %>%
  as.data.frame() %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Performance of two stock packages.")  %>%
  flextable::border_outer()

# create data
Corpus <- c(rep("C1", 5), 
            rep("C2", 5))
Speaker <- rep(c("A", "B", "C", "D", "E"), 2)  
Frequency <- c(11.4, 5.2, 27.1, 9.6, 13.7, 0.2, 0.0, 1.1, 65.3, 0.4)  
particletable <- data.frame(Corpus, Speaker, Frequency)
# show data
particletable %>%
  as.data.frame() %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Relative frequencies of discourse particles per speaker in two corpora.")  %>%
  flextable::border_outer()

pdat1 <- particletable %>%
  dplyr::group_by(Corpus) %>%
  dplyr::mutate(Median = median(Frequency),
                Mean = mean(Frequency))
pdat1 %>%
  ggplot(aes(x = Speaker, y = Frequency, label = Frequency, fill = Corpus)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  labs(title = "Use of discourse particles across two corpora.", y = "Relative Frequency") +
  scale_fill_manual(breaks = Corpus, values = c(C1 = "gray80", C2 = "red")) +
  geom_text(aes(y = -5, label = Frequency), position = position_dodge(0.9)) +
  coord_cartesian(ylim = c(-10, 80)) +
  theme(legend.position = "top")

## pdat1 %>%
##   ggplot(aes(x = Speaker, y = Frequency, label = Frequency, fill = Corpus)) +
##   geom_bar(stat = "identity", position = position_dodge()) +
##   theme_bw() +
##   geom_hline(aes(yintercept = Mean), col = "black", linetype = 2, size = 1) +
##   geom_text(aes(.75, Mean, label = paste0("Mean = ", Mean), vjust = -1)) +
##   geom_hline(data = pdat1 %>% filter(Corpus == "C1"),
##              aes(yintercept = Median), col = "darkgray", linetype = 3, size = 1) +
##   geom_hline(data = pdat1 %>% filter(Corpus == "C2"),
##              aes(yintercept = Median), col = "red", linetype = 3, size = 1) +
##   geom_text(aes(.75, Mean, label = paste0("Mean = ", Mean), vjust = -1)) +
##   geom_text(data = pdat1 %>% filter(Corpus == "C1"),
##              aes(5, Median, label = paste0("Median C1 = ", Median)), vjust = 1) +
##   geom_text(data = pdat1 %>% filter(Corpus == "C2"),
##              aes(5, Median, label = paste0("Median C2 = ", Median)), vjust = 0) +
##   labs(title = "Use of discourse particles across two corpora.", y = "Relative Frequency") +
##   scale_fill_manual(breaks = Corpus, values = c(C1 = "gray80", C2 = "red")) +
##   geom_text(aes(y = -5, label = Frequency), position = position_dodge(0.9)) +
##   coord_cartesian(ylim = c(-10, 80)) +
##   theme(legend.position = "top")

Month <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "Mean")
Moscow <- c(-5, -12, 5, 12, 15, 18, 22, 23, 20, 16, 8, 1, 10.25)  
Hamburg <- c(7, 7, 8, 9, 10, 13, 15, 15, 13, 11, 8, 7, 10.25)  
temprature <- data.frame(Month, Moscow, Hamburg)
# show data
temprature %>%
  as.data.frame() %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Average temperature in Hamburg and Moscow by month.")  %>%
  flextable::border_outer()

Temperature <- c(-5, -12, 5, 12, 15, 18, 22, 23,  20, 16, 8, 1,
                 7, 7,  8,  9, 10, 13, 15, 15,  13, 11,  8, 7)
Month <- rep(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 2)
City = c(rep("Moscow", 12), rep("Hamburg", 12))
# combine data in data frame
lineplotdata <- data.frame(City, Month, Temperature) %>%
  dplyr::mutate(Month = factor(Month, levels = c("January", "February", "March", 
                                                 "April", "May", "June", "July", "August", 
                                                 "September", "October", "November", "December")))
lineplotdata %>%
  ggplot(aes(x = Month, y = Temperature, group = City, color = City,  linetype = City)) +
  geom_line(size = 1) +
  theme_bw() +
  theme(legend.position = "top", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "") +
  scale_color_manual(name = "City",
                     labels = c("Moscow", "Hamburg"), 
                     breaks = c("Moscow", "Hamburg"), 
                     values = c("gray80", "red"))  +
  scale_linetype_manual(name = "City",
                     labels = c("Moscow", "Hamburg"), 
                     breaks = c("Moscow", "Hamburg"), 
                     values = c("dashed", "solid"))

# create a numeric vector
Moscow <- c(-5, -12, 5, 12, 15, 18, 22, 23, 20, 16, 8, 1)
min(Moscow); max(Moscow) # extract range

summary(Moscow) # extract IQR

sd(Moscow)^2

# calculate standard deviation
sd(Moscow) 

## A <- c(1, 3, 6, 2, 1, 1, 6, 8, 4, 2, 3, 5, 0, 0, 2, 1, 2, 1, 0)
## B <- c(3, 2, 5, 1, 1, 4, 0, 0, 2, 3, 0, 3, 0, 5, 4, 5, 3, 3, 4)
## mean(A)
## median(A)
## max(A)
## sd(A)
## mean(B)
## median(B)
## max(B)
## sd(B)

set.seed(12345)
RT = c(rnorm(5, 400, 50), rnorm(5, 380, 50), rnorm(5, 450, 50), rnorm(5, 480, 50))
State = c(rep("Sober", 10), rep("Drunk", 10))
Gender = rep(c(rep("Male", 5), rep("Female", 5)), 2)
rts <- data.frame(RT, State, Gender) %>%
  dplyr::mutate(RT = round(RT, 3))
# show data
rts %>%
  as.data.frame() %>%
  head(20) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Reaction times while sober and drunk.")  %>%
  flextable::border_outer()

sd(rts$RT, na.rm=TRUE) /  
   sqrt(length(rts$RT[!is.na(rts$RT)]))  

# describe data
psych::describe(rts$RT, type=2)

qnorm(0.975)   

# calculate mean
m <- mean(c(4,5,2,3,1,4,3,6,3,2,4,1))
# calculate standard deviation
s <- sd(c(4,5,2,3,1,4,3,6,3,2,4,1))
# calculate n
n <- length(c(4,5,2,3,1,4,3,6,3,2,4,1))
# calculate lower and upper ci
lower <- m-1.96*(s/sqrt(n))
upper <- m+1.96*(s/sqrt(n))
# show lower ci, mean, and upper ci
lower; m; upper

# calculate mean
m <- mean(c(1, 2, 3, 4, 5, 6, 7, 8, 9))
# calculate standard deviation
s <- sd(c(1, 2, 3, 4, 5, 6, 7, 8, 9))
# calculate n
n <- length(c(1, 2, 3, 4, 5, 6, 7, 8, 9))
# calculate lower and upper ci
lower <- m-1.96*(s/sqrt(n))
upper <- m+1.96*(s/sqrt(n))
# show lower ci, mean, and upper ci
lower; m; upper

# calculate mean
m <- mean(c(3, 4, 5, 4, 3, 4, 5, 4, 3))
# calculate standard deviation
s <- sd(c(3, 4, 5, 4, 3, 4, 5, 4, 3))
# calculate n
n <- length(c(3, 4, 5, 4, 3, 4, 5, 4, 3))
# calculate lower and upper ci
lower <- m-1.96*(s/sqrt(n))
upper <- m+1.96*(s/sqrt(n))
# show lower ci, mean, and upper ci
lower; m; upper

# extract mean and confidence intervals
Rmisc::CI(rts$RT, ci=0.95)   

# extract mean and confidence intervals
tt <- Rmisc::CI(rts$RT, ci=0.95)  

# extract mean and confidence intervals
stats::t.test(rts$RT, conf.level=0.95)  

# extract mean and confidence intervals
DescTools::MeanCI(rts$RT, conf.level=0.95)   

# extract mean CIs
DescTools::MeanCI(rts$RT, method="boot", type="norm", R=1000)

# extract mean CIs
DescTools::MeanCI(rts$RT, method="boot", type="norm", R=1000)

# function to extract values
BootFunction = function(x, index) {                        
                  return(c(mean(x[index]),
                           var(x[index]) / length(index)))
}
# apply function to data
Bootstrapped = boot(data=rts$RT,     
                    statistic=BootFunction,
                    R=1000)
# extract values
mean(Bootstrapped$t[,1])                                   
# alternative to extract values
boot.ci(Bootstrapped, conf=0.95)                           

# apply summarySE function to data
Rmisc::summarySE(data=rts,   
                 # define variable representing frequencies
                 measurevar="RT", 
                 # define grouping variable
                 groupvars="Gender",
                 # extract standard deviation, standard error, and confidence intervals
                 conf.interval = 0.95)   

stats::binom.test(2, 20, 0.5,              # binom.test(x, n, p = 0.5, ...)
                  alternative="two.sided", # define sidedness
                  conf.level=0.95)         # define confidence level

# extract CIs                  
BinomCI(2, 20,                        # apply BinomCI function
        conf.level = 0.95,            # define ci
        method = "modified wilson")   # define method for ci extraction

observed = c(35,74,22,69)       # define multinominal vector
MultinomCI(observed,            # apply MultinomCI function
           conf.level=0.95,     # define ci
           method="goodman")    # define method for ci extraction

sessionInfo()

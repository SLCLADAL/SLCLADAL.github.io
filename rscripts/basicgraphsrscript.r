
# "Visualizing Data with R"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)
# install libraries
install.packages(c("lattice", "ggplot2", "dplyr", "likert", 
                   "scales", "vcd", "tm", "wordcloud", 
                   "stringr", "SnowballC", "tidyr"))
# activate packages
library(knitr)
library(lattice)             
library(ggplot2)               
library(dplyr)
library(likert)                         
library(scales)
library(vcd)    
library(tm)
library(wordcloud)
library(stringr)
library(SnowballC)
library(tidyr)
# load data
plotdata <- read.delim("https://slcladal.github.io/data/lmmdata.txt", header = TRUE)
# inspect data
str(plotdata); summary(plotdata)
# attach plotdata
attach(plotdata)
# create simple scatter plot
plot(Prepositions ~ Date,                 # plot Prepositions by Date
     type = "p",                          # plot type p (points) 
     data = plotdata,                     # data from data set plotdata  
     ylab = "Prepositions (Frequency)",   # add y-axis label 
     xlab = "Date (year of composition)", # add x-axis label 
     main = "plot type 'p' (points)"      # add title 
     )                                    # end drawing plot
# create simple scatter plot with ablines
plot(Prepositions ~ Date,                 # plot Prepositions by Date
     type = "p",                          # plot type p (points) 
     data = plotdata,                     # data from data set iris  
     ylab = "Prepositions (Frequency)",   # add y-axis label 
     xlab = "Date (year of composition)", # add x-axis label 
     main = "Scatterplot",                # add title 
     pch = 20,                            # use point symbol 20 (filled circles)
     col = "lightgrey"                    # define symbol colour as light grey
     )                                    # end drawing plot
abline(                                   # add regression line (y~x) 
  lm(Prepositions ~ Date),                # draw regression line of linear model (lm) 
  col="red"                               # define line colour as red
  )                                       # end drawing line             
lines(                                    # add line (x,y)
  lowess(Prepositions ~ Date),            # draw smoothed lowess line (x,y) 
  col="blue"                              # define line colour as blue
  )                                       # end drawing line
# load data03
data03 <- read.delim("https://slcladal.github.io/data/data03.txt", sep = "\t", header = T)
# show data03
data03
# load data03
data03 <- read.delim("https://slcladal.github.io/data/data03.txt", sep = "\t", header = T)
# create simple scatter plot
plot(Variable2 ~ Variable1, 
     type = "p", 
     data = data03, 
     ylab = "Variable1", 
     xlab = "Variable2",       
     main = "Scatterplot Exercise",   
     pch = 20,        
     col = "darkgrey" 
     )
# create simple scatter plot
xyplot(Prepositions ~ Date,                 # plot Prepositions by Date
       ylab = "Prepositions (Frequency)",   # add y-axis label 
       xlab = "Date (year of composition)", # add x-axis label
       )                                    # end drawing plot
# create scatter plots by species
xyplot(Prepositions ~ Date | Genre,         # plot Prepositions by Date by Genre
       ylab = "Prepositions (Frequency)",   # add y-axis label
       xlab = "Date (year of composition)", # add y-axis label
       grid = TRUE                          # add grids to panels
       )                                    # end drawing plot
# create scatter plots by species
xyplot(Prepositions ~ Date | Genre,           # plot Prepositions by Date by Genre
       ylab = "Prepositions (Frequency)",     # add y-axis label
       xlab = "Date (year of composition)",   # add y-axis label
       grid = TRUE,                           # cerate a grid
       pch = 20,                              # symbol type (20 = filled dots)
       col = "black"                          # color of symbols
       )                                      # end drawing plot
# create simple scatter plot
ggplot(plotdata,                # plot data from data set plodata  
       aes(x= Date,             # define x-axis
           y= Prepositions)) +  # define y-axis
  geom_point()                  # define plot type
# create scatter plot colored by genre
ggplot(plotdata,               # plot data from data set plotdata
       aes(x=Date,             # define x-axis
           y= Prepositions,    # define y-axis
           color = Genre)) +   # define to color by Species
  geom_point() +               # define plot type
  theme_bw()                   # define theme  as black and white (bw)
# create scatter plot colored by genre
ggplot(plotdata, aes(x=Date, y= Prepositions, color = Genre)) +
  geom_point() +
  theme_bw() +
  scale_color_manual(         # define colours to be used
    values = c("indianred4", "darkblue", "orange", "lightgreen", "darkgreen",
               "darkgrey", "grey50", "gray80", "brown", "red",
               "goldenrod", "chartreuse", "black", "lightblue", 
               "blueviolet", "burlywood"))
# modify data
plotdataredux <- plotdata %>%
  dplyr::filter(Genre == "Bible" | Genre == "Fiction" | Genre == "PrivateLetter" | Genre == "Science" | Genre == "History" | Genre == "Sermon") %>%
  dplyr::mutate(Date =as.numeric(Date)) %>% 
  droplevels() %>%
  dplyr::select(-Text)
# inspect data
str(plotdataredux)
# create scatter plot colored by genre
ggplot(plotdataredux, aes(Date, Prepositions, group = Genre, color = Genre)) +
  geom_point(aes(y = Prepositions, shape = Genre)) +
  guides(shape=guide_legend(override.aes=list(fill=NA))) +
  scale_shape_manual(name = "Genre", values = c("B", "F", "H","L","S","R")) +
  theme_bw() +
  theme(legend.position="top")
# create scatter plot colored by genre in different panels
ggplot(plotdata, aes(Date, Prepositions,  color = Genre)) +
  facet_wrap(Genre, ncol = 4) +
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(size=8, angle=90))
# create scatter plot colored by genre in different panels
ggplot(plotdata, aes(x=Date, y= Prepositions,  color = Genre)) +
  facet_wrap(Genre, ncol = 4) +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(size=8, angle=90))
# create scatter density plot
ggplot(plotdata, aes(x=Date, y= Prepositions,  color = Genre)) +
    facet_wrap(Genre, ncol = 4) +
  theme_bw() +                  
  geom_density_2d() +
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(size=8, angle=90))
# scatter plot with error bars
ggplot(plotdata, aes(x=reorder(Genre, Prepositions, mean), y= Prepositions,  group = Genre)) +                 
  # add title
  ggtitle("Prepositions by Genre") +          
  # create a dot at means
  stat_summary(fun.y = mean, geom = "point",     
               # means by Species
               aes(group= Genre)) +          
  # bootstrap data
  stat_summary(fun.data = mean_cl_boot,       
               # add error bars
               geom = "errorbar", width = 0.2) + 
  # def. y-axis range
  coord_cartesian(ylim = c(100, 200)) +              
  # def. font size
  theme_bw(base_size = 15) +         
  # def. x- and y-axis
  theme(axis.text.x = element_text(size=10, angle = 90),  
        axis.text.y = element_text(size=10, face="plain")) + 
  # def. axes labels
  labs(x = "Genre", y = "Prepositions (Frequency)") +     
  # def. to col.
  scale_color_manual(guide = FALSE)          
# modify data
lineplotdata <- plotdataredux %>%
  dplyr::mutate(Date = ifelse(Date < 1600, "1600",
                              ifelse(Date < 1700, "1700",
                              ifelse(Date < 1800, "1800",
                              ifelse(Date < 1900, "1900", "1900"))))) %>%
  group_by(Date, Genre) %>%
  dplyr::summarise(Mean = mean(Prepositions)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Date =as.numeric(Date))
# inspect data
str(lineplotdata)
# define aesthetics
ggplot(lineplotdata, aes(x=Date, y= Mean,  color = Genre)) +
  # add geom layer with lines
  geom_line()
# define aesthetics
ggplot(lineplotdata, aes(x=Date, y= Mean,  color = Genre, linetype = Genre)) +
  # add geom layer with lines
  geom_smooth()
# define aesthetics
ggplot(lineplotdata, aes(x=Date, y= Mean,  color = Genre, linetype = Genre)) +
  # add geom layer with lines
  geom_smooth() +  
  # legend without background color
  guides(color=guide_legend(override.aes=list(fill=NA))) +  
  # def. legend position
  theme(legend.position="top") +  
  # def. linetype
  scale_linetype_manual(values=c("longdash", "twodash", "dashed", 
                                 "dotdash", "dotted", "solid"), 
                        # def. legend header
                        name=c("Genre"),
                        # def. linetypes
                        breaks = c("Bible", "Fiction", "History",
                                   "PrivateLetter", "Science", "Sermon"),
                        # def. labels
                        labels = c("Bible", "Fiction", "History",
                                   "Private letter", "Science", "Sermon")) + 
  # def. col.
  scale_colour_manual(values=c("goldenrod2", "gray30", "blue", 
                               "burlywood", "gray80", "indianred4"),
                      # define legend header
                      name=c("Genre"),
                      # define elements
                      breaks=c("Bible", "Fiction", "History",
                               "PrivateLetter", "Science", "Sermon"),  
                      # define labels
                      labels = c("Bible", "Fiction", "History",
                                 "Private letter", "Science", "Sermon")) +
  # add x-axis label
  labs(x = "Year") +      
  # customize x-axis tick positions
  scale_x_continuous(breaks=seq(1600, 1900, 100), 
                     # add labels to x-axis tick pos.
                     labels=seq(1600, 1900, 100)) +
  # add y-axis label
  scale_y_continuous(name="Relative frequency \n(per 1,000 words)",  
                     # customize tick y-axis
                     limits=c(100, 200)) + 
  # define theme  as black and white
  theme_set(theme_bw(base_size = 10))    
# create lickert data
likertdata <- data.frame(Course=
                           c(rep(c("Chinese",
                                   "German",
                                   "Japanese"),
                                 each = 100)),
                         Satisfaction=
                           c(c(rep(1, 20),
                               rep(2, 30),
                               rep(3, 25),
                               rep(4, 10),
                               rep(5, 15)),
                             c(rep(1, 40),
                               rep(2, 25),
                               rep(3, 15),
                               rep(4, 15),
                               rep(5, 5)),
                             c(rep(1, 10),
                               rep(2, 15),
                               rep(3, 20),
                               rep(4, 25),
                               rep(5, 30))))
# inspect data
head(likertdata)
# create cumulative density plot
ggplot(likertdata,aes(x = Satisfaction, color = Course)) + 
  geom_step(aes(y = ..y..), stat = "ecdf") +
  labs(y = "Cumulative Density") + 
  scale_x_discrete(limits = c("1","2","3","4","5"), breaks = c(1,2,3,4,5),
        labels=c("very dissatisfied", "dissatisfied", "neutral", "satisfied", "very satisfied")) + 
  scale_colour_manual(values = c("goldenrod2", "indianred4", "blue"))  
# create bar plot data
bardata <- plotdata %>%
  dplyr::mutate(Date = ifelse(Date < 1600, "1600",
                              ifelse(Date < 1700, "1700",
                              ifelse(Date < 1800, "1800",
                              ifelse(Date < 1900, "1900", "1900"))))) %>%
  dplyr::mutate(Date = factor(Date)) %>%
  group_by(Date) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1))
# inpsect data
head(bardata)
# create pie chart
ggplot(bardata,  aes("", Percent, fill = Date)) + 
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("red", "blue", "gray70", "goldenrod")) +
  theme_void()
# create pie chart
ggplot(bardata,  aes("", Percent, fill = Date)) + 
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("red", "blue", "gray70", "goldenrod")) +
  theme_void() +
  geom_text(aes(y = Percent, label = Percent), color = "white", size=6)
piedata <- bardata %>%
  dplyr::arrange(desc(Date)) %>%
  dplyr::mutate(Position = cumsum(Percent)- 0.5*Percent)
# inspect piedata
piedata
# create pie chart
ggplot(piedata,  aes("", Percent, fill = Date)) + 
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("red", "blue", "gray70", "goldenrod")) +
  theme_void() +
  geom_text(aes(y = Position, label = Percent), color = "white", size=6)
# bar plot
ggplot(bardata, aes(Date, Percent, fill = Date)) +
  geom_bar(stat="identity") +          # determine type of plot
  theme_bw() +                         # use black & white theme
  # add and define text
  geom_text(aes(y = Percent-5, label = Percent), color = "white", size=3) + 
  # add colors
  scale_fill_manual(values = c("red", "blue", "gray70", "goldenrod")) +
  # supress legend
  theme(legend.position="none")
# create bar plot data
newbardata <- plotdata %>%
    dplyr::filter(Genre == "PrivateLetter" | Genre == "PublicLetter" | Genre == "Science" | Genre == "History" | Genre == "Sermon") %>%
  dplyr::mutate(Date = ifelse(Date < 1600, "1600",
                              ifelse(Date < 1700, "1700",
                              ifelse(Date < 1800, "1800",
                              ifelse(Date < 1900, "1900", "1900"))))) %>%
  dplyr::mutate(Date = factor(Date)) %>%
  group_by(Date, Genre) %>%
  dplyr::summarise(Frequency = n())
# inpsect data
head(newbardata)
# bar plot
ggplot(newbardata, aes(Date, Frequency, fill = Genre)) + 
  geom_bar(stat="identity", position = position_dodge()) +  
  theme_bw()                         
# bar plot
ggplot(newbardata, aes(Date, Frequency, fill = Genre)) + 
  geom_bar(stat="identity") +  
  theme_bw()                         
# bar plot
ggplot(newbardata, aes(Date, Frequency, fill = Genre)) + 
  geom_bar(stat="identity", position="fill") +  
  theme_bw()                         
# create likert data
newlikertdata <- likertdata %>%
  group_by(Course, Satisfaction) %>%
  mutate(Frequency = n())
newlikertdata <- unique(newlikertdata)
# inspect data
head(newlikertdata)
# create grouped bar plot
ggplot(newlikertdata, aes(Satisfaction, Frequency,  fill = Course)) +
  geom_bar(stat="identity", position=position_dodge()) +
  # define colors
  scale_fill_manual(values=c("goldenrod2", "gray70",  "indianred4")) + 
  # add text and define colour
  geom_text(aes(label=Frequency), vjust=1.6, color="white", 
            # define text position and size
            position = position_dodge(0.9),  size=3.5) +     
    scale_x_discrete(limits=c("1","2","3","4","5"), breaks=c(1,2,3,4,5),
        labels=c("very dissatisfied", "dissatisfied",  "neutral", "satisfied", 
                 "very satisfied")) + 
  theme_bw()
# load data
data(pisaitems)           # use a provided dataset called pisaitems
# extract subset from data for visualization
items28 <- pisaitems[, substr(names(pisaitems), 1, 5) == "ST24Q"]
# transform into a likert object
questionl28 <- likert(items28)
# summarize data
summary(questionl28) 
# plot likert data
plot(questionl28)
# create a vector with values called Test1
Test1 <- c(11.2, 13.5, 200, 185, 1.3, 3.5) 
# create a vector with values called Test2
Test2 <- c(12.2, 14.7, 210, 175, 1.9, 3.0)   
# create a vector with values called Test3
Test3 <- c(13.2, 15.1, 177, 173, 2.4, 2.9)    
# combine vectors in a data frame
testdata <- data.frame(Test1, Test2, Test3)     
# add rownames
rownames(testdata) <- c("Feature1_Student",     
                        "Feature1_Reference", 
                        "Feature2_Student", 
                        "Feature2_Reference", 
                        "Feature3_Student", 
                        "Feature3_Reference")
# inspect data
testdata                                        
# determine divergence from reference
# row 1 (student) minus row 2 (reference)
FeatureA <- t(testdata[1,] - testdata[2,]) 
# row 3 (student) minus row 4 (reference)
FeatureB <- t(testdata[3,] - testdata[4,])  
# row 5 (student) minus row 6 (reference)
FeatureC <- t(testdata[5,] - testdata[6,])  
# create data frame
plottable <- data.frame(rep(rownames(FeatureA), 3), 
                  c(FeatureA, FeatureB, FeatureC), 
                  c(rep("FeatureA", 3), 
                    rep("FeatureB", 3), 
                    rep("FeatureC", 3)))
# def. col. names
colnames(plottable) <- c("Test", "Value", "Feature")
# inspect data
plottable                                         
# create plot
ggplot(plottable, 
       aes(Test, Value)) + # def. x/y-axes
  # separate plots for each feature
  facet_grid(vars(Feature), scales = "free_y") +
  # create bars
  geom_bar(stat = "identity", aes(fill = Test)) +  
  # black and white theme
  theme_bw() +
  # supress legend   
  guides(fill=FALSE) + 
  # def. colours   
  geom_bar(stat="identity", fill=rep(c("goldenrod2", 
                                       "gray70", 
                                       "indianred4"), 3)) + 
  # axes titles
  labs(x = "", y = "Score")                                               
# reduce data
assocdata <- plotdataredux %>%
  dplyr::filter(Genre != "PrivateLetter", Genre != "History") %>%
  droplevels() %>%
  group_by(Genre, Region) %>%
  dplyr::summarise(Prepositions = round(mean(Prepositions), 0)) %>%
  tidyr::spread(Region, Prepositions) 
# create matrix 
assocmx <- as.matrix(assocdata[,2:3])
attr(assocmx, "dimnames")[1] <- as.vector(assocdata[,1])
# inspect data
assocmx
# create association plot
assoc(assocmx, shade=TRUE)
# create a mosaic plot
mosaic(assocmx, shade=T, legend=TRUE)  
# create data
heatdata <- plotdata %>%
  dplyr::select(Genre, Date, Prepositions) %>%
  dplyr::mutate(Date = ifelse(Date < 1600, "1600",
                              ifelse(Date < 1700, "1700",
                              ifelse(Date < 1800, "1800",
                              ifelse(Date < 1900, "1900", "1900"))))) %>%
  group_by(Date, Genre) %>%
  dplyr::summarise(Prepositions = round(mean(Prepositions), 0)) %>%
  tidyr::spread(Date, Prepositions) %>%
  dplyr::filter(Genre != "Religion")
# create matrix 
heatmx <- as.matrix(heatdata[,2:5])
attr(heatmx, "dimnames")[1] <- as.vector(heatdata[,1])
heatmx <- scale(heatmx)
# inspect data
heatmx
# create heat map
heatmap(heatmx, scale = "none")
# create data
boxdata <- plotdata %>%
  dplyr::filter(Genre == "PrivateLetter" | Genre == "PublicLetter" | Genre == "Science" | Genre == "History" | Genre == "Sermon") %>%
  dplyr::mutate(Date = ifelse(Date < 1600, "1600",
                              ifelse(Date < 1700, "1700",
                              ifelse(Date < 1800, "1800",
                              ifelse(Date < 1900, "1900", "1900")))))%>%
  dplyr::mutate(Date = factor(Date))
# inspect data
head(boxdata)
# create boxplot
ggplot(boxdata, aes(Date, Prepositions, color = Genre)) +                 
  geom_boxplot(fill=c("gold", "gray70", "indianred4", "blue"), color="black") 
# create boxplot
ggplot(boxdata, aes(Date, Prepositions, color = Genre)) +                 
  geom_boxplot(outlier.colour="red", outlier.shape=2, outlier.size=5, 
               notch=T, fill=c("gold", "gray70", "indianred4", "blue"), 
               color="black") 
# create violin plot
ggplot(boxdata, aes(Date, Prepositions, fill = Date)) +  
  geom_violin(trim = FALSE) +  
  geom_boxplot(width=0.1, fill="white") +
  scale_fill_manual(values = c("gold", "gray70", "indianred4", "blue")) +
  theme_bw() +
  theme(legend.position = "none")         
# create dot plot
ggplot(plotdata, aes(x = Date, y = Prepositions, color=Region)) +  
  geom_point() +  
  scale_color_manual(values = c("indianred4",  "gray50")) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1)) 
# create dot plot
ggplot(plotdata, aes(Date, fill=Region)) +  
  geom_density(alpha=.5) +  
  scale_fill_manual(values = c("indianred4",  "gray50")) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1)) 
# load and process speeches by clinton
clinton <- readLines("https://slcladal.github.io/data/Clinton.txt") %>%
  paste(sep = " ", collapse = " ")
# load and process speeches by trump
trump <- readLines("https://slcladal.github.io/data/Trump.txt") %>%
  paste(sep = " ", collapse = " ")
# clean texts
docs <- Corpus(VectorSource(c(clinton, trump))) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument)
# create term document matrix
tdm <- TermDocumentMatrix(docs) %>%
  as.matrix()
colnames(tdm) <- c("Clinton","Trump")
# calculate rel. freq.
tdm[, 1] <- as.vector(unlist(sapply(tdm[, 1], function(x) round(x/colSums(tdm)[1]*1000, 0) )))
# calculate rel. freq.
tdm[, 2] <- as.vector(unlist(sapply(tdm[, 2], function(x) round(x/colSums(tdm)[2]*1000, 0) )))
# create word cloud
wordcloud(docs, max.words = 100, 
          colors = brewer.pal(6, "BrBG"), 
          random.order = FALSE)
# create comparison cloud
comparison.cloud(tdm, 
                 max.words = 100, 
                 random.order = FALSE, 
                 colors = c("blue", "red"), 
                 title.bg.colors="white",
                 bg.color = "black")
# create commonality cloud
commonality.cloud(tdm, 
                  max.words = 100, 
                  random.order = FALSE, 
          colors = brewer.pal(6, "Spectral"))
At first, I thought that word clouds are simply a fancy but not very helpful way to inspect language data but I have to admit that word clouds really surprised me as they do appear to possess potential to provide an idea of what groups of people are talking about. The comparative word cloud shows that the Trump uses a lot of contractions ("'re", "'ll", etc.) and stresses concepts linked to the future ("going") thereby stressing his vision of the US (great). In Contrast, Clinton did not use contractions but talked about "Americans", "work", the "economy", and "women".
# References

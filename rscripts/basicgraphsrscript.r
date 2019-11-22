
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
                   "stringr", "SnowballC"))
# attach iris
attach(iris) 
# inspect  iris data
str(iris)                  
# attach iris
attach(mtcars) 
# inspect  iris data
str(mtcars)                  
# create simple scatter plot
plot(Sepal.Length ~ Sepal.Width,         # plot Sepal.Length by Sepal.Width
     type = "p",                         # plot type p (points) 
     data = iris,                        # data from data set iris  
     ylab = "Sepal Length",              # add y-axis label 
     xlab = "Sepal Width",               # add x-axis label 
     main = "plot type 'p' (points)"     # add title 
     )                                   # end drawing plot
# create simple scatter plot with ablines
plot(Sepal.Length ~ Sepal.Width,          # plot Sepal.Length by Sepal.Width
     type = "p",                          # plot type p (points) 
     data = iris,                         # data from data set iris  
     ylab = "Sepal Length",               # add y-axis label 
     xlab = "Sepal Width",                # add x-axis label 
     main = "Scatterplot",                # add title 
     pch = 20,                            # use point symbol 20 (filled circles)
     col = "lightgrey"                    # define symbol color as lightgrey
     )                                    # end drawing plot
abline(                                   # add regression line (y~x) 
  lm(Sepal.Length ~ Sepal.Width),         # draw rgeression line of linear model (lm)   
  col="red"                               # define line colour as red
  )                                       # end drawing line             
lines(                                    # add line (x,y)
  lowess(Sepal.Width, Sepal.Length),      # draw smoothed lowess line (x,y) 
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
# activate lattice package
library(lattice)             
# create simple scatter plot
xyplot(Sepal.Length ~ Sepal.Width,  # plot Sepal.Length by Sepal.Width
       ylab = "Sepal Length",       # add y-axis label 
       xlab = "Sepal Width"         # add x-axis label 
       )                            # end drawing plot
# create scatter plots by species
xyplot(Sepal.Length ~ Sepal.Width | Species, # plot Sepal.Length by Sepal.Width by Species
       ylab = "Sepal Length",                # add y-axis label
       xlab = "Sepal Width",                 # add y-axis label
       grid = TRUE                           # add grids to panels
       )                                     # end drawing plot
# activate ggplot2 package
library(ggplot2)               
# create simple scatter plot
ggplot(iris,                    # plot data from data set iris  
       aes(x=Sepal.Length,      # define x-axis
           y= Sepal.Width)) +   # define y-axis
  geom_point()                  # define plot type
# create scatter plot colored by species
ggplot(iris,                   # plot data from data set iris
       aes(x=Sepal.Length,     # define x-axis
           y= Sepal.Width,     # define y-axis
           color = Species)) + # define to color by Species
  geom_point() +               # define plot type
  theme_bw()                   # define theme  as black and white (bw)
# create scatter plot colored by species
scatterplot <- ggplot(          # create plot object called scatterplot
  iris,                         # plot data from data set iris 
       aes(x=Sepal.Length,      # define x--axis
           y= Sepal.Width,      # define y-axis and color by Species
           color = Species)) +  # color by Species
  geom_point() +                # define plot type (what should be drawn)
  scale_color_manual(           # color manually
    values = c('indianred4',    # define colours to be used
               'darkgrey', 
               'gold')) + 
  theme_bw()                    # define theme as black and white
# add ablines (regression lines)
scatterplot + geom_smooth(method = "lm")
# create scatter density plot
ggplot(          # create plot object called scatterplot
  iris,                         # plot data from data set iris 
       aes(x=Sepal.Length,      # define x--axis
           y= Sepal.Width,      # define y-axis and color by Species
           color = Species)) +  # color by Species
    scale_color_manual(         # color manually
    values = c('indianred4',    # define colors to be used
               'darkgrey', 
               'gold')) + 
  theme_bw() +                  # define theme (black and white theme)
  geom_density_2d()             # add 2-dm. density
# scatter plot with error bars
ggplot(iris,      
        # def. x/y-axes
       aes(Species, Sepal.Length,              
           # col. by Species
           colour = Species)) +                  
  # add title
  ggtitle("Sepal Length by Species") +          
  # create a dot at means
  stat_summary(fun.y = mean, geom = "point",     
               # means by Species
               aes(group= Species)) +          
  # bootstrap data
  stat_summary(fun.data = mean_cl_boot,       
               # add error bars
               geom = "errorbar", width = 0.2) + 
  # def. y-axis range
  coord_cartesian(ylim = c(4, 8)) +              
  # def. font size
  theme_set(theme_bw(base_size = 20)) +         
  # def. size of x-axis
  theme(axis.text.x = element_text(size=24,     
                                   # def. style
                                   face="plain"),  
        # def. size of y-axis
        axis.text.y = element_text(size=24,        
                                   # def. style
                                   face="plain"),  
        # def. col. of x-axis title
        axis.title.x = element_text(colour="grey20", 
                                     # def. font size of x-axis title
                                    size=24,  
                                    # def. style
                                    face="plain"), 
        # def. col. of y-axis title
        axis.title.y = element_text(colour="grey20",
                                    # def. font size of x-axis title
                                    size=24, 
                                    # def. angle of x-axis title
                                    angle=90, 
                                    # def. style
                                    face="plain")) + 
  # def. axes labels
  labs(x = "Species", y = "Sepal Length (cm)") +     
  # def. to col.
  scale_color_manual(values = c("grey20", "grey20", "grey20"), 
                     # suppress legend 
                     guide = FALSE) + 
  # define theme as black and white
  theme_bw()                    
# clean workspace
rm(list=ls(all=T))
# unload package Rmsic
unloadNamespace("Rmisc")
# activate dplyr package
library(dplyr)
# process data
linedata <- mtcars %>%       # create object linedata with mtcars data
  group_by(cyl) %>%          # group mtcars data by cyl
  summarise(                 # summarise grouped data
    MilesPerGallon_mean = mean(mpg), # create var. MilesPerGallon_mean
    Weigth_mean  = mean(wt), # create variable Weigth_mean
    HorsePower_mean  = mean(hp),     # create var. HorsePower_mean
    Speed_mean = mean(qsec)  # create variable Speed_mean
  )                          # end summary
# attach data
attach(linedata)
# inspect data
linedata                     
# cerate simple line graph
plot(MilesPerGallon_mean, # plot MilesPerGallon_mean 
     type = "b")          # type of graph (both symbols and lines)
lines(Speed_mean,         # draw line for Speed_mean
      type = "b")         # type of graph (both symbols and lines)
# create customizes line graph
plot(MilesPerGallon_mean,     # plot MilesPerGallon_mean
     type = "b",              # type of graph (symbols plus lines)
     ylim = c(10, 30),        # y-axis range
     ylab = "Value",          # y-axis title 
     xlab = "Number of Cylinders",    # x-axis title  
     axes = F)                # suppress drawing of axes 
lines(Speed_mean,             # draw line for Speed_mean
      type = "b",             # draw both symbols and lines 
      lty = 2,                # draw line type 2 (dashed) 
      pch = 2)                # draw symbols type 2 (empty triangles)
axis(1,                       # x-axis
     at = 1:3,                # tick marks at pos. 1 to 3
     lab = c("4","6","8")     # define tick marks
     )                        # end x-axis definition
axis(2,                       # y-axis 
     at = seq(10, 30, 5),     # tick marks at pos. 10 to 30, int. 5 
     las = 1,                 # draw tick marks perpendicular to axis
     lab = seq(10, 30, 5)     # define tick marks labels
     )                        # end y-axis definition
box()                         # draw box around plotting area
legend("topright",            # add legend at position top right 
       c("Miles per Gallon",  # add description of legend elements
         "Speed (1/4 Mile in sec.)"), 
       lty = c(1, 2)          # define line types
       )                      # end legend definition
# load data
data03 <- read.delim("https://slcladal.github.io/data/data03.txt", 
                     sep = "\t", header = T)
# load data
data03 <- read.delim("https://slcladal.github.io/data/data03.txt", 
                     sep = "\t", header = T)
# create line graph
plot(data03$Variable1,   
     type = "b", 
     col = "blue",
     ylab = "Value")
# draw lines
lines(data03$Variable2, 
      type = "b",
      col = "red") 
# create line graph
ggplot(linedata,   
       # def. x- and y-axis, col. by cyl
       aes(cyl, MilesPerGallon_mean)) +  
  # draw line and def. line col.
  geom_line(color = 'lightgray') +       
  # draw line for Speed_mean
  geom_line(y = Speed_mean, color = 'gold') +  
  # add x-axis label
  labs(x = "Number of Cylinders") +      
  # customize y-axis label
  scale_y_continuous(name="Value",       
                     # customize tick positions 
                     limits=c(10, 30)) + 
  # customize x-axis tick positions
  scale_x_continuous(breaks=c(4, 6, 8),  
                     # add labels to x-axis
        labels=c("4", "6", "8")) +       
  # define theme as black and white
  theme_bw()                             
# create customized line graph
ggplot(linedata,                         
       # define x/y-axis, col. by cyl
       aes(cyl, MilesPerGallon_mean)) +  
  # draw smoothed line for MilesPerGallon_mean
  geom_smooth(aes(y = MilesPerGallon_mean,       
                  # def. color
                  color = "MilesPerGallon_mean", 
                  # def. line type
                  linetype = "MilesPerGallon_mean")) + 
  # draw smoothed line for Speed_mean
  geom_smooth(aes(y = Speed_mean,       
                  # def. col.
                  color = "Speed_mean",        
                  # def. line type
                  linetype = "Speed_mean"))  +    
  # legend without background color
  guides(color=guide_legend(override.aes=list(fill=NA))) +  
  # def. legend position
  theme(legend.position="top") +  
  # def. linetype
  scale_linetype_manual(values=c("longdash", "dotted"), 
                        # def. legend header
                        name=c("Variables"),
                        # def. linetypes
                        breaks = c("MilesPerGallon_mean", 
                                   "Speed_mean"),
                        # def. labels
                        labels = c("MilesPerGallon_mean", 
                                   "Speed_mean")) + 
  # def. col.
  scale_colour_manual(values=c("goldenrod2",        
                               "indianred4"),
                      # define legend header
                      name=c("Variables"),
                      # define elements
                      breaks=c("MilesPerGallon_mean", "Speed_mean"),  
                      # define labels
                      labels = c("MilesPerGallon_mean", "Speed_mean")) +
  # add x-axis label
  labs(x = "Number of Cylinders") +      
  # customize x-axis tick positions
  scale_x_continuous(breaks=c(4, 6, 8), 
                     # add labels to x-axis tick pos.
                     labels=c("4", "6", "8")) +
  # add y-axis label
  scale_y_continuous(name="Value",  
                     # customize tick y-axis
                     limits=c(10, 30)) + 
  # define theme  as black and white
  theme_set(theme_bw(base_size = 10))    
# create lickert data
LikertData <- data.frame(Course=
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
head(LikertData)
# create cumulative density plot
ggplot(LikertData,aes(x=Satisfaction,color=Course)) + 
  geom_step(aes(y=..y..),stat="ecdf") +
  labs(y = "Cumulative Density") + 
  scale_x_discrete(limits=c("1","2","3","4","5"), breaks=c(1,2,3,4,5),
        labels=c("very dissatisfied", "dissatisfied", "neutral",
                              "satisfied", "very satisfied")) + 
  scale_colour_manual(values=c("goldenrod2", # def. col.
                               "indianred4", 
                               "blue"))  
# create tabulated data 
counts <- table(mtcars$gear)     # create table of gears in mtcars 
# create simple bar plot
barplot(counts,                  # start barplot and use counts table
        main="Frequency of cars by their number of gears",   # title
        xlab="Number of Gears",  # x-axis label
        ylim = c(0,20)           # y-axis range
        )                        # end plot
# create customized bar plot
barplot(counts,
        main="Frequency of cars by their number of gears", # title
        horiz = T,                # horizontal bars
        xlab="Number of Gears",   # x-axis label
        col = c("grey30", # def. col.
                "grey50", 
                "grey70"),
        xlim = c(0,20),  # y-axis range
        las = 1          # def. labels as perpendicular to axis
        )                # end plot
box()                    # create box around panel
# create a vector of values
Values <- c(10, 12, 7, 14, 9) 
# create a vector of labels
Labels <- c("US", "UK", "Australia", "Germany", "France")
# calculate the percentages for each element in values
Percentages <- round(Values/sum(Values)*100)
# add percentage values to labels
Labels <- paste(Labels, Percentages) 
# add % symbol to labels
Labels <- paste(Labels,"%",sep="")            
# combine in a data frame
piedata <- data.frame(Values, Percentages, Labels)  
# attach piedata to avoid having to specify the data
attach(piedata) 
# inspect data
piedata 
# create pie chart
pie(Values,
    labels = Labels, 
    col=rainbow(length(Values)),
    main="Pie Chart of Countries")
# create bar plot
barplot(
# plot Values in decreasing order data
        Values[order(Values, decreasing = T)],   
        ylim = c(0,20),                # def. y-axis range
        col=rainbow(length(Values)),   # def. col.
        main="Pie Chart of Countries"  # title
        )                              # end plot
# add text (labels)
text( 
# def. x-axis pos. of labels
  seq(0.7, 5.5, 1.2),                  
# def. the y-axis positions of the labels (2 higher than bar)
  Values[order(Values, decreasing = T)] + 2,  
# define text to be plotted
  Labels[order(Values, decreasing = T)]         
  )
# create bar plot data
barplotdata=matrix(sample(1:50,15) , nrow=3) 
# add column names
colnames(barplotdata)=c("1900","1920","1940","1960","1980") 
# add rownames
rownames(barplotdata)=c("A","B","C") 
# calculate percentages
barplotdatapercent=apply(barplotdata, 2, function(x) {
  round(x*100/sum(x),1) }) 
# inspect data
barplotdatapercent                                                              
# create bar plot
barplot(                      # create a barplot
  barplotdatapercent,         # use barplotdatapercent
  main="Stacked Barplot",     # add title
  xlab="Decade",              # add x-axis label
  ylab="Percent",             # add y-axis label
  col=c("darkblue","red", "gold"),       # def. col.
  legend = rownames(barplotdatapercent), # add legend
  beside=F                    # stack bars
)                             # end barplot
box()                         # draw box around plot
# create grouped bar plot
barplot(                     # create a barplot
  barplotdata,               # use barplotdata
  main="Grouped Barplot",    # add title
  xlab="Decade",             # add x-axis label
  ylab="Percent",            # add y-axis label
  ylim=c(0,50),              # def. y-axis range
  col=c("darkblue",          # def. col.
        "red", 
        "gold"),       
  beside=T,                  # place bars beside each other
  legend = rownames(barplotdata),        # add legend
  args.legend=list(          # place legend beside plotting area
    x=22,                    # define x-axis position
    y=max(barplotdata),      # define y-axis position
    bty = "n"                # separate legend elements
    )                        # end legend definition
)                            # end barplot
# load data
data02 <- read.delim("https://slcladal.github.io/data/data02.txt", sep = "\t", header = F)
# transform data into a matrix
data02 <- as.matrix(data02)
# load data
data02 <- read.delim("https://slcladal.github.io/data/data02.txt", sep = "\t", header = F)
# transform data into a matrix
data02 <- as.matrix(data02)
# create a barplot
barplot(                                 
  data02,                   # use barplotdata
  main="Barplot Exercise",  # add title
  xlab="",                  # add x-axis label
  ylab="Frequency",         # add y-axis label
  ylim=c(0,700),            # define y-axis range
  col=c("darkblue","red"),  # define colours
  beside=T                  # bars next to each other
)
# create a data set called barplotdatagg1
barplotdatagg1 <- data.frame(             
  # create a vector with Variable values
  rep(rownames(barplotdata), ncol(barplotdata)),  
  # create a vector with Decade values
  rep(colnames(barplotdata), each = nrow(barplotdata)), 
  # create a vector with Frequency values
  as.vector(barplotdata)                          
  )       
# define column names
colnames(barplotdatagg1) <- c("Variable", "Decade", "Frequency") 
# attach barplotdatagg1 data set
attach(barplotdatagg1)                             
# inspect barplotdatagg1
head(barplotdatagg1)                              
# create a data set called barplotdatagg2
barplotdatagg2 <- barplotdatagg1 %>% 
  group_by(Decade) %>%        # group by Decade
  summarise(                  # start summary 
    CumFreq = sum(Frequency)  # create var. of cum. freqs.
  )    
# attach barplotdatagg2 data set
attach(barplotdatagg2)               
# inspect barplotdatagg2
head(barplotdatagg2)                 
# plot barplotdatagg
ggplot(barplotdatagg2,             
       aes(Decade, CumFreq)) + # define x- and y-axis
  geom_bar(stat="identity") +  # determine type of plot
  theme_bw()                   # define theme as black and white
# plot barplotdatagg1
ggplot(barplotdatagg1,                   
       aes(Decade, Frequency,            # def. x/y-axis
           fill = Variable)) +           # def. grouping variable
  geom_bar(stat="identity",              # def. type of plot
           position=position_dodge()) +  # def. grouping
  # define colours
  scale_fill_manual(values=c("goldenrod2", 
                             "gray70", 
                             "indianred4")) + 
  # add text and define colour
  geom_text(aes(label=Frequency), vjust=1.6, color="white", 
            # define text position and size
            position = position_dodge(0.9), size=3.5) +   
  # define theme as black and white
  theme_bw()                                                
# create likert data
LikertData <- data.frame(Course=c(rep(c("Chinese", "German","Japanese"),each = 100)),
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
Likert <- LikertData %>%
  group_by(Course, Satisfaction) %>%
  mutate(Frequency = n())
Likert <- unique(Likert)
# inspect data
head(Likert)
# create grouped bar plot
ggplot(Likert,           
       aes(Satisfaction, Frequency,     # def. x/y-axis
           fill = Course)) +            # def. grouping var.
  geom_bar(stat="identity",             # def. type of plot
           position=position_dodge()) + # def. grouping
  # define colours
  scale_fill_manual(values=c("goldenrod2", 
                             "gray70", 
                             "indianred4")) + 
  # add text and define colour
  geom_text(aes(label=Frequency), vjust=1.6, color="white", 
            # define text position and size
            position = position_dodge(0.9), 
            size=3.5) +     
    scale_x_discrete(limits=c("1","2","3","4","5"), 
                     breaks=c(1,2,3,4,5),
        labels=c("very dissatisfied", 
                 "dissatisfied", 
                 "neutral",
                 "satisfied", 
                 "very satisfied")) + 
  theme_bw()    # def. theme as black and white
# activate package
library(likert)                         
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
# activate scales library
library(scales)
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
# activate vcd package
library(vcd)    
# recode Sex var. to avoid overlay in plot
attr(HairEyeColor, "dimnames")$Sex=c("M", "F")   
# create association plot
assoc(HairEyeColor, 
      shade=TRUE)
# create a mosaic plot
mosaic(HairEyeColor, # use HairEyeColor data set
       # color code difference between observed and expected values
       shade=T,  
       # add a legend to explain the colour coding
       legend=TRUE)  
# create a boxplot of Sepal.Length by Species
boxplot(Sepal.Length~Species, 
        # use data set iris
        data=iris,       
        # def. title
        main="Boxplot",     
        # def. y-axis label
        ylab="Sepal Length (cm)",                          
        # add x-axis label
        xlab="Species",                              
        # def. col. of boxes
        col = c("lightgrey", "lightblue", "lightgreen")    
        ) 
# create a boxplot of Sepal.Length by Species
boxplot(Sepal.Length~Species,                              
        # use data set iris
        data=iris,                                         
        # def. title
        main="Boxplot",                                   
        # def. y-axis label
        ylab="Sepal Length (cm)",                          
        # def. x-axis label
        xlab="Species",                                   
        # def. col. of boxes
        col = c("lightgrey", "lightblue", "lightgreen"),  
        # add notches
        notch = T                                          
        ) 
# create data set
Language <- c(rep("German", 10), rep("English", 10))
Score <- c(6, 65, 12, 56, 45, 84, 38, 46, 64, 24, 67, 16, 56, 34, 54, 42, 36, 47, 54, 29)
# create data frame
databp <- data.frame(Language, Score)
# inspect data
databp
# create a boxplot of Sepal.Length by Species
boxplot(Score~Language,                              
        # use data set iris
        data=databp,                                       
        # def. title
        main="Boxplot",                       
        # def. y-axis label
        ylab="Score",   
        # def. x-axis label
        xlab="Language",                  
        # def. col. of boxes
        col = c("lightgrey", "darkgrey"),   
        # add notches
        notch = F)
# create a boxplot with iris data
ggplot(iris,                   
       # def. x/y-axis
       aes(Species, Sepal.Length,              
           # define coloring factor
           color = Species)) +                 
  # define outlier color
  geom_boxplot(outlier.colour="black",  
                # define outlier shape
               outlier.shape=16,       
                # define outlier size
               outlier.size=2,         
               # do not draw notches
               notch=FALSE,             
               # def. col. of boxes
               fill=c("gold", "gray70", "indianred4"), 
               # define color edges
               color="black") 
# create violin plot
ggplot(iris,   # use iris data          
       # def. x/y-axes
       aes(x=Species, y=Sepal.Length,       
           # col. by Species
           fill=Species)) +                 
  # def. plot type (violin)
  geom_violin(trim=FALSE)+             
  # create (additional) boxplots
  geom_boxplot(width=0.1, fill="white")+   
  # def. title
  labs(title="Sepal Length  by Species",    
       # def. axes labels
       x="Species", y = "Length (cm)") +    
  # def. col.
  scale_fill_manual(values=c("#999999", 
                             "#E69F00", 
                             "#56B4E9")) + 
  # use black and white theme
  theme_bw() +                              
  # surpress legend
  theme(legend.position="none")             
# create dot plot
ggplot(iris,  # use iris data                              
       # def. x/y-axes
       aes(x=Sepal.Length, y=Sepal.Width,       
           # col. by Species
           color=Species)) +                    
  # create dot plot
  geom_point() +                                
  # def. col. 
  scale_color_manual(values = c('indianred4', 
                                'darkgrey', 
                                'gold')) + 
  # add legend
  theme(legend.position=c(0,1), legend.justification=c(0,1)) 
# create density plot
ggplot(iris,     # use iris data    
       # define axes and coloring
       aes(Sepal.Length, fill=Species)) +  
  # create density plot and define smoothing factor
  geom_density(alpha=.5) +                 
  # def. col.
  scale_fill_manual(values = c('indianred4', 
                               'darkgrey', 
                               'gold')) + 
  # surpress legend
  theme(legend.position = "none")          
# activate packages
library(dplyr)
library(tm)
library(wordcloud)
library(stringr)
library(SnowballC)
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

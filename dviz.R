knitr::include_graphics("https://slcladal.github.io/images/uq1.jpg")

## # install packages
## install.packages("knitr")
## install.packages("lattice")
## install.packages("tidyverse")
## install.packages("likert")
## install.packages("scales")
## install.packages("vcd")
## install.packages("tm")
## install.packages("ggridges")
## install.packages("SnowballC")
## install.packages("tidyr")
## install.packages("gridExtra")
## install.packages("knitr")
## install.packages("DT")
## install.packages("wordcloud")
## install.packages("flextable")
## install.packages("hexbin")
## install.packages("EnvStats")
## install.packages("ggstatsplot")
## install.packages("PMCMRplus")
## # install klippy for copy-to-clipboard button in code chunks
## install.packages("remotes")
## remotes::install_github("rlesur/klippy")

# set options
options(stringsAsFactors = F)          # no automatic data transformation
options("scipen" = 100, "digits" = 12) # suppress math annotation
# activate packages
library(knitr)
library(lattice)
library(tidyverse)
library(likert)
library(scales)
library(vcd)
library(tm)
library(ggridges)
library(SnowballC)
library(tidyr)
library(gridExtra)
library(knitr)
library(DT)
library(wordcloud)
library(flextable)
library(hexbin)
# activate klippy for copy-to-clipboard button
klippy::klippy()

# load data
pdat  <- base::readRDS(url("https://slcladal.github.io/data/pvd.rda", "rb"))

# inspect data
pdat %>%
  as.data.frame() %>%
  head(15) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 15 rows of the pdat data.")  %>%
  flextable::border_outer()

clrs5 <- c("indianred4", "gray30", "darkblue", "orange", "gray80")
clrs3 <- c("indianred4", "gray30", "darkblue")
clrs2 <- c("orange", "gray80")

# create simple scatter plot
plot(Prepositions ~ Date,                 # plot Prepositions by Date
     type = "p",                          # plot type p (points) 
     data = pdat,                     # data from data set pdat  
     ylab = "Prepositions (Frequency)",   # add y-axis label 
     xlab = "Date (year of composition)", # add x-axis label 
     main = "plot type 'p' (points)"      # add title 
     )                                    # end drawing plot

# create simple scatter plot with regression lines (ablines)
plot(Prepositions ~ Date,                 # plot Prepositions by Date
     type = "p",                          # plot type p (points) 
     data = pdat,                     # data from data set iris  
     ylab = "Prepositions (Frequency)",   # add y-axis label 
     xlab = "Date (year of composition)", # add x-axis label 
     main = "Scatterplot",                # add title 
     pch = 20,                            # use point symbol 20 (filled circles)
     col = "lightgrey"                    # define symbol colour as light grey
     )                                    # end drawing plot
abline(                                   # add regression line (y~x) 
  lm(pdat$Prepositions ~ pdat$Date),      # draw regression line of linear model (lm) 
  col="red"                               # define line colour as red
  )                                       # end drawing line             
lines(                                    # add line (x,y)
  lowess(pdat$Prepositions ~ pdat$Date),  # draw smoothed lowess line (x,y) 
  col="blue"                              # define line colour as blue
  )                                       # end drawing line

data03  <- base::readRDS(url("https://slcladal.github.io/data/d03.rda", "rb"))

# load data03
data03 <- read.delim("https://slcladal.github.io/data/data03.txt", 
  sep = "\t", header = T)
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
xyplot(Prepositions ~ Date,  
       # add y-axis label 
       ylab = "Prepositions (Frequency)",   
       # add x-axis label
       xlab = "Date (year of composition)", 
       data = pdat)                                    

# create scatter plots by species
xyplot(Prepositions ~ Date | Genre,     
       # add y-axis label
       ylab = "Prepositions (Frequency)", 
       # add y-axis label
       xlab = "Date (year of composition)", 
       # add grids to panels
       grid = TRUE,
       data = pdat
       )    

# create scatter plots by species
xyplot(Prepositions ~ Date | Genre,
       ylab = "Prepositions (Frequency)",  
       xlab = "Date (year of composition)", 
       grid = TRUE,   
       # symbol type (20 = filled dots)
       pch = 20,            
       # color of symbols
       col = "black",
       data = pdat
       ) 

# create simple scatter plot
# use data set "pdat"
ggplot(pdat,  
       # define axes
       aes(x= Date,        
           y= Prepositions)) + 
  # define plot type
  geom_point()                  

ggplot(pdat,    
       # define axes
       aes(x=Date,             
           y= Prepositions, 
           # define to color by Species
           color = GenreRedux)) + 
  # define plot type
  geom_point() +   
  # define theme  as black and white (bw)
  theme_bw()                   

# create scatter plot colored by genre
ggplot(pdat, aes(Date, Prepositions, color = GenreRedux, shape = GenreRedux)) +
  geom_point() +
  guides(shape=guide_legend(override.aes=list(fill=NA))) +
  scale_shape_manual(name = "Genre", 
                     breaks = names(table(pdat$GenreRedux)), 
                     values = 1:5) +
  scale_color_manual(name = "Genre", 
                     breaks = names(table(pdat$GenreRedux)), 
                     values = clrs5) +
  theme_bw() +
  theme(legend.position="top")

# create scatter plot colored by genre in different panels
ggplot(pdat, aes(Date, Prepositions,  color = Genre)) +
  facet_wrap(vars(Genre), ncol = 4) +
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(size=8, angle=90))

# create scatter plot colored by genre in different panels
ggplot(pdat, aes(x=Date, y= Prepositions,  color = Genre)) +
  facet_wrap(vars(Genre), ncol = 4) +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  theme(legend.title = element_blank(), 
        axis.text.x = element_text(size=8, angle=90))

# create scatter density plot
ggplot(pdat, aes(x=Date, y= Prepositions,  color = GenreRedux)) +
    facet_wrap(vars(GenreRedux), ncol = 5) +
  theme_bw() +                  
  geom_density_2d() +
  theme(legend.position = "top",
        legend.title = element_blank(), 
        axis.text.x = element_text(size=8, angle=90))

# scatter plot with error bars
ggplot(pdat, aes(x=reorder(Genre, Prepositions, mean), y= Prepositions,  group = Genre)) +                 
  stat_summary(fun = mean, geom = "point", aes(group= Genre)) +          
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

# ballon plot
pdat %>%
  dplyr::mutate(DateRedux = factor(DateRedux)) %>%
  dplyr::group_by(DateRedux, GenreRedux) %>%
  dplyr::summarise(Prepositions = mean(Prepositions)) %>%
  ggplot(aes(DateRedux, 100, 
             size = Prepositions,
             fill = GenreRedux)) +
  facet_grid(vars(GenreRedux)) +
  geom_point(shape = 21) +
  scale_size_area(max_size = 15) +
  coord_cartesian(ylim = c(50, 150)) +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_discrete(guide = "none")

# create dot plot
ggplot(pdat, aes(x = Date, y = Prepositions, color=Region)) +  
  geom_point() +  
  scale_color_manual(values = clrs2) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1)) 

# create dot plot
ggplot(pdat, aes(Date, fill=Region)) +  
  geom_density(alpha=.5) +  
  scale_fill_manual(values = clrs2) + 
  theme(legend.position=c(0,1), legend.justification=c(0,1)) 

# create dot plot
ggplot(pdat, aes(Date, Prepositions)) +  
  geom_density2d_filled()

# create dot plot
pdat %>%
  ggplot(aes(x = Date, y = Prepositions)) +  
  geom_hex()

pdat %>%
  dplyr::group_by(DateRedux, GenreRedux) %>%
  dplyr::summarise(Frequency = mean(Prepositions)) %>%
  ggplot(aes(x=DateRedux, y= Frequency, group= GenreRedux, color = GenreRedux)) +
  # add geom layer with lines
  geom_line()

ggplot(pdat, aes(x=DateRedux, y= Prepositions, group= GenreRedux, color = GenreRedux)) +
  # add geom layer with lines
  geom_smooth()

# define aesthetics
ggplot(pdat, aes(x=Date, y= Prepositions,  color = GenreRedux, linetype = GenreRedux)) +
  # add geom layer with lines
  geom_smooth(se = F) +  
  # legend without background color
  guides(color=guide_legend(override.aes=list(fill=NA))) +  
  # def. legend position
  theme(legend.position="top") +  
  # def. linetype
  scale_linetype_manual(values=c("twodash", "dashed", "dotdash", "dotted", "solid"), 
                        # def. legend header
                        name=c("Genre"),
                        # def. linetypes
                        breaks = names(table(pdat$GenreRedux)),
                        # def. labels
                        labels = names(table(pdat$GenreRedux))) + 
  # def. col.
  scale_colour_manual(values=clrs5,
                      # define legend header
                      name=c("Genre"),
                      # define elements
                      breaks=names(table(pdat$GenreRedux)),  
                      # define labels
                      labels = names(table(pdat$GenreRedux))) +
  # add x-axis label
  labs(x = "Year") +      
  # customize x-axis tick positions
  scale_x_continuous(breaks=seq(1100, 1900, 100), 
                     # add labels to x-axis tick pos.
                     labels=seq(1100, 1900, 100)) +
  # add y-axis label
  scale_y_continuous(name="Relative frequency \n(per 1,000 words)",  
                     # customize tick y-axis
                     limits=c(100, 200)) + 
  # define theme  as black and white
  theme_bw(base_size = 10)  

# create dot plot
pdat %>%
  dplyr::mutate(DateRedux = as.numeric(DateRedux)) %>%
  dplyr::group_by(DateRedux) %>%
  dplyr::summarise(Mean = mean(Prepositions),
                   Min = min(Prepositions),
                   Max = max(Prepositions)) %>%
  ggplot(aes(x = DateRedux, y = Mean)) +  
  geom_ribbon(aes(ymin = Min, ymax = Max), fill = "gray80") +
  geom_line() +
  scale_x_continuous(labels = names(table(pdat$DateRedux)))

ldat <- base::readRDS(url("https://slcladal.github.io/data/lid.rda", "rb"))

# inspect data
ldat %>%
  as.data.frame() %>%
  head(15) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 15 rows of the ldat data.")  %>%
  flextable::border_outer()

# create cumulative density plot
ggplot(ldat,aes(x = Satisfaction, color = Course)) + 
  geom_step(aes(y = ..y..), stat = "ecdf") +
  labs(y = "Cumulative Density") + 
  scale_x_discrete(limits = 1:5, breaks = 1:5,
        labels=c("very dissatisfied", "dissatisfied", "neutral", "satisfied", "very satisfied")) + 
  scale_colour_manual(values = clrs3)  

# create bar plot data
bdat <- pdat %>%
  dplyr::mutate(DateRedux = factor(DateRedux)) %>%
  group_by(DateRedux) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::mutate(Percent = round(Frequency/sum(Frequency)*100, 1))

# inspect data
bdat %>%
  as.data.frame() %>%
  head(15) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 15 rows of the bdat data.")  %>%
  flextable::border_outer()

pie(bdat$Percent,
    col = clrs5,
    labels = bdat$Percent)
legend("topright", names(table(bdat$DateRedux)), 
       fill = clrs5)

ggplot(bdat,  aes("", Percent, fill = DateRedux)) + 
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = clrs5) +
  theme_void()

# create pie chart
ggplot(bdat,  aes("", Percent, fill = DateRedux)) + 
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = clrs5) +
  theme_void() +
  geom_text(aes(y = Percent, label = Percent), color = "white", size=6)

piedata <- bdat %>%
  dplyr::arrange(desc(DateRedux)) %>%
  dplyr::mutate(Position = cumsum(Percent)- 0.5*Percent)

# inspect data
piedata %>%
  as.data.frame() %>%
  head(15) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 15 rows of the edited and re-arranged piedata data.")  %>%
  flextable::border_outer()

# create pie chart
ggplot(piedata,  aes("", Percent, fill = DateRedux)) + 
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = clrs5) +
  theme_void() +
  geom_text(aes(y = Position, label = Percent), color = "white", size=6)

hist(pdat$Prepositions,
     xlab = "Prepositions (per 1,000 words)",
     main = "")

ggplot(pdat, aes(Prepositions)) +
  geom_histogram()

ggplot(pdat, aes(Prepositions, fill = Region)) +
  geom_histogram()

# create simple scatter plot
barplot(table(pdat$DateRedux),        # plot Texts by DateRedux
     ylab = "Texts (Frequency)",          # add y-axis label 
     xlab = "Period of composition",      # add x-axis label 
     main = "bar plot in base R",         # add title
     col = clrs5,                         # add colors
     ylim = c(0, 250)                     # define y-axis limits
     )                                    # end drawing plot
grid()                                    # add grid
text(seq(0.7, 5.5, 1.2),                  # add label positions (x-axis)
     table(pdat$DateRedux)+10,        # add label positions (y-axis)
     table(pdat$DateRedux))           # add labels
box()                                     # add box

# create simple scatter plot
barplot(table(pdat$DateRedux, pdat$Region), # plot Texts by DateRedux
        beside = T,                          # bars beside each other
        ylab = "Texts (Frequency)",          # add y-axis label 
        xlab = "Period of composition",      # add x-axis label
        main = "grouped bar plot in base R", # add title
        col = clrs5,                         # add colors
        ylim = c(0, 250)                     # define y-axis limits
        )                                    # end drawing plot
grid()                                       # add grid
text(c(seq(1.5, 5.5, 1.0), seq(7.5, 11.5, 1.0)),    # add label positions (x-axis)
     table(pdat$DateRedux, pdat$Region)+10, # add label positions (y-axis)
     table(pdat$DateRedux, pdat$Region))    # add labels
legend("topleft", names(table(pdat$DateRedux)), # add legend
       fill = clrs5)                        # add colors
box()                                       # add box

# create simple scatter plot
barplot(table(pdat$DateRedux),        # plot Texts by DateRedux
     ylab = "Texts (Frequency)",          # add y-axis label 
     xlab = "Period of composition",      # add x-axis label
     col = clrs5,                         # add colors
     horiz = T,                           # horizontal bars
     xlim = c(0, 250),                    # define x-axis limits
     las = 2,                             # change direction of axis labels
     cex.names = .5)                      # reduce font of axis labels
box()                                     # add box

# bar plot
ggplot(bdat, aes(DateRedux, Percent, fill = DateRedux)) +
  geom_bar(stat="identity") +          # determine type of plot
  theme_bw() +                         # use black & white theme
  # add and define text
  geom_text(aes(y = Percent-5, label = Percent), color = "white", size=3) + 
  # add colors
  scale_fill_manual(values = clrs5) +
  # suppress legend
  theme(legend.position="none")

# bar plot
ggplot(pdat, aes(Region, fill = DateRedux)) + 
  geom_bar(position = position_dodge(), stat = "count") +  
  theme_bw() +
  scale_fill_manual(values = clrs5)

# bar plot
ggplot(pdat, aes(DateRedux, fill = GenreRedux)) + 
  geom_bar(stat="count") +  
  theme_bw() +
  scale_fill_manual(values = clrs5)    

# bar plot
ggplot(pdat, aes(DateRedux, fill = GenreRedux)) + 
  geom_bar(stat="count", position="fill") +  
  theme_bw() +
  scale_fill_manual(values = clrs5) +
  labs(y = "Probability")

# create likert data
nlik <- ldat %>%
  dplyr::group_by(Course, Satisfaction) %>%
  dplyr::summarize(Frequency = n())
# inspect data
head(nlik)

# create grouped bar plot
ggplot(nlik, aes(Satisfaction, Frequency,  fill = Course)) +
  geom_bar(stat="identity", position=position_dodge()) +
  # define colors
  scale_fill_manual(values=clrs5) + 
  # add text and define colour
  geom_text(aes(label=Frequency), vjust=1.6, color="white", 
            # define text position and size
            position = position_dodge(0.9),  size=3.5) +     
    scale_x_discrete(limits=c("1","2","3","4","5"), breaks=c(1,2,3,4,5),
        labels=c("very dissatisfied", "dissatisfied",  "neutral", "satisfied", 
                 "very satisfied")) + 
  theme_bw()

sdat <- base::readRDS(url("https://slcladal.github.io/data/sdd.rda", "rb"))

# inspect data
sdat %>%
  as.data.frame() %>%
  head(15) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 15 rows of the sdat data.")  %>%
  flextable::border_outer()

# clean column names
colnames(sdat)[3:ncol(sdat)] <- paste0("Q ", str_pad(1:10, 2, "left", "0"), ": ", colnames(sdat)[3:ncol(sdat)]) %>%
  stringr::str_replace_all("\\.", " ") %>%
  stringr::str_squish() %>%
  stringr::str_replace_all("$", "?")
# inspect column names
colnames(sdat)

lbs <- c("disagree", "somewhat disagree", "neither agree nor disagree",  "somewhat agree", "agree")
survey <- sdat %>%
  dplyr::mutate_if(is.character, factor) %>%
  dplyr::mutate_if(is.numeric, factor, levels = 1:5, labels = lbs) %>%
  drop_na() %>%
  as.data.frame()

# inspect data
survey %>%
  as.data.frame() %>%
  head(15) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 15 rows of the survey data.")  %>%
  flextable::border_outer()

# load package
library(likert)
# generate plot
plot(likert(survey[,3:12]), ordered = F, wrap= 60)

## survey_p1 <- plot(likert(survey[,3:12]), ordered = F, wrap= 60)
## # save plot
## cowplot::save_plot(here("images", "stu_p1.png"), # where to save the plot
##                    survey_p1,        # object to plot
##                    base_asp = 1.5,  # ratio of space fro questions vs space for plot
##                    base_height = 8) # size! higher for smaller font size
## 

# create plot
plot(likert(survey[,3:8], grouping = survey[,1]))

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
  # suppress legend   
  guides(fill=FALSE) + 
  # def. colours   
  geom_bar(stat="identity", fill=rep(clrs5[1:3], 3)) + 
  # axes titles
  labs(x = "", y = "Score")                                               

# load package
library(ggridges)
# create ridge plot
pdat %>%
  ggplot(aes(x = Prepositions, y = GenreRedux, fill = GenreRedux)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") + 
  labs(y = "", x = "Density of the relative frequency of prepostions")

# create ridge plot
pdat %>%
  ggplot(aes(x = Prepositions, y = GenreRedux, fill = GenreRedux)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges() + 
  theme(legend.position = "none") + 
  labs(y = "", x = "Histograms of the relative frequency of prepostions")

# create boxplot
ggplot(pdat, aes(DateRedux, Prepositions, color = GenreRedux)) +                 
  geom_boxplot(fill=clrs5, 
               color="black") 

# create boxplot
ggplot(pdat, aes(DateRedux, Prepositions, color = GenreRedux)) +                 
  geom_boxplot(outlier.colour="red", 
               outlier.shape=2, 
               outlier.size=5, 
               notch=T, 
               fill=clrs5, 
               color="black") 

library(EnvStats)
# create boxplot
ggplot(pdat, aes(DateRedux, Prepositions, fill = DateRedux, color = DateRedux)) +                 
  geom_boxplot(varwidth = T, color = "black", alpha = .2) +
  geom_jitter(alpha = .2, height = 0, width = .2) + 
  facet_grid(~Region) +
  EnvStats::stat_n_text(y.pos = 65) +
  theme(legend.position = "none") +
  labs(x = "", y = "Frequency (per 1,000 words)") +
  ggtitle("Use of prepositions in English texts across time and regions")

library(ggstatsplot)
# create boxplot
ggstatsplot::ggbetweenstats(data = pdat,
                            x = DateRedux,
                            y = Prepositions,
                            plottype = "box",
                            type = "p",
                            conf.level = 0.95)

ggplot(pdat, aes(DateRedux, Prepositions, fill = DateRedux)) +  
  geom_violin(trim = FALSE, alpha = .5) +  
  scale_fill_manual(values = clrs5) +
  theme_bw() +
  theme(legend.position = "none")         

# load and process speeches by clinton
clinton <- base::readRDS(url("https://slcladal.github.io/data/Clinton.rda", "rb")) %>% paste0(collapse = " ")
# load and process speeches by trump
trump <- base::readRDS(url("https://slcladal.github.io/data/Trump.rda", "rb")) %>%  paste0(collapse = " ")

# clean texts
docs <- Corpus(VectorSource(c(clinton, trump))) %>%
  # clean text data
  tm::tm_map(removePunctuation) %>%
  tm::tm_map(removeNumbers) %>%
  tm::tm_map(tolower)  %>%
  tm::tm_map(removeWords, stopwords("english")) %>%
  tm::tm_map(stripWhitespace) %>%
  tm::tm_map(PlainTextDocument)
# create term document matrix
tdm <- TermDocumentMatrix(docs) %>%
  as.matrix()
colnames(tdm) <- c("Clinton","Trump")

# calculate rel. freq.
tdm[, 1] <- as.vector(unlist(sapply(tdm[, 1], function(x) round(x/colSums(tdm)[1]*1000, 0) )))
# calculate rel. freq.
tdm[, 2] <- as.vector(unlist(sapply(tdm[, 2], function(x) round(x/colSums(tdm)[2]*1000, 0) )))

# load package
library(wordcloud)
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

# reduce data
assocdata <- pdat %>%
  droplevels() %>%
  dplyr::mutate(GenreRedux <- as.character(GenreRedux),
                GenreRedux = dplyr::case_when(GenreRedux == "Conversational" ~ "Conv.",
                                              GenreRedux == "Religious" ~ "Relig.",
                                              TRUE ~ GenreRedux)) %>%
  dplyr::group_by(GenreRedux, DateRedux) %>%
  dplyr::summarise(Prepositions = round(mean(Prepositions), 0)) %>%
  tidyr::spread(DateRedux, Prepositions)
# create matrix 
assocmx <- as.matrix(assocdata[,2:6])
attr(assocmx, "dimnames")[1] <- as.vector(assocdata[,1])

# inspect data
assocmx %>%
  as.data.frame() %>%
  head(15) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Overview of the matrix.")  %>%
  flextable::border_outer()

# create association plot
assoc(assocmx, shade=TRUE)

# create a mosaic plot
mosaic(assocmx, shade=T, legend=TRUE)  

# create data
heatdata <- pdat %>%
  dplyr::group_by(DateRedux, GenreRedux) %>%
  dplyr::summarise(Prepositions = mean(Prepositions)) %>%
  tidyr::spread(DateRedux, Prepositions)
# create matrix 
heatmx <- as.matrix(heatdata[,2:5])
attr(heatmx, "dimnames")[1] <- as.vector(heatdata[,1])
heatmx <- scale(heatmx) %>%
  round(., 2)

# inspect data
heatmx %>%
  as.data.frame() %>%
  head(15) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Overview of the matrix.")  %>%
  flextable::border_outer()

# create heat map
heatmap(heatmx, scale = "none", cexCol = 1, cexRow = 1)

sessionInfo()

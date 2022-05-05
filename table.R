knitr::include_graphics("https://slcladal.github.io/images/uq1.jpg")

knitr::include_graphics("https://slcladal.github.io/images/gy_chili.jpg")

## # install packages
## install.packages("xlsx")
## install.packages("tidyverse")
## install.packages("flextable")
## install.packages("openxlsx")
## install.packages("here")
## # install klippy for copy-to-clipboard button in code chunks
## install.packages("remotes")
## remotes::install_github("rlesur/klippy")

# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation
# load packages
library(dplyr)
library(tidyr)
library(flextable)
library(xlsx)
library(openxlsx)
library(here)
# activate klippy for copy-to-clipboard button
klippy::klippy()

# load data with read.delim
mytable <- base::readRDS(url("https://slcladal.github.io/data/mld.rda", "rb"))

mytable %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 10 rows of mytable.")  %>%
  flextable::border_outer()

## # load data with read.delim
## tab1 <- read.delim("https://slcladal.github.io/data/mlrdata.txt",
##                    sep = "\t", header = TRUE)
## tab2 <- read.table("https://slcladal.github.io/data/mlrdata.txt", header = TRUE)

# load data
exceldata <- openxlsx::read.xlsx("https://slcladal.github.io/data/testdata1.xlsx", 
                                 sheet = 1)

exceldata %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 10 rows of the exceldata.")  %>%
  flextable::border_outer()

# load data
excelcomp <- readxl::read_excel(here::here("data", "testdata1.xlsx"), sheet = 1)

excelcomp %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 10 rows of the excelcomp data.")  %>%
  flextable::border_outer()

head(mytable)

head(mytable, 10)

str(mytable)

# load new data
newdata <- base::readRDS(url("https://slcladal.github.io/data/mld.rda", "rb"))

newdata %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 10 rows of the excelcomp data.")  %>%
  flextable::border_outer()

# example of a data processing pipeline
pipeddata <- base::readRDS(url("https://slcladal.github.io/data/mld.rda", "rb")) %>%
  dplyr::rename(Status = status, Attraction = attraction, Money = money) %>%
  dplyr::group_by(Status, Attraction) %>%
  dplyr::summarise(Mean = mean(Money))
# inspect summarized data
pipeddata

# select and filter
reduceddata <- newdata %>%
  # select the columns attraction and money
  dplyr::select(attraction, money) %>%
  # extract rows which represent cases where the person was interested in someone
  dplyr::filter(attraction == "Interested")
# inspect new table
nrow(reduceddata); table(reduceddata$attraction)

# select and filter
datawithoutmoney <- newdata %>%
  # remove money
  dplyr::select(-money) 
# inspect data
head(datawithoutmoney)

# creating a new column
newdata <- newdata %>%
  dplyr::mutate(Spendalot = ifelse(money >= 100, "Alot", "Alittle")) 
# inspect data
head(newdata)

# renaming columns
newdata <- newdata  %>%
  dplyr::rename(Status = status, Attraction = attraction, Money = money)
# inspect data
head(newdata)

#grouping and summarizing data 
datasummary <- newdata %>%
  dplyr::group_by(Status, Attraction) %>%
  dplyr::summarise(Mean = round(mean(Money), 2), SD = round(sd(Money), 1))
# inspect summarized data
datasummary

# converting data to wide format 
widedata <- datasummary %>%
  # remove SD column
  dplyr::select(-SD) %>% 
  # convert into wide format
  tidyr::spread(Attraction, Mean)
# inspect wide data
widedata

# converting data to long format 
longdata <- widedata %>%
  # convert into long format
  tidyr::gather(Attraction, Money, Interested:NotInterested)
# inspect wide data
longdata

sessionInfo()

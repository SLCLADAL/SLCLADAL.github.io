
# "String Processing"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)
# install libraries
install.packages(c("stringr"))
# read in text
exampletext <- readLines("https://slcladal.github.io/data/testcorpus/linguistics02.txt")
exampletext <-  paste(exampletext, sep = " ", collapse = " ")
# split example text into sentences
splitexampletext <- unlist(strsplit(gsub("(\\.) ", "\\1

# extract substring by position
substr(exampletext, start=14, stop=30)
# find substring
grep("language", splitexampletext, value=FALSE, ignore.case=FALSE, fixed=FALSE)
# find substring
grep("language", splitexampletext, value=TRUE, ignore.case=FALSE, fixed=FALSE)
# find substring
grepl("language", splitexampletext, ignore.case=FALSE, fixed=FALSE)
sub("and", "AND", exampletext, ignore.case=FALSE, fixed=FALSE)
gsub("and", "AND", exampletext, ignore.case=FALSE, fixed=FALSE)
gregexpr("and", exampletext, ignore.case=FALSE, perl=FALSE,
fixed=FALSE)
strsplit(exampletext, "\\. ")
strsplit(gsub("(\\.) ", "\\1somestring", exampletext), "somestring")
paste(splitexampletext, sep=" ", collapse= " ")
toupper(exampletext)
tolower(exampletext)
nchar(exampletext)
# load stringr library
library(stringr)
str_count(splitexampletext)
str_detect(splitexampletext, "and")
str_extract(exampletext, "and")
str_extract_all(exampletext, "and")
str_locate(exampletext, "and") 
str_locate_all(exampletext, "and")
str_match(exampletext, "and") 
str_match_all(exampletext, "and")
str_remove(exampletext, "and") 
str_remove_all(exampletext, "and")
str_replace(exampletext, "and", "AND")
str_replace_all(exampletext, "and", "AND")
str_starts(exampletext, "and") 
str_ends(exampletext, "and")
str_split(exampletext, "and") 
str_split_fixed(exampletext, "and", n = 3)
str_subset(splitexampletext, "and") 
str_which(splitexampletext, "and")
str_view(splitexampletext, "and")
str_view_all(exampletext, "and")
# cretae text with white spaces
text <- " this    is a    text   "
str_pad(text, width = 30)
str_trim(text) 
str_squish(text)
str_wrap(text)
str_order(splitexampletext)
str_sort(splitexampletext)
str_to_upper(exampletext) 
str_to_lower(exampletext) 
str_c(exampletext, additionaltext)
str_conv(exampletext, encoding = "UTF-8")
str_dup(exampletext, times=2)
str_flatten(sentences, collapse = " ")
str_flatten(sentences)
str_length(exampletext)
# create sentences with NA
sentencesna <- c("Some text", NA, "Some more text", "Some NA text")
# apply str_replace_na function
str_replace_na(sentencesna, replacement = "Something new")
str_trunc(sentences, width = 20)
str_sub(exampletext, 5, 25)
word(exampletext, 2:7)
name <- "Fred"
age <- 50
anniversary <- as.Date("1991-10-12")
str_glue(
  "My name is {name}, ",
  "my age next year is {age + 1}, ",
  "and my anniversary is {format(anniversary, '%A, %B %d, %Y')}."
)
mtcars %>% 
  str_glue_data("{rownames(.)} has {hp} hp")

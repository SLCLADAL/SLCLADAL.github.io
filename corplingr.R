knitr::include_graphics("https://slcladal.github.io/images/uq1.jpg")

knitr::include_graphics("https://slcladal.github.io/images/antconc.png")

knitr::include_graphics("https://slcladal.github.io/images/quantcl_gries.png")

## corpusfiles <- list.files(here::here("data/Corpus"), # path to the corpus data
##                           # file types you want to analyze, e.g. txt-files
##                           pattern = ".*.txt",
##                           # full paths - not just the names of the files
##                           full.names = T)

## corpus <- sapply(corpusfiles, function(x){
##   x <- scan(x,
##             what = "char",
##             sep = "",
##             quote = "",
##             quiet = T,
##             skipNul = T)
##   x <- paste0(x, sep = " ", collapse = " ")
##   x <- stringr::str_squish(x)
## })

## # installing packages
## install.packages("tidyverse")
## install.packages("flextable")
## install.packages("knitr")
## install.packages("here")
## install.packages("quanteda")
## install.packages("cfa")
## # install klippy for copy-to-clipboard button in code chunks
## install.packages("remotes")
## remotes::install_github("rlesur/klippy")

# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation
# load packages
library(tidyverse)
library(flextable)
library(knitr)
library(here)
library(quanteda)
library(cfa)
# activate klippy for copy-to-clipboard button
klippy::klippy()

id <- data.frame(1:6)
id <- id %>%
  dplyr::rename("id" = colnames(id)[1]) %>%
  dplyr::mutate(file = c("aab", "aab", "aab", "aab", "aab", "aab"),
                childage = c("4;6", "4;6", "4;6", "4;6", "4;6", "4;6"),
                child = c("ben", "ben", "ben", "ben", "ben", "ben"),
                speaker = c("MOT", "MOT", "ben", "MOT", "ben", "MOT"),
                utterance = c("How are you ?", "Ben ?", "Okay", "Are you hungry ?", "No", "Sure ?"),
                tagged = c("How|WH are|BE you|PN ?|PC", "Ben|NNP ?|PC", "Okay|RB", "Are|BE you|PN hungry|JJ ?|PC", "No|NG", "Sure|RB ?|PC"),
                comment = c("", "", "", "", "shakes head", ""))
# inspect data
flextable::flextable(id) %>%
  flextable::autofit()

knitr::include_graphics("https://slcladal.github.io/images/childes01.png")

knitr::include_graphics("https://slcladal.github.io/images/childes02.png")

## knitr::include_graphics("https://slcladal.github.io/images/childes03.png")

url = "https://slcladal.github.io/data/hslld.rda"
download.file(url,"hslld.rds", method="curl")
hslld <- readRDS("hslld.rds")
# inspect
str(hslld[1:3])

# create version of corpus fit for concordancing
corpus <- sapply(hslld, function(x) {
  # clean data
  x <- stringr::str_trim(x, side = "both") # remove superfluous white spaces at the edges of strings
  x <- stringr::str_squish(x)              # remove superfluous white spaces within strings
  x <- paste0(x, collapse = " ")           # paste all utterances in a file together
  # split files into individual utterances
  x <- strsplit(gsub("([%|*][a-z|A-Z]{2,4}[0-9]{0,1}:)", "~~~\\1", x), "~~~")
})
# inspect results
str(corpus[1:3])

# extract file info for each file
fileinfo <- sapply(corpus, function(x){ 
  # extract first element of each corpus file because this contains the file info
  x <- x[1]
  })
#inspect
fileinfo[1:3]

content <- sapply(corpus, function(x){
  x <- x[2:length(x)]
  x <- paste0(x, collapse = " ")
  x <- stringr::str_split(stringr::str_replace_all(x, "(\\*[A-Z])", "~~~\\1"), "~~~")
})
# inspect data
content[[1]][1:6]

elements <- sapply(content, function(x){
  x <- length(x)
})
# inspect
head(elements)

files <- rep(names(elements), elements)
fileinfo <- rep(fileinfo, elements)
rawcontent <- as.vector(unlist(content))
chitb <- data.frame(1:length(rawcontent),
                    files,
                    fileinfo,
                    rawcontent)

# inspect data
chitb %>% 
  head(3) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

hslld <- chitb %>%
  # rename id column
  dplyr::rename(id = colnames(chitb)[1]) %>%
  # clean file names
  dplyr::mutate(files = gsub(".*/(.*?).cha", "\\1", files))

# inspect data
# inspect data
hslld %>% 
  head() %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

hslld <- hslld %>%  
  dplyr::mutate(speaker = stringr::str_remove_all(rawcontent, ":.*"),
                speaker = stringr::str_remove_all(speaker, "\\W"))

hslld <- hslld %>%  
  dplyr::mutate(utterance = stringr::str_remove_all(rawcontent, "%mor:.*"),
                utterance = stringr::str_remove_all(utterance, "%.*"),
                utterance = stringr::str_remove_all(utterance, "\\*\\w{2,6}:"),
                utterance = stringr::str_squish(utterance))

hslld <- hslld %>%  
  dplyr::mutate(postag = stringr::str_remove_all(rawcontent, ".*%mor:"),
                postag = stringr::str_remove_all(postag, "%.*"),
                postag = stringr::str_remove_all(postag, "\\*\\w{2,6}:"),
                postag = stringr::str_squish(postag))

hslld <- hslld %>%  
  dplyr::mutate(comment = stringr::str_remove_all(rawcontent, ".*%mor:"),
                comment = stringr::str_remove(comment, ".*?%"),
                comment = stringr::str_remove_all(comment, ".*|.*"),
                comment = stringr::str_squish(comment))

hslld <- hslld %>%  
  dplyr::mutate(participants = gsub(".*@Participants:(.*?)@.*", "\\1", fileinfo))

hslld <- hslld %>%
  dplyr::mutate(age_targetchild = gsub(".*\\|([0-9]{1,3};[0-9]{1,3}\\.[0-9]{1,3})\\|.*", "\\1", fileinfo)) 

hslld <- hslld %>%
  dplyr::mutate(age_years_targetchild = stringr::str_remove_all(age_targetchild, ";.*")) 

hslld <- hslld %>%
  dplyr::mutate(gender_targetchild = gsub(".*\\|([female]{4,6})\\|.*", "\\1", fileinfo))

hslld <- hslld %>%  
  # create dob_targetchild column
  dplyr::mutate(dob_targetchild = gsub(".*@Birth of CHI:(.*?)@.*","\\1", fileinfo)) %>%
  # create comment_file column
  dplyr::mutate(comment_file = gsub(".*@Comment: (.*?)@.*", "\\1", fileinfo)) %>%
  # create date column
  dplyr::mutate(date = gsub(".*@Date: (.*?)@.*", "\\1", fileinfo))

hslld <- hslld %>%  
  # create location column,
  dplyr::mutate(location = gsub(".*@Location: (.*?)@.*", "\\1", fileinfo)) %>%
  # create situation column
  dplyr::mutate(situation = gsub(".*@Situation: (.*?)@.*", "\\1", fileinfo))

hslld <- hslld %>%  
  # create homevisit_activity column
  dplyr::mutate(homevisit_activity = stringr::str_remove_all(situation, ";.*")) %>%
  # create activity column
  dplyr::mutate(activity = gsub(".*@Activities: (.*?)@.*", "\\1", fileinfo)) %>%
  # create homevisit column
  dplyr::mutate(homevisit = stringr::str_sub(files, 4, 6))

hslld <- hslld %>%  
  # create words column
  dplyr::mutate(words = stringr::str_replace_all(utterance, "\\W", " "),
                words = stringr::str_squish(words),
                words = stringr::str_count(words, "\\w+"))

hslld <- hslld %>%  
  # remove rows without speakers (contain only metadata)
  dplyr::filter(speaker != "") %>%
  # remove rows with incorrect age of child
  dplyr::filter(nchar(age_years_targetchild) < 5) %>%
  # remove superfluous columns
  dplyr::select(-fileinfo, -rawcontent, -situation)  %>%
  # create words column
  dplyr::mutate(collection = "EngNA",
                corpus = "HSLLD") %>%
  dplyr::rename(transcript_id = files) %>%
    # code activity
  dplyr::mutate(visit = substr(transcript_id, 6, 6)) %>%
  dplyr::mutate(situation = substr(transcript_id, 4, 5),
                situation = str_replace_all(situation, "br", "Book reading"),
                situation = str_replace_all(situation, "er", "Elicited report"),
                situation = str_replace_all(situation, "et", "Experimental task"),
                situation = str_replace_all(situation, "lw", "Letter writing"),
                situation = str_replace_all(situation, "md", "Mother defined situation"),
                situation = str_replace_all(situation, "mt", "Meal time"),
                situation = str_replace_all(situation, "re", "Reading"),
                situation = str_replace_all(situation, "tp", "Toy play"))

# inspect data
hslld %>% 
  head() %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

no <- hslld %>%
  dplyr::filter(speaker == "CHI") %>%
  dplyr::filter(stringr::str_detect(utterance, "\\b[Nn][Oo]\\b")) 

# inspect data
no %>% 
  head() %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

no_no <- no %>%
  dplyr::group_by(transcript_id, gender_targetchild, age_years_targetchild) %>%
  dplyr::summarise(nos = nrow(.))
head(no_no)

no_words <- hslld %>%
  dplyr::filter(speaker == "CHI") %>%
  dplyr::group_by(transcript_id, gender_targetchild, age_years_targetchild) %>%
  dplyr::mutate(nos = stringr::str_detect(utterance, "\\b[Nn][Oo]\\b")) %>%
  dplyr::summarise(nos = sum(nos),
                   words = sum(words)) %>%
  # add relative frequency
  dplyr::mutate(freq = round(nos/words*1000, 3))
# inspect data
head(no_words)

no_words %>%
  dplyr::mutate(age_years_targetchild = as.numeric(age_years_targetchild)) %>%
  ggplot(aes(x = age_years_targetchild, y = freq)) +
  geom_smooth() +
  theme_bw() +
  labs(x = "Age of target child", y = "Relative frequency of NOs \n (per 1,000 words)")

questions <- hslld %>%
  dplyr::filter(speaker == "MOT") %>%
  dplyr::filter(stringr::str_detect(utterance, "\\?"))
# inspect data
head(questions)

qmot <- hslld %>%
  dplyr::filter(speaker == "MOT") %>%
  dplyr::mutate(questions = ifelse(stringr::str_detect(utterance, "\\?") == T, 1,0),
                utterances = 1) %>%
  dplyr::group_by(age_years_targetchild) %>%
  dplyr::summarise(utterances = sum(utterances),
                questions = sum(questions),
                percent = round(questions/utterances*100, 2))
# inspect data
head(qmot)

qmot %>%
  dplyr::mutate(age_years_targetchild = as.numeric(age_years_targetchild)) %>%
  ggplot(aes(x = age_years_targetchild, y = percent)) +
  geom_smooth() +
  theme_bw() +
  labs(x = "Age of target child", y = "Percent \n (questions)")

auxv <- hslld %>%
  dplyr::filter(speaker == "MOT") %>%
  dplyr::filter(stringr::str_detect(postag, "aux\\|\\S{1,} part\\|"))
# inspect data
head(auxv)

auxv_verbs <- auxv %>%
  dplyr::mutate(participle = gsub(".*part\\|(\\w{1,})-.*", "\\1", postag)) %>%
  dplyr::pull(participle)
head(auxv_verbs)

auxv_verbs_df <- auxv_verbs %>%
  as.data.frame(.)  %>%
  dplyr::rename("verb" = colnames(.)[1]) %>%
  dplyr::group_by(verb) %>%
  dplyr::summarise(freq = n()) %>%
  dplyr::arrange(-freq) %>%
  head(20)
# inspect
head(auxv_verbs_df)

auxv_verbs_df %>%
  ggplot(aes(x = reorder(verb, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(x = "Verb", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90))

nverbs <- hslld %>%
  dplyr::filter(speaker == "CHI") %>%
  dplyr::mutate(nverbs = stringr::str_count(postag, "^v\\|| v\\|"),
  age_years_targetchild = as.numeric(age_years_targetchild)) %>%
  dplyr::group_by(age_years_targetchild) %>%
  dplyr::summarise(words = sum(words),
                verbs = sum(nverbs)) %>%
  dplyr::mutate(verb.word.ratio = round(verbs/words, 3))
# inspect data
nverbs

nverbs %>%
  ggplot(aes(x = age_years_targetchild, y = verb.word.ratio)) +
  geom_line() +
  coord_cartesian(ylim = c(0, 0.2)) +
  theme_bw() +
  labs(x = "Age of target child", y = "Verb-Word Ratio")

utterance_tb <- hslld %>%
  dplyr::filter(speaker == "CHI") %>%
  dplyr::group_by(age_years_targetchild) %>%
  dplyr::summarise(allutts = paste0(utterance, collapse = " ")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(age_years_targetchild = as.numeric(age_years_targetchild),
                # clean utterance
                allutts = stringr::str_replace_all(allutts, "\\W", " "),
                allutts = stringr::str_replace_all(allutts, "\\d", " "),
                allutts = stringr::str_remove_all(allutts, "xxx"),
                allutts = stringr::str_remove_all(allutts, "zzz"),
                allutts = tolower(allutts)) %>%
  # remove superfluous white spaces
  dplyr::mutate(allutts = gsub(" {2,}", " ", allutts)) %>%
  dplyr::mutate(allutts = stringr::str_squish(allutts))
# inspect data
head(utterance_tb)

tokens <- stringr::str_count(utterance_tb$allutts, " ") +1
types <- stringr::str_split(utterance_tb$allutts, " ")
types <- sapply(types, function(x){
  x <- length(names(table(x)))
})
ttr <- utterance_tb %>%
  dplyr::mutate(tokens = tokens,
                types = types) %>%
  dplyr::select(-allutts) %>%
  dplyr::mutate(TypeTokenRatio = round(types/tokens, 3))
# inspect 
ttr

ttr %>%
  ggplot(aes(x = age_years_targetchild, y = TypeTokenRatio)) +
  geom_line() +
  coord_cartesian(ylim = c(0, 0.75)) +
  theme_bw() +
  labs(x = "Age of target child", y = "Type-Token Ratio")

## # load txt tidyr style
## tbl <- list.files(pattern = "*.txt") %>%
##         map_chr(~ read_file(.)) %>%
##         data_frame(text = .)

# define path to corpus
corpuspath <- "https://slcladal.github.io/data/ICEIrelandSample/"
# define corpusfiles
files <- paste(corpuspath, "S1A-00", 1:20, ".txt", sep = "")
files <- gsub("[0-9]([0-9][0-9][0-9])", "\\1", files)
# load corpus files
corpus <- sapply(files, function(x){
  x <- readLines(x)
  x <- paste(x, collapse = " ")
  x <- tolower(x)
})
# inspect corpus
str(corpus)

searchpatterns <- c("\\bass[ingedholes]{0,6}\\b|\\bbitch[a-z]{0,3}\\b|\\b[a-z]{0,}fuck[a-z]{0,3}\\b|\\bshit[a-z]{0,3}\\b|\\bcock[a-z]{0,3}\\b|\\bwanker[a-z]{0,3}\\b|\\bboll[io]{1,1}[a-z]{0,3}\\b|\\bcrap[a-z]{0,3}\\b|\\bbugger[a-z]{0,3}\\b|\\bcunt[a-z]{0,3}\\b")

# extract kwic
kwicswears <- quanteda::kwic(corpus, searchpatterns,window = 10, valuetype = "regex")

# inspect data
kwicswears %>%
  as.data.frame() %>%
  head(10) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

kwicswearsclean <- kwicswears %>%
  as.data.frame() %>%
  dplyr::rename("File" = colnames(.)[1], 
                "StartPosition" = colnames(.)[2], 
                "EndPosition" = colnames(.)[3], 
                "PreviousContext" = colnames(.)[4], 
                "Token" = colnames(.)[5], 
                "FollowingContext" = colnames(.)[6], 
                "SearchPattern" = colnames(.)[7]) %>%
  dplyr::select(-StartPosition, -EndPosition, -SearchPattern) %>%
  dplyr::mutate(File = str_remove_all(File, ".*/"),
                File = stringr::str_remove_all(File, ".txt"))

# inspect data
kwicswearsclean %>% 
  head(10) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# extract kwic
kwiclong <- kwic(corpus, searchpatterns,window = 1000, valuetype = "regex")
kwiclong <- as.data.frame(kwiclong)
colnames(kwiclong) <- c("File", "StartPosition", "EndPosition", "PreviousContext", "Token", "FollowingContext", "SearchPattern")
kwiclong <- kwiclong %>%
  dplyr::select(-StartPosition, -EndPosition, -SearchPattern) %>%
  dplyr::mutate(File = str_remove_all(File, ".*/"),
         File = str_remove_all(File, ".txt"),
         Speaker = str_remove_all(PreviousContext, ".*\\$"),
         Speaker = str_remove_all(Speaker, ">.*"),
         Speaker = str_squish(Speaker),
         Speaker = toupper(Speaker)) %>%
  dplyr::select(Speaker)
# inspect results
head(kwiclong)

swire <- cbind(kwicswearsclean, kwiclong)

# inspect data
swire %>% 
  head(10) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# convert tokens to lower case
swire$Token <- tolower(swire$Token)
# inspect tokens
table(swire$Token)

swire <- swire %>%
  dplyr::group_by(File, Speaker) %>%
  dplyr::summarise(Swearwords = n())

# inspect data
swire %>% 
  head(10) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# load bio data
bio <- base::readRDS(url("https://slcladal.github.io/data/d01.rda", "rb"))

# inspect data
bio %>% 
  head(10) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

bio <- bio %>%
  dplyr::rename(File = text.id, 
         Speaker = spk.ref,
         Gender = sex,
         Age = age,
         Words = word.count) %>%
  dplyr::select(File, Speaker, Gender, Age, Words)

# inspect data
bio %>% 
  head(10) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# combine frequencies and biodata
swire <- dplyr::left_join(bio, swire, by = c("File", "Speaker")) %>%
  # replace NA with 0
  dplyr::mutate(Swearwords = ifelse(is.na(Swearwords), 0, Swearwords),
                File = factor(File),
                Speaker = factor(Speaker),
                Gender = factor(Gender),
                Age = factor(Age))
# inspect data
head(swire)

# inspect data
swire %>% 
  head(10) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# clean data
swire <- swire %>%
  dplyr::filter(is.na(Gender) == F,
         is.na(Age) == F) %>%
  dplyr::group_by(Age, Gender) %>%
  dplyr::mutate(SumWords = sum(Words),
                SumSwearwords = sum(Swearwords),
                FrequencySwearwords = round(SumSwearwords/SumWords*1000, 3)) 

# inspect data
swire %>% 
  head(10) %>%
  flextable::flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

swire %>%
  dplyr::filter(Age != "0-18") %>%
  dplyr::group_by(Age, Gender) %>%
  dplyr::summarise(Swears_ptw = SumSwearwords/SumWords*1000) %>%
  unique() %>%
  tidyr::spread(Gender, Swears_ptw)

swire %>%
  dplyr::filter(Age != "0-18") %>%
  dplyr::group_by(Age, Gender) %>%
  dplyr::summarise(Swears_ptw = SumSwearwords/SumWords*1000) %>%
  unique() %>%
ggplot(aes(x = Age, y = Swears_ptw, group = Gender, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  scale_fill_manual(values = c("orange", "darkgrey")) +
  labs(y = "Relative frequency \n swear words per 1,000 words")

cfa_swear <- swire %>%
  dplyr::group_by(Gender, Age) %>%
  dplyr::summarise(Words = sum(Words),
                   Swearwords = sum(Swearwords)) %>%
  dplyr::mutate(Words = Words - Swearwords) %>%
  tidyr::gather(Type, Frequency,Words:Swearwords) %>%
  dplyr::filter(Age != "0-18")

# inspect data
head(cfa_swear, 20) %>%  
  as.data.frame() %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::border_outer()

# define configurations
configs <- cfa_swear %>%
  dplyr::select(Age, Gender, Type)
# define counts
counts <- cfa_swear$Frequency

# perform cfa
cfa(configs,counts)$table %>%
  as.data.frame() %>%
  dplyr::filter(p.chisq < .1,
                stringr::str_detect(label, "Swear")) %>%
  dplyr::select(-z, -p.z, -sig.z, -sig.chisq, -Q)

sessionInfo()

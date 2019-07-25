
# "Getting Data from the Web: Web Crawling"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)
# install libraries
install.packages(c("rvest", "readtext"))r, message=FALSE, eval=F}
# important option for text analysis
options(stringsAsFactors = F)
# check working directory. It should be the destination folder of the extracted 
# zip file. If necessary, use `setwd("your-tutorial-folder-path")` to change it.
getwd()
url <- "https://www.theguardian.com/world/2017/jun/26/angela-merkel-and-donald-trump-head-for-clash-at-g20-summit"
require("rvest")r}
html_document <- read_html(url)r}
title_xpath <- "//h1[contains(@class, 'content__headline')]"
title_text <- html_document %>%
  html_node(xpath = title_xpath) %>%
  html_text(trim = T)r}
cat(title_text)r}
intro_xpath <- "//div[contains(@class, 'content__standfirst')]//p"
intro_text <- html_document %>%
  html_node(xpath = intro_xpath) %>%
  html_text(trim = T)
cat(intro_text)
body_xpath <- "//div[contains(@class, 'content__article-body')]//p"
body_text <- html_document %>%
  html_nodes(xpath = body_xpath) %>%
  html_text(trim = T) %>%
  paste0(collapse = "\n")r eval=F, echo=T}
cat(body_text)r eval=T, echo=F}
cat(substr(body_text, 0, 150))r}
date_xpath <- "//time"
date_object <- html_document %>%
  html_node(xpath = date_xpath) %>%
  html_attr(name = "datetime") %>%
  as.Date()
r}
url <- "https://www.theguardian.com/world/angela-merkel"
html_document <- read_html(url)r}
links <- html_document %>%
  html_nodes(xpath = "//div[contains(@class, 'fc-item__container')]/a") %>%
  html_attr(name = "href")r}
head(links, 3)r}
page_numbers <- 1:3
base_url <- "https://www.theguardian.com/world/angela-merkel?page="
paging_urls <- paste0(base_url, page_numbers)
# View first 3 urls
head(paging_urls, 3)r}
all_links <- NULL
for (url in paging_urls) {
  # download and parse single ta overview page
  html_document <- read_html(url)
  # extract links to articles
  links <- html_document %>%
    html_nodes(xpath = "//div[contains(@class, 'fc-item__container')]/a") %>%
    html_attr(name = "href")
  # append links to vector of all links
  all_links <- c(all_links, links)
}r}
scrape_guardian_article <- function(url) {
  html_document <- read_html(url)
  title_xpath <- "//h1[contains(@class, 'content__headline')]"
  title_text <- html_document %>%
    html_node(xpath = title_xpath) %>%
    html_text(trim = T)
  intro_xpath <- "//div[contains(@class, 'content__standfirst')]//p"
  intro_text <- html_document %>%
    html_node(xpath = intro_xpath) %>%
    html_text(trim = T)
  body_xpath <- "//div[contains(@class, 'content__article-body')]//p"
  body_text <- html_document %>%
    html_nodes(xpath = body_xpath) %>%
    html_text(trim = T) %>%
    paste0(collapse = "\n")
  date_xpath <- "//time"
  date_text <- html_document %>%
    html_node(xpath = date_xpath) %>%
    html_attr(name = "datetime") %>%
    as.Date()
  article <- data.frame(
    url = url,
    date = date_text,
    title = title_text,
    body = paste0(intro_text, "\n", body_text)
  )
  return(article)
}r, echo=T, eval=F}
all_articles <- data.frame()
for (i in 1:length(all_links)) {
  cat("Downloading", i, "of", length(all_links), "URL:", all_links[i], "\n")
  article <- scrape_guardian_article(all_links[i])
  # Append current article data.frame to the data.frame of all articles
  all_articles <- rbind(all_articles, article)
}r, echo=F, eval=T}
all_articles <- data.frame()
for (i in 1:10) {
  if (i < 4) {
    cat("Downloading", i, "of", length(all_links), "URL:", all_links[i], "\n")
  }
  article <- scrape_guardian_article(all_links[i])
  # Append current article data.frame to the data.frame of all articles
  all_articles <- rbind(all_articles, article)
}r, eval=F}
# View first articles
head(all_articles, 3)
# Write articles to disk
write.csv2(all_articles, file = "data/guardian_merkel.csv")r}
data_files <- list.files(path = "data/documents", full.names = T, recursive = T)
# View first file paths
head(data_files, 3)r eval=F, message=F}
require(readtext)
extracted_texts <- readtext(data_files, docvarsfrom = "filepaths", dvsep = "/")
# View first rows of the extracted texts
head(extracted_texts)
# View beginning of tenth extracted text
substr(trimws(extracted_texts$text[10]) , 0, 100)r, eval=F}
write.csv2(extracted_texts, file = "data/text_extracts.csv")

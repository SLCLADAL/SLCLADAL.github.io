# save texts to disc
# Use lapply to iterate over the indices of x
# and write each text to a separate file in the MyOutput folder

require(stringr)

savetxts <- function(x){
  lapply(seq_along(x), function(i) {
    # unlist the i-th element in x to get the text content
    text_content <- unlist(x[i])
    
    # construct the file path using the 'here' package
    file_path <- paste0(here::here("notebooks/MyOutput/"), names(x)[i])
    
    # write the text content to the specified file
    writeLines(text = text_content, con = file_path)
})
}


library(dplyr)
library(tidytext)
library(textclean)
library(textstem)
library(tm)
library(hunspell)
library(stringr)


df <- read.csv('C:/Users/ASUS/Desktop/Data Science/Final/Data Scraping/final data/New folder/bbc_scraped_data.csv')

df <- df %>% filter(!is.na(content) & str_trim(content) != "")



df$content <- tolower(df$content) 
df$content <- str_replace_all(df$content, "http\\S+|www\\S+", "") 
df$content <- str_replace_all(df$content, "[^[:alnum:]\\s]", " ")
df$content <- str_replace_all(df$content, "[0-9]+", "") 
df$content <- str_squish(df$content) 



tokens <- df %>%
  unnest_tokens(word, content)

data("stop_words") 
tokens <- tokens %>%
  anti_join(stop_words, by = "word")


tokens$word <- lemmatize_words(tokens$word)




df$content <- replace_contraction(df$content) 




df$content <- replace_emoji(df$content)      
df$content <- replace_emoticon(df$content)    




df$content <- sapply(df$content, function(x) {
  words <- unlist(str_split(x, " "))
  corrected <- sapply(words, function(w) {
    if (!hunspell_check(w)) {
      suggestions <- hunspell_suggest(w)[[1]]
      if (length(suggestions) > 0) return(suggestions[1])
    }
    return(w)
  })
  paste(corrected, collapse = " ")
})



write.csv(df,'C:/Users/ASUS/Desktop/Data Science/Final/Data Scraping/final data/New folder/preprocessed_bbc_scraped_data.csv')


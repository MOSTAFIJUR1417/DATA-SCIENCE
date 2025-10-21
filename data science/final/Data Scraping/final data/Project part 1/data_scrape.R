
library(rvest)
library(dplyr)
library(xml2)

query <- URLencode("internation")
results <- list()

for (i in 1:780) {
  cat("Scraping page", i, "...\n")
  search_url <- paste0("https://www.bbc.com/search?q=", query, "&page=", i)
  webpage <- tryCatch(read_html(search_url), error = function(e) NULL)
  if (is.null(webpage)) next
  
  cards <- html_nodes(webpage, 'div[data-testid="newport-card"]')
  if (length(cards) == 0) next
  
  page_data <- lapply(cards, function(card) {
    tibble(
      url = card %>%
        html_node('a[data-testid="internal-link"]') %>%
        html_attr('href') %>%
        { if (!is.null(.)) paste0('https://www.bbc.com', .) else NA_character_ },
      
      headline = card %>%
        html_node('h2[data-testid="card-headline"]') %>%
        html_text(trim = TRUE) %>%
        { if (!is.null(.)) . else NA_character_ },
      
      summary = card %>%
        html_node('div.sc-cdecfb63-3') %>%
        html_text(trim = TRUE) %>%
        { if (!is.null(.)) . else NA_character_ },
      
      date = card %>%
        html_node('span[data-testid="card-metadata-lastupdated"]') %>%
        html_text(trim = TRUE) %>%
        { if (!is.null(.)) . else NA_character_ },
      
      tag = card %>%
        html_node('span[data-testid="card-metadata-tag"]') %>%
        html_text(trim = TRUE) %>%
        { if (!is.null(.)) . else NA_character_ }
    )
  })
  
  results[[length(results) + 1]] <- bind_rows(page_data)
}





news_data <- bind_rows(results)

write.csv(
  news_data,
  file = "C:/Users/ASUS/Desktop/Data Science/Final/Data Scraping/final data/1final_bbc_all_links.csv",
  row.names = FALSE
)



bbc_data <- read.csv("C:/Users/ASUS/Desktop/Data Science/Final/Data Scraping/final data/1final_bbc_all_links.csv")

length(bbc_data$url)






library(rvest)
library(dplyr)
library(tibble)

bbc_data <- read.csv("C:/Users/ASUS/Desktop/Data Science/Final/Data Scraping/final data/1final_bbc_all_links.csv")

all_articles <- list()
invalid_count <- 0


scrape_webpage <- function(url, is_sport = FALSE) {
  tryCatch({
    webpage <- read_html(url)
    
    if (is_sport) {
      headline <- webpage %>% html_node("h1.ssrcss-1mguc0h-Heading") %>% html_text(trim = TRUE)
      date <- webpage %>% html_node("time") %>% html_attr("datetime") %>% as.Date()
      content <- webpage %>% html_nodes("div.ssrcss-7uxr49-RichTextContainer p") %>% html_text(trim = TRUE) %>% paste(collapse = " ")
      author <- NA
      affiliation <- "BBC Sports"
    } else {
      headline <- webpage %>% html_node('div[data-component="headline-block"] h1') %>% html_text(trim = TRUE)
      date <- webpage %>% html_node('div[data-component="byline-block"] time') %>% html_attr('datetime') %>% as.Date()
      author <- webpage %>% html_node('div[data-testid="byline-new-contributors"] span.sc-801dd632-7') %>% html_text(trim = TRUE)
      affiliation <- webpage %>% html_node('div[data-testid="byline-new-contributors"] div.sc-801dd632-8') %>% html_text(trim = TRUE)
      content <- webpage %>% html_nodes('div[data-component="text-block"] p') %>% html_text() %>% paste(collapse = " ")
    }
    
    tibble(
      url = url,
      headline = headline,
      date = date,
      author = author,
      affiliation = affiliation,
      content = content
    )
  }, error = function(e) {
    invalid_count <<- invalid_count + 1
    message(paste("Invalid URL skipped:", url))
    NULL
  })
}

for (i in seq_along(bbc_data$url)) {
  url <- bbc_data$url[i]
  
  if (grepl("audio", url, ignore.case = TRUE)) {
    next
    
  } else if (grepl("sport", url, ignore.case = TRUE)) {
    result <- scrape_webpage(url, is_sport = TRUE)
  } else {
    result <- scrape_webpage(url, is_sport = FALSE)
  }
  
  if (!is.null(result)) {
    all_articles[[length(all_articles) + 1]] <- result
  }
}

final_articles <- bind_rows(all_articles)

write.csv(final_articles, "C:/Users/ASUS/Desktop/Data Science/Final/Data Scraping/final data/final_bbc_scraped_data.csv", row.names = FALSE)

cat("Number of invalid URLs skipped:", invalid_count, "\n")


file <- read.csv("C:/Users/ASUS/Desktop/Data Science/Final/Data Scraping/final data/final_bbc_scraped_data.csv")


sapply(file, function(x) sum(is.na(x) | x == ""))





final_articles <- final_articles %>%
  filter(content != "" & !is.na(content))  
write.csv(final_articles, "C:/Users/ASUS/Desktop/Data Science/Final/Data Scraping/final data/empty_Free_bbc_scraped_data.csv", row.names = FALSE)

file <- read.csv("C:/Users/ASUS/Desktop/Data Science/Final/Data Scraping/final data/empty_Free_bbc_scraped_data.csv")



sapply(file, function(x) sum(is.na(x) | x == ""))






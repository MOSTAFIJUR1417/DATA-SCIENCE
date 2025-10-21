library(topicmodels)
library(tidytext)
library(tm)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(wordcloud)
library(slam)
library(LDAvis)
library(servr)

df <- read.csv('C:/Users/ASUS/Desktop/Data Science/Final/Data Scraping/final data/New folder/preprocessed_bbc_scraped_data.csv')
df_copy <- df
df <- df %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         year = format(date, "%Y"))

content_per_year <- df %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(content_count = n(), .groups = "drop") %>%
  arrange(desc(content_count)) %>%
  mutate(percentage = content_count / sum(content_count) * 100,
         legend_label = paste0(year, " (", round(percentage, 1), "%)"))

ggplot(content_per_year, aes(x = "", y = content_count, fill = legend_label)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Content Distribution per Year", fill = "Year (Percentage)") +
  theme_void() +
  theme(legend.position = "right")

author_counts <- df %>%
  mutate(author = ifelse(is.na(author), "Other", author)) %>%
  group_by(author) %>%
  summarise(total_content = n(), .groups = "drop") %>%
  arrange(desc(total_content))

total_authors <- n_distinct(df$author)
cat("Total number of unique authors:", total_authors, "\n")

top_20_authors <- author_counts$author[1:20]

df_top_authors <- df %>%
  mutate(author = ifelse(is.na(author), "Other", author)) %>%
  mutate(author_grouped = ifelse(author %in% top_20_authors, author, "Other"))

author_grouped_counts <- df_top_authors %>%
  group_by(author_grouped) %>%
  summarise(total_content = n(), .groups = "drop") %>%
  arrange(desc(total_content))

ggplot(author_grouped_counts, aes(x = reorder(author_grouped, total_content), y = total_content)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_content), hjust = -0.2) +
  coord_flip() +
  labs(title = "Top 20 Authors by Content Count (Others grouped as 'Other')",
       x = "Author",
       y = "Content Count") +
  theme_minimal()

df_clean <- df %>%
  filter(!is.na(affiliation) & affiliation != "")

affiliation_counts <- df_clean %>%
  group_by(affiliation) %>%
  summarise(content_count = n(), .groups = "drop") %>%
  arrange(desc(content_count))

top_20_affiliations <- affiliation_counts$affiliation[1:20]

affiliation_counts_grouped <- affiliation_counts %>%
  mutate(affiliation_grouped = ifelse(affiliation %in% top_20_affiliations, affiliation, "Other")) %>%
  group_by(affiliation_grouped) %>%
  summarise(content_count = sum(content_count), .groups = "drop")

set.seed(123)
random_colors <- sample(colors(), length(unique(affiliation_counts_grouped$affiliation_grouped)))

ggplot(affiliation_counts_grouped, aes(x = reorder(affiliation_grouped, -content_count), y = content_count, fill = affiliation_grouped)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = content_count), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = random_colors) +
  labs(title = "Content Contribution by Affiliation (Top 20 + 'Other')",
       x = "Affiliation",
       y = "Content Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")



top3_authors_per_year <- df %>%
  filter(!is.na(year)) %>%
  mutate(author = ifelse(is.na(author) | author == "", "Other", author)) %>%
  group_by(year, author) %>%
  summarise(content_count = n(), .groups = "drop") %>%
  arrange(year, desc(content_count)) %>%
  group_by(year) %>%
  slice_max(content_count, n = 3) %>%
  ungroup()

top3_affiliations_per_year <- df %>%
  filter(!is.na(year), !is.na(affiliation), affiliation != "") %>%
  group_by(year, affiliation) %>%
  summarise(content_count = n(), .groups = "drop") %>%
  arrange(year, desc(content_count)) %>%
  group_by(year) %>%
  slice_max(content_count, n = 3) %>%
  ungroup()
years <- unique(top3_authors_per_year$year)

for (yr in years) {
  data_year_authors <- top3_authors_per_year %>% filter(year == yr)
  p_authors <- ggplot(data_year_authors, aes(x = reorder(author, content_count), y = content_count, fill = author)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = content_count), vjust = -0.3, size = 4) +
    labs(title = paste("Top 3 Authors in", yr),
         x = "Author",
         y = "Content Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p_authors)
}

top3_authors_per_year <- df %>%
  filter(!is.na(year)) %>%
  mutate(author = ifelse(is.na(author) | author == "", "Other", author)) %>%
  group_by(year, author) %>%
  summarise(content_count = n(), .groups = "drop") %>%
  arrange(year, desc(content_count)) %>%
  group_by(year) %>%
  slice_max(content_count, n = 3) %>%
  ungroup()

top3_affiliations_per_year <- df %>%
  filter(!is.na(year), !is.na(affiliation), affiliation != "") %>%
  group_by(year, affiliation) %>%
  summarise(content_count = n(), .groups = "drop") %>%
  arrange(year, desc(content_count)) %>%
  group_by(year) %>%
  slice_max(content_count, n = 3) %>%
  ungroup()

years <- unique(top3_authors_per_year$year)

for (yr in years) {
  data_year_affiliations <- top3_affiliations_per_year %>% filter(year == yr)
  p_affiliations <- ggplot(data_year_affiliations, aes(x = reorder(affiliation, content_count), y = content_count, fill = affiliation)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = content_count), vjust = -0.3, size = 4) +
    labs(title = paste("Top 3 Affiliations in", yr),
         x = "Affiliation",
         y = "Content Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p_affiliations)
}

df <- df_copy 
corpus <- Corpus(VectorSource(df$content))
dtm <- DocumentTermMatrix(corpus)

k <- 10

lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

print(perplexity(lda_model, newdata = dtm))

terms(lda_model, 20)

topics <- tidy(lda_model, matrix = "gamma")

top_topics <- topics %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  arrange(as.numeric(document)) %>%
  select(document, topic)

df$topic <- as.integer(top_topics$topic)

top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

topic_names <- c(
  "Israel-Palestine war",
  "Crime, justice, and women’s issues",
  "International sports, clubs, and players",
  "Global festivals and social media",
  "Major global sports events",
  "Daily life & community stories",
  "Space exploration and scientific progress & Education",
  "Technology & business innovation",
  "Government policies & public welfare",
  "Global trade and economic relations"
)

top_terms$topic_name <- factor(topic_names[top_terms$topic], levels = topic_names)

ggplot(top_terms, aes(x = reorder_within(term, beta, topic_name), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_name, scales = "free", ncol = 2) +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Top Terms in LDA Topics", x = "Terms", y = "Beta (Probability)")

df$topic_name <- topic_names[df$topic]

#write.csv(df, 'C:/Users/ASUS/Desktop/Data Science/Final/Data Scraping/final data/New folder/final_interpret_data.csv', row.names = FALSE)

topic_counts <- df %>%
  group_by(topic_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percentage = count / sum(count) * 100,
         label = paste0(topic_name, "\n", round(percentage, 1), "%"))

ggplot(topic_counts, aes(x = "", y = count, fill = topic_name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Topic Distribution (in %)", x = NULL, y = NULL) +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

top_n_terms <- 300

term_freq <- slam::col_sums(dtm)
top_terms_freq <- sort(term_freq, decreasing = TRUE)[1:top_n_terms]

terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 50)

for (i in 1:k) {
  topic_terms <- terms %>% filter(topic == i)
  if (nrow(topic_terms) > 0) {
    wordcloud(
      words = topic_terms$term,
      freq = topic_terms$beta,
      max.words = min(nrow(topic_terms), 300),
      colors = brewer.pal(8, "Dark2"),
      random.order = FALSE,
      scale = c(4, 0.4)
    )
    title(paste("Topic", i, "-", topic_names[i]))
  }
}

wordclibrary(topicmodels)
library(tidytext)
library(tm)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(wordcloud)
library(slam)
library(LDAvis)
library(servr)

df <- read.csv('C:/Users/ASUS/Desktop/Data Science/Final/Data Scraping/final data/New folder/preprocessed_bbc_scraped_data.csv')
df_copy <- df
df <- df %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         year = format(date, "%Y"))

content_per_year <- df %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(content_count = n(), .groups = "drop") %>%
  arrange(desc(content_count)) %>%
  mutate(percentage = content_count / sum(content_count) * 100,
         legend_label = paste0(year, " (", round(percentage, 1), "%)"))

ggplot(content_per_year, aes(x = "", y = content_count, fill = legend_label)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Content Distribution per Year", fill = "Year (Percentage)") +
  theme_void() +
  theme(legend.position = "right")

author_counts <- df %>%
  mutate(author = ifelse(is.na(author), "Other", author)) %>%
  group_by(author) %>%
  summarise(total_content = n(), .groups = "drop") %>%
  arrange(desc(total_content))

total_authors <- n_distinct(df$author)
cat("Total number of unique authors:", total_authors, "\n")

top_20_authors <- author_counts$author[1:20]

df_top_authors <- df %>%
  mutate(author = ifelse(is.na(author), "Other", author)) %>%
  mutate(author_grouped = ifelse(author %in% top_20_authors, author, "Other"))

author_grouped_counts <- df_top_authors %>%
  group_by(author_grouped) %>%
  summarise(total_content = n(), .groups = "drop") %>%
  arrange(desc(total_content))

ggplot(author_grouped_counts, aes(x = reorder(author_grouped, total_content), y = total_content)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = total_content), hjust = -0.2) +
  coord_flip() +
  labs(title = "Top 20 Authors by Content Count (Others grouped as 'Other')",
       x = "Author",
       y = "Content Count") +
  theme_minimal()

df_clean <- df %>%
  filter(!is.na(affiliation) & affiliation != "")

affiliation_counts <- df_clean %>%
  group_by(affiliation) %>%
  summarise(content_count = n(), .groups = "drop") %>%
  arrange(desc(content_count))

top_20_affiliations <- affiliation_counts$affiliation[1:20]

affiliation_counts_grouped <- affiliation_counts %>%
  mutate(affiliation_grouped = ifelse(affiliation %in% top_20_affiliations, affiliation, "Other")) %>%
  group_by(affiliation_grouped) %>%
  summarise(content_count = sum(content_count), .groups = "drop")

set.seed(123)
random_colors <- sample(colors(), length(unique(affiliation_counts_grouped$affiliation_grouped)))

ggplot(affiliation_counts_grouped, aes(x = reorder(affiliation_grouped, -content_count), y = content_count, fill = affiliation_grouped)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = content_count), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = random_colors) +
  labs(title = "Content Contribution by Affiliation (Top 20 + 'Other')",
       x = "Affiliation",
       y = "Content Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")



top3_authors_per_year <- df %>%
  filter(!is.na(year)) %>%
  mutate(author = ifelse(is.na(author) | author == "", "Other", author)) %>%
  group_by(year, author) %>%
  summarise(content_count = n(), .groups = "drop") %>%
  arrange(year, desc(content_count)) %>%
  group_by(year) %>%
  slice_max(content_count, n = 3) %>%
  ungroup()

top3_affiliations_per_year <- df %>%
  filter(!is.na(year), !is.na(affiliation), affiliation != "") %>%
  group_by(year, affiliation) %>%
  summarise(content_count = n(), .groups = "drop") %>%
  arrange(year, desc(content_count)) %>%
  group_by(year) %>%
  slice_max(content_count, n = 3) %>%
  ungroup()
years <- unique(top3_authors_per_year$year)

for (yr in years) {
  data_year_authors <- top3_authors_per_year %>% filter(year == yr)
  p_authors <- ggplot(data_year_authors, aes(x = reorder(author, content_count), y = content_count, fill = author)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = content_count), vjust = -0.3, size = 4) +
    labs(title = paste("Top 3 Authors in", yr),
         x = "Author",
         y = "Content Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p_authors)
}

top3_authors_per_year <- df %>%
  filter(!is.na(year)) %>%
  mutate(author = ifelse(is.na(author) | author == "", "Other", author)) %>%
  group_by(year, author) %>%
  summarise(content_count = n(), .groups = "drop") %>%
  arrange(year, desc(content_count)) %>%
  group_by(year) %>%
  slice_max(content_count, n = 3) %>%
  ungroup()

top3_affiliations_per_year <- df %>%
  filter(!is.na(year), !is.na(affiliation), affiliation != "") %>%
  group_by(year, affiliation) %>%
  summarise(content_count = n(), .groups = "drop") %>%
  arrange(year, desc(content_count)) %>%
  group_by(year) %>%
  slice_max(content_count, n = 3) %>%
  ungroup()

years <- unique(top3_authors_per_year$year)

for (yr in years) {
  data_year_affiliations <- top3_affiliations_per_year %>% filter(year == yr)
  p_affiliations <- ggplot(data_year_affiliations, aes(x = reorder(affiliation, content_count), y = content_count, fill = affiliation)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = content_count), vjust = -0.3, size = 4) +
    labs(title = paste("Top 3 Affiliations in", yr),
         x = "Affiliation",
         y = "Content Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p_affiliations)
}

df <- df_copy 
corpus <- Corpus(VectorSource(df$content))
dtm <- DocumentTermMatrix(corpus)

k <- 10

lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

print(perplexity(lda_model, newdata = dtm))

terms(lda_model, 20)

topics <- tidy(lda_model, matrix = "gamma")

top_topics <- topics %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  arrange(as.numeric(document)) %>%
  select(document, topic)

df$topic <- as.integer(top_topics$topic)

top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

topic_names <- c(
  "Israel-Palestine war",
  "Crime, justice, and women’s issues",
  "International sports, clubs, and players",
  "Global festivals and social media",
  "Major global sports events",
  "Daily life & community stories",
  "Space exploration and scientific progress & Education",
  "Technology & business innovation",
  "Government policies & public welfare",
  "Global trade and economic relations"
)

top_terms$topic_name <- factor(topic_names[top_terms$topic], levels = topic_names)

ggplot(top_terms, aes(x = reorder_within(term, beta, topic_name), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_name, scales = "free", ncol = 2) +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Top Terms in LDA Topics", x = "Terms", y = "Beta (Probability)")

df$topic_name <- topic_names[df$topic]

#write.csv(df, 'C:/Users/ASUS/Desktop/Data Science/Final/Data Scraping/final data/New folder/final_interpret_data.csv', row.names = FALSE)

topic_counts <- df %>%
  group_by(topic_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(percentage = count / sum(count) * 100,
         label = paste0(topic_name, "\n", round(percentage, 1), "%"))

ggplot(topic_counts, aes(x = "", y = count, fill = topic_name)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Topic Distribution (in %)", x = NULL, y = NULL) +
  theme_void() +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

top_n_terms <- 300

term_freq <- slam::col_sums(dtm)
top_terms_freq <- sort(term_freq, decreasing = TRUE)[1:top_n_terms]

terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 50)

for (i in 1:k) {
  topic_terms <- terms %>% filter(topic == i)
  if (nrow(topic_terms) > 0) {
    wordcloud(
      words = topic_terms$term,
      freq = topic_terms$beta,
      max.words = min(nrow(topic_terms), 300),
      colors = brewer.pal(8, "Dark2"),
      random.order = FALSE,
      scale = c(4, 0.4)
    )
    title(paste("Topic", i, "-", topic_names[i]))
  }
}

wordcloud(
  words = names(top_terms_freq),
  freq = top_terms_freq,
  max.words = 300,
  colors = brewer.pal(8, "Dark2"),
  random.order = FALSE,
  scale = c(4, 0.4)
)
title(main = "Most Frequent Words in News Articles")

phi <- posterior(lda_model)$terms
theta <- posterior(lda_model)$topics
vocab <- colnames(phi)
doc_length <- rowSums(as.matrix(dtm))
term_freq <- colSums(as.matrix(dtm))

json_lda <- createJSON(phi = phi, theta = theta,
                       doc.length = doc_length,
                       vocab = vocab,
                       term.frequency = term_freq)

serVis(json_lda, out.dir = tempfile(), open.browser = TRUE)loud(
  words = names(top_terms_freq),
  freq = top_terms_freq,
  max.words = 300,
  colors = brewer.pal(8, "Dark2"),
  random.order = FALSE,
  scale = c(4, 0.4)
)
title(main = "Most Frequent Words in News Articles")

phi <- posterior(lda_model)$terms
theta <- posterior(lda_model)$topics
vocab <- colnames(phi)
doc_length <- rowSums(as.matrix(dtm))
term_freq <- colSums(as.matrix(dtm))

json_lda <- createJSON(phi = phi, theta = theta,
                       doc.length = doc_length,
                       vocab = vocab,
                       term.frequency = term_freq)

serVis(json_lda, out.dir = tempfile(), open.browser = TRUE)


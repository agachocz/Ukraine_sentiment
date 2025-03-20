
# joining data
data_1 <- read.csv("nyt_archive_Ukraine_2.csv")
data_2 <- read.csv("nyt_archive_Ukraine_3.csv")

data_3 <- data_2[1960:2062,] %>% mutate(date = if_else(str_detect(date, "ago"), "Feb. 11", date)) %>%
  mutate(date = paste(date, "2025"))
data_2 <- data_2[1:1959,]

data_4 <- read.csv("nyt_archive_Ukraine_4.csv")
data_4 <- data_4  %>%
  mutate(date = paste(date, "2025"))


library(lubridate)



data_UKR <- rbind(data_1, data_2, data_3, data_4) %>% unique() %>% select(-X) %>% slice(-1) %>%
  mutate(date = str_remove(date, "\\.")) %>% 
  mutate(date = str_remove(date, "\\,")) %>% 
  mutate(date = mdy(date)) %>% 
  mutate(author = str_remove(author, "By "))

summary(data_UKR)  
ukr_by_month <- data_UKR %>% mutate(month = str_sub(date, 1, 7)) %>%
  mutate(month = as.Date(paste0(month, "-01"))) %>%
  group_by(month) %>% tally() %>% 
  ggplot(aes(x = month, y = n)) + geom_line()

write.csv(data_UKR, "NYT_Ukraine_full_data.csv")

length(unique(data_UKR$header))


full_data <- read.csv("NYT_Ukraine_full_data.csv") %>% select(-X, -link) %>%
  mutate(desc = if_else(is.na(desc), "", desc)) %>%
  mutate(header = if_else(str_ends(header, "\\."), header, paste0(header, "."))) %>%
  mutate(text = paste(header, desc))

full_data$id <- 1:nrow(full_data)

# simple prompt for sentiment
prompt_sent <- "You will receive a snippet of a news article. 
           Assign the sentiment of the article on a scale from -5 (strongly negative)
           to 5 (strongly positive). Return just the sentiment value as a single number."

# prompt with reasoning
prompt_reason <- "You will receive a snippet of a news article and you need to analyse its sentiment.
           Assign the sentiment of the article as positive (+), negative (-), or neutral/ambiguous (0)
           and provide your reasoning in one or two sentences.
           Return the sentiment value as a single number and your reasoning in form of a JSON with keys: 
           sentiment and reasoning.

Example: 
INPUT: "

# prompt war/peace with reasoning

# prompt Russia/Ukraine with reasoning




articles <- full_data$text

# preparing batches from NYT articles
text <- ''
for (i in 2001:length(articles)){
  json <- paste0('{"custom_id": "req-sent-',i,'", "method": "POST",
                 "url": "/v1/chat/completions", 
                 "body": {"model": "gpt-4o-mini", 
                 "messages": [{"role": "system", "content": "',
                 prompt_sent, '"},
                 {"role": "user", "content": "', articles[i], '"}],
                 "temperature": 0.00,
                 "max_tokens": 1000}}')
  json <- str_remove_all(json, "\n") %>% str_squish()
  text <- paste(text, '\n', json)
}

# write in jsonl file
text <- str_trim(text, side = "both")
utf8 <- enc2utf8(text)
con <- file("batches/sent_Ukraine_3.jsonl", open = "w+", encoding = "UTF-8")
writeLines(utf8, con = con)
close(con)


# read the results

library(jsonlite)

filenames <- list.files(path = "GPT sent", full.names = TRUE)
first = TRUE

for(i in 1:length(filenames)){
  con <- file(filenames[i], open = "r", encoding = "UTF-8")
  file <- readLines(con = con)
  close(con)
  
  res_df <- lapply(file, function(x){
    results <- fromJSON(x)
    df <- data.frame(
      id = results$custom_id,
      text = results[["response"]][["body"]][["choices"]][["message"]][["content"]]
    )}) %>% bind_rows()
  
  if(first) {
    results_table_sent <- res_df
    first <- FALSE
  } else { results_table_sent <- rbind(results_table_sent, res_df) }
}

results_table_sent <- results_table_sent %>% 
  mutate(sentiment = as.numeric(results_table_sent$text),
         id = str_remove(id, "req-sent-"))

# join with article data
sentiment_data <- results_table_sent %>% select(id, sentiment) %>% mutate(id = as.numeric(id)) %>%
  left_join(full_data, by = "id") %>% mutate(date = as.Date(date, format = "%Y-%m-%d"))

# aggregate by month
sentiment_monthly <- sentiment_data %>% mutate(month = substr(date, 1, 7)) %>% 
  group_by(month) %>%
  summarise(sent = mean(sentiment)) %>%
  mutate(month = as.Date(paste0(month, "-01")))

# aggregate by day
sentiment_daily <- sentiment_data %>%
  group_by(date) %>%
  summarise(sent = mean(sentiment))


# plot
sentiment_daily %>% ggplot(aes(x = date, y = sent)) + geom_line()

write.csv(sentiment_data, "NYT_articles_with_sentiment.csv", row.names = F)
write.csv(sentiment_daily, "NYT_sentiment_daily.csv", row.names = F)
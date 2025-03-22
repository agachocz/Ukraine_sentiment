
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
  mutate(author = str_remove(author, "By ")) %>%
  filter(!is.na(date))

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
prompt_sent <- c("You will receive a snippet of a news article about a war between Russia and Ukraine.",
           "Assign the sentiment of the article as positive (+), negative (-), or neutral/ambiguous (0).",
           "Return just the sentiment as a single character: -, +, or 0.")

prompt_sent <- prompt_sent %>% paste(collapse = " ")

# prompt with reasoning
prompt_reason <- c("You will receive a snippet of a news article about a war between Russia and Ukraine and you need to analyse its sentiment.",
           "Assign the sentiment of the article as positive (+), negative (-), or neutral/ambiguous (0)",
           "and provide your reasoning in one or two sentences.",
           "Return the sentiment value as a single character (-, +, or 0) and short explanation of your reasoning after a colon.", 
           "Example:",
           "INPUT: A year after Russian aggression, many businesses omit sanctions and restart activity in the country.",
           "OUTPUT: -: Since sanctions don't stop business activity, they are not likely to stop the aggression.")

prompt_reason <- prompt_reason %>% paste(collapse = " ")

# prompt war/peace with reasoning

# prompt Russia/Ukraine with reasoning




articles <- full_data$text
n <- length(articles)

# preparing batches from NYT articles
text <- ''
for (i in 1:n){
  json <- paste0('{"custom_id": "req-simple-',i,'", "method": "POST",
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
con <- file("batches/sent_simple.jsonl", open = "w+", encoding = "UTF-8")
writeLines(utf8, con = con)
close(con)



# preparing batches from NYT articles - REASONING
text <- ''
for (i in 1:n){
  json <- paste0('{"custom_id": "req-reason-',i,'", "method": "POST",
                 "url": "/v1/chat/completions", 
                 "body": {"model": "gpt-4o-mini",
                 "messages": [{"role": "system", "content": "',
                 prompt_reason, '"},
                 {"role": "user", "content": "', articles[i], '"}],
                 "temperature": 0.00,
                 "max_tokens": 2000}}')
  json <- str_remove_all(json, "\n") %>% str_squish()
  text <- paste(text, '\n', json)
}

# write in jsonl file
text <- str_trim(text, side = "both")
utf8 <- enc2utf8(text)
con <- file("batches/sent_reason.jsonl", open = "w+", encoding = "UTF-8")
writeLines(utf8, con = con)
close(con)


# prompt for war vs peace
prompt_war <- c("You will receive a snippet of a news article about a war between Russia and Ukraine.",
                 "Decide if the events described in this article suggest an increase in conflict intensity or a change towards peace.",
                 "Assign '+' in case of events making peace more likely, '-' if the described events increase conflict, or '0' if it is hard to tell.",
                 "Return a single character: -, +, or 0.")

prompt_war <- prompt_war %>% paste(collapse = " ")

prompt_war_alt <- c("You will receive a snippet of a news article about a war between Russia and Ukraine.",
                "Decide if the events described in this article suggest an increase in conflict intensity or a change towards peace.",
                "Assign '-' in case of events making peace more likely, '+' if the described events increase conflict, or '0' if it is hard to tell.",
                "Return a single character: -, +, or 0.")

prompt_war_alt <- prompt_war_alt %>% paste(collapse = " ")

# preparing batches from NYT articles - WAR/PEACE
text <- ''
for (i in 1:n){
  json <- paste0('{"custom_id": "req-war-',i,'", "method": "POST",
                 "url": "/v1/chat/completions", 
                 "body": {"model": "gpt-4o-mini",
                 "messages": [{"role": "system", "content": "',
                 prompt_war_alt, '"},
                 {"role": "user", "content": "', articles[i], '"}],
                 "temperature": 0.00,
                 "max_tokens": 2000}}')
  json <- str_remove_all(json, "\n") %>% str_squish()
  text <- paste(text, '\n', json)
}

# write in jsonl file
text <- str_trim(text, side = "both")
utf8 <- enc2utf8(text)
con <- file("batches/sent_war_alt.jsonl", open = "w+", encoding = "UTF-8")
writeLines(utf8, con = con)
close(con)




# prompt for Ukraine vs Russia
prompt_sides <- c("You will receive a snippet of a news article about a war between Russia and Ukraine.",
                "Decide which side would benefit or take advantage of described events.",
                "Assign '+' in case of events beneficial for Ukraine, '-' if they benefit Russia, or '0' if it is hard to tell.",
                "Return a single character: -, +, or 0.")

prompt_sides <- prompt_sides %>% paste(collapse = " ")

prompt_sides_alt <- c("You will receive a snippet of a news article about a war between Russia and Ukraine.",
                  "Decide which side would benefit or take advantage of described events.",
                  "Assign '-' in case of events beneficial for Ukraine, '+' if they benefit Russia, or '0' if it is hard to tell.",
                  "Return a single character: -, +, or 0.")

prompt_sides_alt <- prompt_sides_alt %>% paste(collapse = " ")

# preparing batches from NYT articles - Ukraine vs Russia
text <- ''
for (i in 1:n){
  json <- paste0('{"custom_id": "req-war-',i,'", "method": "POST",
                 "url": "/v1/chat/completions", 
                 "body": {"model": "gpt-4o-mini",
                 "messages": [{"role": "system", "content": "',
                 prompt_sides_alt, '"},
                 {"role": "user", "content": "', articles[i], '"}],
                 "temperature": 0.00,
                 "max_tokens": 2000}}')
  json <- str_remove_all(json, "\n") %>% str_squish()
  text <- paste(text, '\n', json)
}

# write in jsonl file
text <- str_trim(text, side = "both")
utf8 <- enc2utf8(text)
con <- file("batches/sent_sides_alt.jsonl", open = "w+", encoding = "UTF-8")
writeLines(utf8, con = con)
close(con)







# sample of articles for human annotation

sample <- full_data %>% slice_sample(n = 100) %>% select(id, text)
write.csv(sample, "human sample.csv")

sample_done <- read.csv("human sample.csv", sep = ";", encoding = "UTF-8")
colnames(sample_done) <- c("nr", "id", "text", "sentiment")

sample_done <- sample_done %>% select(id, human = sentiment)


# read the results

library(jsonlite)

# SIMPLE
filename <- "GPT sent/simple sent.jsonl"

  con <- file(filename, open = "r", encoding = "UTF-8")
  file <- readLines(con = con)
  close(con)
  
  sent_simple <- lapply(file, function(x){
    results <- fromJSON(x)
    df <- data.frame(
      id = results$custom_id,
      text = results[["response"]][["body"]][["choices"]][["message"]][["content"]]
    )}) %>% bind_rows()
  


sent_simple <- sent_simple %>% 
  mutate(id = as.numeric(str_remove(id, "req-simple-"))) %>%
  rename(simple = text)


# WITH REASONING
filename <- "GPT sent/reason sent.jsonl"

con <- file(filename, open = "r", encoding = "UTF-8")
file <- readLines(con = con)
close(con)

sent_reason <- lapply(file, function(x){
  results <- fromJSON(x)
  df <- data.frame(
    id = results$custom_id,
    text = results[["response"]][["body"]][["choices"]][["message"]][["content"]]
  )}) %>% bind_rows()

split_reason <- function(text, which){ # 1 for sentiment, 2 for explanation
  str_split(text, ":")[[1]][which]
}

sent_reason$reason <- sapply(sent_reason$text, split_reason, which = 1)
sent_reason$expl <- sapply(sent_reason$text, split_reason, which = 2)

sent_reason <- sent_reason %>% 
  mutate(id = as.numeric(str_remove(id, "req-reason-"))) %>%
  select(id, reason, expl)




# join with article data
sentiment_data <- full_data %>% left_join(sent_simple, by = "id") %>% 
  left_join(sent_reason, by = "id") %>% left_join(sample_done, by = "id") %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  mutate(gpt_match = if_else(simple == reason, TRUE, FALSE))
  
sum(sentiment_data$gpt_match)/4972

unique(sentiment_data$simple)
unique(sentiment_data$reason)


# check which is more aligned with human annotation

sentiment_data %>% filter(!is.na(human)) %>%
  mutate(simple_match = if_else(simple == human, TRUE, FALSE),
         reason_match = if_else(reason == human, TRUE, FALSE)) %>%
  summarise(
    simple_acc = sum(simple_match)/100,
    reason_acc = sum(reason_match)/100,
    gpt_match = sum(gpt_match)/100,
    n = n()
  )

N <- nrow(sentiment_data)
sentiment_data %>% group_by(simple) %>% tally() %>% mutate(prop = n/N)
sentiment_data %>% group_by(reason) %>% tally() %>% mutate(prop = n/N)
sentiment_data %>% filter(!is.na(human)) %>% group_by(human) %>% tally() %>% mutate(prop = n/100)


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
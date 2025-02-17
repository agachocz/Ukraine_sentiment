library(tidyverse)
library(rvest)
library(RSelenium)
library(wdman)

url <- "https://www.nytimes.com/news-event/ukraine-russia?page="
url2 <- "https://www.nytimes.com/news-event/israel-hamas-gaza?page="

rD <- rsDriver(browser="firefox", port=4575L, verbose=F, chromever = NULL)
remDr <- rD[["client"]]
remDr$open()

# go to page
remDr$maxWindowSize()
remDr$navigate(url)

data <- data.frame(header = "", link = "", desc = "", date = "", author = "")


  page_url <- paste0(url,10)
  remDr$navigate(page_url)
  Sys.sleep(10)
  
repeat{
  page <- read_html(remDr$getPageSource()[[1]])
  offers_A <- page %>% html_elements(".css-18yolpw")
  
  # TRY: after 10 pages, scroll down several times (in a loop)
  # save after each chunk of data 
  # scroll up to the moment when the last headline repeats? 
  # (meaning there are no more articles)
  # expected about 5 per day, so 5500 articles
  webElem <- remDr$findElement("css", "body")
  
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(5)
  
  page <- read_html(remDr$getPageSource()[[1]])
  offers_B <- page %>% html_elements(".css-18yolpw")
  
  print(paste("A", length(offers_A)))
  print(paste("B", length(offers_B)))
 
  if(length(offers_A) == length(offers_B)) break
}
  
  offers <- offers_B
  data_page <- data.frame(
    header = offers %>% html_element("h3") %>% html_text2(),
    link = offers %>% html_element("a") %>% html_attr("href"),
    desc = offers %>% html_element("p") %>% html_text2(),
    date = offers %>% html_element(".css-e0xall.e15t083i3") %>% html_text2(),
    author = offers %>% html_nodes("p:last-of-type") %>% html_text2()
  )
  
  data <- rbind(data, data_page)
  # scraped on 06.02.2025

  
  
nrow(unique(data_page))

remDr$close()
rD$client$closeServer()

data_israel <- unique(data_israel)
nrow(unique(data_page))

write.csv(data_page, "nyt_Ukraine_news.csv")
write.csv(data_israel, "nyt_Israel_news.csv")


length(unique(data_page$date))
nrow(data_page)/484 # two articles per day


# joining data
data_1 <- read.csv("nyt_archive_Ukraine_2.csv")
data_2 <- read.csv("nyt_archive_Ukraine_3.csv")

data_3 <- data_2[1960:2062,] %>% mutate(date = if_else(str_detect(date, "ago"), "Feb. 11", date)) %>%
  mutate(date = paste(date, "2025"))
data_2 <- data_2[1:1959,]

library(lubridate)


data_UKR <- rbind(data_1, data_2, data_3) %>% unique() %>% select(-X) %>% slice(-1) %>%
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


prompt_sent <- "You will receive a header of a news article. 
           Assign the sentiment of the article on a scale from -5 (strongly negative)
           to 5 (strongly positive). Return just the sentiment value as a single number."

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

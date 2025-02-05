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
remDr$navigate(url2)

data_israel <- data.frame(header = "", link = "", desc = "", date = "", author = "")


for(i in 1:last_page){
  
  page_url <- paste0(url2,i)
  remDr$navigate(page_url)
  Sys.sleep(10)
  
  page <- read_html(remDr$getPageSource()[[1]])
  offers <- page %>% html_elements(".css-18yolpw")
  
  # TRY: after 10 pages, scroll down several times (in a loop)
  # save after each chunk of data 
  # scroll up to the moment when the last headline repeats? 
  # (meaning there are no more articles)
  # expected about 5 per day, so 5500 articles
  webElem <- remDr$findElement("css", "body")
  
  webElem$sendKeysToElement(list(key = "end"))
  for(j in 1:5){
    webElem$sendKeysToElement(list(key = "up_arrow"))
  }
  Sys.sleep(5)
  
  
  data_page <- data.frame(
    header = offers %>% html_element("h3") %>% html_text2(),
    link = offers %>% html_element("a") %>% html_attr("href"),
    desc = offers %>% html_element("p") %>% html_text2(),
    date = offers %>% html_element(".css-e0xall.e15t083i3") %>% html_text2(),
    author = offers %>% html_nodes("p:last-of-type") %>% html_text2()
  )
  
  data_israel <- rbind(data, data_page)
  
}

length(unique(data_page$date))

remDr$close()
rD$client$closeServer()

data_israel <- unique(data_israel)
nrow(unique(data_page))

write.csv(data, "nyt_Ukraine_news.csv")
write.csv(data_israel, "nyt_Israel_news.csv")

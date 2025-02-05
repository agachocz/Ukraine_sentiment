library(tidyverse)
library(rvest)
library(RSelenium)
library(wdman)

url <- "https://www.nytimes.com/news-event/ukraine-russia?page="
nr <- 1

rD <- rsDriver(browser="firefox", port=4560L, verbose=F, chromever = NULL)
remDr <- rD[["client"]]
remDr$open()

# go to page
remDr$maxWindowSize()
remDr$navigate(url)

# accept cookies
cookies <- remDr$findElement(using = "tag name", value = "button")
cookies$clickElement()

ads <- remDr$findElement(using = "class", value = "css-1axchoo")
ads$clickElement()



data <- data.frame(header = "", link = "", desc = "", date = "", author = "")

# in a loop: move through all months
# https://www.tvp.info/biznes?page=2
page <- read_html(remDr$getPageSource()[[1]])
remDr$screenshot(file = "screenshoot.png")
#last_page <- page %>% html_element(".pagination__item pagination__item--last") %>% 
#  html_text2() %>% as.numeric()
last_page <- 10

for(i in 1:last_page){
  
  page_url <- paste0(url,i)
  remDr$navigate(page_url)
  Sys.sleep(10)
  
  page <- read_html(remDr$getPageSource()[[1]])
  offers <- page %>% html_elements(".css-18yolpw")
  
  
  data_page <- data.frame(
    header = offers %>% html_element("h3") %>% html_text2(),
    link = offers %>% html_element("a") %>% html_attr("href"),
    desc = offers %>% html_element("p") %>% html_text2(),
    date = offers %>% html_element(".css-e0xall.e15t083i3") %>% html_text2(),
    author = offers %>% html_element("span") %>% html_text2()
  )
  
  data <- rbind(data, data_page)
  
}

remDr$close()
rD$client$closeServer()



write.csv(data, "nyt_Ukraine_news.csv")

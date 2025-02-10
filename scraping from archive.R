# enter each date and scrape articles
url_1 <- "https://www.nytimes.com/search?dropmab=false&endDate="
url_2 <- "&lang=en&query=Ukraine&sections=World%7Cnyt%3A%2F%2Fsection%2F70e865b6-cc70-5181-84c9-8368b3a5c34b&sort=best&startDate="


start_date <- as.Date("2022-01-01")
end_date <- as.Date("2023-07-27")


rD <- rsDriver(browser="firefox", port=4570L, verbose=F, chromever = NULL)
remDr <- rD[["client"]]
#remDr$open()

# go to page
remDr$maxWindowSize()
remDr$navigate(url)

data <- data.frame(header = "", link = "", desc = "", date = "", author = "")

date_1 <- start_date
date_2 <- date_1+1

repeat{
  
  page_url <- paste0(url_1,date_2,url_2,date_1)
  remDr$navigate(page_url)
  Sys.sleep(10)
  
  page <- read_html(remDr$getPageSource()[[1]])
  offers <- page %>% html_elements(".css-1l4w6pd")
  
  data_page <- data.frame(
    header = offers %>% html_element(".css-nsjm9t") %>% html_text2(),
    link = offers %>% html_element("a") %>% html_attr("href"),
    desc = offers %>% html_element(".css-e5tzus") %>% html_text2(),
    date = offers %>% html_element(".css-y0k07m") %>% html_text2(),
    author = offers %>% html_element(".css-1engk30") %>% html_text2()
  )
  
  data <- rbind(data, data_page)
  
  date_1 <- date_2+1
  date_2 <- date_1+1
  print(date_1)

  
  if(date_1 >= end_date) break
}

write.csv(data, "nyt_archive_Ukraine_2.csv")

remDr$close()
rD$client$closeServer()

# scraped on 07.02.2025

# 
# crawler for eBay kleinanzeigen
# 
# thx to: https://stackoverflow.com/questions/44320008/parse-html-data-using-r
# 
rm(list=ls())
gc()
library (RCurl)
library(dplyr)
library(tidyr)
library(rvest)


fileURL<-"https://www.ebay-kleinanzeigen.de/s-autos/81379/c216l9618"
fileURL<-"https://www.ebay-kleinanzeigen.de/s-81379/blu-ray/k0l9618r10"

xData <- getURL(fileURL, ssl.verifypeer = FALSE)
html <- read_html(xData)

dat <- tibble(
  name = html %>% html_nodes(".aditem-main .text-module-begin") %>%  html_text() %>% trimws(),
  link = html %>% html_nodes(".aditem-main .text-module-begin a") %>%  html_attr('href'),
  prices = html %>% html_nodes(".aditem-details ") %>%  html_text()%>% trimws(),
  when = html %>% html_nodes(".aditem-addon ") %>%  html_text()%>% trimws()
)

dat$prices <- as.integer( trimws(gsub("\\???.*","",dat$prices)) )

dat %>% separate(prices, into = 'D', extra = 'drop', remove = FALSE) %>% 
  select(0:9)

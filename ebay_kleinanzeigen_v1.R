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
library(tidyverse)
library(rvest)


# fileURL <-"https://www.ebay-kleinanzeigen.de/s-autos/81379/c216l9618"
# fileURL <-"https://www.ebay-kleinanzeigen.de/s-81379/blu-ray/k0l9618r10"

# https://www.ebay-kleinanzeigen.de/s-81379/seite:1/blu-ray/k0l9618r10
fileURL_suffix<-"https://www.ebay-kleinanzeigen.de/s-81379/anzeige:angebote/seite:"
fileURL_prefix<-"/blu-ray/k0l9618r10"

fileURL_page = 1
fileURL_keepLooping = TRUE
dat_final <- data.frame()

while (fileURL_keepLooping == TRUE) {
  fileURL = paste0(fileURL_suffix, fileURL_page, fileURL_prefix)
  html <- read_html(getURL(fileURL, ssl.verifypeer = FALSE))
  
  dat <- tibble(
    name   = html %>% html_nodes(".aditem-main .text-module-begin") %>%  html_text() %>% trimws(),
    link   = html %>% html_nodes(".aditem-main .text-module-begin a") %>%  html_attr('href'),
    price  = html %>% html_nodes(".aditem-details ") %>%  html_text()%>% trimws(),
    when   = html %>% html_nodes(".aditem-addon ") %>%  html_text()%>% trimws()
  )
  
  dat$price <- as.integer( trimws(gsub("\\€.*","",dat$price)) )
  dat$page <- fileURL_page
  
  # add all entries to one data.frame
  if (!nrow(dat_final)) {
    dat_final <- dat 
  } else {
    dat_final <- dat_final %>%
      union(dat)
  }
  
  if (nrow(dat)>=20) {
    fileURL_page = fileURL_page + 1
    Sys.sleep(runif(1, 1, 5))
  } else {
    fileURL_keepLooping = FALSE
  }
  # dat %>% separate(prices, into = 'D', extra = 'drop', remove = FALSE) %>% 
  #   select(0:9)
}


summary(dat_final)

dat_final %>% 
  rownames_to_column("when") %>% 
  filter(stringr::str_detect(type, 'Heute|Gestern') )


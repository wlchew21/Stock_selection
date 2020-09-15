## Author: Bill Chew ##
## Date: 09/14/2020  ##
## Purpose: Pull historical stock price on Stock Symbols ###


library(tidyverse)
library(quantmod)
options("getSymbols.warning4.0"=FALSE)

dogs <- read_csv("./derived_data/symbols/2020-09-14_Dogs_of_the_DOW.csv")

stocks <- 
  sapply(dogs$Symbol, possibly(function(x) getSymbols(x, auto.assign = FALSE), otherwise = NULL))

pull.date <- Sys.Date()
saveRDS(stocks, paste0("./derived_data/", pull.date,"_stock_scrape_raw.RDS"))

clean.stocks <- 
lapply(stocks,
       function(x) 
         x %>% 
         as.data.frame() %>% 
         rownames_to_column(var = "Date") %>% 
         rename_all(function(y) sub('.*\\.', '', y))
       )

saveRDS(clean.stocks, paste0("./derived_data/", pull.date,"_stock_scrape_clean.RDS"))

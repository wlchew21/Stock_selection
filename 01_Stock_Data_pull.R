## Author: Bill Chew ##
## Date: 09/14/2020  ##
## File: 01_Stock_Data_pull.R ##
## Purpose: Pull historical stock price on Stock Symbols ###


library(tidyverse)
library(quantmod)
options("getSymbols.warning4.0"=FALSE)

# Use Dogs of the DOW for more accurate list. 
dogs.input.files <- list.files("./derived_data/symbols/", pattern = "Dogs")

# Find which one has the most recent pull (naming convention is to use the date as first 10 char)
latest.file <- which.max(as.Date(substr(dogs.input.files, 1, 10)))

dogs <- read_csv(file.path("./derived_data/symbols/", dogs.input.files[latest.file]))

pull_symbols <- function(x){
  require(tidyverse)
  require(quantmod)
  print(paste0("Starting Symbol: ", x))
  getSymbols(x, auto.assign = FALSE)
}

library(parallel)

cl <- makeCluster( detectCores() - 1, outfile = "")

clusterExport(cl, ls())

# Get the historical price data
stocks <-
  parSapply(cl, dogs$Symbol[1:30], safely(function(x) pull_symbols(x)))

stopCluster(cl)

stocks.copy <- stocks
stocks <- stocks["result", ]

pull.date <- Sys.Date()
saveRDS(stocks, paste0("./derived_data/", pull.date,"_stock_scrape_raw.RDS"))

# Preliminary cleaning of the data
clean.stocks <- 
lapply(stocks,
       function(x) 
         x %>% 
         as.data.frame() %>% 
         rownames_to_column(var = "Date") %>% 
         rename_all(function(y) sub('.*\\.', '', y))
       )

saveRDS(clean.stocks, paste0("./derived_data/", pull.date,"_stock_scrape_clean.RDS"))

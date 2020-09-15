## Author: Bill Chew ##
## Date: 09/14/2020  ##
## Purpose: Pull all ticker symbols that are on the NYSE ###


ticker.pull <- function(letter){
  require(tidyverse)
  require(rvest)
  tickers.raw <- 
    # Dogs of the DOW website listed on pages by alphabetic order by company name
    read_html(paste0("https://www.dogsofthedow.com/stock/stock-symbols-list-", letter, ".htm")) %>%
    # Stock Code and Name - found via selector gadget
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "column-2", " " ))] | //*[contains(concat( " ", @class, " " ), concat( " ", "column-1", " " ))]') %>% 
    # Put into a vector
    html_text()
  
  # Index along the vector
  indices <- seq_along(tickers.raw)
  
  # Even: Stock Names - first record is "Name" 
  # Odd: Code - first record is "Code"
  even.index <- indices[indices %% 2 == 0]
  odd.index <- indices[indices %% 2 != 0]
  
  Company.Name <- tickers.raw[odd.index]
  Symbol <- tickers.raw[even.index]
  
  Company.Name <- Company.Name[!Company.Name %in% "Company Name"]
  Symbol <- Symbol[!Symbol %in% "Symbol"]
  
  return(data.frame(Symbol, Company.Name))
}

library(tidyverse)
# Pull all stock symbols by letter
all.tickers <- 
  lapply(c("number", letters), ticker.pull) %>% 
  bind_rows()

# Date of the pull
pull.date <- Sys.Date()

# output
write_csv(all.tickers, file.path("./derived_data/NYSE_symbols/", paste0(pull.date, "_Dogs_of_the_DOW.csv")))

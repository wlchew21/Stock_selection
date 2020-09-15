## Author: Bill Chew ##
## Date: 09/14/2020  ##
## Purpose: Pull all ticker symbols that are on the NYSE ###


ticker.pull <- function(letter){
  require(tidyverse)
  require(rvest)
  tickers.raw <- 
    # NYSE ticker website listed on pages by alphabetic order
    read_html(paste0("http://eoddata.com/stocklist/NYSE/", letter, ".htm")) %>%
    # Stock Code and Name - found via selector gadget
    html_nodes(xpath = '//th[(((count(preceding-sibling::*) + 1) = 2) and parent::*)] | //*[(@id = "ctl00_cph1_divSymbols")]//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)] | //*[(@id = "ctl00_cph1_divSymbols")]//th[(((count(preceding-sibling::*) + 1) = 1) and parent::*)] | //*[(@id = "ctl00_cph1_divSymbols")]//td[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]') %>% 
    # Put into a vector
    html_text()
  
  # Index along the vector
  indices <- seq_along(tickers.raw)
  
  # Even: Company Names - first record is "Name" 
  # Odd: Code - first record is "Code" last record is "Last"
  even.index <- indices[indices %% 2 == 0]
  odd.index <- indices[indices %% 2 != 0]
  
  Name <- tickers.raw[even.index]
  Code <- tickers.raw[odd.index]
  
  Name <- Name[!Name %in% "Name"]
  Code <- Code[!Code %in% c("Code", "Last")]
  
  return(data.frame(Code, Name))
}

library(tidyverse)
# Pull all stock symbols by letter
all.tickers <- 
  lapply(LETTERS, ticker.pull) %>% 
  bind_rows()

# Date of the pull
pull.date <- Sys.Date()

# output
write_csv(all.tickers, file.path("./derived_data/NYSE_symbols/", paste0(pull.date, "_NYSE_symbols.csv")))

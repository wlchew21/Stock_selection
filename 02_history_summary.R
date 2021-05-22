## Author: Bill Chew ##
## Date: 09/14/2020  ##
## Purpose: Summarize the historical stock data with summary metrics ###

library(parallel)
n.cores <- detectCores()
cl <- makePSOCKcluster(n.cores - 1)
setDefaultCluster(cl)

# Convert from daily into weekly series
# Use close value to reflect actual price
weekly.conversion <- function(single.stock){
  require(tidyverse)
  require(lubridate)
  single.stock %>% 
    mutate(Year = year(Date),
           Month = month(Date),
           Day = day(Date),
           Week = week(Date)) %>% 
    # Get the end of each week
    group_by(Year, Month, Week) %>% 
    filter(Day == max(Day)) %>% 
    ungroup() %>% 
    arrange(Date) %>% 
    mutate(Close.previous = lag(Close)) %>% 
    # Change from previous week close
    mutate(CHG = Close - Close.previous)
}

# Summarize stocks into various summary metrics
stock.summary <- function(single.stock){
  require(tidyverse)
  require(lubridate)
  single.stock %>% 
    summarize(Date.end = max(Date),
              Date.start = min(Date),
              Close.end = Close[Date == Date.end],
              Close.CV = sd(Close, na.rm = TRUE) / mean(Close, na.rm = TRUE),
              CHG.CV = sd(CHG, na.rm = TRUE) / mean(CHG, na.rm = TRUE),
              CHG.CV.1yr = sd(CHG[Date >= ymd(Date.end) - years(1) & Date <= Date.end], na.rm = TRUE) / 
                mean(CHG[Date >= ymd(Date.end) - years(1) & Date <= Date.end], na.rm = TRUE),
              has.1yr = (ymd(Date.end) - ymd(Date.start)) >= years(1),
              has.2yr = (ymd(Date.end) - ymd(Date.start)) >= years(2),
              Close.1yr.ago = ifelse(has.1yr, Close[Year == year(Date.end)-1 & Week == week(Date.end)], NA),
              Close.2yr.ago = ifelse(has.2yr == TRUE, Close[Year == year(Date.end)-2 & Week == week(Date.end)], NA),
              prop.increases = mean(CHG > 0, na.rm = T),
              prop.increases.lastyr = mean(CHG[Date >= ymd(Date.end) - years(1) & Date <= Date.end] > 0, na.rm=T),
              pct.return.1yr = 100*((Close.end - Close.1yr.ago) / Close.1yr.ago),
              pct.return.2yr = 100*((Close.end - Close.2yr.ago) / Close.2yr.ago))
}

library(tidyverse)
library(lubridate)

# Read in Stock data - list with each object being a stock history data

## Symbol key
# Use Dogs of the DOW for more accurate list. 
dogs.input.files <- list.files("./derived_data/symbols/", pattern = "Dogs")

# Find which one has the most recent pull (naming convention is to use the date as first 10 char)
latest.file <- which.max(as.Date(substr(dogs.input.files, 1, 10)))

symbol.key <- read_csv(file.path("./derived_data/symbols/", dogs.input.files[latest.file]))

## Stock history
hist.file <- list.files("./derived_data/", pattern = "clean")
file.dates <- as.Date(substr(hist.file, 1, 10))
latest.file.H <- which.max(file.dates)
stock.list <- readRDS(file.path("./derived_data/", hist.file[latest.file.H]))
pull.date <- file.dates[latest.file.H]


# Export to cluster
clusterExport(NULL, c('weekly.conversion', 'stock.summary', 'stock.list', 'symbol.key'))
# Load libraries
clusterEvalQ(NULL, library(tidyverse))
clusterEvalQ(NULL, library(lubridate))

# Convert to weekly
weekly.stock <- 
  parLapply(NULL, 
            plyr::compact(stock.list),
         possibly(weekly.conversion, otherwise = NULL))

## Output The weekly stock data
saveRDS(weekly.stock, paste0("./derived_data/", pull.date, "_weekly_stock_history.RDS"))


# Summary stats
summarized.stocks <- parLapply(NULL,
                               plyr::compact(weekly.stock),
                            possibly(stock.summary, otherwise = NULL))

# Add in the symbol to the summary and combine to a single data frame
data.stock.summary <- 
  lapply(seq_along(summarized.stocks), 
       function(i)
         summarized.stocks[[i]] %>% 
         add_column(Symbol = names(summarized.stocks)[i], .before = 1)
) %>% 
  bind_rows()


## Output The stock summary
saveRDS(data.stock.summary, paste0("./derived_data/", pull.date, "_stock_summary_stats.RDS"))

# Search through for interesting stocks
symbol.key %>% 
  right_join(data.stock.summary) %>% 
  filter(has.2yr) %>% 
  filter(Close.end < 1000, pct.return.2yr < 50, 
         prop.increases.lastyr > 0.6, pct.return.2yr > 10) %>% 
  arrange(abs(CHG.CV)) %>% 
  View()

stopCluster(cl)

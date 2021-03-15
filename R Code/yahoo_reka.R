# PREFIX ===============================================

  # Author: Kevin Tikvic
  # Title: yahoo_reka

  # Description:  load security prices from Yahoo Finance, based on tickers contained in "merged_meta_reka.txt"

  # file created on: 4/16/18
  # last updated on: 4/16/18

# PRELIMINARY STEPS ===============================================

rm(list=ls()) # clear environment 
setwd("/Users/kevintikvic/Desktop/Rekabsaz_Data") # set working directory

# Load libraries:
library(ggplot2)
library(stargazer)
library(BatchGetSymbols)

# DOWNLOAD PRICES ===============================================
merged_reka <- read.csv("./merged_meta_reka.txt", 
                        sep = ";", header = T)

tickerlist_reka <- unique(merged_reka$ticker) # extract all unique tickers
length(tickerlist_reka); length(merged_reka$ticker) # compare no. of unique companies vs. total reports

reka_yahoo_prices <- BatchGetSymbols(tickers = tickerlist_reka, 
                         first.date = "2005-01-01", # data starts from 2006, we need vola 1y before
                         last.date = "2016-12-31",  # data stops at 2015, we need vola 1y afterwards
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') )

downl_stati <- reka_yahoo_prices$df.control # extract download status summary
summary(downl_stati$download.status) # check how many fails we had 
reka_yahoo <- reka_yahoo_prices$df.tickers # extract only the sub-dataframe with the price info

rm(reka_yahoo_prices, merged_reka, tickerlist_reka) # remove the combined dataframe, as we have extracted the two subframes

save.image("~/Desktop/Rekabsaz_Data/Reka_Yahoo_Prices.RData") # Save the RData fail to avoid downloading again (time-intensive)

# write.csv2(reka_yahoo,
#            file = "prices_reka.txt",
#            sep = ";",
#            dec = ".",
#            row.names = F)

# END ===============================================

# problem: 168 tickers are not available in Yahoo Finance 
# (this will correspond to about 1,000-1,500 reports to be dropped)
# PREFIX ===============================================

  # Author: Kevin Tikvic
  # Title: yahoo_kogan

  # Description:  load security prices from Yahoo Finance, based on tickers contained in "merged_meta_kogan.txt"

  # file created on: 4/16/18
  # last updated on: 4/16/18

# PRELIMINARY STEPS ===============================================

rm(list=ls()) # clear environment 
setwd("/Users/kevintikvic/Desktop/Kogan_Data") # set working directory

# Load libraries:
library(ggplot2)
library(stargazer)
library(BatchGetSymbols)

# DOWNLOAD PRICES ===============================================
merged_kogan <- read.csv("./merged_meta_kogan.txt", 
                        sep = ";", header = T)

# Load tickers, as we only have CIK for this database
CIK2TICKER <- read.csv("./cik_ticker.csv", sep = "|",
                       header = T)

# merge the two databases, adding in the new info (e.g. tickers, exchange, SIC code)
merged_kogan_expanded <- (merge(CIK2TICKER, merged_kogan, by = 'CIK'))

# export the new, expanded dataframe as csv for future use (incl CIK, exchange and SIC codes)
# that way, it will be very close to the one of Rekabsaz
write.csv2(merged_kogan_expanded,
           file = "merged_meta_kogan_expanded.txt",
           sep = ";",
           row.names = F,
           dec = ".")

# drop the two sub-dataframes, keep only the merged (total)
rm(CIK2TICKER, merged_kogan)

tickerlist_kogan <- unique(merged_kogan_expanded$Ticker) # extract all unique tickers
length(tickerlist_kogan); length(merged_kogan_expanded$Ticker) # compare no. of unique companies vs. total reports
#******* RUN FROM HERE
kogan_yahoo_prices <- BatchGetSymbols(tickers = tickerlist_kogan, 
                         first.date = "1995-01-01", # data starts from 1996, we need vola 1y before
                         last.date = "2007-12-31",  # data stops at 206, we need vola 1y afterwards
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') )

downl_stati <- kogan_yahoo_prices$df.control # extract download status summary
summary(downl_stati$download.status) # check how many fails we had 
kogan_yahoo <- kogan_yahoo_prices$df.tickers # extract only the sub-dataframe with the price info

rm(kogan_yahoo_prices, tickerlist_kogan) # remove the combined dataframe, as we have extracted the two subframes

save.image("~/Desktop/Kogan_Data/Kogan_Yahoo_Prices.RData") # Save the RData fail to avoid downloading again (time-intensive)

# write.csv2(kogan_yahoo,
#            file = "prices_kogan.txt",
#            sep = ";",
#            dec = ".",
#            row.names = F)

# END ===============================================

# problem: > 50% of the tickers are not available in Yahoo Finance !!! (2787 companies)
# (this will correspond to about 10,000-15,000 reports to be dropped)
# to do: 
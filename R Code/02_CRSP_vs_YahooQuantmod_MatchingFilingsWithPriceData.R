# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: CRSP_investigation                                                        #
#     Description:  1) inspect CRSP filings that were downloaded based on the 10-K CIKs   #
#                   2) create some descriptive statistics / distributions                 #
#                   3) create some descriptive, meaningful graphs                         #
#                                                                                         #
#     Date (last updated): June 13th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

rm(list=ls()) # clear environment 
setwd("/Users/kevin/Desktop/lm_testdata")

# CIK-Ticker Matching -----------------------------------------------------

# load cik to ticker list from http://rankandfiled.com/#/data/tickers

cikticker <- read.csv(file = "cik_ticker.csv",
                      sep = "|")

length(cikticker$CIK)
length(unique(cikticker$CIK))

# load my list of CIKs from meta script
myciks <- read.table(file = "cik_list.txt", 
                     header = F)

havingtickers <- intersect(unlist(myciks), (cikticker$CIK))
havingtickers <- as.data.frame(unlist(havingtickers))

colnames(havingtickers) <- "CIK"

# thus, we have tickers for 12,525 CIKs

# load global meta & see how many *reports* are affected
meta10k <- read.table(file = "total_LM_only_k.txt", 
                     header = T,
                     sep = ";")

havingtickers_full <- meta10k[meta10k$CIK %in% unlist(havingtickers), ]
havingnotickers_full <- meta10k[!meta10k$CIK %in% unlist(havingtickers), ]

# check the sum of the two
nrow(meta10k); nrow(havingtickers_full) + nrow(havingnotickers_full)

# thus, 111,388 reports are to be deleted, because we do not have ticker name for them
# more might likely follow, because for certain TICKER-DATE combinations yahoo finance might have no daily prices abailable

# retention rate (at this stage!): 62.33 %
nrow(havingtickers_full)/nrow(meta10k) * 100
# LM2011 had 75,252 / 121,217 = 62.08 % --> thus, sounds like a reasonable drop

# WRDS Tickers ------------------------------------------------------------

# load snapshot as per 31 Dec 2017, i.e., take only those that "survived till today" (= have ticker today)
wrds_list <- read.table(file = "wrds_CIK_ticker.csv", 
                      header = T,
                      sep = ",")

length(unique(wrds_list$cik))
length(unique(wrds_list$tic)) 


# there are only 4,104 ... that is too few -- extend the window of the WRDS search back to 1993
wrds_list_long <- read.table(file = "wrds_CIK_ticker_2.csv", 
                             header = T, 
                             sep = ",")

length(unique(wrds_list_long$cik))
length(unique(wrds_list_long$tic))

# this are also only 6,325 ... that is still too few 




# Yahoo DL (Quantmod) -----------------------------------------------------

# install.packages("quantmod")
library(quantmod)
library(xts)
library(zoo)

# list all txt files, recursive options enables browsing through subfolders
setwd("/Users/kevin/Desktop/lm_testdata/filings")
filinglist <- list.files(pattern = ".\\d+\\.txt$", 
                         recursive = T)

filinglist <- substring(filinglist, 11)

# take only a sample to try the code
havingtickers_full_sample <- havingtickers_full[1:184000, ]
nrow(havingtickers_full_sample)

i <- 1
for (i in 1:nrow(havingtickers_full_sample)) {
  
  # Extract filing date from columns 3,4,5
  filedate_yah_form <- as.Date(paste(havingtickers_full_sample[i,3], 
                                     havingtickers_full_sample[i,4], 
                                     havingtickers_full_sample[i,5], 
                                     sep = "/"))
  
  # Add the window around the filing day: start = -1Y, end = +1M
  enddate <- filedate_yah_form + 30 
  startdate <- filedate_yah_form - 365
  
  # Transform start & end dates into quanteda readable mode (using / instead of -)
  startdate <- as.character(sub("-", "/", startdate))
  startdate <- as.character(sub("-", "/", startdate))
  enddate <- as.character(sub("-", "/", enddate))
  enddate <- as.character(sub("-", "/", enddate))
  
  # find the Ticker from the other list
  tickertbd <- as.character(cikticker[cikticker$CIK == havingtickers_full_sample[i,8], 2])
  
  # load data based on ticker and start/end date
  # wrap it in tryCatch so as to skip the "error" ones (I guess where yahoo has no prices stored...)
  tryCatch({
    priceseries <- as.xts(getSymbols(tickertbd, 
                                     from = startdate, 
                                     to = enddate, 
                                     auto.assign = F))
    
    write.zoo(priceseries,
              file = paste("/Users/kevin/Desktop/lm_testdata/pricedata/", 
                             havingtickers_full_sample[i,1], 
                             sep = ""),
              col.names = T, 
              row.names = F,
              index.name = "Date",
              sep = ";",
              quote = F)
    
    rm(priceseries)
    
  }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")})
  
  # Increase row counter
  i <- i + 1
  print(i)
}

# Deleting non-price filings ----------------------------------------------

rm(list=ls()) # clear environment 

# Delete the non-survivor reports
setwd("/Volumes/LaCie/LM_data/pricedata")

# this is the list of 
survivorlist_havingprices <- list.files(pattern = ".\\d+\\.txt$", 
                                        recursive = T) 

# intersect with a list of ALL filings:
setwd("/Volumes/LaCie/LM_data/filings")
filinglist <- list.files(pattern = ".\\d+\\.txt$", 
                         recursive = T)

filinglist <- substring(filinglist, 11)

# those are to be kept
filings_to_be_kept <- intersect(unlist(filinglist), 
                                unlist(survivorlist_havingprices))

# those are to be deleted
"%ni%" <- Negate("%in%") # creates function for "not in" 
filings_to_be_deleted <- filinglist[filinglist %ni% filings_to_be_kept]

# control that we captured all:
length(filinglist); length(filings_to_be_kept) + length(filings_to_be_deleted)

# Extract years and quarters in order to merge them for the full file path
filings_to_be_deleted <- as.data.frame(filings_to_be_deleted)

filings_to_be_deleted$year <- substr(filings_to_be_deleted$filings_to_be_deleted, 1, 4)

filings_to_be_deleted$quarter <- quarters(as.Date(substr(filings_to_be_deleted$filings_to_be_deleted, 1, 8),
                                                  format = "%Y%m%d"))

# extract the quarter number without the Q and trailing zero + convert to factor
filings_to_be_deleted$quarter <- as.factor(substr(filings_to_be_deleted$quarter, 2, 2))

# create delete-able links, i.e., full URLs incl. the year and qrtr subfolders
list_to_del_fulllinks <- paste("/Volumes/LaCie/LM_data/filings/", 
                               filings_to_be_deleted$year, 
                               "/QTR",
                               filings_to_be_deleted$quarter,
                               "/",
                               filings_to_be_deleted$filings_to_be_deleted,
                               sep = "")

# delete all elements in this using their URL
lapply(list_to_del_fulllinks, 
       function(x) unlink(x))

# check whether only the rest survived
setwd("/Volumes/LaCie/LM_data/filings")

remained_files <- list.files(pattern = ".\\d+\\.txt$", 
                             recursive = T)

remained_files <- substring(remained_files, 11)

all.equal(remained_files, filings_to_be_kept) 

# all good ... 

# Export one new, survivor meta list (incl. tickers, for further control variables)
# this is based on remained_files list

length(remained_files) # -- thus, there should be a meta file with 77,951 filings

# load meta again (redundant, but wayne...)
first_meta_all <- read.table(file = "/Volumes/LaCie/LM_data/total_LM_only_k.txt", 
                             header = T,
                             sep = ";")

# load cik to ticker list from http://rankandfiled.com/#/data/tickers
cikticker <- read.csv(file = "/Volumes/LaCie/LM_data/cik_ticker.csv",
                      sep = "|")

# first step: keep only the meta for the circa 77K survivors
second_meta_having_pricedata <- first_meta_all[first_meta_all$txtfilename %in% remained_files, ]

# there are some dupes -- this is due to the fact that in the original meta we had 1421 replicates as well
length(second_meta_having_pricedata$txtfilename)
length(unique(second_meta_having_pricedata$txtfilename))

# see here the original duplicates; there are 1,421
length(first_meta_all$txtfilename) - length(unique(first_meta_all$txtfilename))
sum(duplicated(first_meta_all$txtfilename))

# Investigate which they are, keep both the "original" and the "copy"
idx <- duplicated(first_meta_all$txtfilename) | duplicated(first_meta_all$txtfilename, fromLast = T)
somedupes <- first_meta_all[idx, ]

# here are the 1,421 (x2 = 2,842) cases
nrow(somedupes); nrow(unique(somedupes)); length(unique(somedupes$txtfilename))

# order them by first column to see the pairs "orig-copy" next to each other
somedupes <- as_data_frame(somedupes)

somedupes %>% 
  arrange(., txtfilename)

# indeed, some seem duplicated with a "systematical" step size of 2,000 (see column ID)
# thus, we will remove the duplicates from the intial filing and then intersect with the survivors again

first_meta_all <- first_meta_all[!duplicated(first_meta_all$txtfilename), ]
second_meta_having_pricedata <- first_meta_all[first_meta_all$txtfilename %in% remained_files, ]

# here we go, all good now:
length(second_meta_having_pricedata$txtfilename)
length(unique(second_meta_having_pricedata$txtfilename))

second_meta_having_pricedata <- as_data_frame(second_meta_having_pricedata)

# let's merge
cikticker <- as_data_frame(cikticker)
cikticker

# from this DF we want to merge based on CIK, and import the following columns:
# Ticker / Name / Exchange / SIC / Incorporated 

third_meta_having_all <- merge(second_meta_having_pricedata, 
                               cikticker, 
                               by = "CIK")

third_meta_having_all <- as_data_frame(third_meta_having_all)

# Export this, then when Vola-Loop is done, we will merge with the 14 variables there as well

write.csv2(third_meta_having_all,
           file = "/Volumes/LaCie/LM_data/meta_having_prices_tickers.csv", 
           row.names = F)

# export additionally a list of tickers and a list of CIKs, based on which I will search for controls in WRDS
write.table(unique(third_meta_having_all$CIK),
            file = "/Volumes/LaCie/LM_data/ciks_only_havingprices.txt", 
            row.names = F,
            col.names = F,
            sep = "\n")

write.table(unique(third_meta_having_all$Ticker),
            file = "/Volumes/LaCie/LM_data/tickers_only_havingprices.txt", 
            row.names = F,
            col.names = F,
            quote = F,
            sep = "\n")

# just out of interest -- let's see the discrepancies between ticker and CIK
length(unique(third_meta_having_all$Ticker)); length(unique(third_meta_having_all$CIK))

# there are more CIKs than tickers? 
# -- probably because a company can have more than one? 
# -- or CIKs get "recycled" after some years
# -- most likely: ticker A was something else in 1994, 2000, or 2014 (gives one ticker, but 3 CIKS)


# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: Control_Variables                                                         #
#     Description:  1) Load all control variables                                         #
#                   2) Perform necessary manipulations and bring them in usable form      #
#                   3) Export table with X+1 variables: filename (ID), X controls         #
#                                                                                         #
#     Date (last updated): June 20th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries & Settings ----------------------------------------------------

rm(list=ls()) # clear environment 
setwd("/Volumes/LaCie/LM_data/")
# install.packages("lubridate")
library(lubridate)
# install.packages("data.table")
library(data.table)
# install.packages("pastecs")
library(pastecs)
library(xts)
library(zoo)
library(dplyr)
# install.packages("mltools")
library(mltools)
# install.packages("fastDummies")
library(fastDummies)

# ticker-based ------------------------------------------------------------

rm(list=ls()) # clear environment 
setwd("/Users/kevin/Desktop/lm_testdata")

# # Control variables downloaded based on the list of TICKERS
# CVS <- read.csv2("controls_wrds_compustat_13062018.csv",
#                  sep = ",")
# 
# CVS <- as_data_frame(CVS); CVS
# 
# # see how many NAs are in each columns
# na_count <- sapply(CVS, 
#                    function(y) sum(length(which(is.na(y)))))
# 
# na_count <- data.frame(na_count)
# na_count

# CIK based ---------------------------------------------------------------

rm(list=ls()) # clear environment 
setwd("/Users/kevin/Desktop/lm_testdata")

# I WILL CONTINUE USING CIK BASED DATA -- this is more related to filings +  less NAs !

CVS_cikbased <- read.csv2("controls_wrds_compustat_13062018_basedonCIK.csv",
                 sep = ",",
                 na.strings = c("", "NA")) # last line is crucial, as unavailable data in WRDS is "" and I want it as "NA"

CVS_cikbased <- as_data_frame(CVS_cikbased)

# quick sort based on CIK and then on year
CVS_cikbased %>% 
  arrange(., cik, datadate) %>% 
  print(., n = 30)

nrow(CVS_cikbased)
length(unique(CVS_cikbased$cik))
length(unique(CVS_cikbased$tic))

# see how many NAs are in each columns
na_count <- sapply(CVS_cikbased, 
                   function(y) sum(length(which(is.na(y)))))

na_count <- data.frame(na_count)
na_count

# there a quite many, but I guess the way to go is to keep them 
# then they will simply drop whenever the respective control vars is used (e.g. corrmat, linreg)

# drop the 206 NA cases where fyear is zero, as those will be the indicator var for merging
CVS_cikbased <- CVS_cikbased %>% 
  filter(., is.na(fyear) == F)

# load last version of meta, so as to see how to merge them -- need some sort of CIK+Year combo
meta_with_prices <- read.csv2("meta_having_prices_tickers.csv",
                              sep = ";")

meta_with_prices <- as_data_frame(meta_with_prices)

meta_with_prices %>% 
  arrange(., CIK, reportdate) %>% 
  print(., n = 30)

# create CIK-yr identifier for both tibbles
meta_with_prices <- meta_with_prices %>% 
  mutate(., CIKYR = paste(CIK, repyear, sep = "_"))

# for the meta, create also one that shows "lagged" value, i.e. CVS from year before
meta_with_prices <- meta_with_prices %>% 
  mutate(., CIKYR_lag = paste(CIK, repyear - 1, sep = "_"))

# inspect if it worked
meta_with_prices %>% 
  select(., CIK, reportdate, CIKYR, CIKYR_lag) %>% 
  arrange(., CIK, reportdate) %>% 
  print(., n = 30)

# for CVS:
CVS_cikbased <- CVS_cikbased %>% 
  mutate(., CIKYR = paste(cik, fyear, sep = "_"))

# inspect if it worked
CVS_cikbased %>% 
  select(., cik, datadate, CIKYR) %>% 
  arrange(., cik, datadate) %>% 
  print(., n = 30)

# create merge ID, as the colnames are not equal
meta_with_prices <- meta_with_prices %>% 
  mutate(., merge_id = CIKYR_lag)

CVS_cikbased <- CVS_cikbased %>% 
  mutate(., merge_id = CIKYR)

# let's investigate how many we could potentially have:
length(CVS_cikbased$merge_id)
length(meta_with_prices$merge_id)

# this is what we would get if we do not use the lag
# but this is a) smaller number and b) also not realistic 
intersect(CVS_cikbased$CIKYR, meta_with_prices$CIKYR) %>% 
  length(.)

intersect(CVS_cikbased$merge_id, meta_with_prices$merge_id) %>% 
  length(.) 

# however, there might be dupes in the merge_id:
# this could be due to the fact that 1 company has > 1 filing per year
length(CVS_cikbased$merge_id); length(unique(CVS_cikbased$merge_id))
length(meta_with_prices$merge_id); length(unique(meta_with_prices$merge_id))

# for meta this is not a problem, there we only care that the txtfilename column is unique
length(meta_with_prices$txtfilename); length(unique(meta_with_prices$txtfilename))

# but I don't understand why the CVS has dupes... ?
# let's investigate
dupies <- CVS_cikbased[duplicated(CVS_cikbased$merge_id) | duplicated(CVS_cikbased$merge_id, fromLast = T), ]
dupies
# there we go: INDL (industrial) vs. FS (financial services)
# some companies appear under both -- CIK is the same, as is ticker; thus they are dupes

# let's check whether the merge_id / fyear NAs were removed: 
dupies %>% 
  select(., merge_id) %>% 
  filter(., grepl("NA$", merge_id)) # indeed, they are gone as they should be... 

# let's group by merge_id; if it is solely due to the INDL & FS issue, there should be pairs of 28 years x 2
dupies_by_cik <- dupies %>% 
  group_by(., merge_id, indfmt) %>% 
  summarise(., howmany = n())

dupies_by_cik

# in the "howmany" col, there hence should be only 1s
length(dupies_by_cik$howmany)
nrow(dupies_by_cik[dupies_by_cik$howmany == 1, ])

# that works -- but now we have to decide which one to delete? INDL or FS?
# let's see where we have more blanks -- I strongly assume in FS
dupies <- dupies %>% 
  group_by(., merge_id) %>% 
  arrange(., fyear)

dupies

# INDL is 1st, FS is 2nd
rowSums(is.na(dupies))

# extract even and odd entries and compare which one is bigger
even_indexes <- seq(2, length(rowSums(is.na(dupies))), 2)
odd_indexes <- seq(1, length(rowSums(is.na(dupies))), 2)

indls <- rowSums(is.na(dupies))[odd_indexes]
fss <- rowSums(is.na(dupies))[even_indexes]

sum(indls > fss); sum(indls < fss); sum(indls == fss)

# there are 11,994 / 13,891 = 86% cases where FS has more NAs than INDL
# based on this, we will simply elimnate the FS ones using the anti-join function (ignoring the 14% error)
fs_dupies <- dupies %>% 
  filter(., indfmt == "FS")

CVS_cikbased <- CVS_cikbased %>% 
  anti_join(., fs_dupies)

# inspect the merge_id variable now:
length(CVS_cikbased$merge_id); length(unique(CVS_cikbased$merge_id))

intersect(CVS_cikbased$merge_id, meta_with_prices$merge_id) %>% 
  length(.)

rm(list = c("fss", "indls", "even_indexes", "odd_indexes", "dupies", "fs_dupies", "dupies_by_cik"))

# MERGING -----------------------------------------------------------------

# merge them:
sl_database <- merge(meta_with_prices,
                     CVS_cikbased,
                     by = "merge_id",
                     all = F)

sl_database <- as_data_frame(sl_database)

# inspect why it is larger after merging:
dup_aftermerge <- sl_database[duplicated(sl_database$merge_id) | duplicated(sl_database$merge_id, fromLast = T), ]
nrow(dup_aftermerge); length(meta_with_prices$merge_id) - length(unique(meta_with_prices$merge_id))

nrow(sl_database); nrow(unique(sl_database)); length(unique(sl_database$txtfilename))

dup_aftermerge %>% 
  select(., cik, CIK, merge_id, at, ni, indfmt, fyear, repyear, txtfilename, tic)

# so the merging is done based on merge_id, which is NOT unique in the meta list
# that is fine; they are different filings, yet by same CIK and in same repyear

# Deletion of redundant files ---------------------------------------------

# Delete the roundabout 7K metas from the hard drive to keep only the final 70,048:
filings_to_be_deleted <- anti_join(meta_with_prices, sl_database, by = "txtfilename")
# quick check: 77,951 - 70,048  = 7,903 are to be deleted

# extract the file name only:
filings_to_be_deleted <- as.character(filings_to_be_deleted$txtfilename)

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
# commented out, as it is a non-reversible, one-shot operation
# lapply(list_to_del_fulllinks, 
#        function(x) unlink(x))

# check whether only the rest survived
setwd("/Volumes/LaCie/LM_data/filings")

remained_files <- list.files(pattern = ".\\d+\\.txt$", 
                             recursive = T)

remained_files <- substring(remained_files, 11)

# I initiated this loop wrongly, thereby deleting 2318 filings per accident
shouldhave <- as_data_frame(sl_database$txtfilename)
remained_files <- as_data_frame(remained_files)

wronglydel <- anti_join(shouldhave, remained_files)

# I export this list, in case I want to start from the zip files once more and "restore" them
write.csv2(wronglydel, 
           file = "wronglydeleted.txt", 
           row.names = F)

# keep only the survivor 67,730:
colnames(remained_files) <- "txtfilename"

sl_database <- sl_database %>% 
  inner_join(., remained_files, by = "txtfilename")

write.csv2(sl_database, 
           file = "DB_before_controls_generated_49x67730.txt", 
           row.names = F)

rm(list = c("wronglydel", "shouldhave", "remained_files", "meta_with_prices",
            "filings_to_be_deleted", "dup_aftermerge", "CVS_cikbased", "list_to_del_fulllinks"))

# Control Vars in Merged DB -----------------------------------------------

# add the weekday variable for the surviving
sl_database$weekday <- weekdays(as.Date(paste(sl_database$repyear, 
                                              sl_database$repmth, 
                                              sl_database$repday, 
                                              sep = "/")),
                                abbreviate = T)

# Construct the controls of interest
# -- Source: https://www.wiwi.uni-muenster.de/uf/sites/uf/files/2017_10_12_wrds_data_items.pdf
# and: http://www.wrds.us/index.php/forum_wrds/viewthread/568/#1441

# transform data into numeric values so that we can perform manipulations on them
sl_database <- sl_database %>% 
  mutate_at(., c("at", "bkvlps", "csho", "dlc", "dltt", "ebit", "ebitda", 
                 "lt", "ni", "sale", "seq", "xint", "mkvalt"), funs(paste)) %>% 
  mutate_at(., c("at", "bkvlps", "csho", "dlc", "dltt", "ebit", "ebitda", 
                 "lt", "ni", "sale", "seq", "xint", "mkvalt"), funs(as.numeric))

sl_database %>% 
  select(., xint, dlc, dltt, seq, at, ni, lt, CIK) %>% 
  arrange(., CIK) %>% 
  print(., n = 30)

# modifications / trimming / winsorizing / NAs: 
# RHS variables that need to be inspected and reported a little further:
# trading volume, size, book-to-market, leverage, % of institutional ownership, and % of foreign ownership, plus dummies

is.na(sl_database$txtfilename) %>% sum(.) # no NAs in txtfilename, this is key! 

# // SIZE //  
summary(sl_database$at) # according to this stat, the min is zero, so there should be no negatives in NA

# the sum of pos, neg, zero and NA should give the full 67,730
a <- sum(is.na(sl_database$at)) # NAs
b <- (sl_database$at == 0) %>% sum(., na.rm = T) # zeros
c <- (sl_database$at > 0) %>% sum(., na.rm = T) # pos
d <- (sl_database$at < 0) %>% sum(., na.rm = T) # neg

a;b;c;d; (a + b + c + d) # looks good

# Replace 89 zeros to NAs, so there should be 306 + 89 = 395 NAs in the end:
sl_database$at[sl_database$at == 0] <- NA

# check a, b, c, d again:
a <- sum(is.na(sl_database$at)) # NAs
b <- (sl_database$at == 0) %>% sum(., na.rm = T) # zeros
c <- (sl_database$at > 0) %>% sum(., na.rm = T) # pos
d <- (sl_database$at < 0) %>% sum(., na.rm = T) # neg
a;b;c;d; (a + b + c + d) # all correct! 

# summary stats
hist(sl_database$at); summary(sl_database$at); boxplot(sl_database$at)

# what about outliers? Winsorize! 
# -- There is one max point of 3,287 BILLION!! impossible...

# NB: the function summary() ROUNDS!!!, the values are correct, see:
summary(sl_database$at, digits = 10) 
quantile(sl_database$at, na.rm = T)

# explanation on winsorization: 
# https://blogs.sas.com/content/iml/2017/02/08/winsorization-good-bad-and-ugly.html
# http://www.statsblogs.com/2017/02/08/winsorization-the-good-the-bad-and-the-ugly/
# should be based on counts rather than percentiles + always SYMMETRIC

# investigation on the winsorize function in R
# aa <- c(1:99, rep(100, 13), 101:103, rep(NA, 5))
# sum(aa); sum(aa, na.rm = T); length(aa)
# summary(aa)
# quantile(aa, na.rm = T, probs = seq(0, 1, 0.05))
# 
# bb <- Winsorize(aa, na.rm = T)
# sum(bb); sum(bb, na.rm = T); length(bb)
# summary(bb)
# quantile(bb, na.rm = T, probs = seq(0, 1, 0.05))

# hence, we will winsorize symm. on x% level (as LM2011 did for BTM ratio on 1%)
quantile(sl_database$at, na.rm = T, probs = seq(0, 1, 0.05)) # orig TA
sl_database$at.wins1 <- Winsorize(sl_database$at, na.rm = T, probs = c(0.01, 0.99)) # 1%  
sl_database$at.wins5 <- Winsorize(sl_database$at, na.rm = T, probs = c(0.05, 0.95)) # 5%  

# NOTE: the 2nd variant, using mkt value of eq. for size will not be used, as there are more NAs
# the modification is, however, anyways conducted for BTM ratio just below

# // BTM //  
# how many BV and MV are zero/negative/NA? also CSHO is required for this ... 

# a) BV per share
summary(sl_database$bkvlps) # according to this stat, there are negatives 

# the sum of pos, neg, zero and NA should give the full 67,730
a <- sum(is.na(sl_database$bkvlps)) # NAs
b <- (sl_database$bkvlps == 0) %>% sum(., na.rm = T) # zeros
c <- (sl_database$bkvlps > 0) %>% sum(., na.rm = T) # pos
d <- (sl_database$bkvlps < 0) %>% sum(., na.rm = T) # neg
a;b;c;d; (a + b + c + d) # looks good

# Replace 26 zeros and 5,340 negatives to NAs, so there should be 782 + 26 + 5,340 = 6,148 NAs in the end:
sl_database$bkvlps[sl_database$bkvlps == 0] <- NA
sl_database$bkvlps[sl_database$bkvlps < 0] <- NA

# check a, b, c, d again:
a <- sum(is.na(sl_database$bkvlps)) # NAs
b <- (sl_database$bkvlps == 0) %>% sum(., na.rm = T) # zeros
c <- (sl_database$bkvlps > 0) %>% sum(., na.rm = T) # pos
d <- (sl_database$bkvlps < 0) %>% sum(., na.rm = T) # neg
a;b;c;d; (a + b + c + d) # all correct! 

# winsorization, there is a firm with 1.07 billion book value PER SHARE (!)
summary(sl_database$bkvlps, digits = 10)
quantile(sl_database$bkvlps, na.rm = T, probs = seq(0, 1, 0.05)) # orig

sl_database$bkvlps.wins1 <- Winsorize(sl_database$bkvlps, na.rm = T, probs = c(0.01, 0.99)) # 1%  
sl_database$bkvlps.wins5 <- Winsorize(sl_database$bkvlps, na.rm = T, probs = c(0.05, 0.95)) # 5%

# b) MV (total, not per share)
summary(sl_database$mkvalt) # according to this stat, there are NO negatives

# the sum of pos, neg, zero and NA should give the full 67,730
a <- sum(is.na(sl_database$mkvalt)) # NAs
b <- (sl_database$mkvalt == 0) %>% sum(., na.rm = T) # zeros
c <- (sl_database$mkvalt > 0) %>% sum(., na.rm = T) # pos
d <- (sl_database$mkvalt < 0) %>% sum(., na.rm = T) # neg
a;b;c;d; (a + b + c + d) # looks good

# nothing to do in terms of replacing -- will still winsorize, although values look good; 
# I keep all 3 series anyways and use what works best
# min is 0.0025 mio = 2500 USD market value... a bit low...
# max is 626 billion, can very well be ...
summary(sl_database$bkvlps, digits = 10)
quantile(sl_database$mkvalt, na.rm = T, probs = seq(0, 1, 0.05)) # orig

sl_database$mkvalt.wins1 <- Winsorize(sl_database$mkvalt, na.rm = T, probs = c(0.01, 0.99)) # 1%  
sl_database$mkvalt.wins5 <- Winsorize(sl_database$mkvalt, na.rm = T, probs = c(0.05, 0.95)) # 5%

# b) CSHO (in mio of shares)
summary(sl_database$csho) # that looks very plausible, however some 0s -- no negatives at least

# the sum of pos, neg, zero and NA should give the full 67,730
a <- sum(is.na(sl_database$csho)) # NAs
b <- (sl_database$csho == 0) %>% sum(., na.rm = T) # zeros
c <- (sl_database$csho > 0) %>% sum(., na.rm = T) # pos
d <- (sl_database$csho < 0) %>% sum(., na.rm = T) # neg
a;b;c;d; (a + b + c + d) # looks good

# zero share outstanding is implausible, will replace the 25 zeros to NA (should have 539 then)
sl_database$csho[sl_database$csho == 0] <- NA

# check a, b, c, d again:
a <- sum(is.na(sl_database$csho)) # NAs
b <- (sl_database$csho == 0) %>% sum(., na.rm = T) # zeros
c <- (sl_database$csho > 0) %>% sum(., na.rm = T) # pos
d <- (sl_database$csho < 0) %>% sum(., na.rm = T) # neg
a;b;c;d; (a + b + c + d) # all correct! 

summary(sl_database$csho, digits = 10)
quantile(sl_database$csho, na.rm = T, probs = seq(0, 1, 0.05))

# all good, still winsorize:
sl_database$csho.wins1 <- Winsorize(sl_database$csho, na.rm = T, probs = c(0.01, 0.99)) # 1%  
sl_database$csho.wins5 <- Winsorize(sl_database$csho, na.rm = T, probs = c(0.05, 0.95)) # 5%

# // LEVERAGE // 

# long-term debt (DLTT), debt in current liabilities (DLC), total liabilities (LT):
# I will use LT, as in D / (D+E) = D/TA

# quick check on how often accounting identity isn't satisfied
head(sl_database$lt) + head(sl_database$seq); head(sl_database$at)
sum((sl_database$lt + sl_database$seq - sl_database$at) != 0, na.rm = T)

# hmmmm, 28K out of 67K, that's quite a lot ... 

# let's see how often LT --exceeds-- total assets, which would imply leverage ratios larger than 1, or, in other words, negative equity
sum(sl_database$lt > sl_database$at, na.rm = T)

# 4,541 cases -- that's 6.7 percent --> can live with that

# investigate LT:
summary(sl_database$lt) # that looks very plausible, however some 0s -- no negatives at least

# the sum of pos, neg, zero and NA should give the full 67,730
a <- sum(is.na(sl_database$lt)) # NAs
b <- (sl_database$lt == 0) %>% sum(., na.rm = T) # zeros
c <- (sl_database$lt > 0) %>% sum(., na.rm = T) # pos
d <- (sl_database$lt < 0) %>% sum(., na.rm = T) # neg
a;b;c;d; (a + b + c + d) # looks good

# zero replacement in this case might not be suitable, as liabilities CAN be zero plausibly
# it relates to 427 cases only, so in this case I will not manipulate further

# winsorization:
sl_database$lt.wins1 <- Winsorize(sl_database$lt, na.rm = T, probs = c(0.01, 0.99)) # 1%  
sl_database$lt.wins5 <- Winsorize(sl_database$lt, na.rm = T, probs = c(0.05, 0.95)) # 5%

# Create controls 
sl_database <- sl_database %>% 
  mutate(., 
         costofcap = xint/dlc,
         size_ta = log(at), # NB: given in mio.
         size_mveq = log(mkvalt), # NB: given in mio.
         gearing = lt / seq, 
         leverage = lt / at,
         ROA = ni / at,
         mtb_ratio = mkvalt / (bkvlps * csho), # NB: one given in total, one per share
         EPS = ni / csho)

# compute those controls using 1% and 5% values as well (tag bowi, "based on winsorized inputs"):
# before that, create winsorized versions of NI and SEQ (I skip xint and dlc, CoC is not used anyways)
# SEQ will also be cleaned of non-positive values

summary(sl_database$ni)
sl_database$ni.wins1 <- Winsorize(sl_database$ni, na.rm = T, probs = c(0.01, 0.99)) # 1%  
sl_database$ni.wins5 <- Winsorize(sl_database$ni, na.rm = T, probs = c(0.05, 0.95)) # 5%

summary(sl_database$seq)
a <- sum(is.na(sl_database$seq)) # NAs
b <- (sl_database$seq == 0) %>% sum(., na.rm = T) # zeros
c <- (sl_database$seq > 0) %>% sum(., na.rm = T) # pos
d <- (sl_database$seq < 0) %>% sum(., na.rm = T) # neg
a;b;c;d; (a + b + c + d) # looks good
sl_database$seq[sl_database$seq == 0] <- NA
sl_database$seq[sl_database$seq < 0] <- NA
sl_database$seq.wins1 <- Winsorize(sl_database$seq, na.rm = T, probs = c(0.01, 0.99)) # 1%  
sl_database$seq.wins5 <- Winsorize(sl_database$seq, na.rm = T, probs = c(0.05, 0.95)) # 5%

# controls based on 1% wins. inputs
sl_database <- sl_database %>% 
  mutate(., 
         size_ta_bowi1 = log(at.wins1), # NB: given in mio.
         size_mveq_bowi1 = log(mkvalt.wins1), # NB: given in mio.
         gearing_bowi1 = lt.wins1 / seq.wins1, 
         leverage_bowi1 = lt.wins1 / at.wins1,
         ROA_bowi1 = ni.wins1 / at.wins1,
         mtb_ratio_bowi1 = mkvalt.wins1 / (bkvlps.wins1 * csho.wins1), # NB: one given in total, one per share
         EPS_bowi1 = ni.wins1 / csho.wins1)

# controls based on 5% wins. inputs
sl_database <- sl_database %>% 
  mutate(., 
         size_ta_bowi5 = log(at.wins5), # NB: given in mio.
         size_mveq_bowi5 = log(mkvalt.wins5), # NB: given in mio.
         gearing_bowi5 = lt.wins5 / seq.wins5, 
         leverage_bowi5 = lt.wins5 / at.wins5,
         ROA_bowi5 = ni.wins5 / at.wins5,
         mtb_ratio_bowi5 = mkvalt.wins5 / (bkvlps.wins5 * csho.wins5), # NB: one given in total, one per share
         EPS_bowi5 = ni.wins5 / csho.wins5)

# winsorize the controls that are calculated on original inputs on their own
# tag: "woto1" and "woto5"
sl_database <- sl_database %>% 
  mutate(., 
         costofcap_woto1 = Winsorize(costofcap, na.rm = T, probs = c(0.01, 0.99)),
         costofcap_woto5 = Winsorize(costofcap, na.rm = T, probs = c(0.05, 0.95)),
         size_ta_woto1 = Winsorize(size_ta, na.rm = T, probs = c(0.01, 0.99)),
         size_ta_woto5 = Winsorize(size_ta, na.rm = T, probs = c(0.05, 0.95)),
         size_mveq_woto1 = Winsorize(size_mveq, na.rm = T, probs = c(0.01, 0.99)),
         size_mveq_woto5 = Winsorize(size_mveq, na.rm = T, probs = c(0.05, 0.95)),
         gearing_woto1 = Winsorize(gearing, na.rm = T, probs = c(0.01, 0.99)), 
         gearing_woto5 = Winsorize(gearing, na.rm = T, probs = c(0.05, 0.95)), 
         leverage_woto1 = Winsorize(leverage, na.rm = T, probs = c(0.01, 0.99)),
         leverage_woto5 = Winsorize(leverage, na.rm = T, probs = c(0.05, 0.95)),
         ROA_woto1 = Winsorize(ROA, na.rm = T, probs = c(0.01, 0.99)),
         ROA_woto5 = Winsorize(ROA, na.rm = T, probs = c(0.05, 0.95)),
         mtb_ratio_woto1 = Winsorize(mtb_ratio, na.rm = T, probs = c(0.01, 0.99)),
         mtb_ratio_woto5 = Winsorize(mtb_ratio, na.rm = T, probs = c(0.05, 0.95)),
         EPS_woto1 = Winsorize(EPS, na.rm = T, probs = c(0.01, 0.99)),
         EPS_woto5 = Winsorize(EPS, na.rm = T, probs = c(0.05, 0.95))
         )

rm(list = "a", "b", "c", "d")

# check now the NAs across the cols
na_count <- sapply(sl_database, 
                   function(y) sum(length(which(is.na(y)))))

na_count <- data.frame(na_count)
na_count

# in case we want to delete every row that contains at least 1 NA, use:
# na.omit(sl_database)

# VIX ---------------------------------------------------------------------

# Calculate VIX average in pre-filing period (- 5d), also do for post-filing (for "safety")
setwd("/Users/kevin/Desktop/lm_testdata")

VIX_fulldata <- read.csv("VIX_daily.csv")

# convert first col to date and then use in xts format

# transform date column into date format (was given in strings like "Sep", "Jan", etc.)
VIX_fulldata$Date <- parse_date_time(VIX_fulldata$Date, orders = "d-b-Y")
VIX_fulldata$Date <- as.Date(VIX_fulldata$Date)

# extract the VIX only, the other indices are by far less relevant and will not be used
# could have unticked them in WRDS in the first place probably
VIX_series <- as.xts(VIX_fulldata[, 2], 
                     order.by = VIX_fulldata$Date)

colnames(VIX_series) <- "VIX"

# quick inspect if all looks good
head(VIX_series); tail(VIX_series)
length(VIX_series)
sum(is.na(VIX_series)) # only 3 NAs out of 6,655 daily data points, looks good
ts.plot(VIX_series)

# add the VIX to the sl_database

# empty cols to be filled:
sl_database$med_VIX_pre <- NA
sl_database$med_VIX_post <- NA
sl_database$avg_VIX_pre <- NA
sl_database$avg_VIX_post <- NA

# loop
# Note: the method for idx looks for the CLOSEST date in VIX
# in almost all cases, the delta is 0, i.e., there is an exact match of dates
# for 16 cases, where it is exactly equidistant from 2 adjacent VIX dates, we take the one preceding the currdate
# code source: https://stackoverflow.com/questions/15133815/closest-date-in-a-vector-to-a-given-date

i <- 1
for (i in 1:nrow(sl_database)) {
  
  currdate <- as.character(sl_database$reportdate[i])
  currdate <- as.Date(currdate, format = "%Y%m%d")
  
  # Split the VIX vector to pre and post-filing and calculate its median/mean
  idx <- (which(abs(index(VIX_series) - currdate) == min(abs(index(VIX_series) - currdate))))

  median_VIX_post <- median(VIX_series[(idx):(idx + 4), ], 
                            na.rm = T)
  median_VIX_pre <- median(VIX_series[(idx - 5):(idx - 1), ], 
                           na.rm = T)
  
  mean_VIX_pre <- mean(VIX_series[(idx):(idx + 4), ], 
                       na.rm = T)
  mean_VIX_post <- mean(VIX_series[(idx - 5):(idx - 1), ], 
                        na.rm = T)
  
  # Fill the calculated VIX values
  sl_database$med_VIX_pre[i] <- median_VIX_pre # fill with median VIX (pre-filing 5D)
  sl_database$med_VIX_post[i] <- median_VIX_post # fill with median VIX (filing + 4D post filing)
  sl_database$avg_VIX_pre[i] <- mean_VIX_pre # fill with mean VIX (pre-filing 5D)
  sl_database$avg_VIX_post[i] <- mean_VIX_post # fill with mean VIX (filing + 4D post filing)
  
  # incr. counter
  i <- i + 1
  print(i)
}

rm(list = c("i", "VIX_fulldata", "VIX_series", "currdate", "idx", "mean_VIX_post", "mean_VIX_pre", "median_VIX_post", "median_VIX_pre"))

# One Hot Dummies ---------------------------------------------------------

# for weekday where filing occurred (6, sunday non existent)
sl_database$weekday <- factor(sl_database$weekday)
sl_database <- dummy_cols(sl_database, select_columns = "weekday")

# for filing month (12)
sl_database$repmth <- factor(sl_database$repmth)
sl_database <- dummy_cols(sl_database, select_columns = "repmth")

# for filing year (24)
sl_database$repyear <- factor(sl_database$repyear)
sl_database <- dummy_cols(sl_database, select_columns = "repyear")

# for SIC / industry
sl_database %>% 
  select(., sic) # capital SIC has NAs , this one from WRDS has not

# extract first two digits
sl_database$sic2D <- substr(sl_database$sic, 1, 2)

# note that there are  66 unique 2-digit SIC codes -- this calls for 66 dummy variables
length(sl_database$sic2D)
length(unique(sl_database$sic2D))

# convert to factor, then one-hot them
sl_database$sic2D <- factor(sl_database$sic2D)
sl_database <- dummy_cols(sl_database, select_columns = "sic2D")

# for day of month
sl_database$repday <- factor(sl_database$repday)
sl_database <- dummy_cols(sl_database, select_columns = "repday")

# for filing type
sl_database$reptype <- factor(sl_database$reptype)
sl_database <- dummy_cols(sl_database, select_columns = "reptype")

# Database Export ---------------------------------------------------------

# Export the final table
# setwd("/Volumes/LaCie/LM_data/")

write.csv2(sl_database, 
           file = "DB_after_controls_generated_259x67730.txt", 
           row.names = F)

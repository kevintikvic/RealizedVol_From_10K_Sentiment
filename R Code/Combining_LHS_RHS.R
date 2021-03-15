
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: Combining_LHS_RHS                                                         #
#     Description:  1) merge full database of all RHS control vars with the price data    #
#                   2) load and all text-related RHS variables                            #
#                   3) merge the result of (1) with the loaded DB from (2)                #
#                                                                                         #
#     Date (last updated): July 17th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries / Settings ----------------------------------------------------

rm(list=ls()); gc() # clear environment 
options(scipen = 999)

# setwd("/Volumes/LaCie/LM_data/")
setwd("/Users/kevin/Desktop/lm_testdata")

library(tidyverse)
library(ggcorrplot)
library(DescTools)
library(data.table)

# Outline -----------------------------------------------------------------

# there will be four steps:
    # 1: DB1 = original DB excl. price data: 259 variables
    # 2: DB2 = original DB incl. price data: 259 + (13*3 + 1) - 1 = 298 variables
    # 3: DB3 = this is DB2 plus (some) filing-related RHS: 298 + 4 + 7 = 309 variables
         # NB: This is WITHOUT LM DTM word counts, as these will be split into test/training and would blow up the col dimension
    # 4: DB4 = DB3 cleaned by all "non-usable" data 
         # --> e.g. drop all NA rows (at least those without PFRV)
         # NB: cols remain, but less than 67,730 rows
    # 4: DB5 = DB4 cleaned by economic-nonsense data
         # --> e.g. drop all those with leverage > 100%
         # NB: cols remain, but less rows

# Load DB1 and 7 sub-DB2’s ------------------------------------------------

DB1 <- read.delim(file = "DB_after_controls_generated_259x67730.txt",
                  sep = ";")
DB1 <- as_tibble(DB1)

# load 7 subfiles
DB2.1 <- read.delim(file = "./garchs_pfrvs_tvols/price_vola_related_data_9495.txt",
                    sep = ";")
DB2.2 <- read.delim(file = "./garchs_pfrvs_tvols/price_vola_related_data_9697.txt",
                    sep = ";")
DB2.3 <- read.delim(file = "./garchs_pfrvs_tvols/price_vola_related_data_9899.txt",
                    sep = ";")
DB2.4 <- read.delim(file = "./garchs_pfrvs_tvols/price_vola_related_data_00010203.txt",
                    sep = ";")
DB2.5 <- read.delim(file = "./garchs_pfrvs_tvols/price_vola_related_data_04050607.txt",
                    sep = ";")
DB2.6 <- read.delim(file = "./garchs_pfrvs_tvols/price_vola_related_data_0809101112.txt",
                    sep = ";")
DB2.7 <- read.delim(file = "./garchs_pfrvs_tvols/price_vola_related_data_1314151617.txt",
                    sep = ";")

# rowbind the 7 subfiles
DB_pricedata <- rbind(DB2.1, DB2.2, DB2.3, DB2.4, DB2.5, DB2.6, DB2.7)
DB_pricedata <- as_tibble(DB_pricedata)

# short note: those are still 77K from the price data
# will drop after merging with DB1 with 67K 

# export this rowbinded dataframe, as I have it nowhere except the 7 subfiles
write.table(DB_pricedata,
            file = "/Users/kevin/Desktop/lm_testdata/pricedata_allcombined_14x77951.txt",
            sep = ";",
            row.names = F)

# drop the 7 subfiles
rm(list = c("DB2.1", "DB2.2", "DB2.3", "DB2.4", "DB2.5", "DB2.6", "DB2.7"))

# Export the .RData to avoid loading them over again in case needed
save.image("~/Desktop/lm_testdata/DB1_DBpricedata_before_DB2.RData")

# Modify & Inspect DB_pricedata -------------------------------------------

# investigate PFRV
summary(DB_pricedata$PFRV_main) # according to this stat, the min is zero, so there should be no negatives

a <- sum(is.na(DB_pricedata$PFRV_main)) # NAs
b <- (DB_pricedata$PFRV_main == 0) %>% sum(., na.rm = T) # zeros
c <- (DB_pricedata$PFRV_main > 0) %>% sum(., na.rm = T) # pos
d <- (DB_pricedata$PFRV_main < 0) %>% sum(., na.rm = T) # neg

a;b;c;d; (a + b + c + d) # looks good
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
summary(DB_pricedata$PFRV_sq_ret) # according to this stat, the min is zero, so there should be no negatives

a <- sum(is.na(DB_pricedata$PFRV_sq_ret)) # NAs
b <- (DB_pricedata$PFRV_sq_ret == 0) %>% sum(., na.rm = T) # zeros
c <- (DB_pricedata$PFRV_sq_ret > 0) %>% sum(., na.rm = T) # pos
d <- (DB_pricedata$PFRV_sq_ret < 0) %>% sum(., na.rm = T) # neg

a;b;c;d; (a + b + c + d) # looks good
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
summary(DB_pricedata$PFRV_abs_ret) 
quantile(DB_pricedata$PFRV_abs_ret, probs = seq(0,1,0.05), na.rm = T) # there are negatives

a <- sum(is.na(DB_pricedata$PFRV_abs_ret)) # NAs
b <- (DB_pricedata$PFRV_abs_ret == 0) %>% sum(., na.rm = T) # zeros
c <- (DB_pricedata$PFRV_abs_ret > 0) %>% sum(., na.rm = T) # pos
d <- (DB_pricedata$PFRV_abs_ret < 0) %>% sum(., na.rm = T) # neg

a;b;c;d; (a + b + c + d) # there are 934 negatives ... 

#replace the negatives with NAs, should have 4,903 + 934 = 5,837 then
DB_pricedata$PFRV_abs_ret[DB_pricedata$PFRV_abs_ret < 0] <- NA

# a, b, c, d checked again: all good.
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
# larger than "reasonable" threshold
thold <- 1
(DB_pricedata$PFRV_main >= thold) %>% sum(., na.rm = T)

# but there is -- theoretically -- no upper bound on vola (only lower bound, namely 0)
# check: http://www.macroption.com/can-volatility-be-over-100/
# check: https://quant.stackexchange.com/questions/16705/why-an-option-has-sometimes-and-implied-volatility-greater-than-100

# so I will keep them for the time being ... 

# Winsorization 
# extract first col with txt filenames
DB_pricedata2 <- DB_pricedata[,-1]

# for the remaining 13 create 1% and 5% Dframes, then cbind back together again:
DB_pricedata3 <- DB_pricedata2 %>% 
  mutate_all(funs(Winsorize(., probs = c(0.01, 0.99), na.rm = T)))

colnames(DB_pricedata3) <- paste0(colnames(DB_pricedata3), ".wins1")

DB_pricedata4 <- DB_pricedata2 %>% 
  mutate_all(funs(Winsorize(., probs = c(0.05, 0.95), na.rm = T)))

colnames(DB_pricedata4) <- paste0(colnames(DB_pricedata4), ".wins5")

DB_pricedata_incl_winsies <- cbind(DB_pricedata,
                                   DB_pricedata3,
                                   DB_pricedata4)

DB_pricedata_incl_winsies <- as_tibble(DB_pricedata_incl_winsies)

# check colmaxs and colmins to see whether winsorization worked
aa <- DB_pricedata_incl_winsies %>% 
  select(., 2:40) %>% 
  summarise_all(., max, na.rm = T)

data.frame(aa)

bb <- DB_pricedata_incl_winsies %>% 
  select(., 2:40) %>% 
  summarise_all(., min, na.rm = T)

data.frame(bb)

# drop redundant helper vars
rm(list = c("a", "b", "c", "d", "thold", "aa", "bb", "DB_pricedata", "DB_pricedata2",
            "DB_pricedata3", "DB_pricedata4", "randsamp_garchs", "na_count", "cormat", 
            "p_cormat", "delta_GARCH_PFRV"))

# Create DB2: Merge DB1 with DB_pricedata ---------------------------------

DB2 <- merge(DB_pricedata_incl_winsies,
             DB1,
             by = "txtfilename",
             all = F)

DB2 <- as_tibble(DB2)

# Export DB2 at this stage:
write.table(DB2,
            file = "/Users/kevin/Desktop/lm_testdata/DB2_everything-except-texts_298x67730.txt",
            sep = ";",
            row.names = F)

# Load text-related RHS ---------------------------------------------------

# re-load DB2
DB2 <- read.delim("/Users/kevin/Desktop/lm_testdata/DB2_everything-except-prices_298x67730.txt", 
                  sep = ";", stringsAsFactors = F, dec = ",") %>% 
  as_tibble()

DB2

# Header related data
header.data <- read.delim(file = "/Users/kevin/Desktop/lm_testdata/gfs-nfs-tab-exh_header_data.txt",
                          sep = ";", stringsAsFactors = F) %>% 
  as_tibble()

header.data

# Length related data
length.related.data <- read.delim(file = "/Users/kevin/Desktop/lm_testdata/doc_length_variables.txt",
                                  sep = ";", stringsAsFactors = F) %>% 
  as_tibble()

# Drop the 2 measures based on full DTM counts -- 
# they were too large for RAM to be generated and // should // be equal to the first 2 cols anyways
length.related.data <- length.related.data[, -(5:6)]
length.related.data

# Create DB3: Merge DB2 with text-RHSs ------------------------------------
DB3_step1 <- merge(header.data,
                   DB2,
                   by = "txtfilename",
                   all = F) %>% as_tibble

DB3_step1

DB3 <- merge(length.related.data,
             DB3_step1,
             by = "txtfilename",
             all = F) %>% as_tibble

DB3

# Remove helper dataframes and export single, merged DB3 as .txt as well as .RData
rm(DB2, DB3_step1, header.data, length.related.data)

write.table(DB3,
            file = "/Users/kevin/Desktop/lm_testdata/DB3_everything-except-termcounts_309x67730.txt",
            sep = ";",
            row.names = F)

save.image("/Users/kevin/Desktop/lm_testdata/DB3_309x67730.RData")

# I will NOT cbind the word count but rather leave them in 9 separate DTMs
# that way I can adress those 9 dataframes and use them in regression / score compuatation

# Create DB4: Clean DB3 ---------------------------------------------------

# check NAs first, especially for PFRV as this is the LHS in ALL models
# count NAs
na_count <- sapply(DB3, 
                   function(y) sum(length(which(is.na(y)))))

na_count <- data.frame(na_count)
na_count$varname <- rownames(na_count)

na_count %>% as_tibble() %>% 
  filter(na_count > 0) %>% 
  arrange(varname) %>% 
  print(., n = 80)

# there are 107 variables that have at least one NA

# for the GARCHs:
# we have 3,203 NAs for GARCH_1step (and the 1/5% winsies as well)
# we have 3,811 NAs for GJR_1step (and the 1/5% winsies as well)
# I will keep them, as they are essential "only" for the MZ regressions
# moreover, deleting PFRV_main might drop a lot of those two
# both are calculated based on same price data, and likely to be NAs where yahoo had nothing in the 1st place


# we have 3,203 NAs for PFRV_main (and the 1/5% winsies as well)
# those I will omit as I will need them // everywhere //
DB4 <- na.omit(data.table(DB3), 
               cols = "PFRV_main") %>% 
  as_tibble()

# using sq. returns: 3,790 NAs, using abs. returns: 3,780
# those should (will) be dropped in "robustness" checks when we define PFRV differently

# after checking re GJR_1step: 608 missing survived
# so all 3,203 others were co-occurrences with pfrv_main (good news!)

# Export the txt filenames of the PFRV survivors/droppers 
# so in the DTMs I can drop/keep accordingly 
PFRV_diers <- anti_join(as_tibble(DB3$txtfilename), 
                        as_tibble(txtfilenames_DB4))

PFRV_survivors <- inner_join(as_tibble(DB3$txtfilename), 
                             as_tibble(txtfilenames_DB4))

# Export those 2
write.table(as.matrix(PFRV_diers),
            file = "/Users/kevin/Desktop/lm_testdata/pfrv_diers.txt",
            sep = "\n", quote = F, row.names = F, col.names = F)

write.table(as.matrix(PFRV_survivors),
            file = "/Users/kevin/Desktop/lm_testdata/pfrv_survivors.txt",
            sep = "\n", quote = F, row.names = F, col.names = F)

# Remove helper dataframes and export single DB4 as .txt as well as .RData
rm(DB3, na_count, txtfilenames_DB4)
   
write.table(DB4,
            file = "/Users/kevin/Desktop/lm_testdata/DB4_pfrv-main-cleaned_309x64527.txt",
            sep = ";",
            row.names = F)

save.image("/Users/kevin/Desktop/lm_testdata/DB4_309x64527.RData")

# DB5: dropping zeroes and crazy outliers ---------------------------------

rm(list=ls()); gc() # clear environment
load("/Users/kevin/Desktop/lm_testdata/DB4_309x64527.RData")

# filter out volas equal to zero (n = 366) and those larger than 100% (n=232):
DB4$PFRV_main %>% as.numeric %>% quantile(probs = seq(0,1,.02))
sum(as.numeric(DB4$PFRV_main) == 0)
sum(as.numeric(DB4$PFRV_main) == 0) / 64527 * 100
sum(as.numeric(DB4$PFRV_main) > 1)
sum(as.numeric(DB4$PFRV_main) > 1) / 64527 * 100

# Drop those 598: 
DB5 <- DB4 %>% 
  filter(PFRV_main > 0) %>% 
  filter(PFRV_main <= 1)

# filter out for RHS vars:
# first, create BTM and log(GFS):

DB5 <- DB5 %>% 
  mutate(log_GFS = log(GFS),
         BTM = 1/mtb_ratio)

# Filter steps: 
  # c. leverage ratios > 1 or NA --> impossible
sum(DB5$leverage > 1, na.rm = T) # 3840 cases
sum(is.na(DB5$leverage)) # 408 cases

  # b. TVOL.1W.med.pre == 0 --> only due to taking the log, else median of zero is no biggie
sum(DB5$TVOL_tot_1w_pre == 0) # 1339 cases
sum(is.na(DB5$TVOL_tot_1w_pre)) # 0 cases

DB5$TVOL_tot_1w_pre %>% summary; DB5$TVOL_tot_1w_pre %>% hist(breaks = 50)
log(DB5$TVOL_tot_1w_pre) %>% summary; log(DB5$TVOL_tot_1w_pre) %>% hist(breaks = 50)

  # a. BTM NAs & apply log 
quantile(DB5$BTM, probs = seq(0, 1, .01), na.rm = T)

sum(is.na(DB5$BTM)) # 14264 cases
sum(DB5$BTM == 0, na.rm = T) # no zeroes though, in case we wanna log them

# why log? distri looks smoother:
DB5$BTM %>% summary; DB5$BTM %>% hist(breaks = 50)
log(DB5$BTM) %>% summary; log(DB5$BTM) %>% hist(breaks = 50)

# FILTER:
DB5 <- DB4 %>% 
  # select(PFRV_main, GARCH_1step, GJR_1step, 
  #        mtb_ratio, TVOL_med_1w_pre, leverage, size_ta,
  #        med_VIX_pre, GFS, repyear) %>% 
  # mutate_all(as.numeric) %>% 
  mutate_at(vars(PFRV_main,
                 PFRV_sq_ret,
                 PFRV_abs_ret,
                 GARCH_1step,
                 GJR_1step,
                 TVOL_med_1w_pre,
                 TVOL_tot_1w_pre), 
            funs(as.numeric)) %>% 
  mutate(log_pfrv = log(PFRV_main),
         log_garch = log(GARCH_1step),
         log_gjr = log(GJR_1step),
         log_GFS = log(GFS),
         BTM = 1/mtb_ratio) %>% 
  filter(PFRV_main > 0.0001) %>% # this is the *CRITICAL* filter to remove 150-200 killing "outliers" manually
  filter(GARCH_1step > 0) %>% 
  filter(GJR_1step > 0) %>% 
  filter(!is.na(BTM)) %>% # this is the filter that kills all observations before 1998
  filter(TVOL_med_1w_pre > 0) %>% 
  filter(leverage <= 1)

# INSPECT: 
DB5
DB5 %>% group_by(factor(repyear)) %>% summarise(n()) # that's the prob, all obs < 1998 are gone due to BTM inavailabiltiy

# inspect volas:
hist(DB5$PFRV_main, breaks = 50)
hist(DB5$GARCH_1step, breaks = 50)
hist(DB5$GJR_1step, breaks = 50)
hist(DB5$log_pfrv, breaks = 50)
hist(DB5$log_garch, breaks = 50)
hist(DB5$log_gjr, breaks = 50)

# alternative RV measures, quick look for robustness checks:
hist(DB5$PFRV_sq_ret, breaks = 50)
hist(log(DB5$PFRV_sq_ret), breaks = 50)
hist(DB5$PFRV_abs_ret, breaks = 50)
hist(log(DB5$PFRV_abs_ret), breaks = 50)

# looks very similar in the log-form

# scatter a la MZ 
plot(DB5$PFRV_main,
     DB5$GARCH_1step)

plot(DB5$PFRV_main,
     DB5$GJR_1step)

plot(DB5$log_pfrv,
     DB5$log_garch)

plot(DB5$log_pfrv,
     DB5$log_gjr)

# inspect the controls (histos & NAs)
is.na(DB5$TVOL_med_1w_pre) %>% sum
is.na(DB5$leverage) %>% sum
is.na(DB5$size_ta) %>% sum
is.na(DB5$med_VIX_pre) %>% sum
is.na(DB5$BTM) %>% sum

hist(DB5$GFS, breaks = 50)
hist(DB5$log_GFS, breaks = 50)
hist(DB5$size_ta, breaks = 50)
hist(DB5$TVOL_med_1w_pre, breaks = 50)
hist(log(DB5$TVOL_med_1w_pre), breaks = 50)
# hist(DB5$TVOL_tot_1w_pre, breaks = 50)
# hist(log(DB5$TVOL_tot_1w_pre), breaks = 50)
hist(DB5$med_VIX_pre, breaks = 50)
hist(log(DB5$med_VIX_pre), breaks = 50)
hist(DB5$leverage, breaks = 50)
hist(log(DB5$leverage), breaks = 50)
hist(DB5$BTM, breaks = 50)
hist(log(DB5$BTM), breaks = 50)

# for BTM and TVOL I will use logs inst. of levels, the distri looks better
DB5 <- DB5 %>% mutate(log_TVOL = log(TVOL_med_1w_pre),
                      log_BTM = log(BTM))

# Export DB5 as well as diers2 and survivors2 list:
load("~/Desktop/lm_testdata/DB3_309x67730.RData")

PFRV_diers <- anti_join(as_tibble(DB3$txtfilename), 
                        as_tibble(DB5$txtfilename))
PFRV_survivors <- inner_join(as_tibble(DB3$txtfilename), 
                             as_tibble(DB5$txtfilename))

write.table(as.matrix(PFRV_diers),
            file = "/Users/kevin/Desktop/lm_testdata/pfrv_diers2.txt",
            sep = "\n", quote = F, row.names = F, col.names = F)

write.table(as.matrix(PFRV_survivors),
            file = "/Users/kevin/Desktop/lm_testdata/pfrv_survivors2.txt",
            sep = "\n", quote = F, row.names = F, col.names = F)

write.table(DB5,
            file = "/Users/kevin/Desktop/lm_testdata/DB5_317x46483.txt",
            sep = ";",
            row.names = F)

rm(DB4, DB3)
save.image("/Users/kevin/Desktop/lm_testdata/DB5_316x46483.RData")

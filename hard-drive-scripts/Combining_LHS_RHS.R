
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: Combining_LHS_RHS                                                         #
#     Description:  1) merge full database of all RHS control vars with the price data    #
#                   2) load and all text-related RHS variables                            #
#                   3) merge the result of (1) with the loaded DB from (2)                #
#                                                                                         #
#     Date (last updated): June 28th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries / Settings ----------------------------------------------------

rm(list=ls()) # clear environment 
options(scipen = 999)

# setwd("/Volumes/LaCie/LM_data/")
setwd("/Users/kevin/Desktop/lm_testdata")

library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(DescTools)

# Outline -----------------------------------------------------------------

# there will be four steps:
    # 1: original DB excl. price data: 259 variables
    # 2: original DB incl. price data: 259 + (13*3 + 1) - 1 = 298 variables
    # 3: DB2 plus all text related RHS: 298 + ? = ? variables
    # 4: DB3 cleaned by all "non-used" data --> e.g. drop all NA rows; 
         # NB: cols remain, but less than 67,730 rows

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

# Investigate DB_pricedata ------------------------------------------------

# investigate price data a bit, there were some strange stuff in the loop with NAs, predicitions > 100% vola, etc.
# count NAs
na_count <- sapply(DB_pricedata, 
                   function(y) sum(length(which(is.na(y)))))

na_count <- data.frame(na_count); na_count

# inspect the variables a bit
a <- summary(DB_pricedata[, 2:6]); a

# inspect the main variable for LHS
quantile(DB_pricedata$PFRV_main, probs = seq(0, 1, 0.01), na.rm = T)
quantile(DB_pricedata$GARCH_1step, probs = seq(0, 1, 0.01), na.rm = T)
quantile(DB_pricedata$GJR_1step, probs = seq(0, 1, 0.01), na.rm = T)

# quick glance on TVOLs:
b <- summary(DB_pricedata[, 7:14]); b

# univariate relations between price data & Tvol
cormat <- cor(DB_pricedata[,2:14], use = "pairwise.complete.obs")
p_cormat <- cor_pmat(cormat)

ggcorrplot(cormat, 
           method = "square", 
           type = "lower", 
           ggtheme = theme_bw,
           show.diag = T,
           lab = T, 
           lab_size = 2,
           hc.order = T)

# crazy... all vola-related vars correlate strongly (> 75%) 
# but nothing correlates with TVOL

# let's see how often the GARCH forecast is above/below PFRV
delta_GARCH_PFRV <- DB_pricedata$GARCH_1step - DB_pricedata$PFRV_main
delta_GARCH_PFRV <- as.data.frame(delta_GARCH_PFRV)
delta_GARCH_PFRV$idx <- 1:nrow(delta_GARCH_PFRV)

# take random sample of 500 without NAs
delta_GARCH_PFRV <- as_tibble(delta_GARCH_PFRV)

randsamp_garchs <- delta_GARCH_PFRV %>% 
  na.omit(.) %>% 
  sample_n(., size = 500)

# plot deviation of GARCH from RV
ggplot(randsamp_garchs, 
       aes(x = idx, 
           y = delta_GARCH_PFRV)) + 
  geom_area()

# Note: this is still based on data that contains the outliers
# Next step: clean the price data

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

# Header related data
header.data <- read.delim(file = "/Users/kevin/Desktop/lm_testdata/gfs-nfs-tab-exh_header_data.txt",
                          sep = ";") %>% as_tibble()


# Length related data
length.related.data <- read.delim(file = "/Users/kevin/Desktop/lm_testdata/doc_length_variables.txt",
                                  sep = ";") %>% as_tibble()

# word counts 


# Create DB3: Merge DB2 with text-RHSs ------------------------------------

# will NOT cbind the word count but rather leave them in 9 separate DTMs
# that way I can adress those 9 dataframes and use them in regression / score compuatation





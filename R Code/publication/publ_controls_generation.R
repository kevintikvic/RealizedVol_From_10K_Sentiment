
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: publ_controls_generation                                                  #
#     Description:  1) load "original" DB with 46,483 filings and add new variables       #
#                   2) modify new control vars, if necessary                              #
#                   3) export VIBTW / MZ regression ready RHS / LHS files                 #
#                                                                                         #
#     Date (last updated): October 12th, 2019                                             #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries / Settings ----------------------------------------------------
rm(list=ls()); gc()
options(scipen = 999)
library(tidyverse)
setwd("/Users/kevin/Desktop/lm_testdata/")

# load data
load("./DB5_316x46483.RData")
rm(PFRV_diers, PFRV_survivors)

# inspect the dataset
# colnames(DB5)

# INSPECT SUBSEQUENCY OF REPORTS / LAGS ---------------------------------------------------------------------

DB6 <- DB5 %>%
  mutate(PnL_indicator = ifelse(ni < 0, 1, 0)) # 1 if loss was incurred

quick_inspect_subsequency <- DB6 %>% 
  select(CIK, txtfilename, repyear) %>% 
  arrange(CIK, repyear) %>% # this step is essential
  group_by(CIK) %>% 
  mutate(repyear_lagged = lag(repyear)) %>% 
  mutate(yrs_betw_2_cons_filings = repyear - repyear_lagged) %>% 
  ungroup()

# nice example, that covers the NA at the CIK jump (intentional!) and two filings within the same year
quick_inspect_subsequency[34:46, ]

# how many have no previous, how many from 1yr ago, from 2yr ago, etc.
sum(is.na(quick_inspect_subsequency))
quick_inspect_subsequency$yrs_betw_2_cons_filings %>% table()

quick_inspect_subsequency$yrs_betw_2_cons_filings %>% 
  table() %>% 
  sum(na.rm = T) + 
  sum(is.na(quick_inspect_subsequency$yrs_betw_2_cons_filings))
# all captured

# investigate the 0s and NAs:
quick_inspect_subsequency %>% filter(yrs_betw_2_cons_filings == 0) # these are good, as they have a filing even in the same year (e.g. in an amendment)
quick_inspect_subsequency[is.na(quick_inspect_subsequency$yrs_betw_2_cons_filings), ]

# for these, what we expect is that no CIK is equal to the one before it:
# i.e. we want to see 3736 unique CIKs
length(quick_inspect_subsequency$CIK)
length(unique(quick_inspect_subsequency$CIK))

# likely only the 0yr, 1yr and 2yr lag are usable, the loss of obs would be:
less_than_3y_lagdiff <- quick_inspect_subsequency %>% 
  filter(yrs_betw_2_cons_filings < 3)

less_than_3y_lagdiff$yrs_betw_2_cons_filings %>% table()

# DO FOR THE WHOLE DATA-TABLE
DB6 <- DB6 %>% 
  arrange(CIK, repyear) %>% # this step is essential
  group_by(CIK) %>% 
  mutate(repyear_lagged = lag(repyear)) %>% 
  mutate(yrs_betw_2_cons_filings = repyear - repyear_lagged) %>% 
  ungroup()

# CREATING NEW VARIABLES ---------------------------------------------------------------------

# Create "lagged" control variables
DB7 <- DB6 %>% 
  arrange(CIK, repyear) %>%
  group_by(CIK) %>% 
  filter(yrs_betw_2_cons_filings < 3) %>% # key condition to not have two filings that are too "distant"
  filter(sale > 0) %>% # keep only positive sales (kick also the zeros, as we divide by them in the margin figures and get NAs for division by zero)
  filter(ebitda != 0) %>% # keep only non-zero ebitda to avoid division by 0
  filter(ebit != 0) %>% # keep only non-zero ebit to avoid division by 0
  filter(ni != 0) %>% # keep only non-zero net income to avoid division by 0
  mutate(PnL_bool_lagged = lag(PnL_indicator),
         size_ta_lagged = lag(size_ta),
         leverage_lagged = lag(leverage),
         log_BTM_lagged = lag(log_BTM),
         sales_lagged = lag(sale),
         PnL_turnaround = PnL_indicator - PnL_bool_lagged,
         PnL_turnaround_sum = PnL_indicator + PnL_turnaround,
         csho_lagged = lag(csho), 
         ebit_margin = ebit / sale, 
         ebit_margin_lagged = lag(ebit_margin), 
         ebitda_margin = ebitda / sale, 
         ebitda_margin_lagged = lag(ebitda_margin), 
         profit_margin = ni / sale, 
         profit_margin_lagged = lag(profit_margin)) %>% 
  mutate(PnL_turnaround_pos_neg = ifelse(PnL_turnaround_sum < 1, 0, 1), # 0 = "good" (from neg to pos, or pos to pos), 1 = "bad" (from pos to neg, or neg to neg)
         size_change = size_ta - size_ta_lagged, # already in logs
         leverage_change = leverage / leverage_lagged - 1, # relative change
         BTM_change = log_BTM - log_BTM_lagged, # already in logs
         sales_growth = sale / sales_lagged - 1, # sales growth 
         shr_buyback = ifelse(csho < csho_lagged, 1, 0), # share buyback indicator
         chg_ebit_margin = ebit_margin / ebit_margin_lagged - 1, # change in ebit margin
         chg_ebitda_margin = ebitda_margin / ebitda_margin_lagged - 1, # change in ebitda margin
         chg_profit_margin = profit_margin / profit_margin_lagged - 1) %>% # change in profit margin
  ungroup()

# INSPECTING NEW VARIABLES ---------------------------------------------------------------------

# inspect the created variables a little bit further
table(DB7$PnL_turnaround)
table(DB7$PnL_turnaround_sum)
table(DB7$PnL_turnaround_pos_neg)

table(DB7$yrs_betw_2_cons_filings)

summary(DB7$size_ta) # no NAs (filtered out in thesis version already)
summary(DB7$leverage) # no NAs (filtered out in thesis version already)
summary(DB7$log_BTM) # no NAs (filtered out in thesis version already)

summary(DB7$ni) # only 15 NAs for the net income variable
summary(DB7$PnL_turnaround_pos_neg) # why 3332 NAs here ... ?
# check the lags, in the groupings every "first" element has a NA
summary(DB7$PnL_bool_lagged) # looks promising, there are 3332 here as well
summary(DB7$PnL_indicator)

summary(DB7$size_change) # same magnitude of NAs (3332)
summary(DB7$leverage_change) # same magnitude of NAs (3332)
summary(DB7$BTM_change) # same magnitude of NAs (3332)
summary(DB7$sales_growth) # same magnitude of NAs (3332)
summary(DB7$shr_buyback) # same magnitude of NAs (3332)

summary(DB7$chg_ebit_margin) # same magnitude of NAs (3332)
summary(DB7$chg_ebitda_margin) # same magnitude of NAs (3332)
summary(DB7$chg_profit_margin) # same magnitude of NAs (3332)

# inspect the NAs, drop if appropriate
DB7 %>% select(CIK, txtfilename,
               yrs_betw_2_cons_filings, ni, PnL_indicator, PnL_bool_lagged,
               PnL_turnaround_pos_neg) %>% 
  filter(is.na(PnL_turnaround_pos_neg)) %>% 
  head(10)
# see the alternating CIKs, indicating it is the first obs of the group

# FILTER OUT NAs
DB8 <- DB7 %>% 
  filter(!is.na(PnL_turnaround_pos_neg)) %>% 
  filter(!is.na(size_change)) %>% 
  filter(!is.na(leverage_change)) %>% 
  filter(!is.na(BTM_change)) %>% 
  filter(!is.na(sales_growth)) %>% 
  filter(!is.na(shr_buyback)) %>% 
  filter(!is.na(chg_ebit_margin)) %>% 
  filter(!is.na(chg_ebitda_margin)) %>% 
  filter(!is.na(chg_profit_margin))
  
## DB8 %>% head()

# MERGING WITH SENTI SCORES AND PRE-FIL-VOLA DATA ---------------------------------------------------------------------

# append pre-rv as additional RHS variable to the df using a join on the txtfilename
load("./prefilingvola_46483.RData")

DB9 <- inner_join(DB8, finallist_PFRV_volas, by = "txtfilename")

colnames(DB9)

DB9 <- DB9 %>% arrange(txtfilename)

rm(DB5, DB6, DB7, DB8, less_than_3y_lagdiff, 
   quick_inspect_subsequency, finallist_PFRV_volas)

# what is missing is the senti-scores
# have those in the following RData space
load("./regressions/A/A_MZ_inputs.RData")

# we have all the vola variables that are needed in DB9: 
  # PFRV_main = lhs variable, post filing rv
  # prefiling_RV_main_1_5 = rhs variable, pre filing rv
# thus can remove MZ_LHS
rm(MZ_LHS)

# inspect senti variables
MZ_RHS %>% colnames()

# these are 13679 rows, but I will need a little less due to filtering of DB9
# namely 11759
DB9 %>% filter(repyear > 2012) %>% nrow()

# can add the scores anyways using a join
# keep only the sentiment variables + FIN, the others are duplicates that I already have in DB9
sentivariables_outofsample <- MZ_RHS[, 160:ncol(MZ_RHS)]

# need corresponding txtfilenames first:
outofsample_filenames <- 
  read.table("./all_13679_initial_outofsample_txtfilenames.txt", 
             header = F, stringsAsFactors = F)

sentivariables_outofsample$txtfilename <- outofsample_filenames$V1

rm(outofsample_filenames)

sentivariables_outofsample$txtfilename %>% head

# now can join, based on txtfilename
DB10 <- inner_join(DB9, sentivariables_outofsample, by = "txtfilename")
# have the 11759 rows as expected

# ATTENTION: ----------
# ** KEY ** --> these are the sentim. variables for the WHOLE out of sample period
# PLUS: the scores are from the full (static) training window !!
# caution to not use those scores in extending + rolling regressions but load the yearly input .RData files for each of them

rm(list = setdiff(ls(), c("DB10")))

# the RHS vola variable is either:
# a. prefiling_RV_main_1_5 --> still needs to be logged (four cases of 0s are exluded to avoid log(0))
DB10$prefiling_RV_main_1_5 %>% summary()
(DB10$prefiling_RV_main_1_5 == 0) %>% sum
DB10$prefiling_RV_main_1_5 %>% log %>% is.infinite() %>% sum

# DB10$preRV <- ifelse(DB10$prefiling_RV_main_1_5 == 0, 
#                      NA, 
#                      log(DB10$prefiling_RV_main_1_5))
# 
# summary(DB10$preRV) 
# hist(DB10$preRV)
# the 4 NAs as expected, but the -40 outlier bothers me a bit... esp. in comparison with the other vola variables that end at -7
# reason: in thesis I replaced all preRV <= 0.0001 with the mean

DB10$preRV <- ifelse(DB10$prefiling_RV_main_1_5 <= 0.0001, 
                     NA, 
                     log(DB10$prefiling_RV_main_1_5))

DB10$preRV[is.na(DB10$preRV)] <- mean(DB10$preRV, na.rm = T)

summary(DB10$preRV) 
hist(DB10$preRV)

# just an indication: .58 for full sample in thesis --> .47 out of sample seems reasonable
# cor(DB10$preRV, DB10$log_pfrv, use = "complete.obs")

# b, Garch (already logged, rdy to use)
DB10$log_garch %>% head
DB10$log_garch %>% summary()
DB10$log_garch %>% is.infinite() %>% sum

# c. Gjr Garch (already logged, rdy to used)
DB10$log_gjr %>% head
DB10$log_gjr %>% summary()
DB10$log_gjr %>% is.infinite() %>% sum

# export DB10 space -- Note: DB10 is a pure "out of sample" table
save.image("./publication/DB10_outofsample_390x11759.RData")

# POOLED REGRESSIONS ---------------------------------------------------------------------

# LHS is the post filing rv, named log_pfrv
LHS <- DB10$log_pfrv

weighting_scheme_list <- c("_VIBTW", 
                           "_TFIDF", "_RFIDF",
                           "_WF_1P", "_WF_P1", 
                           "_WFIDF_1P", "_WFIDF_P1",
                           "_TFMAX")

for (scheme in weighting_scheme_list) {
  print(paste0("Creating pooled regression using the ", 
               scheme, 
               " weighting scheme."))
  
  text_related_rhs <- DB10 %>% select(ends_with(scheme))
  colnames(text_related_rhs) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")
  
  RHS <- cbind(TS_FC = DB10$preRV, 
               text_related_rhs,
               GFS = DB10$log_GFS,
               FIN = DB10$FIN,
               SIZE = DB10$size_ta,
               BTM = DB10$log_BTM,
               TRVOL = DB10$log_TVOL,
               VIX = DB10$med_VIX_pre,
               LEVER = DB10$leverage,
               PnL_TURN = DB10$PnL_turnaround_pos_neg,
               SIZE_CHG = DB10$size_change,
               BTM_CHG = DB10$BTM_change,
               LEVER_CHG = DB10$leverage_change,
               SALES_GROWTH = DB10$sales_growth,
               SHR_BUYBACK = DB10$shr_buyback,
               PROFITABILITY_CHG = DB10$chg_ebitda_margin, 
               # PROFITABILITY_CHG = DB10$chg_ebit_margin, 
               # PROFITABILITY_CHG = DB10$chg_profit_margin, 
               DB10[157:309]) %>% # all dummy controls
    as_tibble() %>% 
    mutate(POSxPnL = POS_SENT * PnL_TURN,
           POSxPROFITABILITY = POS_SENT * PROFITABILITY_CHG) # interaction variable: pos sentiment x .
  
  assign(paste0("public_pooled", scheme), 
         lm(as.matrix(LHS) ~ as.matrix(RHS)))
}

# TABLE EXPORT ------------------------------------------------------------
library(stargazer)

stargazer(public_pooled_VIBTW, public_pooled_TFIDF, public_pooled_RFIDF,
          public_pooled_WFIDF_1P, public_pooled_WFIDF_P1,
          public_pooled_WF_1P, public_pooled_WF_P1,
          public_pooled_TFMAX,
          out = "/Users/kevin/Dropbox/Master_Thesis/PUBLICATION/PUBLICATION_VERSION/Results_Tables/publication_pooled",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: publication_pooled", 
          title = "PUBLICATION: (Pooled OLS)",
          model.numbers = T,
          notes.align = "l", 
          notes = "***, **, and * denotes statistical significance at the one-, five- and ten-percent level, respectively. Standard errors are displayed in parentheses. TS_FC is the 1-week pre-filing realized volatility. Coefficients for boolean control variables (YRDUMMY, MTHDUMMY, WEEKDAYDUMMY, MONTHDAYDUMMY, SECTORDUMMY, and 10KDUMMY) are not displayed in the table. Column headers (1) through (8) refer to the weighting schemes in the following order: VIBTW, TFIDF, RFIDF, WFIDF 1PLOG, WFIDF LOG1P, WF 1PLOG, WF LOG1P, TFMAX. Each column is a pooled OLS regression, estimated for the whole out-of-sample period from 2013 to 2017. Weights for the VIBTW, TFIDF, RFIDF, WFIDF 1PLOG, and WFIDF LOG1P are estimated for a fixed training set (1999-2012).",
          notes.append = F,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

rm(list = setdiff(ls(), c("DB10")))

# ANNUAL REGRESSIONS + ONLY VIBTW ------------------------------

# STATIC ------------------------------------------------------------------

# loop through the respective target year:
for (current_yr in 2013:2017) {
  DB10_temp_helper_filtered <- DB10 %>% 
    filter(repyear == current_yr)
  
  print(paste0("Creating annual regression for year ", 
               current_yr, 
               ". Expect ", 
               nrow(DB10_temp_helper_filtered), 
               " number of observations for this year."))
  
  # filter out LHS
  LHS <- DB10_temp_helper_filtered %>% 
    select(log_pfrv)
  
  # filter out text-related rhs
  text_related_rhs <- DB10_temp_helper_filtered  %>% 
    select(ends_with("_VIBTW"))
  
  colnames(text_related_rhs) <- 
    c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")
  
  # filter out other RHS and add text-related rhs
  RHS <- cbind(TS_FC = DB10_temp_helper_filtered$preRV, 
               text_related_rhs,
               GFS = DB10_temp_helper_filtered$log_GFS,
               FIN = DB10_temp_helper_filtered$FIN,
               SIZE = DB10_temp_helper_filtered$size_ta,
               BTM = DB10_temp_helper_filtered$log_BTM,
               TRVOL = DB10_temp_helper_filtered$log_TVOL,
               VIX = DB10_temp_helper_filtered$med_VIX_pre,
               LEVER = DB10_temp_helper_filtered$leverage,
               PnL_TURN = DB10_temp_helper_filtered$PnL_turnaround_pos_neg,
               SIZE_CHG = DB10_temp_helper_filtered$size_change,
               BTM_CHG = DB10_temp_helper_filtered$BTM_change,
               LEVER_CHG = DB10_temp_helper_filtered$leverage_change,
               SALES_GROWTH = DB10_temp_helper_filtered$sales_growth,
               SHR_BUYBACK = DB10_temp_helper_filtered$shr_buyback,
               PROFITABILITY_CHG = DB10_temp_helper_filtered$chg_ebitda_margin, 
               # PROFITABILITY_CHG = DB10_temp_helper_filtered$chg_ebit_margin, 
               # PROFITABILITY_CHG = DB10_temp_helper_filtered$chg_profit_margin, 
               DB10_temp_helper_filtered[157:309]) %>% # all dummy controls
    as_tibble() %>% 
    mutate(POSxPnL = POS_SENT * PnL_TURN,
           POSxPROFITABILITY = POS_SENT * PROFITABILITY_CHG) # interaction variable: pos sentiment x .

  
  # regress and save lm() output
  assign(paste0("public_annual_vibtw_static_", current_yr), 
         lm(as.matrix(LHS) ~ as.matrix(RHS)))

}

# TABLE EXPORT ------------------------------------------------------------
stargazer(public_annual_vibtw_static_2013, 
          public_annual_vibtw_static_2014,
          public_annual_vibtw_static_2015, 
          public_annual_vibtw_static_2016,
          public_annual_vibtw_static_2017,
          out = "/Users/kevin/Dropbox/Master_Thesis/PUBLICATION/PUBLICATION_VERSION/Results_Tables/publication_annual_regressions_static",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: publication_annual_regressions_static", 
          title = "PUBLICATION: (Annual OLS + Static)",          
          model.numbers = T,
          notes.align = "l", 
          notes = "***, **, and * denotes statistical significance at the one-, five- and ten-percent level, respectively. Standard errors are displayed in parentheses. ", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

rm(list = setdiff(ls(), c("DB10")))

# EXTENDING ------------------------------------------------------------------
# recall:
# ** KEY ** --> in DB10 are the sentim. variables for the WHOLE out of sample period
# PLUS: the scores are from the full (static) training window !!
# caution to not use those scores in extending + rolling regressions but load the yearly input .RData files for each of them

# thus modify the loop from above to load the MZ_RHS new for every year, then inner-join them, then filter as before

for (current_yr in 2013:2017) {
  DB10_temp_helper_filtered <- DB10 %>% 
    filter(repyear == current_yr) %>% 
    select(-starts_with("NEG_SENT_")) %>% 
    select(-starts_with("POS_SENT_")) %>% 
    select(-starts_with("ASSERT_")) %>% 
    select(-starts_with("UNCERT_")) %>% 
    select(-starts_with("LITI_"))
    
  print(paste0("Creating annual regression for year ", 
               current_yr, 
               ". Expect ", 
               nrow(DB10_temp_helper_filtered), 
               " number of observations for this year."))
  
  # filter out LHS
  LHS <- DB10_temp_helper_filtered %>% 
    select(log_pfrv)
  
  # filter out text-related rhs
  load(paste0("./regressions/B/B2_MZ_inputs_", current_yr, ".RData"))
  # keep only text vars (last 5 vars)
  sentivariables_outofsample <- MZ_RHS[, c((ncol(MZ_RHS)-4):ncol(MZ_RHS))]
  # add txtfilename to allow for join
  sentivariables_outofsample$txtfilename <- DB5 %>% 
    filter(repyear == current_yr) %>% 
    pull(txtfilename)
  
  # join together to drop the redundant senti variables in the new (publication) setting
  DB10_temp_helper_filtered <- inner_join(DB10_temp_helper_filtered, 
                                          sentivariables_outofsample, 
                                          by = "txtfilename")

  rm(MZ_LHS, MZ_RHS, DB5, sentivariables_outofsample)
  
  # choose last 5 columns as text related rhs variables
  text_related_rhs <- DB10_temp_helper_filtered[tail(seq_along(DB10_temp_helper_filtered), 5)]

  # filter out other RHS and add text-related rhs
  RHS <- cbind(TS_FC = DB10_temp_helper_filtered$preRV, 
               text_related_rhs,
               GFS = DB10_temp_helper_filtered$log_GFS,
               FIN = DB10_temp_helper_filtered$FIN,
               SIZE = DB10_temp_helper_filtered$size_ta,
               BTM = DB10_temp_helper_filtered$log_BTM,
               TRVOL = DB10_temp_helper_filtered$log_TVOL,
               VIX = DB10_temp_helper_filtered$med_VIX_pre,
               LEVER = DB10_temp_helper_filtered$leverage,
               PnL_TURN = DB10_temp_helper_filtered$PnL_turnaround_pos_neg,
               SIZE_CHG = DB10_temp_helper_filtered$size_change,
               BTM_CHG = DB10_temp_helper_filtered$BTM_change,
               LEVER_CHG = DB10_temp_helper_filtered$leverage_change,
               SALES_GROWTH = DB10_temp_helper_filtered$sales_growth,
               SHR_BUYBACK = DB10_temp_helper_filtered$shr_buyback,
               PROFITABILITY_CHG = DB10_temp_helper_filtered$chg_ebitda_margin, 
               # PROFITABILITY_CHG = DB10_temp_helper_filtered$chg_ebit_margin, 
               # PROFITABILITY_CHG = DB10_temp_helper_filtered$chg_profit_margin, 
               DB10_temp_helper_filtered[157:309]) %>% # all dummy controls
    as_tibble() %>% 
    mutate(POSxPnL = POS_SENT * PnL_TURN,
           POSxPROFITABILITY = POS_SENT * PROFITABILITY_CHG) # interaction variable: pos sentiment x .
  
  
  # regress and save lm() output
  assign(paste0("public_annual_vibtw_extending_", current_yr), 
         lm(as.matrix(LHS) ~ as.matrix(RHS)))
  
}

# TABLE EXPORT ------------------------------------------------------------
stargazer(public_annual_vibtw_extending_2013, 
          public_annual_vibtw_extending_2014,
          public_annual_vibtw_extending_2015, 
          public_annual_vibtw_extending_2016,
          public_annual_vibtw_extending_2017,
          out = "/Users/kevin/Dropbox/Master_Thesis/PUBLICATION/PUBLICATION_VERSION/Results_Tables/publication_annual_regressions_extending",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: publication_annual_regressions_extending", 
          title = "PUBLICATION: (Annual OLS + Extending)",          
          model.numbers = T,
          notes.align = "l", 
          notes = "***, **, and * denotes statistical significance at the one-, five- and ten-percent level, respectively. Standard errors are displayed in parentheses. ", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

rm(list = setdiff(ls(), c("DB10")))

# ROLLING ------------------------------------------------------------------
# this one is simple, it is equivalent to extending except for the load .RData call

for (current_yr in 2013:2017) {
  DB10_temp_helper_filtered <- DB10 %>% 
    filter(repyear == current_yr) %>% 
    select(-starts_with("NEG_SENT_")) %>% 
    select(-starts_with("POS_SENT_")) %>% 
    select(-starts_with("ASSERT_")) %>% 
    select(-starts_with("UNCERT_")) %>% 
    select(-starts_with("LITI_"))
  
  print(paste0("Creating annual regression for year ", 
               current_yr, 
               ". Expect ", 
               nrow(DB10_temp_helper_filtered), 
               " number of observations for this year."))
  
  # filter out LHS
  LHS <- DB10_temp_helper_filtered %>% 
    select(log_pfrv)
  
  # filter out text-related rhs
  load(paste0("./regressions/B/B3_MZ_inputs_", current_yr, ".RData"))
  # keep only text vars (last 5 vars)
  sentivariables_outofsample <- MZ_RHS[, c((ncol(MZ_RHS)-4):ncol(MZ_RHS))]
  # add txtfilename to allow for join
  sentivariables_outofsample$txtfilename <- DB5 %>% 
    filter(repyear == current_yr) %>% 
    pull(txtfilename)
  
  # join together to drop the redundant senti variables in the new (publication) setting
  DB10_temp_helper_filtered <- inner_join(DB10_temp_helper_filtered, 
                                          sentivariables_outofsample, 
                                          by = "txtfilename")
  
  rm(MZ_LHS, MZ_RHS, DB5, sentivariables_outofsample)
  
  # choose last 5 columns as text related rhs variables
  text_related_rhs <- DB10_temp_helper_filtered[tail(seq_along(DB10_temp_helper_filtered), 5)]
  
  # filter out other RHS and add text-related rhs
  RHS <- cbind(TS_FC = DB10_temp_helper_filtered$preRV, 
               text_related_rhs,
               GFS = DB10_temp_helper_filtered$log_GFS,
               FIN = DB10_temp_helper_filtered$FIN,
               SIZE = DB10_temp_helper_filtered$size_ta,
               BTM = DB10_temp_helper_filtered$log_BTM,
               TRVOL = DB10_temp_helper_filtered$log_TVOL,
               VIX = DB10_temp_helper_filtered$med_VIX_pre,
               LEVER = DB10_temp_helper_filtered$leverage,
               PnL_TURN = DB10_temp_helper_filtered$PnL_turnaround_pos_neg,
               SIZE_CHG = DB10_temp_helper_filtered$size_change,
               BTM_CHG = DB10_temp_helper_filtered$BTM_change,
               LEVER_CHG = DB10_temp_helper_filtered$leverage_change,
               SALES_GROWTH = DB10_temp_helper_filtered$sales_growth,
               SHR_BUYBACK = DB10_temp_helper_filtered$shr_buyback,
               PROFITABILITY_CHG = DB10_temp_helper_filtered$chg_ebitda_margin, 
               # PROFITABILITY_CHG = DB10_temp_helper_filtered$chg_ebit_margin, 
               # PROFITABILITY_CHG = DB10_temp_helper_filtered$chg_profit_margin, 
               DB10_temp_helper_filtered[157:309]) %>% # all dummy controls
    as_tibble() %>% 
    mutate(POSxPnL = POS_SENT * PnL_TURN,
           POSxPROFITABILITY = POS_SENT * PROFITABILITY_CHG) # interaction variable: pos sentiment x .
  
  # regress and save lm() output
  assign(paste0("public_annual_vibtw_rolling_", current_yr), 
         lm(as.matrix(LHS) ~ as.matrix(RHS)))
  
}

# TABLE EXPORT ------------------------------------------------------------
stargazer(public_annual_vibtw_rolling_2013, 
          public_annual_vibtw_rolling_2014,
          public_annual_vibtw_rolling_2015, 
          public_annual_vibtw_rolling_2016,
          public_annual_vibtw_rolling_2017,
          out = "/Users/kevin/Dropbox/Master_Thesis/PUBLICATION/PUBLICATION_VERSION/Results_Tables/publication_annual_regressions_rolling",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: publication_annual_regressions_rolling", 
          title = "PUBLICATION: (Annual OLS + Rolling)",          
          model.numbers = T,
          notes.align = "l", 
          notes = "***, **, and * denotes statistical significance at the one-, five- and ten-percent level, respectively. Standard errors are displayed in parentheses. ", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

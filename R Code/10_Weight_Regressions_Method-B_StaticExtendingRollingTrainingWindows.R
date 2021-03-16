
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: weight_regressions_B                                                     #
#     Description:  1) load pure-count DTMs XXXX-YYYY (RHS) & PFRV XXXX-YYYY (LHS)        #
#                   2) regress LHS on RHS and obtain weights                              #
#                   3) normalize weights (JW) -> will serve as input in MZ-regr. for ZZZZ #
#                                                                                         #
#     Date (last updated): July 23th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries / Settings ----------------------------------------------------
rm(list=ls()) # clear environment 
gc()
options(scipen = 999)
# setwd("/Volumes/LaCie/LM_data/")
# setwd("/Users/kevin/Desktop/lm_testdata")
setwd("/Users/kevin/Desktop/lm_testdata/regressions/B")

library(tidyverse)
library(Matrix)
library(sjmisc)

load("~/Desktop/lm_testdata/DB5_316x46483.RData")

# load the last "N" variable: FIN
DTM_KT <- read.delim("/Users/kevin/Desktop/lm_testdata/sep_DTMs/DTM_KT.txt",
                     sep = ";", stringsAsFactors = F, dec = ",")
DTM_KT <- DTM_KT[!rownames(DTM_KT) %in% unlist(list(PFRV_diers)), ]
sum(rownames(DTM_KT) %in% unlist(list(PFRV_survivors)))
sum(colSums(DTM_KT) == 0)
DTM_KT <- DTM_KT[, which(colSums(DTM_KT) != 0)]

FIN.var <- rowSums(DTM_KT) %>% log1p

DB5 <- DB5 %>% mutate(FIN = FIN.var)

rm(FIN.var, DTM_KT, PFRV_diers, PFRV_survivors)

# Variant B1 ---------------------------------------------------------------
# estimate w_j's and idf's for period 1999-2012
# apply weights to 2013-2017 filings
# regress OOS filings (2013-2017) in a five separate annual regressions

# no need to adjust weights anyhow, as training window in this case is static
# can use the weights from variant A

# Variant B2 --------------------------------------------------------------
# EXTENDING WINDOW instead of static one 
# need five estimation windows for each year from 2013 to 2017
# no big work, just a lot of code ... 

# 2013 --------------------------------------------------------------------

N1 <- 1999:2012
N2 <- 2013
# extract LHS variable (PFRV)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(log_pfrv)
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(log_pfrv)

# extract RHS 
MZ_RHS <- DB5 %>% filter(repyear %in% N2) %>% 
  select(log_garch, log_gjr, log_BTM,
         log_GFS, # doc length = readability 
         med_VIX_pre, # median VIX 1W before filing
         log_TVOL, # median TVOL 1W before filing
         leverage, # leverage ratio
         repday_27:repday_7, # filing month day dummies
         weekday_Mi:weekday_Sa, # filing week day dummies
         repmth_4:repmth_10, # filing month dummies
         repyear_2005:repyear_1996, # filing year dummies
         sic2D_87:sic2D_76, # SIC Code dummies
         reptype_10.K:reptype_10.KT.A, # 10-K type dummies 
         FIN, # financial topics variable
         size_ta) %>% # size
  mutate_if(is.character, as.numeric)

# extract the N1-counts
N1.size <- nrow(WR_LHS)
N2.size <- nrow(MZ_LHS)

# Define indices for DTMs 
idx.N1 <- 1:N1.size
idx.N2 <- (N1.size + 1):(N1.size + N2.size)

# N -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_N_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
NEG_SENT_VIBTW <- as.matrix(DTM_N_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(NEG_SENT = NEG_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# P -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_P_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
POS_SENT_VIBTW <- as.matrix(DTM_P_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT = POS_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# ASSERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_SM_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
ASSERT_VIBTW <- as.matrix(DTM_SM_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(ASSERT = ASSERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# UNCERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_U_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
UNCERT_VIBTW <- as.matrix(DTM_U_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(UNCERT = UNCERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# LITI --------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_L_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
LITI_VIBTW <- as.matrix(DTM_L_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(LITI = LITI_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# EXPORT FINAL RHS AND LHS MAT
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS")))
save.image("~/Desktop/lm_testdata/regressions/B/B2_MZ_inputs_2013.RData")

# 2014 --------------------------------------------------------------------

N1 <- 1999:2013
N2 <- 2014
# extract LHS variable (PFRV)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(log_pfrv)
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(log_pfrv)

# extract RHS 
MZ_RHS <- DB5 %>% filter(repyear %in% N2) %>% 
  select(log_garch, log_gjr, log_BTM,
         log_GFS, # doc length = readability 
         med_VIX_pre, # median VIX 1W before filing
         log_TVOL, # median TVOL 1W before filing
         leverage, # leverage ratio
         repday_27:repday_7, # filing month day dummies
         weekday_Mi:weekday_Sa, # filing week day dummies
         repmth_4:repmth_10, # filing month dummies
         repyear_2005:repyear_1996, # filing year dummies
         sic2D_87:sic2D_76, # SIC Code dummies
         reptype_10.K:reptype_10.KT.A, # 10-K type dummies 
         FIN, # financial topics variable
         size_ta) %>% # size
  mutate_if(is.character, as.numeric)

# extract the N1-counts
N1.size <- nrow(WR_LHS)
N2.size <- nrow(MZ_LHS)

# Define indices for DTMs 
idx.N1 <- 1:N1.size
idx.N2 <- (N1.size + 1):(N1.size + N2.size)

# N -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_N_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
NEG_SENT_VIBTW <- as.matrix(DTM_N_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(NEG_SENT = NEG_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# P -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_P_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
POS_SENT_VIBTW <- as.matrix(DTM_P_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT = POS_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# ASSERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_SM_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
ASSERT_VIBTW <- as.matrix(DTM_SM_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(ASSERT = ASSERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# UNCERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_U_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
UNCERT_VIBTW <- as.matrix(DTM_U_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(UNCERT = UNCERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# LITI --------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_L_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
LITI_VIBTW <- as.matrix(DTM_L_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(LITI = LITI_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# EXPORT FINAL RHS AND LHS MAT
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS")))
save.image("~/Desktop/lm_testdata/regressions/B/B2_MZ_inputs_2014.RData")

# 2015 --------------------------------------------------------------------

N1 <- 1999:2014
N2 <- 2015
# extract LHS variable (PFRV)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(log_pfrv)
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(log_pfrv)

# extract RHS 
MZ_RHS <- DB5 %>% filter(repyear %in% N2) %>% 
  select(log_garch, log_gjr, log_BTM,
         log_GFS, # doc length = readability 
         med_VIX_pre, # median VIX 1W before filing
         log_TVOL, # median TVOL 1W before filing
         leverage, # leverage ratio
         repday_27:repday_7, # filing month day dummies
         weekday_Mi:weekday_Sa, # filing week day dummies
         repmth_4:repmth_10, # filing month dummies
         repyear_2005:repyear_1996, # filing year dummies
         sic2D_87:sic2D_76, # SIC Code dummies
         reptype_10.K:reptype_10.KT.A, # 10-K type dummies 
         FIN, # financial topics variable
         size_ta) %>% # size
  mutate_if(is.character, as.numeric)

# extract the N1-counts
N1.size <- nrow(WR_LHS)
N2.size <- nrow(MZ_LHS)

# Define indices for DTMs 
idx.N1 <- 1:N1.size
idx.N2 <- (N1.size + 1):(N1.size + N2.size)

# N -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_N_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
NEG_SENT_VIBTW <- as.matrix(DTM_N_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(NEG_SENT = NEG_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# P -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_P_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
POS_SENT_VIBTW <- as.matrix(DTM_P_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT = POS_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# ASSERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_SM_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
ASSERT_VIBTW <- as.matrix(DTM_SM_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(ASSERT = ASSERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# UNCERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_U_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
UNCERT_VIBTW <- as.matrix(DTM_U_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(UNCERT = UNCERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# LITI --------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_L_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
LITI_VIBTW <- as.matrix(DTM_L_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(LITI = LITI_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# EXPORT FINAL RHS AND LHS MAT
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS")))
save.image("~/Desktop/lm_testdata/regressions/B/B2_MZ_inputs_2015.RData")

# 2016 --------------------------------------------------------------------

N1 <- 1999:2015
N2 <- 2016
# extract LHS variable (PFRV)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(log_pfrv)
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(log_pfrv)

# extract RHS 
MZ_RHS <- DB5 %>% filter(repyear %in% N2) %>% 
  select(log_garch, log_gjr, log_BTM,
         log_GFS, # doc length = readability 
         med_VIX_pre, # median VIX 1W before filing
         log_TVOL, # median TVOL 1W before filing
         leverage, # leverage ratio
         repday_27:repday_7, # filing month day dummies
         weekday_Mi:weekday_Sa, # filing week day dummies
         repmth_4:repmth_10, # filing month dummies
         repyear_2005:repyear_1996, # filing year dummies
         sic2D_87:sic2D_76, # SIC Code dummies
         reptype_10.K:reptype_10.KT.A, # 10-K type dummies 
         FIN, # financial topics variable
         size_ta) %>% # size
  mutate_if(is.character, as.numeric)

# extract the N1-counts
N1.size <- nrow(WR_LHS)
N2.size <- nrow(MZ_LHS)

# Define indices for DTMs 
idx.N1 <- 1:N1.size
idx.N2 <- (N1.size + 1):(N1.size + N2.size)

# N -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_N_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
NEG_SENT_VIBTW <- as.matrix(DTM_N_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(NEG_SENT = NEG_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# P -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_P_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
POS_SENT_VIBTW <- as.matrix(DTM_P_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT = POS_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# ASSERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_SM_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
ASSERT_VIBTW <- as.matrix(DTM_SM_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(ASSERT = ASSERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# UNCERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_U_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
UNCERT_VIBTW <- as.matrix(DTM_U_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(UNCERT = UNCERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# LITI --------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_L_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
LITI_VIBTW <- as.matrix(DTM_L_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(LITI = LITI_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# EXPORT FINAL RHS AND LHS MAT
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS")))
save.image("~/Desktop/lm_testdata/regressions/B/B2_MZ_inputs_2016.RData")

# 2017 --------------------------------------------------------------------

N1 <- 1999:2016
N2 <- 2017
# extract LHS variable (PFRV)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(log_pfrv)
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(log_pfrv)

# extract RHS 
MZ_RHS <- DB5 %>% filter(repyear %in% N2) %>% 
  select(log_garch, log_gjr, log_BTM,
         log_GFS, # doc length = readability 
         med_VIX_pre, # median VIX 1W before filing
         log_TVOL, # median TVOL 1W before filing
         leverage, # leverage ratio
         repday_27:repday_7, # filing month day dummies
         weekday_Mi:weekday_Sa, # filing week day dummies
         repmth_4:repmth_10, # filing month dummies
         repyear_2005:repyear_1996, # filing year dummies
         sic2D_87:sic2D_76, # SIC Code dummies
         reptype_10.K:reptype_10.KT.A, # 10-K type dummies 
         FIN, # financial topics variable
         size_ta) %>% # size
  mutate_if(is.character, as.numeric)

# extract the N1-counts
N1.size <- nrow(WR_LHS)
N2.size <- nrow(MZ_LHS)

# Define indices for DTMs 
idx.N1 <- 1:N1.size
idx.N2 <- (N1.size + 1):(N1.size + N2.size)

# N -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_N_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
NEG_SENT_VIBTW <- as.matrix(DTM_N_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(NEG_SENT = NEG_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# P -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_P_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
POS_SENT_VIBTW <- as.matrix(DTM_P_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT = POS_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# ASSERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_SM_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
ASSERT_VIBTW <- as.matrix(DTM_SM_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(ASSERT = ASSERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# UNCERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_U_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
UNCERT_VIBTW <- as.matrix(DTM_U_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(UNCERT = UNCERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# LITI --------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_L_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
LITI_VIBTW <- as.matrix(DTM_L_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(LITI = LITI_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# EXPORT FINAL RHS AND LHS MAT
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS")))
save.image("~/Desktop/lm_testdata/regressions/B/B2_MZ_inputs_2017.RData")

# Variant B3 --------------------------------------------------------------
# ROLLING WINDOW instead of static/extending one 
# need five estimation windows for each year from 2013 to 2017
# no big work, just a lot of code ... 

# 2013 --------------------------------------------------------------------

N1 <- 1999:2012
N2 <- 2013
# extract LHS variable (PFRV)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(log_pfrv)
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(log_pfrv)

# extract RHS 
MZ_RHS <- DB5 %>% filter(repyear %in% N2) %>% 
  select(log_garch, log_gjr, log_BTM,
         log_GFS, # doc length = readability 
         med_VIX_pre, # median VIX 1W before filing
         log_TVOL, # median TVOL 1W before filing
         leverage, # leverage ratio
         repday_27:repday_7, # filing month day dummies
         weekday_Mi:weekday_Sa, # filing week day dummies
         repmth_4:repmth_10, # filing month dummies
         repyear_2005:repyear_1996, # filing year dummies
         sic2D_87:sic2D_76, # SIC Code dummies
         reptype_10.K:reptype_10.KT.A, # 10-K type dummies 
         FIN, # financial topics variable
         size_ta) %>% # size
  mutate_if(is.character, as.numeric)

# extract the N1-counts
N1.size <- nrow(WR_LHS)
N2.size <- nrow(MZ_LHS)

# Define indices for DTMs 
idx.N1 <- 1:N1.size
idx.N2 <- (N1.size + 1):(N1.size + N2.size)

# N -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_N_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
NEG_SENT_VIBTW <- as.matrix(DTM_N_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(NEG_SENT = NEG_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# P -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_P_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
POS_SENT_VIBTW <- as.matrix(DTM_P_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT = POS_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# ASSERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_SM_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
ASSERT_VIBTW <- as.matrix(DTM_SM_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(ASSERT = ASSERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# UNCERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_U_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
UNCERT_VIBTW <- as.matrix(DTM_U_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(UNCERT = UNCERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# LITI --------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_L_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
LITI_VIBTW <- as.matrix(DTM_L_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(LITI = LITI_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# EXPORT FINAL RHS AND LHS MAT
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS")))
save.image("~/Desktop/lm_testdata/regressions/B/B3_MZ_inputs_2013.RData")

# 2014 --------------------------------------------------------------------

N1 <- 2000:2013
N2 <- 2014
# extract LHS variable (PFRV)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(log_pfrv)
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(log_pfrv)

# extract RHS 
MZ_RHS <- DB5 %>% filter(repyear %in% N2) %>% 
  select(log_garch, log_gjr, log_BTM,
         log_GFS, # doc length = readability 
         med_VIX_pre, # median VIX 1W before filing
         log_TVOL, # median TVOL 1W before filing
         leverage, # leverage ratio
         repday_27:repday_7, # filing month day dummies
         weekday_Mi:weekday_Sa, # filing week day dummies
         repmth_4:repmth_10, # filing month dummies
         repyear_2005:repyear_1996, # filing year dummies
         sic2D_87:sic2D_76, # SIC Code dummies
         reptype_10.K:reptype_10.KT.A, # 10-K type dummies 
         FIN, # financial topics variable
         size_ta) %>% # size
  mutate_if(is.character, as.numeric)

# extract the N1-counts
N1.size <- nrow(WR_LHS)
N2.size <- nrow(MZ_LHS)

# Define indices for DTMs 
idx.N1 <- 1:N1.size
idx.N2 <- (N1.size + 1):(N1.size + N2.size)

# N -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_N_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
NEG_SENT_VIBTW <- as.matrix(DTM_N_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(NEG_SENT = NEG_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# P -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_P_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
POS_SENT_VIBTW <- as.matrix(DTM_P_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT = POS_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# ASSERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_SM_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
ASSERT_VIBTW <- as.matrix(DTM_SM_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(ASSERT = ASSERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# UNCERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_U_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
UNCERT_VIBTW <- as.matrix(DTM_U_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(UNCERT = UNCERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# LITI --------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_L_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
LITI_VIBTW <- as.matrix(DTM_L_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(LITI = LITI_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# EXPORT FINAL RHS AND LHS MAT
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS")))
save.image("~/Desktop/lm_testdata/regressions/B/B3_MZ_inputs_2014.RData")

# 2015 --------------------------------------------------------------------

N1 <- 2001:2014
N2 <- 2015
# extract LHS variable (PFRV)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(log_pfrv)
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(log_pfrv)

# extract RHS 
MZ_RHS <- DB5 %>% filter(repyear %in% N2) %>% 
  select(log_garch, log_gjr, log_BTM,
         log_GFS, # doc length = readability 
         med_VIX_pre, # median VIX 1W before filing
         log_TVOL, # median TVOL 1W before filing
         leverage, # leverage ratio
         repday_27:repday_7, # filing month day dummies
         weekday_Mi:weekday_Sa, # filing week day dummies
         repmth_4:repmth_10, # filing month dummies
         repyear_2005:repyear_1996, # filing year dummies
         sic2D_87:sic2D_76, # SIC Code dummies
         reptype_10.K:reptype_10.KT.A, # 10-K type dummies 
         FIN, # financial topics variable
         size_ta) %>% # size
  mutate_if(is.character, as.numeric)

# extract the N1-counts
N1.size <- nrow(WR_LHS)
N2.size <- nrow(MZ_LHS)

# Define indices for DTMs 
idx.N1 <- 1:N1.size
idx.N2 <- (N1.size + 1):(N1.size + N2.size)

# N -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_N_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
NEG_SENT_VIBTW <- as.matrix(DTM_N_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(NEG_SENT = NEG_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# P -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_P_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
POS_SENT_VIBTW <- as.matrix(DTM_P_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT = POS_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# ASSERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_SM_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
ASSERT_VIBTW <- as.matrix(DTM_SM_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(ASSERT = ASSERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# UNCERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_U_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
UNCERT_VIBTW <- as.matrix(DTM_U_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(UNCERT = UNCERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# LITI --------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_L_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
LITI_VIBTW <- as.matrix(DTM_L_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(LITI = LITI_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# EXPORT FINAL RHS AND LHS MAT
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS")))
save.image("~/Desktop/lm_testdata/regressions/B/B3_MZ_inputs_2015.RData")

# 2016 --------------------------------------------------------------------

N1 <- 2002:2015
N2 <- 2016
# extract LHS variable (PFRV)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(log_pfrv)
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(log_pfrv)

# extract RHS 
MZ_RHS <- DB5 %>% filter(repyear %in% N2) %>% 
  select(log_garch, log_gjr, log_BTM,
         log_GFS, # doc length = readability 
         med_VIX_pre, # median VIX 1W before filing
         log_TVOL, # median TVOL 1W before filing
         leverage, # leverage ratio
         repday_27:repday_7, # filing month day dummies
         weekday_Mi:weekday_Sa, # filing week day dummies
         repmth_4:repmth_10, # filing month dummies
         repyear_2005:repyear_1996, # filing year dummies
         sic2D_87:sic2D_76, # SIC Code dummies
         reptype_10.K:reptype_10.KT.A, # 10-K type dummies 
         FIN, # financial topics variable
         size_ta) %>% # size
  mutate_if(is.character, as.numeric)

# extract the N1-counts
N1.size <- nrow(WR_LHS)
N2.size <- nrow(MZ_LHS)

# Define indices for DTMs 
idx.N1 <- 1:N1.size
idx.N2 <- (N1.size + 1):(N1.size + N2.size)

# N -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_N_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
NEG_SENT_VIBTW <- as.matrix(DTM_N_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(NEG_SENT = NEG_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# P -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_P_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
POS_SENT_VIBTW <- as.matrix(DTM_P_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT = POS_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# ASSERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_SM_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
ASSERT_VIBTW <- as.matrix(DTM_SM_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(ASSERT = ASSERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# UNCERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_U_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
UNCERT_VIBTW <- as.matrix(DTM_U_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(UNCERT = UNCERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# LITI --------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_L_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
LITI_VIBTW <- as.matrix(DTM_L_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(LITI = LITI_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# EXPORT FINAL RHS AND LHS MAT
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS")))
save.image("~/Desktop/lm_testdata/regressions/B/B3_MZ_inputs_2016.RData")

# 2017 --------------------------------------------------------------------

N1 <- 2003:2016
N2 <- 2017
# extract LHS variable (PFRV)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(log_pfrv)
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(log_pfrv)

# extract RHS 
MZ_RHS <- DB5 %>% filter(repyear %in% N2) %>% 
  select(log_garch, log_gjr, log_BTM,
         log_GFS, # doc length = readability 
         med_VIX_pre, # median VIX 1W before filing
         log_TVOL, # median TVOL 1W before filing
         leverage, # leverage ratio
         repday_27:repday_7, # filing month day dummies
         weekday_Mi:weekday_Sa, # filing week day dummies
         repmth_4:repmth_10, # filing month dummies
         repyear_2005:repyear_1996, # filing year dummies
         sic2D_87:sic2D_76, # SIC Code dummies
         reptype_10.K:reptype_10.KT.A, # 10-K type dummies 
         FIN, # financial topics variable
         size_ta) %>% # size
  mutate_if(is.character, as.numeric)

# extract the N1-counts
N1.size <- nrow(WR_LHS)
N2.size <- nrow(MZ_LHS)

# Define indices for DTMs 
idx.N1 <- 1:N1.size
idx.N2 <- (N1.size + 1):(N1.size + N2.size)

# N -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_N_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
NEG_SENT_VIBTW <- as.matrix(DTM_N_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(NEG_SENT = NEG_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# P -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_P_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
POS_SENT_VIBTW <- as.matrix(DTM_P_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT = POS_SENT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# ASSERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_SM_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
ASSERT_VIBTW <- as.matrix(DTM_SM_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(ASSERT = ASSERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# UNCERT ------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_U_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
UNCERT_VIBTW <- as.matrix(DTM_U_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(UNCERT = UNCERT_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# LITI --------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")
# VIBTW regression:
WR_RHS <- as.matrix(DTM_L_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
# VIBTW raw coefficients:
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# VIBTW coefficient standardization
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# VIBTW score:
LITI_VIBTW <- as.matrix(DTM_L_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
# add to rhs matrix
MZ_RHS <- MZ_RHS %>% mutate(LITI = LITI_VIBTW)
# clear environment
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS", "WR_LHS",
                          "N1", "N2", "N1.size", "N2.size", "idx.N1", "idx.N2")))

# EXPORT FINAL RHS AND LHS MAT
rm(list = setdiff(ls(), c("DB5", "MZ_RHS", "MZ_LHS")))
save.image("~/Desktop/lm_testdata/regressions/B/B3_MZ_inputs_2017.RData")

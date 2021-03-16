
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: weight_regressions_A                                                      #
#     Description:  1) load pure-count DTMs XXXX-YYYY (RHS) & PFRV XXXX-YYYY (LHS)        #
#                   2) regress LHS on RHS and obtain weights                              #
#                   3) normalize weights (JW) -> will serve as input in MZ-regr. for ZZZZ #
#                                                                                         #
#     Date (last updated): July 27th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries / Settings ----------------------------------------------------
rm(list=ls()) # clear environment 
gc()
options(scipen = 999)
# setwd("/Volumes/LaCie/LM_data/")
# setwd("/Users/kevin/Desktop/lm_testdata")
setwd("/Users/kevin/Desktop/lm_testdata/regressions/A")

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

rm(FIN.var, DTM_KT)

# Variant A ---------------------------------------------------------------
# estimate w_j's and idf's for period 1999-2012
# apply weights to 2013-2017 filings
# regress all OOS filings (2013-2017) in a single regression

N1 <- 1999:2012
N2 <- 2013:2017

# extract LHS variable (PFRV)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(log_pfrv)
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(log_pfrv)

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
         reptype_10.K:reptype_10.KT.A,
         FIN) %>%  # 10-K type dummies 
  mutate_if(is.character, as.numeric)

# extract the counts
N1.size <- nrow(WR_LHS)
N2.size <- nrow(MZ_LHS)

# N -----------------------------------------------------------------------
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")
idx.N1 <- 1:N1.size; idx.N2 <- (N1.size + 1):nrow(DTM_N)

# length:
L_ik <- rowSums(DTM_P[idx.N2,])

# idf:
A_idf_weights_N <- log(rep(N1.size, 
                           ncol(DTM_P)) / sapply(DTM_P[idx.N1,], 
                                                 nnzero))
A_idf_weights_N[is.infinite(A_idf_weights_N)] <- 0

# TFIDF:
POS_SENT_TFIDF <- (as.data.frame(as.matrix(DTM_P[idx.N2,]) %*% diag(A_idf_weights_N)) %>% 
                     rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_TFIDF = POS_SENT_TFIDF)
rm(POS_SENT_TFIDF)

# RFIDF:
POS_SENT_RFIDF <- sweep(DTM_P[idx.N2,], 1, as.matrix(L_ik), FUN = "/")
POS_SENT_RFIDF <- (as.matrix(POS_SENT_RFIDF) %*% diag(A_idf_weights_N) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_RFIDF = POS_SENT_RFIDF)
rm(POS_SENT_RFIDF)

# WF1P 1PLOG:
POS_SENT_WF_1P <- (as.matrix(DTM_P_WF_1plog[idx.N2,]) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_WF_1P = POS_SENT_WF_1P)
rm(POS_SENT_WF_1P)

# WFP1 LOG1P:
POS_SENT_WF_P1 <- (as.matrix(DTM_P_WF_log1p[idx.N2,]) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_WF_P1 = POS_SENT_WF_P1)
rm(POS_SENT_WF_P1)

# WFIDF 1PLOG:
POS_SENT_WFIDF_1P <- ((as.matrix(DTM_P_WF_1plog[idx.N2,]) %*% diag(A_idf_weights_N)) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_WFIDF_1P = POS_SENT_WFIDF_1P)
rm(POS_SENT_WFIDF_1P)

# WFIDF LOG1P:
POS_SENT_WFIDF_P1 <- ((as.matrix(DTM_P_WF_log1p[idx.N2,]) %*% diag(A_idf_weights_N)) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_WFIDF_P1 = POS_SENT_WFIDF_P1)
rm(POS_SENT_WFIDF_P1)

# TFMAX:
POS_SENT_TFMAX <- ((as.matrix(DTM_P_TFMAX[idx.N2,])) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_TFMAX = POS_SENT_TFMAX)
rm(POS_SENT_TFMAX)

# VIBTW:
WR_RHS <- as.matrix(DTM_N_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
NEG_SENT_VIBTW <- as.matrix(DTM_N_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
MZ_RHS <- MZ_RHS %>% mutate(NEG_SENT_VIBTW = NEG_SENT_VIBTW)

# clear environment
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS")))

# P -----------------------------------------------------------------------

# POS_SENT: -- scores in N2 -- 
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")
# length:
L_ik <- rowSums(DTM_P[idx.N2,])

# idf:
A_idf_weights_N <- log(rep(N1.size, 
                           ncol(DTM_P)) / sapply(DTM_P[idx.N1,], 
                                                 nnzero))
A_idf_weights_N[is.infinite(A_idf_weights_N)] <- 0

# TFIDF:
POS_SENT_TFIDF <- (as.data.frame(as.matrix(DTM_P[idx.N2,]) %*% diag(A_idf_weights_N)) %>% 
                     rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_TFIDF = POS_SENT_TFIDF)
rm(POS_SENT_TFIDF)

# RFIDF:
POS_SENT_RFIDF <- sweep(DTM_P[idx.N2,], 1, as.matrix(L_ik), FUN = "/")
POS_SENT_RFIDF <- (as.matrix(POS_SENT_RFIDF) %*% diag(A_idf_weights_N) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_RFIDF = POS_SENT_RFIDF)
rm(POS_SENT_RFIDF)

# WF1P 1PLOG:
POS_SENT_WF_1P <- (as.matrix(DTM_P_WF_1plog[idx.N2,]) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_WF_1P = POS_SENT_WF_1P)
rm(POS_SENT_WF_1P)

# WFP1 LOG1P:
POS_SENT_WF_P1 <- (as.matrix(DTM_P_WF_log1p[idx.N2,]) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_WF_P1 = POS_SENT_WF_P1)
rm(POS_SENT_WF_P1)

# WFIDF 1PLOG:
POS_SENT_WFIDF_1P <- ((as.matrix(DTM_P_WF_1plog[idx.N2,]) %*% diag(A_idf_weights_N)) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_WFIDF_1P = POS_SENT_WFIDF_1P)
rm(POS_SENT_WFIDF_1P)

# WFIDF LOG1P:
POS_SENT_WFIDF_P1 <- ((as.matrix(DTM_P_WF_log1p[idx.N2,]) %*% diag(A_idf_weights_N)) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_WFIDF_P1 = POS_SENT_WFIDF_P1)
rm(POS_SENT_WFIDF_P1)

# TFMAX:
POS_SENT_TFMAX <- ((as.matrix(DTM_P_TFMAX[idx.N2,])) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_TFMAX = POS_SENT_TFMAX)
rm(POS_SENT_TFMAX)

# VIBTW:
WR_RHS <- as.matrix(DTM_P_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)

WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# standardize the betas
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# score them
POS_SENT_VIBTW <- as.matrix(DTM_P_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT_VIBTW = POS_SENT_VIBTW)

# clear environment
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS")))

# ASSERT ------------------------------------------------------------------

# ASSERT: -- scores in N2 -- 
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")
# length:
L_ik <- rowSums(DTM_SM[idx.N2,])

# idf:
A_idf_weights_N <- log(rep(N1.size, 
                           ncol(DTM_SM)) / sapply(DTM_SM[idx.N1,], 
                                                 nnzero))
A_idf_weights_N[is.infinite(A_idf_weights_N)] <- 0

# TFIDF:
ASSERT_TFIDF <- (as.data.frame(as.matrix(DTM_SM[idx.N2,]) %*% diag(A_idf_weights_N)) %>% 
                     rowSums) / L_ik
ASSERT_TFIDF[is.na(ASSERT_TFIDF)] <- 0
MZ_RHS <- MZ_RHS %>% mutate(ASSERT_TFIDF = ASSERT_TFIDF)
rm(ASSERT_TFIDF)

# RFIDF:
ASSERT_RFIDF <- sweep(DTM_SM[idx.N2,], 1, as.matrix(L_ik), FUN = "/")
ASSERT_RFIDF <- (as.matrix(ASSERT_RFIDF) %*% diag(A_idf_weights_N) %>% rowSums) / L_ik
ASSERT_RFIDF[is.na(ASSERT_RFIDF)] <- 0
MZ_RHS <- MZ_RHS %>% mutate(ASSERT_RFIDF = ASSERT_RFIDF)
rm(ASSERT_RFIDF)

# WF1P 1PLOG:
ASSERT_WF_1P <- (as.matrix(DTM_SM_WF_1plog[idx.N2,]) %>% rowSums) / L_ik
ASSERT_WF_1P[is.na(ASSERT_WF_1P)] <- 0
MZ_RHS <- MZ_RHS %>% mutate(ASSERT_WF_1P = ASSERT_WF_1P)
rm(ASSERT_WF_1P)

# WFP1 LOG1P:
ASSERT_WF_P1 <- (as.matrix(DTM_SM_WF_log1p[idx.N2,]) %>% rowSums) / L_ik
ASSERT_WF_P1[is.na(ASSERT_WF_P1)] <- 0
MZ_RHS <- MZ_RHS %>% mutate(ASSERT_WF_P1 = ASSERT_WF_P1)
rm(ASSERT_WF_P1)

# WFIDF 1PLOG:
ASSERT_WFIDF_1P <- ((as.matrix(DTM_SM_WF_1plog[idx.N2,]) %*% diag(A_idf_weights_N)) %>% rowSums) / L_ik
ASSERT_WFIDF_1P[is.na(ASSERT_WFIDF_1P)] <- 0
MZ_RHS <- MZ_RHS %>% mutate(ASSERT_WFIDF_1P = ASSERT_WFIDF_1P)
rm(ASSERT_WFIDF_1P)

# WFIDF LOG1P:
ASSERT_WFIDF_P1 <- ((as.matrix(DTM_SM_WF_log1p[idx.N2,]) %*% diag(A_idf_weights_N)) %>% rowSums) / L_ik
ASSERT_WFIDF_P1[is.na(ASSERT_WFIDF_P1)] <- 0
MZ_RHS <- MZ_RHS %>% mutate(ASSERT_WFIDF_P1 = ASSERT_WFIDF_P1)
rm(ASSERT_WFIDF_P1)

# TFMAX:
ASSERT_TFMAX <- ((as.matrix(DTM_SM_TFMAX[idx.N2,])) %>% rowSums) / L_ik
ASSERT_TFMAX[is.na(ASSERT_TFMAX)] <- 0
ASSERT_TFMAX[is.infinite(ASSERT_TFMAX)] <- 0

MZ_RHS <- MZ_RHS %>% mutate(ASSERT_TFMAX = ASSERT_TFMAX)
rm(ASSERT_TFMAX)

# VIBTW:
WR_RHS <- as.matrix(DTM_SM_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)

WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# standardize the betas
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# score them
ASSERT_VIBTW <- as.matrix(DTM_SM_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
ASSERT_VIBTW[is.na(ASSERT_VIBTW)] <- 0
ASSERT_VIBTW[is.infinite(ASSERT_VIBTW)] <- 0

MZ_RHS <- MZ_RHS %>% mutate(ASSERT_VIBTW = ASSERT_VIBTW)

# clear environment
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS")))

# UNCERT ------------------------------------------------------------------

# UNCERT: -- scores in N2 -- 
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")
# length:
L_ik <- rowSums(DTM_U[idx.N2,])

# idf:
A_idf_weights_N <- log(rep(N1.size, 
                           ncol(DTM_U)) / sapply(DTM_U[idx.N1,], 
                                                 nnzero))
A_idf_weights_N[is.infinite(A_idf_weights_N)] <- 0

# TFIDF:
UNCERT_TFIDF <- (as.data.frame(as.matrix(DTM_U[idx.N2,]) %*% diag(A_idf_weights_N)) %>% 
                   rowSums) / L_ik
UNCERT_TFIDF[is.na(UNCERT_TFIDF)] <- 0
MZ_RHS <- MZ_RHS %>% mutate(UNCERT_TFIDF = UNCERT_TFIDF)
rm(UNCERT_TFIDF)

# RFIDF:
UNCERT_RFIDF <- sweep(DTM_U[idx.N2,], 1, as.matrix(L_ik), FUN = "/")
UNCERT_RFIDF <- (as.matrix(UNCERT_RFIDF) %*% diag(A_idf_weights_N) %>% rowSums) / L_ik
UNCERT_RFIDF[is.na(UNCERT_RFIDF)] <- 0
MZ_RHS <- MZ_RHS %>% mutate(UNCERT_RFIDF = UNCERT_RFIDF)
rm(UNCERT_RFIDF)

# WF1P 1PLOG:
UNCERT_WF_1P <- (as.matrix(DTM_U_WF_1plog[idx.N2,]) %>% rowSums) / L_ik
UNCERT_WF_1P[is.na(UNCERT_WF_1P)] <- 0
MZ_RHS <- MZ_RHS %>% mutate(UNCERT_WF_1P = UNCERT_WF_1P)
rm(UNCERT_WF_1P)

# WFP1 LOG1P:
UNCERT_WF_P1 <- (as.matrix(DTM_U_WF_log1p[idx.N2,]) %>% rowSums) / L_ik
UNCERT_WF_P1[is.na(UNCERT_WF_P1)] <- 0
MZ_RHS <- MZ_RHS %>% mutate(UNCERT_WF_P1 = UNCERT_WF_P1)
rm(UNCERT_WF_P1)

# WFIDF 1PLOG:
UNCERT_WFIDF_1P <- ((as.matrix(DTM_U_WF_1plog[idx.N2,]) %*% diag(A_idf_weights_N)) %>% rowSums) / L_ik
UNCERT_WFIDF_1P[is.na(UNCERT_WFIDF_1P)] <- 0
MZ_RHS <- MZ_RHS %>% mutate(UNCERT_WFIDF_1P = UNCERT_WFIDF_1P)
rm(UNCERT_WFIDF_1P)

# WFIDF LOG1P:
UNCERT_WFIDF_P1 <- ((as.matrix(DTM_U_WF_log1p[idx.N2,]) %*% diag(A_idf_weights_N)) %>% rowSums) / L_ik
UNCERT_WFIDF_P1[is.na(UNCERT_WFIDF_P1)] <- 0
MZ_RHS <- MZ_RHS %>% mutate(UNCERT_WFIDF_P1 = UNCERT_WFIDF_P1)
rm(UNCERT_WFIDF_P1)

# TFMAX:
UNCERT_TFMAX <- ((as.matrix(DTM_U_TFMAX[idx.N2,])) %>% rowSums) / L_ik
UNCERT_TFMAX[is.na(UNCERT_TFMAX)] <- 0
UNCERT_TFMAX[is.infinite(UNCERT_TFMAX)] <- 0
MZ_RHS <- MZ_RHS %>% mutate(UNCERT_TFMAX = UNCERT_TFMAX)
rm(UNCERT_TFMAX)

# VIBTW:
WR_RHS <- as.matrix(DTM_U_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)

WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# standardize the betas
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# score them
UNCERT_VIBTW <- as.matrix(DTM_U_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
UNCERT_VIBTW[is.na(UNCERT_VIBTW)] <- 0
UNCERT_VIBTW[is.infinite(UNCERT_VIBTW)] <- 0

MZ_RHS <- MZ_RHS %>% mutate(UNCERT_VIBTW = UNCERT_VIBTW)

# clear environment
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS")))

# LITI --------------------------------------------------------------------

# LITI: -- scores in N2 -- 
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")
# length:
L_ik <- rowSums(DTM_L[idx.N2,])

# idf:
A_idf_weights_N <- log(rep(N1.size, 
                           ncol(DTM_L)) / sapply(DTM_L[idx.N1,], 
                                                 nnzero))
A_idf_weights_N[is.infinite(A_idf_weights_N)] <- 0

# TFIDF:
LITI_TFIDF <- (as.data.frame(as.matrix(DTM_L[idx.N2,]) %*% diag(A_idf_weights_N)) %>% 
                   rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(LITI_TFIDF = LITI_TFIDF)
rm(LITI_TFIDF)

# RFIDF:
LITI_RFIDF <- sweep(DTM_L[idx.N2,], 1, as.matrix(L_ik), FUN = "/")
LITI_RFIDF <- (as.matrix(LITI_RFIDF) %*% diag(A_idf_weights_N) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(LITI_RFIDF = LITI_RFIDF)
rm(LITI_RFIDF)

# WF1P 1PLOG:
LITI_WF_1P <- (as.matrix(DTM_L_WF_1plog[idx.N2,]) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(LITI_WF_1P = LITI_WF_1P)
rm(LITI_WF_1P)

# WFP1 LOG1P:
LITI_WF_P1 <- (as.matrix(DTM_L_WF_log1p[idx.N2,]) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(LITI_WF_P1 = LITI_WF_P1)
rm(LITI_WF_P1)

# WFIDF 1PLOG:
LITI_WFIDF_1P <- ((as.matrix(DTM_L_WF_1plog[idx.N2,]) %*% diag(A_idf_weights_N)) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(LITI_WFIDF_1P = LITI_WFIDF_1P)
rm(LITI_WFIDF_1P)

# WFIDF LOG1P:
LITI_WFIDF_P1 <- ((as.matrix(DTM_L_WF_log1p[idx.N2,]) %*% diag(A_idf_weights_N)) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(LITI_WFIDF_P1 = LITI_WFIDF_P1)
rm(LITI_WFIDF_P1)

# TFMAX:
LITI_TFMAX <- ((as.matrix(DTM_L_TFMAX[idx.N2,])) %>% rowSums) / L_ik
MZ_RHS <- MZ_RHS %>% mutate(LITI_TFMAX = LITI_TFMAX)
rm(LITI_TFMAX)

# VIBTW:
WR_RHS <- as.matrix(DTM_L_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)

WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
# standardize the betas
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
# score them
LITI_VIBTW <- as.matrix(DTM_L_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
MZ_RHS <- MZ_RHS %>% mutate(LITI_VIBTW = LITI_VIBTW)

# clear environment
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS")))

# export the MZ frames to use them for A-MZ regression
rm(list = setdiff(ls(), c("MZ_RHS", "MZ_LHS")))

save.image("~/Desktop/lm_testdata/regressions/A/A_MZ_inputs.RData")








# 
# write.table(rownames(DTM_SM), 
#             file = "/Users/kevin/Desktop/lm_testdata/publication/mz_rhs_ordered_txtfilenames.txt",
#             col.names = F, row.names = F, quote = F, sep = "\n")
# 












# Time Window Extraction --------------------------------------------------

# Subset the relevant time window:
yearlist <- 1994:2017
weight_reg_wind_1 <- 1994:2012
MZ_reg_wind_1 <- yearlist[!(yearlist  %in% weight_reg_wind_1)]

N1 <- DB4 %>% 
  as_data_frame() %>% 
  filter(repyear %in% weight_reg_wind_1)

N2 <- DB4 %>% 
  as_data_frame() %>% 
  filter(repyear %in% MZ_reg_wind_1)

# rel_RHS_window <- DTM_N %>% 
#   as_data_frame() %>% 
#   mutate(FILYR = DB4$repyear) %>% 
#   filter(FILYR %in% weight_reg_wind_1) %>% 
#   select(-FILYR)

rel_RHS_window <- DTM_N_RELFREQ %>% 
  as_data_frame() %>% 
  mutate(FILYR = DB4$repyear) %>% 
  filter(FILYR %in% weight_reg_wind_1) %>% 
  select(-FILYR)

MZ_RHS_window <- DTM_N_RELFREQ %>% 
  as_data_frame() %>% 
  mutate(FILYR = DB4$repyear) %>% 
  filter(FILYR %in% MZ_reg_wind_1) %>% 
  select(-FILYR)

# Regression / Weights Stdz. ----------------------------------------------

# Regress:
LHS <- as.numeric(rel_vola_window$PFRV_main)
# LHS <- as.numeric(rel_vola_window$PFRV_main.wins1)
RHS <- as.matrix(rel_RHS_window)

weight_reg_1 <- lm(LHS ~ RHS) %>% summary(.)

# extract estimated coefficients
WeiReg1_coeffs <- as.data.frame(weight_reg_1$coefficients) # extracts also t, p, etc.
WeiReg1_coeffs <- WeiReg1_coeffs$Estimate # keep only the betas
WeiReg1_coeffs <- WeiReg1_coeffs[-1] # drop the alpha 

# standardize the betas
WeiReg1_coeffs_stdzd <- (WeiReg1_coeffs - mean(WeiReg1_coeffs)) / sd(WeiReg1_coeffs) 

# quick inspect:
WeiReg1_coeffs_stdzd %>% as_tibble() %>% head()
WeiReg1_coeffs %>% as_tibble() %>% head()
summary(WeiReg1_coeffs_stdzd)
mean(WeiReg1_coeffs_stdzd); var(WeiReg1_coeffs_stdzd)
hist(WeiReg1_coeffs_stdzd, breaks = 50)
hist(WeiReg1_coeffs, breaks = 50)

# find most/least impactful negative words
WeiReg1_coeffs_stdzd <- WeiReg1_coeffs_stdzd %>% 
  as.tibble() %>% 
  mutate(negword = colnames(MZ_RHS_window))

WeiReg1_coeffs_stdzd %>% 
  arrange(value)

WeiReg1_coeffs_stdzd %>% 
  arrange(desc(value))

# MZ ----------------------------------------------------------------------

# multiply the relative term frequencies with their VIBTW-weight:
DTM_N_vibtw <- as.matrix(MZ_RHS_window) %*% diag(as.vector(WeiReg1_coeffs_stdzd$value))
rownames(DTM_N_vibtw) <- rownames(MZ_RHS_window)
colnames(DTM_N_vibtw) <- colnames(MZ_RHS_window)
DTM_N_vibtw %>% as_tibble() %>% head()

# calculate doc-wide negativity score
MZ_scores_oos <- rowSums(DTM_N_vibtw)

summary(MZ_scores_oos)
hist(MZ_scores_oos, breaks = 50)

colnames(DB4) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "repty*"))

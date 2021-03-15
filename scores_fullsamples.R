
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: scores_fullsamples                                                        #
#     Description:  1) load weighted DTMs and aggregate them into scores                  #
#                   2) estimate also VIBTW score *BASED ON FULL SAMPLE*                   #
#                                                                                         #
#     Date (last updated): July 19th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #
library(Matrix)
library(tidyverse)
library(ggcorrplot)
options(scipen = 999)

# NEG_SENT ----------------------------------------------------------------
rm(list=ls()); gc()
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")

# report length L_{i,k}:
L_ik <- rowSums(DTM_N)

# scores:
score_TFIDF <- rowSums(DTM_N_TFIDF) / L_ik
score_WF_1PL <- rowSums(DTM_N_WF_1plog) / L_ik
score_WF_L1P <- rowSums(DTM_N_WF_log1p) / L_ik
score_WFIDF_1PL <- rowSums(DTM_N_WFIDF_1p) / L_ik
score_WFIDF_L1P <- rowSums(DTM_N_WFIDF_p1) / L_ik
score_RFIDF <- rowSums(DTM_N_RFIDF) / L_ik
score_TFMAX <- rowSums(DTM_N_TFMAX) / L_ik
# score_TFAVG <- rowSums(DTM_N_TFAVG) / L_ik ... makes no sense -- always sums up to ncol of DTM
# score_RF <- rowSums(DTM_N_RELFREQ) / L_ik ... makes no sense  -- always sums up to 1

# 2 problems as L_ik can be zero:
# i) most cases: 0 / 0 (if length is zero, then all counts are zero as well)
# ii) single exception: tf.max can be > 0 although all counts are 0, due to scaling with a = 0.4
# i introduces NaN/na, but ii introduces Inf in R
# these need to be replaced by zeroes

# see here how many are affected:
sum(is.nan(score_TFIDF)); sum(is.na(score_TFIDF)); sum(is.infinite(score_TFIDF))
sum(is.nan(score_WF_1PL)); sum(is.na(score_WF_1PL)); sum(is.infinite(score_WF_1PL))
sum(is.nan(score_WF_L1P)); sum(is.na(score_WF_L1P)); sum(is.infinite(score_WF_L1P))
sum(is.nan(score_WFIDF_1PL)); sum(is.na(score_WFIDF_1PL)); sum(is.infinite(score_WFIDF_1PL))
sum(is.nan(score_WFIDF_1PL)); sum(is.na(score_WFIDF_1PL)); sum(is.infinite(score_WFIDF_1PL))
sum(is.nan(score_RFIDF)); sum(is.na(score_RFIDF)); sum(is.infinite(score_RFIDF))
sum(is.nan(score_TFMAX)); sum(is.na(score_TFMAX)); sum(is.infinite(score_TFMAX))

scoremat <- cbind(score_TFIDF, score_WF_1PL, score_WF_L1P,
                  score_WFIDF_1PL, score_WFIDF_L1P, score_RFIDF,
                  score_TFMAX)

scoremat[is.na(scoremat)] <- 0
scoremat[is.nan(scoremat)] <- 0
scoremat[is.infinite(scoremat)] <- 0

# quick inspect:
summary(scoremat)
scoremat %>% as_tibble()

# add VIBTW
load("~/Desktop/lm_testdata/DB5_316x46483.RData")
LHS <- DB5$log_pfrv
RHS <- as.matrix(DTM_N_RELFREQ)

vibtw_reg_fullsample <- lm(LHS ~ RHS) # regress
vibtw_Bj <- vibtw_reg_fullsample$coefficients # extract coefs
vibtw_Bj <- vibtw_Bj[-1] # drop a
vibtw_wj <- (vibtw_Bj - mean(vibtw_Bj)) / sd(vibtw_Bj)

hist(vibtw_wj, 90); summary(vibtw_wj); mean(vibtw_wj); sd(vibtw_wj)

# apply to vibtw matrix
DTM_VIBTW <- as.matrix(DTM_N_RELFREQ) %*% diag(vibtw_wj)
# aggregate to score
score_VIBTW <- rowSums(DTM_VIBTW)
# add to the matrix of other scores
scoremat <- cbind(scoremat, score_VIBTW)

# inspect and export final scoremat
scoremat %>% as_tibble()
round(cor(scoremat), 3); round(cor_pmat(scoremat), 3)

rm(list = setdiff(ls(), c("scoremat")))

save.image("~/Desktop/lm_testdata/scores/N_scores_fullsample.RData")
write.table(scoremat,
            file = "~/Desktop/lm_testdata/scores/N_scores_fullsample.txt",
            sep = ";", row.names = T, col.names = T)

# POS_SENT ----------------------------------------------------------------
rm(list=ls()); gc()
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")

# report length L_{i,k}:
L_ik <- rowSums(DTM_P)

# scores:
score_TFIDF <- rowSums(DTM_P_TFIDF) / L_ik
score_WF_1PL <- rowSums(DTM_P_WF_1plog) / L_ik
score_WF_L1P <- rowSums(DTM_P_WF_log1p) / L_ik
score_WFIDF_1PL <- rowSums(DTM_P_WFIDF_1p) / L_ik
score_WFIDF_L1P <- rowSums(DTM_P_WFIDF_p1) / L_ik
score_RFIDF <- rowSums(DTM_P_RFIDF) / L_ik
score_TFMAX <- rowSums(DTM_P_TFMAX) / L_ik
# score_TFAVG <- rowSums(DTM_P_TFAVG) / L_ik ... makes no sense -- always sums up to ncol of DTM
# score_RF <- rowSums(DTM_P_RELFREQ) / L_ik ... makes no sense  -- always sums up to 1

# 2 problems as L_ik can be zero:
# i) most cases: 0 / 0 (if length is zero, then all counts are zero as well)
# ii) single exception: tf.max can be > 0 although all counts are 0, due to scaling with a = 0.4
# i introduces NaN/na, but ii introduces Inf in R
# these need to be replaced by zeroes

# see here how many are affected:
sum(is.nan(score_TFIDF)); sum(is.na(score_TFIDF)); sum(is.infinite(score_TFIDF))
sum(is.nan(score_WF_1PL)); sum(is.na(score_WF_1PL)); sum(is.infinite(score_WF_1PL))
sum(is.nan(score_WF_L1P)); sum(is.na(score_WF_L1P)); sum(is.infinite(score_WF_L1P))
sum(is.nan(score_WFIDF_1PL)); sum(is.na(score_WFIDF_1PL)); sum(is.infinite(score_WFIDF_1PL))
sum(is.nan(score_WFIDF_1PL)); sum(is.na(score_WFIDF_1PL)); sum(is.infinite(score_WFIDF_1PL))
sum(is.nan(score_RFIDF)); sum(is.na(score_RFIDF)); sum(is.infinite(score_RFIDF))
sum(is.nan(score_TFMAX)); sum(is.na(score_TFMAX)); sum(is.infinite(score_TFMAX))

scoremat <- cbind(score_TFIDF, score_WF_1PL, score_WF_L1P,
                  score_WFIDF_1PL, score_WFIDF_L1P, score_RFIDF,
                  score_TFMAX)

scoremat[is.na(scoremat)] <- 0
scoremat[is.nan(scoremat)] <- 0
scoremat[is.infinite(scoremat)] <- 0

# quick inspect:
summary(scoremat)
scoremat %>% as_tibble()

# add VIBTW
load("~/Desktop/lm_testdata/DB5_316x46483.RData")
LHS <- DB5$log_pfrv
RHS <- as.matrix(DTM_P_RELFREQ)

vibtw_reg_fullsample <- lm(LHS ~ RHS) # regress
vibtw_Bj <- vibtw_reg_fullsample$coefficients # extract coefs
vibtw_Bj <- vibtw_Bj[-1] # drop a
vibtw_wj <- (vibtw_Bj - mean(vibtw_Bj)) / sd(vibtw_Bj)

hist(vibtw_wj, 90); summary(vibtw_wj); mean(vibtw_wj); sd(vibtw_wj)

# apply to vibtw matrix
DTM_VIBTW <- as.matrix(DTM_P_RELFREQ) %*% diag(vibtw_wj)
# aggregate to score
score_VIBTW <- rowSums(DTM_VIBTW)
# add to the matrix of other scores
scoremat <- cbind(scoremat, score_VIBTW)

# inspect and export final scoremat
scoremat %>% as_tibble()
round(cor(scoremat), 3); round(cor_pmat(scoremat), 3)

rm(list = setdiff(ls(), c("scoremat")))

save.image("~/Desktop/lm_testdata/scores/P_scores_fullsample.RData")
write.table(scoremat,
            file = "~/Desktop/lm_testdata/scores/P_scores_fullsample.txt",
            sep = ";", row.names = T, col.names = T)

# ASSERT ------------------------------------------------------------------
rm(list=ls()); gc()
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")

# report length L_{i,k}:
L_ik <- rowSums(DTM_SM)

# scores:
score_TFIDF <- rowSums(DTM_SM_TFIDF) / L_ik
score_WF_1PL <- rowSums(DTM_SM_WF_1plog) / L_ik
score_WF_L1P <- rowSums(DTM_SM_WF_log1p) / L_ik
score_WFIDF_1PL <- rowSums(DTM_SM_WFIDF_1p) / L_ik
score_WFIDF_L1P <- rowSums(DTM_SM_WFIDF_p1) / L_ik
score_RFIDF <- rowSums(DTM_SM_RFIDF) / L_ik
score_TFMAX <- rowSums(DTM_SM_TFMAX) / L_ik
# score_TFAVG <- rowSums(DTM_P_TFAVG) / L_ik ... makes no sense -- always sums up to ncol of DTM
# score_RF <- rowSums(DTM_P_RELFREQ) / L_ik ... makes no sense  -- always sums up to 1

# 2 problems as L_ik can be zero:
# i) most cases: 0 / 0 (if length is zero, then all counts are zero as well)
# ii) single exception: tf.max can be > 0 although all counts are 0, due to scaling with a = 0.4
# i introduces NaN/na, but ii introduces Inf in R
# these need to be replaced by zeroes

# see here how many are affected:
sum(is.nan(score_TFIDF)); sum(is.na(score_TFIDF)); sum(is.infinite(score_TFIDF))
sum(is.nan(score_WF_1PL)); sum(is.na(score_WF_1PL)); sum(is.infinite(score_WF_1PL))
sum(is.nan(score_WF_L1P)); sum(is.na(score_WF_L1P)); sum(is.infinite(score_WF_L1P))
sum(is.nan(score_WFIDF_1PL)); sum(is.na(score_WFIDF_1PL)); sum(is.infinite(score_WFIDF_1PL))
sum(is.nan(score_WFIDF_1PL)); sum(is.na(score_WFIDF_1PL)); sum(is.infinite(score_WFIDF_1PL))
sum(is.nan(score_RFIDF)); sum(is.na(score_RFIDF)); sum(is.infinite(score_RFIDF))
sum(is.nan(score_TFMAX)); sum(is.na(score_TFMAX)); sum(is.infinite(score_TFMAX))

scoremat <- cbind(score_TFIDF, score_WF_1PL, score_WF_L1P,
                  score_WFIDF_1PL, score_WFIDF_L1P, score_RFIDF,
                  score_TFMAX)

scoremat[is.na(scoremat)] <- 0
scoremat[is.nan(scoremat)] <- 0
scoremat[is.infinite(scoremat)] <- 0

# quick inspect:
summary(scoremat)
scoremat %>% as_tibble()

# add VIBTW
load("~/Desktop/lm_testdata/DB5_316x46483.RData")
LHS <- DB5$log_pfrv
RHS <- as.matrix(DTM_SM_RELFREQ)

vibtw_reg_fullsample <- lm(LHS ~ RHS) # regress
vibtw_Bj <- vibtw_reg_fullsample$coefficients # extract coefs
vibtw_Bj <- vibtw_Bj[-1] # drop a
vibtw_wj <- (vibtw_Bj - mean(vibtw_Bj)) / sd(vibtw_Bj)

hist(vibtw_wj, 90); summary(vibtw_wj); mean(vibtw_wj); sd(vibtw_wj)

# apply to vibtw matrix
DTM_VIBTW <- as.matrix(DTM_SM_RELFREQ) %*% diag(vibtw_wj)
# aggregate to score
score_VIBTW <- rowSums(DTM_VIBTW)
# add to the matrix of other scores
scoremat <- cbind(scoremat, score_VIBTW)

# inspect and export final scoremat
scoremat %>% as_tibble()
round(cor(scoremat), 3); round(cor_pmat(scoremat), 3)

rm(list = setdiff(ls(), c("scoremat")))

save.image("~/Desktop/lm_testdata/scores/assert_scores_fullsample.RData")
write.table(scoremat,
            file = "~/Desktop/lm_testdata/scores/assert_scores_fullsample.txt",
            sep = ";", row.names = T, col.names = T)

# UNCERT ------------------------------------------------------------------
rm(list=ls()); gc()
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")

# report length L_{i,k}:
L_ik <- rowSums(DTM_U)

# scores:
score_TFIDF <- rowSums(DTM_U_TFIDF) / L_ik
score_WF_1PL <- rowSums(DTM_U_WF_1plog) / L_ik
score_WF_L1P <- rowSums(DTM_U_WF_log1p) / L_ik
score_WFIDF_1PL <- rowSums(DTM_U_WFIDF_1p) / L_ik
score_WFIDF_L1P <- rowSums(DTM_U_WFIDF_p1) / L_ik
score_RFIDF <- rowSums(DTM_U_RFIDF) / L_ik
score_TFMAX <- rowSums(DTM_U_TFMAX) / L_ik
# score_TFAVG <- rowSums(DTM_P_TFAVG) / L_ik ... makes no sense -- always sums up to ncol of DTM
# score_RF <- rowSums(DTM_P_RELFREQ) / L_ik ... makes no sense  -- always sums up to 1

# 2 problems as L_ik can be zero:
# i) most cases: 0 / 0 (if length is zero, then all counts are zero as well)
# ii) single exception: tf.max can be > 0 although all counts are 0, due to scaling with a = 0.4
# i introduces NaN/na, but ii introduces Inf in R
# these need to be replaced by zeroes

# see here how many are affected:
sum(is.nan(score_TFIDF)); sum(is.na(score_TFIDF)); sum(is.infinite(score_TFIDF))
sum(is.nan(score_WF_1PL)); sum(is.na(score_WF_1PL)); sum(is.infinite(score_WF_1PL))
sum(is.nan(score_WF_L1P)); sum(is.na(score_WF_L1P)); sum(is.infinite(score_WF_L1P))
sum(is.nan(score_WFIDF_1PL)); sum(is.na(score_WFIDF_1PL)); sum(is.infinite(score_WFIDF_1PL))
sum(is.nan(score_WFIDF_1PL)); sum(is.na(score_WFIDF_1PL)); sum(is.infinite(score_WFIDF_1PL))
sum(is.nan(score_RFIDF)); sum(is.na(score_RFIDF)); sum(is.infinite(score_RFIDF))
sum(is.nan(score_TFMAX)); sum(is.na(score_TFMAX)); sum(is.infinite(score_TFMAX))

scoremat <- cbind(score_TFIDF, score_WF_1PL, score_WF_L1P,
                  score_WFIDF_1PL, score_WFIDF_L1P, score_RFIDF,
                  score_TFMAX)

scoremat[is.na(scoremat)] <- 0
scoremat[is.nan(scoremat)] <- 0
scoremat[is.infinite(scoremat)] <- 0

# quick inspect:
summary(scoremat)
scoremat %>% as_tibble()

# add VIBTW
load("~/Desktop/lm_testdata/DB5_316x46483.RData")
LHS <- DB5$log_pfrv
RHS <- as.matrix(DTM_U_RELFREQ)

vibtw_reg_fullsample <- lm(LHS ~ RHS) # regress
vibtw_Bj <- vibtw_reg_fullsample$coefficients # extract coefs
vibtw_Bj <- vibtw_Bj[-1] # drop a
vibtw_wj <- (vibtw_Bj - mean(vibtw_Bj)) / sd(vibtw_Bj)

hist(vibtw_wj, 90); summary(vibtw_wj); mean(vibtw_wj); sd(vibtw_wj)

# apply to vibtw matrix
DTM_VIBTW <- as.matrix(DTM_U_RELFREQ) %*% diag(vibtw_wj)
# aggregate to score
score_VIBTW <- rowSums(DTM_VIBTW)
# add to the matrix of other scores
scoremat <- cbind(scoremat, score_VIBTW)

# inspect and export final scoremat
scoremat %>% as_tibble()
round(cor(scoremat), 3); round(cor_pmat(scoremat), 3)

rm(list = setdiff(ls(), c("scoremat")))

save.image("~/Desktop/lm_testdata/scores/uncert_scores_fullsample.RData")
write.table(scoremat,
            file = "~/Desktop/lm_testdata/scores/uncert_scores_fullsample.txt",
            sep = ";", row.names = T, col.names = T)

# LITI --------------------------------------------------------------------
rm(list=ls()); gc()
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")

# report length L_{i,k}:
L_ik <- rowSums(DTM_L)

# scores:
score_TFIDF <- rowSums(DTM_L_TFIDF) / L_ik
score_WF_1PL <- rowSums(DTM_L_WF_1plog) / L_ik
score_WF_L1P <- rowSums(DTM_L_WF_log1p) / L_ik
score_WFIDF_1PL <- rowSums(DTM_L_WFIDF_1p) / L_ik
score_WFIDF_L1P <- rowSums(DTM_L_WFIDF_p1) / L_ik
score_RFIDF <- rowSums(DTM_L_RFIDF) / L_ik
score_TFMAX <- rowSums(DTM_L_TFMAX) / L_ik
# score_TFAVG <- rowSums(DTM_P_TFAVG) / L_ik ... makes no sense -- always sums up to ncol of DTM
# score_RF <- rowSums(DTM_P_RELFREQ) / L_ik ... makes no sense  -- always sums up to 1

# 2 problems as L_ik can be zero:
# i) most cases: 0 / 0 (if length is zero, then all counts are zero as well)
# ii) single exception: tf.max can be > 0 although all counts are 0, due to scaling with a = 0.4
# i introduces NaN/na, but ii introduces Inf in R
# these need to be replaced by zeroes

# see here how many are affected:
sum(is.nan(score_TFIDF)); sum(is.na(score_TFIDF)); sum(is.infinite(score_TFIDF))
sum(is.nan(score_WF_1PL)); sum(is.na(score_WF_1PL)); sum(is.infinite(score_WF_1PL))
sum(is.nan(score_WF_L1P)); sum(is.na(score_WF_L1P)); sum(is.infinite(score_WF_L1P))
sum(is.nan(score_WFIDF_1PL)); sum(is.na(score_WFIDF_1PL)); sum(is.infinite(score_WFIDF_1PL))
sum(is.nan(score_WFIDF_1PL)); sum(is.na(score_WFIDF_1PL)); sum(is.infinite(score_WFIDF_1PL))
sum(is.nan(score_RFIDF)); sum(is.na(score_RFIDF)); sum(is.infinite(score_RFIDF))
sum(is.nan(score_TFMAX)); sum(is.na(score_TFMAX)); sum(is.infinite(score_TFMAX))

scoremat <- cbind(score_TFIDF, score_WF_1PL, score_WF_L1P,
                  score_WFIDF_1PL, score_WFIDF_L1P, score_RFIDF,
                  score_TFMAX)

scoremat[is.na(scoremat)] <- 0
scoremat[is.nan(scoremat)] <- 0
scoremat[is.infinite(scoremat)] <- 0

# quick inspect:
summary(scoremat)
scoremat %>% as_tibble()

# add VIBTW
load("~/Desktop/lm_testdata/DB5_316x46483.RData")
LHS <- DB5$log_pfrv
RHS <- as.matrix(DTM_L_RELFREQ)

vibtw_reg_fullsample <- lm(LHS ~ RHS) # regress
vibtw_Bj <- vibtw_reg_fullsample$coefficients # extract coefs
vibtw_Bj <- vibtw_Bj[-1] # drop a

# replace "henceforward" beta with the average, else it is NA due to collinearity
vibtw_Bj[length(vibtw_Bj)] <- mean(vibtw_Bj, na.rm = T)

vibtw_wj <- (vibtw_Bj - mean(vibtw_Bj)) / sd(vibtw_Bj)

hist(vibtw_wj, 90); summary(vibtw_wj); mean(vibtw_wj); sd(vibtw_wj)

# apply to vibtw matrix
DTM_VIBTW <- as.matrix(DTM_L_RELFREQ) %*% diag(vibtw_wj)
# aggregate to score
score_VIBTW <- rowSums(DTM_VIBTW)
# add to the matrix of other scores
scoremat <- cbind(scoremat, score_VIBTW)

# inspect and export final scoremat
scoremat %>% as_tibble()
round(cor(scoremat), 3); round(cor_pmat(scoremat), 3)

rm(list = setdiff(ls(), c("scoremat")))

save.image("~/Desktop/lm_testdata/scores/liti_scores_fullsample.RData")
write.table(scoremat,
            file = "~/Desktop/lm_testdata/scores/liti_scores_fullsample.txt",
            sep = ";", row.names = T, col.names = T)





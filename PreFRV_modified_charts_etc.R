
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: PreFRV_modified_charts_etc.R                                              # 
#     Date (last updated): August 27th, 2018                                              #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries / Settings ----------------------------------------------------
rm(list=ls()) # clear environment 
gc()
options(scipen = 999)
# setwd("/Volumes/LaCie/LM_data/")
setwd("/Users/kevin/Desktop/lm_testdata")
library(tidyverse); library(ggcorrplot)
library(stargazer); library(scales)
library(ggExtra); library(GGally); library(ggpubr)

load("DB5_316x46483.RData")
load("prefilingvola_46483.RData")

finallist_PFRV_volas <- finallist_PFRV_volas %>% as_tibble

# check the 0s / NAs
finallist_PFRV_volas$prefiling_RV_main_1_5 %>% summary
sum(is.na(finallist_PFRV_volas$prefiling_RV_main_1_5))
sum(finallist_PFRV_volas$prefiling_RV_main_1_5 == 0)

# take log for the non-zero variants 
# key filter as for PFRV: > 0.0001
log_prefrv <- ifelse(finallist_PFRV_volas$prefiling_RV_main_1_5 <= 0.0001, 
                     NA, 
                     log(finallist_PFRV_volas$prefiling_RV_main_1_5))

# then replace the NAs with the mean to leave unbiased and keep the sample size
sum(is.na(log_prefrv))
sum(log_prefrv == 0, na.rm = T)

log_prefrv[is.na(log_prefrv)] <- mean(log_prefrv, na.rm = T)

log_prefrv %>% hist
log_prefrv %>% summary

# (1) FULL CORMAT OF VOLA VARS --------------------------------------------
DB5 <- DB5 %>% mutate(log_prefrv = log_prefrv,
                      log_sq_ret = log(PFRV_sq_ret), 
                      log_abs_ret = log(PFRV_abs_ret))

VOLAS <- DB5 %>% select(log_pfrv, log_prefrv, 
                        log_garch, log_gjr, 
                        log_sq_ret, log_abs_ret)

VOLAS <- as.data.frame(VOLAS)

# number of pairwise obs:
x <- !is.na(VOLAS); t(x) %*% x
min(t(x) %*% x) # this is what will survive after na.omit = casewise deletion

# corrmat:
COR_VOLAS <- cor(VOLAS, use = "complete.obs")

# p-mat:
PMAT_VOLAS <- cor_pmat(VOLAS)

# they are all zero, so I can replace the upper diag
COR_VOLAS[upper.tri(COR_VOLAS, diag = F)] <- 0
colnames(COR_VOLAS) <- rownames(COR_VOLAS) <- c("PFRV", "PreFRV", "GARCH",
                                                "GJR", "PFSqR", "PFAbsR")

# export via stargazer:
stargazer(COR_VOLAS, 
          summary = F, type = "latex",
          rownames = T, colnames = T, out.header = F, header = F,
          title = "Correlation Matrix: Volatility-Related Measures",
          label = "tab: COR_VOLAS",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/COR_VOLAS",
          digit.separate = 3,
          digit.separator = ",",
          digits = 3, decimal.mark = ".", initial.zero = F,
          font.size = "small", align = F)


# (2) MZ-Reg Univariate: Post & Pre ---------------------------------------

scat1 <- ggplot(VOLAS,
                aes(x = log_prefrv,
                    y = log_pfrv)) +
  geom_jitter(size = .0001, alpha = .20) +
  geom_smooth(method = "lm", color = "#332288",
              linetype = "solid", size = 0.75) +
  labs(subtitle = "", 
       y = "PFRV\n", 
       x = "\nPreFRV") +
  theme(legend.key.size = unit(2, "mm"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(size = .25, 
                                 color = "black")) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))

scat1 <- ggMarginal(scat1, 
                    type = "histogram", 
                    fill = "#88CCEE",
                    binwidth = 0.15)

annotate_figure(scat1, 
                bottom = text_grob("\nThe regression is estimated on variables in logarithmic form. Sample Size: 46,483."))

ggsave(file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Images/MZ_scatter_pre_post.pdf", 
       width = 297, height = 210, units = "mm")

# Regression stats:
lm(log_pfrv ~ log_prefrv, data = DB5) %>% summary

# (3) Post-Pre: Delta Distribution ----------------------------------------
deltaseries <- VOLAS$log_prefrv - VOLAS$log_pfrv
deltaseries %>% summary
deltaseries %>% exp %>% summary

library(moments)
skewness(deltaseries); kurtosis(deltaseries)

deltaseries_v2 <- DB5$PFRV_main - finallist_PFRV_volas$prefiling_RV_main_1_5
deltaseries_v2 %>% summary

# check to resemble
(DB5$PFRV_main - DB5$GARCH_1step) %>% summary
(DB5$PFRV_main - DB5$GJR_1step) %>% summary

histo1 <- ggplot(as.data.frame(deltaseries),
                 aes(deltaseries)) + 
  geom_histogram(binwidth = .25, fill = "#88CCEE", color = "black") + 
  labs(subtitle = "", 
       y = "No. of filings\n", 
       x = "\nPreFRV - PFRV (Forecast Error)", 
       title = "") +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(size = .25, 
                                 color = "black"),
        panel.grid.major.y = element_line(size = .05, 
                                          color = "black",
                                          linetype = "solid"),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(expand = c(0, 0), labels = comma,
                     breaks = pretty_breaks(n = 8),
                     limits = c(0, 8000)) +
  scale_x_continuous(breaks = pretty_breaks(n = 14),
                     limits = c(-7, 7))

annotate_figure(histo1, 
                bottom = text_grob("\nDifferences are obtained from log-transformed volatility variables. Sample Size: 46,483."))

ggsave(file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Images/delta_pfrv_vs_prefrv.pdf", 
       width = 297, height = 150, units = "mm")

# (4) All RHS corrmat -----------------------------------------------------
# add FIN var
DTM_KT <- read.delim("/Users/kevin/Desktop/lm_testdata/sep_DTMs/DTM_KT.txt",
                     sep = ";", stringsAsFactors = F, dec = ",")
DTM_KT <- DTM_KT[!rownames(DTM_KT) %in% unlist(list(PFRV_diers)), ]
sum(rownames(DTM_KT) %in% unlist(list(PFRV_survivors)))
sum(colSums(DTM_KT) == 0)
DTM_KT <- DTM_KT[, which(colSums(DTM_KT) != 0)]

FIN.var <- rowSums(DTM_KT) %>% log1p
hist(FIN.var, breaks = 60); summary(FIN.var)

DB5 <- DB5 %>% mutate(FIN = FIN.var)

rm(aa, deltaseries, deltaseries_v2, histo1, COR_VOLAS, PMAT_VOLAS, x, scat1, scat2,
   PFRV_diers, PFRV_survivors, DTM_KT, FIN.var)

# add to DB5 the H1-5 scores: POS_SENT, NEG_SENT, ASSERT, UNCERT, LITI
# note: these are based on weights from the FULL sample, i.e.:
# a) estimate weights on all 46K obs, b) multiply weights for all 46K term counts

load("~/Desktop/lm_testdata/scores/N_scores_fullsample.RData")
scoremat <- as.data.frame(scoremat)
DB5 <- DB5 %>% mutate(NEG_SENT = scoremat$score_VIBTW)

load("~/Desktop/lm_testdata/scores/P_scores_fullsample.RData")
scoremat <- as.data.frame(scoremat)
DB5 <- DB5 %>% mutate(POS_SENT = scoremat$score_VIBTW)

load("~/Desktop/lm_testdata/scores/assert_scores_fullsample.RData")
scoremat <- as.data.frame(scoremat)
DB5 <- DB5 %>% mutate(ASSERT = scoremat$score_VIBTW)

load("~/Desktop/lm_testdata/scores/uncert_scores_fullsample.RData")
scoremat <- as.data.frame(scoremat)
DB5 <- DB5 %>% mutate(UNCERT = scoremat$score_VIBTW)

load("~/Desktop/lm_testdata/scores/liti_scores_fullsample.RData")
scoremat <- as.data.frame(scoremat)
DB5 <- DB5 %>% mutate(LITI = scoremat$score_VIBTW)

rm(scoremat)

# add PreFRV
DB5 <- DB5 %>% mutate(PreFRV = log_prefrv)

# extract all vars
pfrv_allcontrols <- DB5 %>% 
  select(log_pfrv, PreFRV,
         NEG_SENT, POS_SENT, ASSERT, UNCERT, LITI,
         log_GFS, FIN,
         size_ta, log_BTM, log_TVOL, med_VIX_pre, leverage)

# create corr matrix 
x <- !is.na(pfrv_allcontrols); t(x) %*% x; min(t(x) %*% x) # this is what will survive after na.omit = casewise deletion
cormat_allvars <- round(cor(pfrv_allcontrols, use = "complete.obs"), 2)
pmat_allvars <- round(cor_pmat(pfrv_allcontrols), 2)

# combine r & p values into 1 matrix (upper vs lower triangle)
cormat_comb_RHS <- matrix(NA, nrow(cormat_allvars), ncol(cormat_allvars))
cormat_comb_RHS[upper.tri(cormat_comb_RHS, diag = F)] <- 
  pmat_allvars[upper.tri(pmat_allvars, diag = F)]
cormat_comb_RHS[lower.tri(cormat_comb_RHS, diag = T)] <- 
  cormat_allvars[lower.tri(cormat_allvars, diag = T)]

# format corr mat
colnames(cormat_comb_RHS) <- rownames(cormat_comb_RHS) <- c("PFRV",
                                                            "PreFRV",
                                                            "NEGSENT", 
                                                            "POSSENT", 
                                                            "ASSERT", 
                                                            "UNCERT", 
                                                            "LITI",
                                                            "GFS",
                                                            "FIN",
                                                            "SIZE",
                                                            "BTM",
                                                            "TRVOL",
                                                            "VIX",
                                                            "LEVER")

cormat_comb_RHS <- format(cormat_comb_RHS, nsmall = 2)

# export for Latex / Stargazer:
stargazer(cormat_comb_RHS, 
          summary = F, type = "latex",
          rownames = T, colnames = T, out.header = F, header = F,
          title = "Correlation Matrix: All Variables",
          label = "tab: pfrv_all_RHS",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/cormat_allvars",
          digit.separate = 3,
          digit.separator = ",",
          digits = 3, decimal.mark = ".", initial.zero = F,
          font.size = "small", align = F, float.env = "sidewaystable")

# (5) All RHS Descr. Stats ------------------------------------------------
# replace only one line in the Tex table manually by copy-pasting 

stargazer(as.data.frame(log_prefrv), summary = T, median = T, 
          initial.zero = F, digits = 2)

# (6) Sq. & Abs. Returns Checks -------------------------------------------

# estimate w_j's and idf's for period 1999-2012
# apply weights to 2013-2017 filings
# regress all OOS filings (2013-2017) in a single regression
N1 <- 1999:2012; N2 <- 2013:2017
# extract LHS variable (SQ RET)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(PFRV_sq_ret) %>% log
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(PFRV_sq_ret) %>% log

MZ_RHS <- DB5 %>% filter(repyear %in% N2) %>% 
  select(log_prefrv, log_BTM,
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
         FIN, size_ta) %>%  # 10-K type dummies 
  mutate_if(is.character, as.numeric)

# extract the N1-counts
N1.size <- nrow(WR_LHS); N2.size <- nrow(MZ_LHS)

# NEGSENT:
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")
idx.N1 <- 1:N1.size; idx.N2 <- (N1.size + 1):nrow(DTM_N)
WR_RHS <- as.matrix(DTM_N_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
NEG_SENT <- as.matrix(DTM_N_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
MZ_RHS <- MZ_RHS %>% mutate(NEG_SENT = NEG_SENT)
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS")))

# POSSENT:
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")
idx.N1 <- 1:N1.size; idx.N2 <- (N1.size + 1):nrow(DTM_P)
WR_RHS <- as.matrix(DTM_P_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
POS_SENT <- as.matrix(DTM_P_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT = POS_SENT)
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS")))

# ASSERT:
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")
idx.N1 <- 1:N1.size; idx.N2 <- (N1.size + 1):nrow(DTM_SM)
WR_RHS <- as.matrix(DTM_SM_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
ASSERT <- as.matrix(DTM_SM_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
MZ_RHS <- MZ_RHS %>% mutate(ASSERT = ASSERT)
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS")))

# UNCERT:
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")
idx.N1 <- 1:N1.size; idx.N2 <- (N1.size + 1):nrow(DTM_U)
WR_RHS <- as.matrix(DTM_U_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
UNCERT <- as.matrix(DTM_U_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
MZ_RHS <- MZ_RHS %>% mutate(UNCERT = UNCERT)
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS")))

# LITI:
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")
idx.N1 <- 1:N1.size; idx.N2 <- (N1.size + 1):nrow(DTM_L)
WR_RHS <- as.matrix(DTM_L_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
LITI <- as.matrix(DTM_L_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
MZ_RHS <- MZ_RHS %>% mutate(LITI = LITI)
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS")))

# MZ REGRESSIONS:
RHS <- cbind(PreFRC = MZ_RHS$log_prefrv, 
  NEG_SENT = MZ_RHS$NEG_SENT,
  POS_SENT = MZ_RHS$POS_SENT,
  ASSERT = MZ_RHS$ASSERT,
  UNCERT = MZ_RHS$UNCERT,
  LITI = MZ_RHS$LITI,
  GFS = MZ_RHS$log_GFS,
  FINFOCUS = MZ_RHS$FIN, # had to rename it ... ? 
  SIZE = MZ_RHS$size_ta,
  BTM = MZ_RHS$log_BTM,
  TRVOL = MZ_RHS$log_TVOL,
  VIX = MZ_RHS$med_VIX_pre,
  LEVER = MZ_RHS$leverage,
  MZ_RHS[8:159]) %>% as_tibble()

robustness_sq <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# for abs. returns:
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(PFRV_abs_ret) %>% log
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(PFRV_abs_ret) %>% log

MZ_RHS <- DB5 %>% filter(repyear %in% N2) %>% 
  select(log_prefrv, log_BTM,
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
         FIN, size_ta) %>%  # 10-K type dummies 
  mutate_if(is.character, as.numeric)

# extract the N1-counts
N1.size <- nrow(WR_LHS); N2.size <- nrow(MZ_LHS)

# NEGSENT:
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_N_weightschemes.RData")
idx.N1 <- 1:N1.size; idx.N2 <- (N1.size + 1):nrow(DTM_N)
WR_RHS <- as.matrix(DTM_N_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
NEG_SENT <- as.matrix(DTM_N_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
MZ_RHS <- MZ_RHS %>% mutate(NEG_SENT = NEG_SENT)
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS",
                          "robustness_sq")))

# POSSENT:
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_P_weightschemes.RData")
idx.N1 <- 1:N1.size; idx.N2 <- (N1.size + 1):nrow(DTM_P)
WR_RHS <- as.matrix(DTM_P_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
POS_SENT <- as.matrix(DTM_P_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
MZ_RHS <- MZ_RHS %>% mutate(POS_SENT = POS_SENT)
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS",
                          "robustness_sq")))

# ASSERT:
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_SM_weightschemes.RData")
idx.N1 <- 1:N1.size; idx.N2 <- (N1.size + 1):nrow(DTM_SM)
WR_RHS <- as.matrix(DTM_SM_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
ASSERT <- as.matrix(DTM_SM_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
MZ_RHS <- MZ_RHS %>% mutate(ASSERT = ASSERT)
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS",
                          "robustness_sq")))

# UNCERT:
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_U_weightschemes.RData")
idx.N1 <- 1:N1.size; idx.N2 <- (N1.size + 1):nrow(DTM_U)
WR_RHS <- as.matrix(DTM_U_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
UNCERT <- as.matrix(DTM_U_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
MZ_RHS <- MZ_RHS %>% mutate(UNCERT = UNCERT)
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS",
                          "robustness_sq")))

# LITI:
load("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_L_weightschemes.RData")
idx.N1 <- 1:N1.size; idx.N2 <- (N1.size + 1):nrow(DTM_L)
WR_RHS <- as.matrix(DTM_L_RELFREQ[idx.N1,])
WR_LHS <- as.matrix(WR_LHS)
WR <- lm(WR_LHS~WR_RHS)
WR_coefs <- WR$coefficients # extracts also t, p, etc.
WR_coefs <- WR_coefs[-1] # drop the alpha 
WR_coefs[is.na(WR_coefs)] <- mean(WR_coefs, na.rm = T) # replace the NAs from singularity with mean
wj <- (WR_coefs - mean(WR_coefs)) / sd(WR_coefs)
LITI <- as.matrix(DTM_L_RELFREQ[idx.N2,]) %*% diag(wj) %>% 
  rowSums
MZ_RHS <- MZ_RHS %>% mutate(LITI = LITI)
rm(list = setdiff(ls(), c("DB5", "idx.N1", "idx.N2", "N1.size", "N2.size",
                          "MZ_RHS", "MZ_LHS", "N1", "N2", "WR_LHS",
                          "robustness_sq")))

# MZ REGRESSIONS:
RHS <- cbind(PreFRC = MZ_RHS$log_prefrv, 
             NEG_SENT = MZ_RHS$NEG_SENT,
             POS_SENT = MZ_RHS$POS_SENT,
             ASSERT = MZ_RHS$ASSERT,
             UNCERT = MZ_RHS$UNCERT,
             LITI = MZ_RHS$LITI,
             GFS = MZ_RHS$log_GFS,
             FINFOCUS = MZ_RHS$FIN, # had to rename it ... ? 
             SIZE = MZ_RHS$size_ta,
             BTM = MZ_RHS$log_BTM,
             TRVOL = MZ_RHS$log_TVOL,
             VIX = MZ_RHS$med_VIX_pre,
             LEVER = MZ_RHS$leverage,
             MZ_RHS[8:159]) %>% as_tibble()

robustness_abs <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# export:
stargazer(robustness_sq, robustness_abs,
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/robust_new_sq_abs_rets",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: robust_new_sq_abs_rets", 
          title = "Robustness Check: Pooled OLS with Alternative Volatility Proxies",
          model.numbers = T,
          notes.align = "l", 
          column.labels   = c("Squared Returns", "Absolute Returns"),
          column.separate = c(1, 1),
          model.names = T,
          notes = "Standard errors in parentheses. VIBTW weights were estimated using 10-K filings from 1999 to 2012.", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))


# (7) signistarsummary ----------------------------------------------------
# counted manually the number of significant vars

SSS <- matrix(NA, nrow = 4, ncol = 5)

SSS[1,] <- c("", "Static", "Rolling", "Extending", "Pooled")
SSS[2,] <- c("PreFRV", 14, 7, 15, 3)
SSS[3,] <- c("GARCH", 7, 9, 8, 2)
SSS[4,] <- c("GJRGARCH", 12, 9, 12, 3)

stargazer(SSS, summary = F,
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/SSS",
          type = "latex",
          align = F, out.header = F, header = F, 
          font.size = "small",
          label = "tab: SSS", 
          title = "Significance of Textual Variables Across Baseline and Robustness Check Models")


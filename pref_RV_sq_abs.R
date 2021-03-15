load("prefilingvola_46483.RData")

DB5 <- DB5 %>% mutate(pre_sq = log(finallist_PFRV_volas$prefiling_sq_ret_1_5),
                      pre_abs = log(finallist_PFRV_volas$prefiling_abs_ret_1_5))

sum(DB5$pre_sq == 0, na.rm = T)
sum(DB5$pre_abs == 0, na.rm = T)

sum(is.na(DB5$pre_sq))
sum(is.na(DB5$pre_abs))

sum(is.infinite(DB5$pre_sq))
sum(is.infinite(DB5$pre_abs))

sum(is.nan(DB5$pre_sq))
sum(is.nan(DB5$pre_abs))

DB5$pre_sq[is.na(DB5$pre_sq)] <- mean(DB5$pre_sq, na.rm = T)
DB5$pre_sq[is.infinite(DB5$pre_sq)] <- mean(DB5$pre_sq, na.rm = T)
DB5$pre_sq[is.nan(DB5$pre_sq)] <- mean(DB5$pre_sq, na.rm = T)
DB5$pre_abs[is.na(DB5$pre_abs)] <- mean(DB5$pre_abs, na.rm = T)
DB5$pre_abs[is.infinite(DB5$pre_abs)] <- mean(DB5$pre_abs, na.rm = T)
DB5$pre_abs[is.nan(DB5$pre_abs)] <- mean(DB5$pre_abs, na.rm = T)

# estimate w_j's and idf's for period 1999-2012
# apply weights to 2013-2017 filings
# regress all OOS filings (2013-2017) in a single regression
N1 <- 1999:2012; N2 <- 2013:2017
# extract LHS variable (SQ RET)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(PFRV_sq_ret) %>% log
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(PFRV_sq_ret) %>% log

MZ_RHS <- DB5 %>% filter(repyear %in% N2) %>% 
  select(pre_sq, log_BTM,
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
RHS <- cbind(PreFRC = MZ_RHS$pre_sq, 
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
  select(pre_abs, log_BTM,
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
RHS <- cbind(PreFRC = MZ_RHS$pre_abs, 
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

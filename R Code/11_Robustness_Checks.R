
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: robustness_checks.R                                                       #
#     Description:  1) define cormats for each sentiment category and 8 different weights #
#                   2) use sq. returns and abs. returns instead of RV as LHS / re-run MZ  #
#                   3) export nice stargazer latex tables                                 #
#                                                                                         #
#     Date (last updated): July 26th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries / Settings ----------------------------------------------------

# Different weighting schemes ---------------------------------------------
rm(list = ls()); gc(); options(scipen = 999)
library(stargazer)
library(tidyverse)
library(ggcorrplot)

setwd("/Users/kevin/Desktop/lm_testdata")

# define list of text variables used in thesis
textvar_list <- list("N", "P", "assert", "uncert", "liti")

# define function to calculate the corr. mat. for each of the 8 scores
weightschemes_cormats <- function(x) {
  load(paste0("/Users/kevin/Desktop/lm_testdata/scores/", 
              x,
              "_scores_fullsample.RData"))
  
  cormat <- scoremat %>% as.tibble() %>% 
    select(score_VIBTW, score_TFIDF, score_RFIDF,
           score_WFIDF_1PL, score_WFIDF_L1P,
           score_WF_1PL, score_WF_L1P,
           score_TFMAX) %>% 
    as.matrix() %>% 
    cor()
  
  return(cormat)
}

# define function to calculate the p-mat. for each of the 8 scores
weightschemes_pmats <- function(x) {
  load(paste0("/Users/kevin/Desktop/lm_testdata/scores/", 
              x,
              "_scores_fullsample.RData"))
  
  pmat <- scoremat %>% as.tibble() %>% 
    select(score_VIBTW, score_TFIDF, score_RFIDF,
           score_WFIDF_1PL, score_WFIDF_L1P,
           score_WF_1PL, score_WF_L1P,
           score_TFMAX) %>% 
    as.matrix() %>% 
    ggcorrplot::cor_pmat()
  
  return(pmat)
}

# apply function to all 5 variables --> returns five 8x8 cormats and five 8x8 pmats
allcormats_weightschemes <- lapply(textvar_list, weightschemes_cormats)
allpmats_weightschemes <- lapply(textvar_list, weightschemes_pmats)

# extract each element from the list and store in a single matrix
# pearson on the lower triangular, p-val on the upper triang.

# define weighting schemes in latex-ready form
scheme.names <- paste0("\\texttt{",
                       c("VIBTW", "TFIDF", "RFIDF",
                         "WFIDF\\_1PLOG", "WFIDF\\_LOG1P",
                         "WF\\_1PLOG", "WF\\_LOG1P", "TFMAX"),
                       "}")

i <- 1
for (i in 1:5) {
  combined_mat <- matrix(NA, 8, 8)
  
  lower <- allcormats_weightschemes[[i]] %>% lower.tri(diag = T)
  lower <- allcormats_weightschemes[[i]][lower]
  
  upper <- allpmats_weightschemes[[i]] %>% upper.tri(diag = F)
  upper <- allpmats_weightschemes[[i]][upper]
  
  combined_mat[lower.tri(combined_mat, diag = T)] <- lower
  combined_mat[upper.tri(combined_mat, diag = F)] <- upper
  
  colnames(combined_mat) <- rownames(combined_mat) <- scheme.names
  
  assign(paste0("combined_mat_", i), format(round(combined_mat, 2),
                                            nsmall = 2))
  
  i <- i + 1
  
  rm(combined_mat)
}

# rowbind all five of 'em together
total_comb_mat_weightschemes <- rbind(combined_mat_1, combined_mat_2,
                                      combined_mat_3, combined_mat_4,
                                      combined_mat_5)

# export for Latex / Stargazer:
stargazer(total_comb_mat_weightschemes, 
          summary = F, type = "latex",
          rownames = T, colnames = T, out.header = F, header = F,
          title = "Correlation Matrix: Scores Calculated by Different Term-Weighting Schemes",
          label = "tab: cormat_schemes",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/cormat_schemes",
          digit.separate = 3,
          digit.separator = ",",
          digits = 3, decimal.mark = ".", initial.zero = F,
          font.size = "small", align = F)

# PFRV alternatives -------------------------------------------------------

rm(list = ls()); gc(); options(scipen = 999)
load("~/Desktop/lm_testdata/DB5_316x46483.RData")

DTM_KT <- read.delim("/Users/kevin/Desktop/lm_testdata/sep_DTMs/DTM_KT.txt",
                     sep = ";", stringsAsFactors = F, dec = ",")
DTM_KT <- DTM_KT[!rownames(DTM_KT) %in% unlist(list(PFRV_diers)), ]
sum(rownames(DTM_KT) %in% unlist(list(PFRV_survivors)))
sum(colSums(DTM_KT) == 0)
DTM_KT <- DTM_KT[, which(colSums(DTM_KT) != 0)]
FIN.var <- rowSums(DTM_KT) %>% log1p
DB5 <- DB5 %>% mutate(FIN = FIN.var)
rm(FIN.var, DTM_KT)

# estimate w_j's and idf's for period 1999-2012
# apply weights to 2013-2017 filings
# regress all OOS filings (2013-2017) in a single regression
N1 <- 1999:2012; N2 <- 2013:2017
# extract LHS variable (SQ RET)
WR_LHS <- DB5 %>% filter(repyear %in% N1) %>% select(PFRV_sq_ret) %>% log
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(PFRV_sq_ret) %>% log

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
RHS <- cbind(         #TS_FC = MZ_RHS$log_garch, 
                      NEG_SENT = MZ_RHS$NEG_SENT,
                      POS_SENT = MZ_RHS$POS_SENT,
                      ASSERT = MZ_RHS$ASSERT,
                      UNCERT = MZ_RHS$UNCERT,
                      LITI = MZ_RHS$LITI,
                      GFS = MZ_RHS$log_GFS,
                      FIN = MZ_RHS$FIN,
                      SIZE = MZ_RHS$size_ta,
                      BTM = MZ_RHS$log_BTM,
                      TRVOL = MZ_RHS$log_TVOL,
                      VIX = MZ_RHS$med_VIX_pre,
                      LEVER = MZ_RHS$leverage,
                      MZ_RHS[8:159]) %>% as_tibble()

robustness_sq_garch <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(PFRV_abs_ret) %>% log
robustness_abs_garch <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# for GJR:
RHS <- cbind(         TS_FC = MZ_RHS$log_gjr, 
                      NEG_SENT = MZ_RHS$NEG_SENT,
                      POS_SENT = MZ_RHS$POS_SENT,
                      ASSERT = MZ_RHS$ASSERT,
                      UNCERT = MZ_RHS$UNCERT,
                      LITI = MZ_RHS$LITI,
                      GFS = MZ_RHS$log_GFS,
                      FIN = MZ_RHS$FIN,
                      SIZE = MZ_RHS$size_ta,
                      BTM = MZ_RHS$log_BTM,
                      TRVOL = MZ_RHS$log_TVOL,
                      VIX = MZ_RHS$med_VIX_pre,
                      LEVER = MZ_RHS$leverage,
                      MZ_RHS[8:159]) %>% as_tibble()

robustness_abs_gjr <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))
MZ_LHS <- DB5 %>% filter(repyear %in% N2) %>% select(PFRV_sq_ret) %>% log
robustness_sq_gjr <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# export the 4 models
stargazer(robustness_sq_garch, robustness_sq_gjr, 
          robustness_abs_garch, robustness_abs_gjr,
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/robustness_sq_abs_regressions",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: robustness_sq_abs_regressions", 
          title = "Robustness Check: Pooled OLS with Alternative Volatility Proxies",
          model.numbers = T,
          notes.align = "l", 
          column.labels   = c("Squared Returns", "Absolute Returns"),
          column.separate = c(2, 2),
          model.names = T,
          notes = "Standard errors in parentheses. VIBTW weights were estimated using 10-K filings from 1999 to 2012.", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))




# CorMat Size/Lengths -----------------------------------------------------

lengthvars <- DB4 %>% 
  select(nb.tokens, nb.types, nb.sentences, LM.DTM.counts,
         LM.DTM.nonzeroes, nb.tokens.cleancorp, nb.types.cleancorp,
         GFS, NFS, Exhi, Tabs)

x <- !is.na(lengthvars); t(x) %*% x; min(t(x) %*% x) # this is what will survive after na.omit = casewise deletion
cormat_lengthvars <- cor(lengthvars, use = "complete.obs")
pmat_lengthvars <- cor_pmat(lengthvars)

colnames(cormat_lengthvars) <- rownames(cormat_lengthvars) <- c("# of tokens (raw)",
                                                                "# of types (raw)",
                                                                "# of sentences (raw)",
                                                                "LM-DTM total count (all k)",
                                                                "LM-DTM incidence count (all k)",
                                                                "# of tokens (cleaned)",
                                                                "# of types (cleaned)",
                                                                "Gross file size",
                                                                "Net file size",
                                                                "# of exhibits",
                                                                "# of tables")

ggcorrplot(cormat_lengthvars, type = "upper", show.diag = T, method = "square",
           lab = T, lab_size = 6, lab_col = "white", 
           p.mat = pmat_lengthvars, 
           sig.level = .05, pch.col = "red", pch.cex = 20, 
           legend.title = "Color Legend:",
           colors = c("black", "white", "black"),
           outline.color = "white") + 
  labs(x = "", y = "", 
       title = "Correlation matrix: filing-related length measures",
       subtitle = "Note: Pearson correlations coefficients are displayed. Sample size = 64,527") +
  theme(panel.grid.major = element_blank()) +
  geom_vline(xintercept = 1:ncol(cormat_lengthvars) - 0.5, colour = "white", size = 2) +
  geom_hline(yintercept = 1:ncol(cormat_lengthvars) - 0.5, colour = "white", size = 2) + 
  theme(axis.text.x = element_text(size = 11, angle = 45), 
        axis.text.y = element_text(size = 11, angle = 0))

ggsave(filename = "/Users/kevin/Desktop/lm_testdata/cormat_length-measures.pdf",
       width = 297, height = 210, units = "mm")

# delete redundant df's
rm(list = setdiff(ls(), c("DB4", "DTM_N", "volas", 
                          "PFRV_diers", "PFRV_survivors")))

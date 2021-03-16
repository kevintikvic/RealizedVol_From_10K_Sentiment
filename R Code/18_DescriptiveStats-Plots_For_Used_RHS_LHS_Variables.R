
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: RHS_LHS_stats_pics                                                        #
#     Description:  1) Load LHS + RHS variables and create descr. stats / plots           #
#                                                                                         #
#     Date (last updated): August 28th, 2018                                              #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries / Settings ----------------------------------------------------
rm(list=ls()) # clear environment 
gc()
options(scipen = 999)
# setwd("/Volumes/LaCie/LM_data/")
setwd("/Users/kevin/Desktop/lm_testdata")

library(ggpubr)
library(scales)
# install.packages("GGally")
library(GGally)
library(colorspace)
# install.packages("ggExtra")
library(ggExtra)
library(tidyverse)
library(ggcorrplot)
library(RColorBrewer)
# install.packages("lsa")
library(lsa)
# install.packages("corrplot")
library(corrplot)
# install.packages("Matrix")
library(Matrix)
library(xtable)
library(stargazer)

load("~/Desktop/lm_testdata/DB5_316x46483.RData")

# LM N: Centroid Vectors  -------------------------------------------------
# add filing year as last column
DTM_N$REPYEAR <- DB4$repyear

# calculate mean count per filing for every year
centr.vect.N <- DTM_N %>%
  as_data_frame %>% 
  group_by(REPYEAR) %>%
  summarise_at(vars(-REPYEAR), funs(mean(., na.rm = T)))

# transpose it in col-vector form
centr.vect.N <- t(centr.vect.N[, -1])

# calculate cor. coef. 
N.cormat <- cor(centr.vect.N) # corr matrix 
N.cormat.pvals <- cor_pmat(centr.vect.N) # p values
colnames(N.cormat) <- rownames(N.cormat) <- unique(DB4$repyear) # labels
colnames(N.cormat.pvals) <- rownames(N.cormat.pvals) <-  unique(DB4$repyear) # labels

# cos similarity
N.cossim.mat <- lsa::cosine(centr.vect.N)
colnames(N.cossim.mat) <- rownames(N.cossim.mat) <- unique(DB4$repyear)

# plot those 2 similarities
ggcorrplot(N.cormat, 
           type = "upper", show.diag = T, 
           lab = T, lab_size = 3, lab_col = "white", 
           p.mat = N.cormat.pvals, 
           sig.level = .05, pch.col = "red", pch.cex = 20, 
           legend.title = "Color Legend:",
           colors = c("black", "white", "black"),
           outline.color = "white") + 
  labs(x = "",
       y = "", 
       title = "Correlation matrix: centroid vectors per year (k = N)",
       subtitle = "Note: Pearson correlations coefficients are displayed. Sample size = 64,527.") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 90, color = "black"), 
        axis.text.y = element_text(size = 9, angle = 00, color = "black")) +
  scale_x_discrete(limit = c(unique(DB4$repyear))) +
  scale_y_discrete(limit = c(unique(DB4$repyear)))

ggsave(filename = "/Users/kevin/Desktop/lm_testdata/cormat_centroids_LM-NEG.pdf",
       width = 297, height = 210, units = "mm")

ggcorrplot(N.cossim.mat, 
           type = "upper", show.diag = T, 
           lab = T, lab_size = 3, lab_col = "white",
           legend.title = "Color Legend:",
           colors = c("black", "white", "black"),
           outline.color = "white") + 
  labs(x = "",
       y = "", 
       title = "Cosine similarity matrix: centroid vectors per year (k = N)",
       subtitle = "Note: Sample size = 64,527.") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 90, color = "black"), 
        axis.text.y = element_text(size = 9, angle = 00, color = "black")) +
  scale_x_discrete(limit = c(unique(DB4$repyear))) +
  scale_y_discrete(limit = c(unique(DB4$repyear)))

ggsave(filename = "/Users/kevin/Desktop/lm_testdata/cosmat_centroids_LM-NEG.pdf",
       width = 297, height = 210, units = "mm")

# instead of 24x24 matrix, split into 6 groups each 4y:
# add GROUP.var as last column
DTM_N$GROUP <- 1 # fill all, will be kept for 1994-1997 only
DTM_N$GROUP[DTM_N$REPYEAR %in% c("1998", "1999", "2000", "2001")] <- 2 # 1998 - 2001
DTM_N$GROUP[DTM_N$REPYEAR %in% c("2002", "2003", "2004", "2005")] <- 3 # 2002 - 2005
DTM_N$GROUP[DTM_N$REPYEAR %in% c("2006", "2007", "2008", "2009")] <- 4 # 2006 - 2009
DTM_N$GROUP[DTM_N$REPYEAR %in% c("2010", "2011", "2012", "2013")] <- 5 # 2010 - 2013
DTM_N$GROUP[DTM_N$REPYEAR %in% c("2014", "2015", "2016", "2017")] <- 6 # 2014 - 2017

# calculate mean count per filing for every year
centr.vect.N.6g <- DTM_N %>%
  group_by(GROUP) %>%
  summarise_at(vars(-GROUP), funs(mean(., na.rm = T)))

centr.vect.N.6g <- t(centr.vect.N.6g[, -1])

# calculate cor. coef. 
N.cormat.6g <- cor(centr.vect.N.6g)
N.cormat.pvals.6g <- cor_pmat(centr.vect.N.6g)
colnames(N.cormat.6g) <- rownames(N.cormat.6g) <- paste("Group", unique(DTM_N$GROUP))
colnames(N.cormat.pvals.6g) <- rownames(N.cormat.pvals.6g) <- paste("Group", unique(DTM_N$GROUP))

# cos similarity
N.cossim.mat.6g <- lsa::cosine(centr.vect.N.6g)
colnames(N.cossim.mat.6g) <- rownames(N.cossim.mat.6g) <- paste("Group", unique(DTM_N$GROUP))

# plot those 2 similarities
ggcorrplot(N.cormat.6g, 
           type = "upper", show.diag = T, 
           lab = T, lab_size = 3, lab_col = "white", 
           p.mat = N.cormat.pvals.6g, 
           sig.level = .05, pch.col = "red", pch.cex = 20, 
           legend.title = "Color Legend:",
           colors = c("black", "white", "black"),
           outline.color = "white") + 
  labs(x = "",
       y = "", 
       title = "Correlation matrix: centroid vectors per 4-year groups (k = N)",
       subtitle = "Note: Pearson correlations coefficients are displayed. Sample size = 64,527.") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 90, color = "black"), 
        axis.text.y = element_text(size = 9, angle = 00, color = "black")) +
  scale_x_discrete(limit = c(paste("Group", unique(DTM_N$GROUP)))) +
  scale_y_discrete(limit = c(paste("Group", unique(DTM_N$GROUP))))

ggsave(filename = "/Users/kevin/Desktop/lm_testdata/cormat_groups_centroids_LM-NEG.pdf",
       width = 297, height = 210, units = "mm")

ggcorrplot(N.cossim.mat.6g, 
           type = "upper", show.diag = T, 
           lab = T, lab_size = 3, lab_col = "white",
           legend.title = "Color Legend:",
           colors = c("black", "white", "black"),
           outline.color = "white") + 
  labs(x = "",
       y = "", 
       title = "Cosine similarity matrix: centroid vectors per 4-year groups (k = N)",
       subtitle = "Note: Sample size = 64,527.") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 90, color = "black"), 
        axis.text.y = element_text(size = 9, angle = 00, color = "black")) +
  scale_x_discrete(limit = c(paste("Group", unique(DTM_N$GROUP)))) +
  scale_y_discrete(limit = c(paste("Group", unique(DTM_N$GROUP))))

ggsave(filename = "/Users/kevin/Desktop/lm_testdata/cosmat_groups_centroids_LM-NEG.pdf",
       width = 297, height = 210, units = "mm")

# Remove excess variables:
rm(N.cormat, N.cormat.6g, N.cormat.pvals, N.cormat.pvals.6g,
   cos.sim.mat, centr_vect, centr.vect.N, centr.vect.N.6g, N.cossim.mat,
   N.cossim.mat.6g)

# CorrMat Volas -----------------------------------------------------------

# 1) LHS / PFRV measures: RV, sq. ret., abs. ret.
# 2) LHS with RHS, i.e, basic MZ-regression inputs

# ad 1:
volas <- DB5 %>% 
  select(txtfilename, 
         PFRV_main, log_pfrv,
         PFRV_sq_ret, PFRV_abs_ret, # 2 robustness PFRV measures
         GARCH_1step, log_garch, # 1/2 RHS MZ inpits
         GJR_1step, log_gjr) # 2/2 RHS MZ inpits

# add logged versions for the robustness measures
volas <- volas %>% 
  mutate(log_pfrv_sq = log(PFRV_sq_ret),
         log_pfrv_abs = log(PFRV_abs_ret))

# create corr. matrix
PFRV_measures <- volas %>% 
  select(PFRV_main, PFRV_sq_ret, PFRV_abs_ret) %>% 
  as.matrix()

log_PFRV_measures <- volas %>% 
  select(log_pfrv, log_pfrv_sq, log_pfrv_abs) %>% 
  as.matrix()

# Extract no. of observations that will be used for corr. calculation
# we will use CASEWISE inst. of PAIRWISE corr. so as to allow comparison
x <- !is.na(PFRV_measures); t(x) %*% x
min(t(x) %*% x) # this is what will survive after na.omit = casewise deletion

x <- !is.na(log_PFRV_measures); t(x) %*% x
min(t(x) %*% x) # this is what will survive after na.omit = casewise deletion

# Source Code: https://stackoverflow.com/questions/9413457/count-the-number-of-valid-observations-no-na-pairwise-in-a-data-frame
# Pair vs. Case: 
  # http://documentation.statsoft.com/STATISTICAHelp.aspx?path=Basicstats/BasicStatistics/Overview/Correlations/CorrelationsIntroductoryOverviewCasewisevsPairwiseDeletionofMissingData
  # http://www.statisticssolutions.com/missing-data-listwise-vs-pairwise/
  # http://bwlewis.github.io/covar/missing.html

# create corr. matrix
cormat_PFRV_measures <- cor(PFRV_measures, use = "complete.obs")
cormat_log_PFRV_measures <- cor(log_PFRV_measures, use = "complete.obs")
cormat_PFRV_measures; cormat_log_PFRV_measures

# create p-value matrix
pmat_PFRV_measures <- cor_pmat(PFRV_measures)
pmat_log_PFRV_measures <- cor_pmat(log_PFRV_measures)
pmat_PFRV_measures; pmat_log_PFRV_measures

# combine the two into 1 matrix (lower = logs, upper = origs)
# source code: https://stackoverflow.com/questions/38031892/combine-upper-tri-and-lower-tri-matrices-into-a-single-data-frame
cormat_comb_PFRV_measures <- matrix(NA, 3, 3)
cormat_comb_PFRV_measures[upper.tri(cormat_comb_PFRV_measures, diag = T)] <- 
  cormat_PFRV_measures[upper.tri(cormat_PFRV_measures, diag = T)]
cormat_comb_PFRV_measures[lower.tri(cormat_comb_PFRV_measures, diag = F)] <- 
  cormat_log_PFRV_measures[lower.tri(cormat_log_PFRV_measures, diag = F)]

# change labels for combined corr. mat
colnames(cormat_comb_PFRV_measures) <- 
  rownames(cormat_comb_PFRV_measures) <- 
  c("Realized Volatility", "Squared Returns", "Absolute Returns")

# export / stargazer 'em:
stargazer(cormat_comb_PFRV_measures, 
          summary = F, type = "latex",
          rownames = T, colnames = T, out.header = F, header = F,
          title = "Correlation matrix of PFRV measures",
          label = "tab: pfrv_measures",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/cormat_pfrv_robustness_measures",
          digit.separate = 3,
          digit.separator = ",",
          digits = 3, decimal.mark = ".", initial.zero = F,
          font.size = "small", align = F)

# MZ cormat
MZ_inputs <- volas %>% 
  select(PFRV_main, GARCH_1step, GJR_1step) %>% 
  as.matrix()
x <- !is.na(MZ_inputs); t(x) %*% x; min(t(x) %*% x)
cormat_MZ_inputs <- cor(MZ_inputs)
pmat_MZ_inputs <- cor_pmat(MZ_inputs)

log_MZ_inputs <- volas %>% 
  select(log_pfrv, log_garch, log_gjr) %>% 
  as.matrix()
x <- !is.na(log_MZ_inputs); t(x) %*% x; min(t(x) %*% x)
cormat_log_MZ_inputs <- cor(log_MZ_inputs)
pmat_log_MZ_inputs <- cor_pmat(log_MZ_inputs)

cormat_comb_MZ <- matrix(NA, 3, 3)
cormat_comb_MZ[upper.tri(cormat_comb_MZ, diag = T)] <- 
  cormat_MZ_inputs[upper.tri(cormat_MZ_inputs, diag = T)]
cormat_comb_MZ[lower.tri(cormat_comb_MZ, diag = F)] <- 
  cormat_log_MZ_inputs[lower.tri(cormat_log_MZ_inputs, diag = F)]

colnames(cormat_comb_MZ) <- 
  rownames(cormat_comb_MZ) <- 
  c("PFRV", "GARCH(1,1)", "GJR-GARCH(1,1)")

stargazer(cormat_comb_MZ, 
          summary = F, type = "latex",
          rownames = T, colnames = T, out.header = F, header = F,
          title = "Correlation matrix: PFRV versus time-series forecasts",
          label = "tab: pfrv_tsfc",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/cormat_pfrv_tsfc",
          digit.separate = 3,
          digit.separator = ",",
          digits = 3, decimal.mark = ".", initial.zero = F,
          font.size = "small", align = F)

# plot:
# ggcorrplot(cor.lhs.volas.0pw, type = "upper", show.diag = T, method = "square",
#            lab = T, lab_size = 10, lab_col = "white", 
#            p.mat = pvals.lhs.volas.0pw, 
#            sig.level = .05, pch.col = "red", pch.cex = 20, 
#            legend.title = "Color Legend:",
#            colors = c("black", "white", "black"),
#            outline.color = "white") + 
#   labs(x = "", y = "", 
#        title = "Correlation matrix: post-filing volatility measures",
#        subtitle = "Note: Pearson correlations coefficients are displayed. Sample size = 63,637.") +
#   theme(panel.grid.major = element_blank()) +
#   geom_vline(xintercept = 1:ncol(cor.lhs.volas.0pw) - 0.5, colour = "white", size = 2) +
#   geom_hline(yintercept = 1:ncol(cor.lhs.volas.0pw) - 0.5, colour = "white", size = 2) + 
#   theme(axis.text.x = element_text(size = 11, angle = 45), 
#         axis.text.y = element_text(size = 11, angle = 0))
# 
# ggsave(filename = "/Users/kevin/Desktop/lm_testdata/cormat_LHS_volas.pdf",
#        width = 297, height = 210, units = "mm")

# delete redundant df's
rm(list = setdiff(ls(), c("DB5", "volas", 
                          "PFRV_diers", "PFRV_survivors")))

# CorrMat RHS -------------------------------------------------------------

# add to DB5 the variable FIN:
DTM_KT <- read.delim("/Users/kevin/Desktop/lm_testdata/sep_DTMs/DTM_KT.txt",
                     sep = ";", stringsAsFactors = F, dec = ",")
DTM_KT <- DTM_KT[!rownames(DTM_KT) %in% unlist(list(PFRV_diers)), ]
sum(rownames(DTM_KT) %in% unlist(list(PFRV_survivors)))
sum(colSums(DTM_KT) == 0)
DTM_KT <- DTM_KT[, which(colSums(DTM_KT) != 0)]

FIN.var <- rowSums(DTM_KT) %>% log1p
hist(FIN.var, breaks = 60); summary(FIN.var)

DB5 <- DB5 %>% mutate(FIN = FIN.var)

rm(list = setdiff(ls(), c("DB5", "volas", 
                          "PFRV_diers", "PFRV_survivors")))

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

# extract all vars
pfrv_allcontrols <- DB5 %>% 
  select(log_pfrv, log_garch, log_gjr,
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
                                                            "GARCH",
                                                            "GJR",
                                                            "NEG", 
                                                            "POS", 
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
          title = "Correlation matrix: PFRV versus all independent variables",
          label = "tab: pfrv_all_RHS",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/cormat_pfrv_all_RHS",
          digit.separate = 3,
          digit.separator = ",",
          digits = 3, decimal.mark = ".", initial.zero = F,
          font.size = "small", align = F, float.env = "sidewaystable")


# Descr Stats Table -------------------------------------------------------

pfrv_allcontrols <- as.data.frame(pfrv_allcontrols)
colnames(pfrv_allcontrols) <- 
  c("PFRV", "GARCH", "GJR", "NEG", "POS", "ASSERT", "UNCERT", 
    "LITI", "GFS", "FIN", "SIZE", "BTM", "TRVOL", "VIX", "LEVER")

stargazer(pfrv_allcontrols, 
          summary = T, type = "latex", median = T,
          rownames = T, colnames = T, out.header = F, header = F,
          title = "Sample summary statistics",
          label = "tab: descr_stats_allvars",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/descr_stats_allvars",
          digit.separate = 3,
          digit.separator = ",",
          digits = 2, decimal.mark = ".", initial.zero = F,
          font.size = "small", align = F, float.env = "table")

# delete redundant df's
rm(list = setdiff(ls(), c("DB5", "volas", 
                          "PFRV_diers", "PFRV_survivors")))

# Vola: LHS vs. GARCHs ----------------------------------------------------
# 
# volaplot1 <- ggplot(delta_pfrv_garch, 
#                     aes(x = idx,
#                         y = delta_pfrv_garch)) + 
#   geom_jitter(size = .01, 
#               aes(col = factor(filing_year))) +
#   labs(subtitle = "", 
#        y = "PFRV - GARCH(1,1) forecast\n", 
#        x = "\nFiling No.", 
#        title = "(A) Prediction Error: Scatter Diagram", 
#        caption = "") +
#   theme(legend.key.size = unit(2, "mm"), 
#         legend.text = element_text(size = 8),
#         legend.title = element_text(size = 8),
#         panel.background = element_blank(),
#         axis.ticks = element_blank(),
#         axis.line = element_line(size = .25, 
#                                  color = "black"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_line(size = .05, 
#                                           color = "black")) +
#   scale_y_continuous(breaks = pretty_breaks(n = 10), 
#                      labels = comma, 
#                      limits = c(-.55, .55)) +
#   scale_x_continuous(breaks = pretty_breaks(n = 10), 
#                      labels = comma, 
#                      limits = c(0, 65000)) + 
#   scale_fill_discrete(name = "Filing Year: ",
#                       labels = unique(DB5$repyear)) +
#   guides(fill = guide_legend(nrow = 1), colour = F)

# Scattermatrix: PFRV vs. GARCH, PFRV vs. GJR
scat1 <- ggplot(volas,
                aes(x = log_garch,
                    y = log_pfrv)) +
  geom_jitter(size = .0001, alpha = .075) +
  geom_smooth(method = "lm", color = "#332288",
              linetype = "solid", size = 0.75) +
  labs(subtitle = "", 
       y = "PFRV\n", 
       x = "\nGARCH(1,1) forecast", 
       title = "(A) PFRV vs. GARCH(1,1) forecast") +
  theme(legend.key.size = unit(2, "mm"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(size = .25, 
                                 color = "black")) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))

scat2 <- ggplot(volas,
                aes(x = log_gjr,
                    y = log_pfrv)) +
  geom_jitter(size = .0001, alpha = .075) +
  geom_smooth(method = "lm", color = "#332288",
              linetype = "solid", size = 0.75) +
  labs(subtitle = "", 
       y = "PFRV\n", 
       x = "\nGJR-GARCH(1,1) forecast", 
       title = "(B) PFRV vs. GJR-GARCH(1,1) forecast") +
  theme(legend.key.size = unit(2, "mm"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(size = .25, 
                                 color = "black")) +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10))

# add marginal distributions:
scat1 <- ggMarginal(scat1, 
                    type = "histogram", 
                    fill = "#88CCEE",
                    binwidth = 0.15)
scat2 <- ggMarginal(scat2, 
                    type = "histogram", 
                    fill = "#88CCEE", 
                    binwidth = 0.15)

mzscats <- ggarrange(scat1, scat2, ncol = 2, nrow = 1, align = "h")
annotate_figure(mzscats, 
                bottom = text_grob("\nAll variables are displayed logarithmic form. Sample Size: 46,483."))

ggsave(file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Images/MZ_scatters.pdf", 
       width = 297, height = 210, units = "mm")

# alternative: 2D bin plot indicating "density" as well
# scat2 <- ggplot(volas,
#                 aes(x = log_gjr,
#                     y = log_pfrv)) +
#   geom_bin2d(binwidth = c(0.25, 0.25)) +
#   labs(subtitle = "", 
#        y = "PFRV\n", 
#        x = "\nGJR-GARCH(1,1) forecast", 
#        title = "(B) PFRV vs. GJR-GARCH(1,1) forecast", 
#        caption = "All variables are displayed in logarithmic form. Sample Size: 46,483.") +
#   theme(legend.key.size = unit(2, "mm"), 
#         legend.text = element_text(size = 8),
#         legend.title = element_text(size = 8),
#         panel.background = element_blank(),
#         axis.ticks = element_blank(),
#         axis.line = element_line(size = .25, 
#                                  color = "black")) +
#   scale_fill_continuous() +
#   scale_y_continuous(breaks = pretty_breaks(n = 10)) +
#   scale_x_continuous(breaks = pretty_breaks(n = 10))

# delta histograms
delta_pfrv_garch <- volas$log_garch - volas$log_pfrv
delta_pfrv_gjr <- volas$log_gjr - volas$log_pfrv
summary(delta_pfrv_garch); summary(delta_pfrv_gjr)

histo1 <- ggplot(as.data.frame(delta_pfrv_garch),
                 aes(delta_pfrv_garch)) + 
  geom_histogram(binwidth = .25, fill = "#88CCEE", color = "black") + 
  labs(subtitle = "", 
       y = "No. of filings\n", 
       x = "\nGARCH(1,1) - PFRV (Forecast Error)", 
       title = "(A) GARCH(1,1) - PFRV") +
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

histo2 <- ggplot(as.data.frame(delta_pfrv_gjr),
                 aes(delta_pfrv_gjr)) + 
  geom_histogram(binwidth = .25, fill = "#88CCEE", color = "black") + 
  labs(subtitle = "", 
       y = "No. of filings\n", 
       x = "\nGJR-GARCH(1,1) - PFRV (Forecast Error)", 
       title = "(B) GJR-GARCH(1,1) - PFRV") +
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

deltahists <- ggarrange(histo1, histo2, ncol = 2, nrow = 1, align = "h")
annotate_figure(deltahists, 
                bottom = text_grob("\nDifferences are obtained from log-transformed volatility variables. Sample Size: 46,483."))

ggsave(file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Images/delta_pfrv_vs_tsfc.pdf", 
       width = 297, height = 150, units = "mm")

# Vola quintiles ----------------------------------------------------------
# vola.quints <- volas %>% 
#   select(log_pfrv) %>% 
#   mutate(quintile = ntile(log_pfrv, 5)) %>% 
#   group_by(quintile) %>% 
#   summarise_all(median)
# 
# score.neg.quints <- DB5 %>% 
#   select(NEG_SENT) %>% 
#   mutate(quintile = ntile(NEG_SENT, 5)) %>% 
#   group_by(quintile) %>% 
#   summarise_all(median)
# 
# score.pos.quints <- DB5 %>% 
#   select(POS_SENT) %>% 
#   mutate(quintile = ntile(POS_SENT, 5)) %>% 
#   group_by(quintile) %>% 
#   summarise_all(median)
# 
# score.assert.quints <- DB5 %>% 
#   select(ASSERT) %>% 
#   mutate(quintile = ntile(ASSERT, 5)) %>% 
#   group_by(quintile) %>% 
#   summarise_all(median)
# 
# score.uncert.quints <- DB5 %>% 
#   select(UNCERT) %>% 
#   mutate(quintile = ntile(UNCERT, 5)) %>% 
#   group_by(quintile) %>% 
#   summarise_all(median)
# 
# score.liti.quints <- DB5 %>% 
#   select(LITI) %>% 
#   mutate(quintile = ntile(LITI, 5)) %>% 
#   group_by(quintile) %>% 
#   summarise_all(median)
# 
# vola_vs_negsent <- inner_join(vola.quints, score.neg.quints,
#                               by = "quintile")
# 
# quinties <- cbind(vola.quints, score.neg.quints, score.pos.quints,
#                   score.assert.quints, score.uncert.quints, score.liti.quints)
# 
# #install.packages("reshape2")
# library(reshape2)
# quinties <- melt(quinties, id = "quintile")
# quinties$quintile <- quinties$value[1:5]
# quinties <- quinties[-(1:5), ]
# 

# # quintile plots ----------------------------------------------------------
# ggplot(quinties, aes(vola.quints)) + 
#   geom_line(aes(y = score.neg.quints, colour = "score.neg.quints")) + 
#   geom_line(aes(y = score.pos.quints, colour = "score.pos.quints"))
# 
# ggplot(quinties, aes(x = value, 
#                      y = quintile, 
#                      colour = variable)) + 
#   geom_line(aes(group = variable)) + 
#   geom_point()
# 
# # plot quintiles vs. vola:
# # Note that, the argument fill can be used only for the point shapes 21 to 25
# 
# ggplot(vola_vs_negsent,
#        aes(x = NEG_SENT, y = log_pfrv)) +
#   geom_point(shape = 22, fill = "black", 
#              color = "black", size = 3) +
#   geom_line()

# Vola Ranking ------------------------------------------------------------

# Tsai Wang 2016 do not use quintiles, but rather 5 categories based on bell shape

bell.volas <- volas$log_pfrv %>% as_tibble %>% 
  mutate(rankoftsai = NA)

m <- mean(bell.volas$value); s <- sd(bell.volas$value); m; s

# assign ranks
bell.volas$rankoftsai[bell.volas$value <= m - 2*s] <- 1
bell.volas$rankoftsai[(bell.volas$value <= m - s) & 
                        (bell.volas$value > m - 2*s)] <- 2
bell.volas$rankoftsai[(bell.volas$value <= m + s) & 
                        (bell.volas$value > m - s)] <- 3
bell.volas$rankoftsai[(bell.volas$value <= m + 2*s) & 
                        (bell.volas$value > m + s)] <- 4
bell.volas$rankoftsai[(bell.volas$value > m + 2*s) ] <- 5

# add years to differentiate
bell.volas$filingyear <- DB5$repyear

bell.volas$value %>% summary
bell.volas$value %>% hist(breaks = 50)

bell.volas$rankoftsai %>% summary
bell.volas$rankoftsai %>% hist(breaks = 60)
table(bell.volas$rankoftsai)

kevcolors <- gray.colors(length(unique(DB5$repyear)))

ggplot(bell.volas, aes(rankoftsai)) + 
  geom_histogram(aes(fill = factor(filingyear)), 
                 binwidth = .5, 
                 col = "black", 
                 size = .1,
                 position = position_dodge(width = .9)) +
  scale_y_continuous(breaks = pretty_breaks(n = 5), 
                     labels = comma, 
                     expand = c(0, 0), 
                     limits = c(0, 2500)) +
  scale_fill_manual(values = kevcolors)

# Scores over time --------------------------------------------------------

LM.vars.over.time <- DB5 %>% 
  mutate(repyear = as.factor(repyear)) %>% 
  select(repyear, NEG_SENT, POS_SENT, ASSERT, UNCERT, LITI) %>% 
  group_by(repyear) %>% 
  summarise_all(median) %>% 
  mutate(repyear = 1999:2017)

# LM.vars.over.time <- melt(LM.vars.over.time, id.vars = "repyear")

p <- ggplot(LM.vars.over.time, aes(repyear, NEG_SENT))
p <- ggplot(LM.vars.over.time, aes(repyear, POS_SENT))
p <- ggplot(LM.vars.over.time, aes(repyear, ASSERT))
p <- ggplot(LM.vars.over.time, aes(repyear, UNCERT))
p <- ggplot(LM.vars.over.time, aes(repyear, LITI))

p + 
  geom_line(col = "darkgray", 
            size = 1) + 
  geom_point(size = 2.5, 
             col = "black") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "black",
                                          size = 0.1),
        axis.ticks = element_blank(),
        panel.background = element_blank())

# score histograms
neg <- ggplot(DB5, aes(x = NEG_SENT)) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  scale_x_continuous(expand = c(0, 0), labels = comma, 
                     breaks = pretty_breaks(10)) +
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .05, 
                                          color = "black",
                                          linetype = "solid"),
        axis.ticks = element_blank(),
        axis.line = element_line(size = .25, 
                                 color = "black")) + 
  geom_histogram(bins = 50, fill = "#88CCEE", color = "black") +
  labs(y = "No. of filings\n")

pos <- ggplot(DB5, aes(x = POS_SENT)) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  scale_x_continuous(expand = c(0, 0), labels = comma, 
                     breaks = pretty_breaks(5)) +
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .05, 
                                          color = "black",
                                          linetype = "solid"),
        axis.ticks = element_blank(),
        axis.line = element_line(size = .25, 
                                 color = "black")) + 
  geom_histogram(bins = 50, fill = "#88CCEE", color = "black") +
  labs(y = "No. of filings\n")

assert <- ggplot(DB5, aes(x = ASSERT)) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  scale_x_continuous(expand = c(0, 0), labels = comma, 
                     breaks = pretty_breaks(5)) +
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .05, 
                                          color = "black",
                                          linetype = "solid"),
        axis.ticks = element_blank(),
        axis.line = element_line(size = .25, 
                                 color = "black")) + 
  geom_histogram(bins = 50, fill = "#88CCEE", color = "black") +
  labs(y = "No. of filings\n")

uncert <- ggplot(DB5, aes(x = UNCERT)) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  scale_x_continuous(expand = c(0, 0), labels = comma, 
                     breaks = pretty_breaks(10)) +
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .05, 
                                          color = "black",
                                          linetype = "solid"),
        axis.ticks = element_blank(),
        axis.line = element_line(size = .25, 
                                 color = "black")) + 
  geom_histogram(bins = 50, fill = "#88CCEE", color = "black") +
  labs(y = "No. of filings\n")

liti <- ggplot(DB5, aes(x = LITI)) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  scale_x_continuous(expand = c(0, 0), labels = comma, 
                     breaks = pretty_breaks(5)) +
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .05, 
                                          color = "black",
                                          linetype = "solid"),
        axis.ticks = element_blank(),
        axis.line = element_line(size = .25, 
                                 color = "black")) + 
  geom_histogram(bins = 50, fill = "#88CCEE", color = "black") +
  labs(y = "No. of filings\n")

readability <- ggplot(DB5, aes(x = log_GFS)) +
  scale_y_continuous(expand = c(0, 0), labels = comma) +
  scale_x_continuous(expand = c(0, 0), labels = comma, 
                     breaks = pretty_breaks(10)) +
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = .05, 
                                          color = "black",
                                          linetype = "solid"),
        axis.ticks = element_blank(),
        axis.line = element_line(size = .25, 
                                 color = "black")) + 
  geom_histogram(bins = 50, fill = "#88CCEE", color = "black") + 
  labs(x = "GFS", y = "No. of filings\n")

scorehists <- ggarrange(neg, pos, assert, uncert, liti, readability, 
                        ncol = 3, nrow = 2, align = "h")

annotate_figure(scorehists, 
                bottom = text_grob("All variables (except for GFS) are VIBTW-scores with weights estimated from the full sample (i.e., 46,483 filings from 1999 to 2017)."))

ggsave(file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Images/histos_LM_scores.pdf", 
       width = 297, height = 200, units = "mm")

# cormat LM scores only:
# cor(cbind(DB5$NEG_SENT, DB5$POS_SENT, DB5$ASSERT,
#           DB5$UNCERT, DB5$LITI, DB5$GFS, DB5$FIN))

# delete redundant df's
rm(list = setdiff(ls(), c("DB5", "volas", 
                          "PFRV_diers", "PFRV_survivors")))

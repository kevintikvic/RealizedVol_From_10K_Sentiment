
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: MZ_regressions_C_prefiling                                                #
#     Description:  1) instead of GARCH / GJR uses pre-filing RV as TS_FC variable        #
#                   2) estimate 5 annual regressions with this RHS variable               #
#                   3) estimate pooled OLS regression with this RHS & 7 BM TW schemes     #
#                                                                                         #
#     Date (last updated): September 2nd, 2018                                            #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries / Settings ----------------------------------------------------
rm(list=ls()); gc()
options(scipen = 999)
library(stargazer); library(tidyverse)
setwd("/Users/kevin/Desktop/lm_testdata/")

# load data
load("./regressions/A/A_MZ_inputs.RData")

# extract the RHS that all models will have in common:
common.rhs <- MZ_RHS[1:159] %>% cbind(., FIN = MZ_RHS$FIN) %>% as_tibble()

# seems I forgot size variable; load via DB5:
load("./DB5_316x46483.RData")
common.rhs <- common.rhs %>% cbind(., 
                                   (DB5 %>% 
                                      filter(repyear >= 2013) %>% 
                                      select(size_ta))) %>% 
  as_tibble()

# load pre-filing RV vars:
load("/Users/kevin/Desktop/lm_testdata/prefilingvola_46483.RData")

finallist_PFRV_volas <- finallist_PFRV_volas[((nrow(finallist_PFRV_volas) - nrow(common.rhs) + 1):(nrow(finallist_PFRV_volas))), ]

common.rhs <- common.rhs %>% mutate(prefiling_RV = finallist_PFRV_volas$prefiling_RV_main_1_5)

common.rhs$prefiling_RV <- ifelse(common.rhs$prefiling_RV == 0, 
                                  NA, 
                                  log(common.rhs$prefiling_RV))

common.rhs$prefiling_RV[is.na(common.rhs$prefiling_RV)] <- mean(common.rhs$prefiling_RV, na.rm = T)

sum(is.na(common.rhs$prefiling_RV)); sum(common.rhs$prefiling_RV == 0, na.rm = T)

# A(1): VIBTW -------------------------------------------------------------
rhs.vibtw <- MZ_RHS %>% select(ends_with("_VIBTW"))
colnames(rhs.vibtw) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
                      rhs.vibtw,
                      GFS = common.rhs$log_GFS,
                      FIN = common.rhs$FIN,
                      SIZE = common.rhs$size_ta,
                      BTM = common.rhs$log_BTM,
                      TRVOL = common.rhs$log_TVOL,
                      VIX = common.rhs$med_VIX_pre,
                      LEVER = common.rhs$leverage,
                      common.rhs[8:159]) %>% as_tibble() # all dummy controls

C1A <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(2): TFIDF -------------------------------------------------------------
rhs.tfidf <- MZ_RHS %>% select(ends_with("_TFIDF"))
colnames(rhs.tfidf) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
                rhs.tfidf,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

C2A <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(3): RFIDF -------------------------------------------------------------
rhs.rfidf <- MZ_RHS %>% select(ends_with("_RFIDF"))
colnames(rhs.rfidf) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
                rhs.rfidf,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

C3A <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(4): WFIDF_1PLOG -------------------------------------------------------------
rhs.wfidf.1plog <- MZ_RHS %>% select(ends_with("WFIDF_1P"))
colnames(rhs.wfidf.1plog) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
                rhs.wfidf.1plog,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

C4A <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(5): WFIDF_LOG1P -------------------------------------------------------------
rhs.wfidf.log1p <- MZ_RHS %>% select(ends_with("WFIDF_P1"))
colnames(rhs.wfidf.log1p) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
                rhs.wfidf.log1p,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

C5A <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(6): WF_1PLOG -------------------------------------------------------------
rhs.wf.1p <- MZ_RHS %>% select(ends_with("WF_1P"))
colnames(rhs.wf.1p) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
                rhs.wf.1p,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

C6A <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(7): WF_LOG1P -------------------------------------------------------------
rhs.wf.p1 <- MZ_RHS %>% select(ends_with("WF_P1"))
colnames(rhs.wf.p1) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
                rhs.wf.p1,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

C7A <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(8): TFMAX -------------------------------------------------------------
rhs.tfmax <- MZ_RHS %>% select(ends_with("_TFMAX"))
colnames(rhs.tfmax) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
                rhs.tfmax,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

C8A <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# Table Export ------------------------------------------------------------

stargazer(C1A, C2A, C3A, C4A, C5A, C6A, C7A, C8A,
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/C_MZ_regression_pooled",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: C_MZ_regression_pooled", 
          title = "Robustness Check: Pre-Filing Realized Volatility Instead of Time-Series Inputs (Pooled OLS)",
          model.numbers = T,
          notes.align = "l", 
          notes = "***, **, and * denotes statistical significance at the one-, five- and ten-percent level, respectively. Standard errors are displayed in parentheses. TS_FC is the 1-week pre-filing realized volatility. Coefficients for boolean control variables (YRDUMMY, MTHDUMMY, WEEKDAYDUMMY, MONTHDAYDUMMY, SECTORDUMMY, and 10KDUMMY) are not displayed in the table. Column headers (1) through (8) refer to the weighting schemes in the following order: VIBTW, TFIDF, RFIDF, WFIDF 1PLOG, WFIDF LOG1P, WF 1PLOG, WF LOG1P, TFMAX. Each column is a pooled OLS regression, estimated for the whole out-of-sample period from 2013 to 2017. Weights for the VIBTW, TFIDF, RFIDF, WFIDF 1PLOG, and WFIDF LOG1P are estimated for a fixed training set (1999-2012).", 
          notes.append = F,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

# Variant B: ANNUAL REGRESSIONS + ONLY VIBTW ------------------------------

# STATIC ------------------------------------------------------------------
rm(list=ls()); gc()
options(scipen = 999)
library(stargazer); library(tidyverse)
setwd("/Users/kevin/Desktop/lm_testdata/")

# load data
load("./regressions/A/A_MZ_inputs.RData")

# extract the RHS that all models will have in common:
common.rhs <- MZ_RHS[1:159] %>% cbind(., FIN = MZ_RHS$FIN) %>% as_tibble()

# seems I forgot size variable; load via DB5:
load("./DB5_316x46483.RData")
common.rhs <- common.rhs %>% cbind(., 
                                   (DB5 %>% 
                                      filter(repyear >= 2013) %>% 
                                      select(size_ta, repyear))) %>% 
  as_tibble()

# load pre-filing RV vars:
load("/Users/kevin/Desktop/lm_testdata/prefilingvola_46483.RData")
finallist_PFRV_volas <- finallist_PFRV_volas[((nrow(finallist_PFRV_volas) - nrow(common.rhs) + 1):(nrow(finallist_PFRV_volas))), ]
common.rhs <- common.rhs %>% mutate(prefiling_RV = finallist_PFRV_volas$prefiling_RV_main_1_5)

common.rhs$prefiling_RV <- ifelse(common.rhs$prefiling_RV == 0, 
                                  NA, 
                                  log(common.rhs$prefiling_RV))

common.rhs$prefiling_RV[is.na(common.rhs$prefiling_RV)] <- mean(common.rhs$prefiling_RV, na.rm = T)

rhs.vibtw <- MZ_RHS %>% select(ends_with("_VIBTW"))
colnames(rhs.vibtw) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(         filingyear = common.rhs$repyear,
                      TS_FC = common.rhs$prefiling_RV, 
                      rhs.vibtw,
                      GFS = common.rhs$log_GFS,
                      FIN = common.rhs$FIN,
                      SIZE = common.rhs$size_ta,
                      BTM = common.rhs$log_BTM,
                      TRVOL = common.rhs$log_TVOL,
                      VIX = common.rhs$med_VIX_pre,
                      LEVER = common.rhs$leverage,
                      common.rhs[8:159]) %>% as_tibble() # all dummy controls

MZ_LHS <- MZ_LHS %>% mutate(filingyear = common.rhs$repyear)

# filter out the target year:
i <- 2013
for (i in 2013:2017) {
  MZ_RHS_CB1 <- RHS %>% filter(filingyear == i) %>% select(-filingyear)
  MZ_LHS_CB1 <- MZ_LHS %>% filter(filingyear == i) %>% select(-filingyear)
  assign(paste0("CB", i), 
         lm(as.matrix(MZ_LHS_CB1) ~ as.matrix(MZ_RHS_CB1)))
  i <- i + 1
}

stargazer(CB2013, CB2014, CB2015, CB2016, CB2017,
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/CB_MZ_static",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: CB_MZ_static", 
          title = "Robustness Check: Annual Augmented MZ-Regression (Pre-Filing Realized Volatility + Static Training Set)",
          model.numbers = T,
          notes.align = "l", 
          notes = "***, **, and * denotes statistical significance at the one-, five- and ten-percent level, respectively. Standard errors are displayed in parentheses. ", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

# EXTENDING ------------------------------------------------------------------
rm(list = ls()); gc()
setwd("/Users/kevin/Desktop/lm_testdata/")
load("/Users/kevin/Desktop/lm_testdata/prefilingvola_46483.RData")
finallist_PFRV_volas$repyear <- substr(finallist_PFRV_volas$txtfilename, 1, 4)
finallist_PFRV_volas$prefiling_RV_main_1_5 <- ifelse(finallist_PFRV_volas$prefiling_RV_main_1_5 == 0, 
                                                     NA, 
                                                     log(finallist_PFRV_volas$prefiling_RV_main_1_5))

i <- 2013
for (i in 2013:2017) {
  helpervar <- finallist_PFRV_volas[finallist_PFRV_volas$repyear %in% i,]
  load(paste0("./regressions/B/B2_MZ_inputs_", i, ".RData"))
  RHS <- cbind(         TS_FC = helpervar$prefiling_RV_main_1_5, 
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
                        MZ_RHS[8:159]) %>% as_tibble() # all dummy controls
  assign(paste0("CB", i), 
         lm(as.matrix(MZ_LHS) ~ as.matrix(RHS)))
  i <- i + 1
}

stargazer(CB2013, CB2014, CB2015, CB2016, CB2017,
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/CB_MZ_extending",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: CB_MZ_extending", 
          title = "Robustness Check: Annual Augmented MZ-Regression (Pre-Filing Realized Volatility + Extending Training Set)",
          model.numbers = T,
          notes.align = "l", 
          notes = "***, **, and * denotes statistical significance at the one-, five- and ten-percent level, respectively. Standard errors are displayed in parentheses. ", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

# ROLLING ------------------------------------------------------------------
rm(list = ls()); gc()
setwd("/Users/kevin/Desktop/lm_testdata/")
load("/Users/kevin/Desktop/lm_testdata/prefilingvola_46483.RData")
finallist_PFRV_volas$repyear <- substr(finallist_PFRV_volas$txtfilename, 1, 4)
finallist_PFRV_volas$prefiling_RV_main_1_5 <- ifelse(finallist_PFRV_volas$prefiling_RV_main_1_5 == 0, 
                                                     NA, 
                                                     log(finallist_PFRV_volas$prefiling_RV_main_1_5))

i <- 2013
for (i in 2013:2017) {
  helpervar <- finallist_PFRV_volas[finallist_PFRV_volas$repyear %in% i,]
  load(paste0("./regressions/B/B3_MZ_inputs_", i, ".RData"))
  RHS <- cbind(         TS_FC = helpervar$prefiling_RV_main_1_5, 
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
                        MZ_RHS[8:159]) %>% as_tibble() # all dummy controls
  assign(paste0("CB", i), 
         lm(as.matrix(MZ_LHS) ~ as.matrix(RHS)))
  i <- i + 1
}

stargazer(CB2013, CB2014, CB2015, CB2016, CB2017,
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/CB_MZ_rolling",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: CB_MZ_rolling", 
          title = "Robustness Check: Annual Augmented MZ-Regression (Pre-Filing Realized Volatility + Rolling Training Set)",
          model.numbers = T,
          notes.align = "l", 
          notes = "***, **, and * denotes statistical significance at the one-, five- and ten-percent level, respectively. Standard errors are displayed in parentheses. ", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

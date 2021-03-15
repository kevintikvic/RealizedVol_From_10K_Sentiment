
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: MZ_regressions_A                                                          #
#     Description:  1) load LHS and RHS for variant A (2013-2017 all-in-one regressions)  #
#                   2) regress LHS on RHS using 7 different weighting schemes             #
#                   3) export nice stargazer latex table for the different models         #
#                                                                                         #
#     Date (last updated): July 23th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries / Settings ----------------------------------------------------
rm(list=ls()) # clear environment 
gc()
options(scipen = 999)

library(stargazer)

# setwd("/Volumes/LaCie/LM_data/")
# setwd("/Users/kevin/Desktop/lm_testdata")
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
# note: this DF contains both GARCH and GJR variables, we will use either or

# A(1): VIBTW -------------------------------------------------------------
rhs.vibtw <- MZ_RHS %>% select(ends_with("_VIBTW"))
colnames(rhs.vibtw) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$log_garch, 
                      rhs.vibtw,
                      GFS = common.rhs$log_GFS,
                      FIN = common.rhs$FIN,
                      SIZE = common.rhs$size_ta,
                      BTM = common.rhs$log_BTM,
                      TRVOL = common.rhs$log_TVOL,
                      VIX = common.rhs$med_VIX_pre,
                      LEVER = common.rhs$leverage,
                      common.rhs[8:159]) %>% as_tibble() # all dummy controls

A1.GARCH <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

RHS <- cbind(TS_FC = common.rhs$log_gjr, 
                    rhs.vibtw,
                    GFS = common.rhs$log_GFS,
                    FIN = common.rhs$FIN,
                    SIZE = common.rhs$size_ta,
                    BTM = common.rhs$log_BTM,
                    TRVOL = common.rhs$log_TVOL,
                    VIX = common.rhs$med_VIX_pre,
                    LEVER = common.rhs$leverage,
                    common.rhs[8:159]) %>% as_tibble()

A1.GJR <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(2): TFIDF -------------------------------------------------------------
rhs.tfidf <- MZ_RHS %>% select(ends_with("_TFIDF"))
colnames(rhs.tfidf) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$log_garch, 
                rhs.tfidf,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

A2.GARCH <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

RHS <- cbind(TS_FC = common.rhs$log_gjr, 
                rhs.tfidf,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble()

A2.GJR <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(3): RFIDF -------------------------------------------------------------
rhs.rfidf <- MZ_RHS %>% select(ends_with("_RFIDF"))
colnames(rhs.rfidf) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$log_garch, 
                rhs.rfidf,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

A3.GARCH <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

RHS <- cbind(TS_FC = common.rhs$log_gjr, 
                rhs.rfidf,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble()

A3.GJR <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(4): WFIDF_1PLOG -------------------------------------------------------------
rhs.wfidf.1plog <- MZ_RHS %>% select(ends_with("WFIDF_1P"))
colnames(rhs.wfidf.1plog) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$log_garch, 
                rhs.wfidf.1plog,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

A4.GARCH <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

RHS <- cbind(TS_FC = common.rhs$log_gjr, 
                rhs.wfidf.1plog,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble()

A4.GJR <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(5): WFIDF_LOG1P -------------------------------------------------------------
rhs.wfidf.log1p <- MZ_RHS %>% select(ends_with("WFIDF_P1"))
colnames(rhs.wfidf.log1p) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$log_garch, 
                rhs.wfidf.log1p,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

A5.GARCH <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

RHS <- cbind(TS_FC = common.rhs$log_gjr, 
                rhs.wfidf.log1p,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble()

A5.GJR <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(6): WF_1PLOG -------------------------------------------------------------
rhs.wf.1p <- MZ_RHS %>% select(ends_with("WF_1P"))
colnames(rhs.wf.1p) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$log_garch, 
                rhs.wf.1p,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

A6.GARCH <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

RHS <- cbind(TS_FC = common.rhs$log_gjr, 
                rhs.wf.1p,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble()

A6.GJR <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(7): WF_LOG1P -------------------------------------------------------------
rhs.wf.p1 <- MZ_RHS %>% select(ends_with("WF_P1"))
colnames(rhs.wf.p1) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$log_garch, 
                rhs.wf.p1,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

A7.GARCH <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

RHS <- cbind(TS_FC = common.rhs$log_gjr, 
                rhs.wf.p1,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble()

A7.GJR <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# A(8): TFMAX -------------------------------------------------------------
rhs.tfmax <- MZ_RHS %>% select(ends_with("_TFMAX"))
colnames(rhs.tfmax) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(TS_FC = common.rhs$log_garch, 
                rhs.tfmax,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble() # all dummy controls

A8.GARCH <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

RHS <- cbind(TS_FC = common.rhs$log_gjr, 
                rhs.tfmax,
                GFS = common.rhs$log_GFS,
                FIN = common.rhs$FIN,
                SIZE = common.rhs$size_ta,
                BTM = common.rhs$log_BTM,
                TRVOL = common.rhs$log_TVOL,
                VIX = common.rhs$med_VIX_pre,
                LEVER = common.rhs$leverage,
                common.rhs[8:159]) %>% as_tibble()

A8.GJR <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# Table Export ------------------------------------------------------------

stargazer(A1.GARCH, A2.GARCH, A3.GARCH, A4.GARCH, A5.GARCH,
          A6.GARCH, A7.GARCH, A8.GARCH,
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/A_MZ_regression_GARCH",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: A_MZ_regression_garch", 
          title = "Regression results: eight different weighting schemes (GARCH)",
          model.numbers = T,
          notes.align = "l", 
          notes = "Standard errors in parentheses. TS_FC is the 1-week ahead GARCH(1,1)-forecast.", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

stargazer(A1.GJR, A2.GJR, A3.GJR, A4.GJR, A5.GJR,
          A6.GJR, A7.GJR, A8.GJR,
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/A_MZ_regression_GJR",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: A_MZ_regression_gjr", 
          title = "Regression results: eight different weighting schemes (GJR-GARCH)",
          model.numbers = T,
          notes.align = "l", 
          notes = "Standard errors in parentheses. TS_FC is the 1-week ahead GJR-GARCH(1,1)-forecast.", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))



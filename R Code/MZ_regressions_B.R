
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: MZ_regressions_B                                                          #
#     Description:  1) load LHS and RHS for variant B (5 yearlyregressions)               #
#                   2) regress LHS on RHS using 8 different weighting schemes             #
#                   3) export nice stargazer latex table for the different models         #
#                                                                                         #
#     Date (last updated): July 26th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries / Settings ----------------------------------------------------
rm(list = ls()) # clear environment 
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
                                      select(size_ta, repyear))) %>% 
  as_tibble()
# note: this DF contains both GARCH and GJR variables, we will use either or

# B(1): Static Window -------------------------------------------------------------
rhs.vibtw <- MZ_RHS %>% select(ends_with("_VIBTW"))
colnames(rhs.vibtw) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

RHS <- cbind(         filingyear = common.rhs$repyear,
                      TS_FC = common.rhs$log_garch, 
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
  MZ_RHS_B1 <- RHS %>% filter(filingyear == i) %>% select(-filingyear)
  MZ_LHS_B1 <- MZ_LHS %>% filter(filingyear == i) %>% select(-filingyear)
  assign(paste0("B1.GARCH.", i), 
         lm(as.matrix(MZ_LHS_B1) ~ as.matrix(MZ_RHS_B1)))
  i <- i + 1
}

# for GJR:
RHS <- cbind(         filingyear = common.rhs$repyear,
                      TS_FC = common.rhs$log_gjr, 
                      rhs.vibtw,
                      GFS = common.rhs$log_GFS,
                      FIN = common.rhs$FIN,
                      SIZE = common.rhs$size_ta,
                      BTM = common.rhs$log_BTM,
                      TRVOL = common.rhs$log_TVOL,
                      VIX = common.rhs$med_VIX_pre,
                      LEVER = common.rhs$leverage,
                      common.rhs[8:159]) %>% as_tibble()

MZ_LHS <- MZ_LHS %>% mutate(filingyear = common.rhs$repyear)

i <- 2013
for (i in 2013:2017) {
  MZ_RHS_B1 <- RHS %>% filter(filingyear == i) %>% select(-filingyear)
  MZ_LHS_B1 <- MZ_LHS %>% filter(filingyear == i) %>% select(-filingyear)
  assign(paste0("B1.GJR.", i), 
         lm(as.matrix(MZ_LHS_B1) ~ as.matrix(MZ_RHS_B1)))
  i <- i + 1
}

# B(1) Table Export ------------------------------------------------------------

stargazer(B1.GARCH.2013, B1.GARCH.2014, 
          B1.GARCH.2015, B1.GARCH.2016, B1.GARCH.2017,
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/B1_MZ_garch",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: B1_MZ_garch", 
          title = "Regression results: annual augmented MZ-regressions (GARCH + static training set)",
          model.numbers = T,
          notes.align = "l", 
          notes = "Standard errors in parentheses. TS_FC is the 1-week ahead GARCH(1,1)-forecast. All VIBTW weights were estimated using 10-K filings from 1999 to 2012.", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

stargazer(B1.GJR.2013, B1.GJR.2014, B1.GJR.2015, B1.GJR.2016, B1.GJR.2017, 
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/B1_MZ_gjr",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: B1_MZ_gjr", 
          title = "Regression results: annual augmented MZ-regressions (GJR-GARCH + static training set)",
          model.numbers = T,
          notes.align = "l", 
          notes = "Standard errors in parentheses. TS_FC is the 1-week ahead GJR-GARCH(1,1)-forecast. All VIBTW weights were estimated using 10-K filings from 1999 to 2012.", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

# B(2): Extending Window --------------------------------------------------
rm(list = ls()); gc()
setwd("/Users/kevin/Desktop/lm_testdata/")

i <- 2013
for (i in 2013:2017) {
  load(paste0("./regressions/B/B2_MZ_inputs_", i, ".RData"))
  RHS <- cbind(         TS_FC = MZ_RHS$log_garch, 
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
  assign(paste0("B2.GARCH.", i), 
         lm(as.matrix(MZ_LHS) ~ as.matrix(RHS)))
  i <- i + 1
}

i <- 2013
for (i in 2013:2017) {
  load(paste0("./regressions/B/B2_MZ_inputs_", i, ".RData"))
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
                        MZ_RHS[8:159]) %>% as_tibble() # all dummy controls
  assign(paste0("B2.GJR.", i), 
         lm(as.matrix(MZ_LHS) ~ as.matrix(RHS)))
  i <- i + 1
}

# B(2): Table Export ------------------------------------------------------

stargazer(B2.GARCH.2013, B2.GARCH.2014, 
          B2.GARCH.2015, B2.GARCH.2016, B2.GARCH.2017,
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/B2_MZ_garch",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: B2_MZ_garch", 
          title = "Regression results: annual augmented MZ-regressions (GARCH + extending training set)",
          model.numbers = T,
          notes.align = "l", 
          notes = "Standard errors in parentheses. TS_FC is the 1-week ahead GARCH(1,1)-forecast. VIBTW weights were estimated using 10-K filings from 1999 to (Y-1).", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

stargazer(B2.GJR.2013, B2.GJR.2014, B2.GJR.2015, B2.GJR.2016, B2.GJR.2017, 
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/B2_MZ_gjr",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: B2_MZ_gjr", 
          title = "Regression results: annual augmented MZ-regressions (GJR-GARCH + extending training set)",
          model.numbers = T,
          notes.align = "l", 
          notes = "Standard errors in parentheses. TS_FC is the 1-week ahead GJR-GARCH(1,1)-forecast. VIBTW weights were estimated using 10-K filings from 1999 to (Y-1).", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

# B(3): Rolling Window --------------------------------------------------
rm(list = ls()); gc()
setwd("/Users/kevin/Desktop/lm_testdata/")

i <- 2013
for (i in 2013:2017) {
  load(paste0("./regressions/B/B3_MZ_inputs_", i, ".RData"))
  RHS <- cbind(         TS_FC = MZ_RHS$log_garch, 
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
  assign(paste0("B3.GARCH.", i), 
         lm(as.matrix(MZ_LHS) ~ as.matrix(RHS)))
  i <- i + 1
}

i <- 2013
for (i in 2013:2017) {
  load(paste0("./regressions/B/B3_MZ_inputs_", i, ".RData"))
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
                        MZ_RHS[8:159]) %>% as_tibble() # all dummy controls
  assign(paste0("B3.GJR.", i), 
         lm(as.matrix(MZ_LHS) ~ as.matrix(RHS)))
  i <- i + 1
}

# B(3): Table Export ------------------------------------------------------

stargazer(B3.GARCH.2013, B3.GARCH.2014, 
          B3.GARCH.2015, B3.GARCH.2016, B3.GARCH.2017,
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/B3_MZ_garch",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: B3_MZ_garch", 
          title = "Regression results: annual augmented MZ-regressions (GARCH + rolling training set)",
          model.numbers = T,
          notes.align = "l", 
          notes = "Standard errors in parentheses. TS_FC is the 1-week ahead GARCH(1,1)-forecast. VIBTW weights were estimated using 10-K filings from the past 14 years (i.e., (Y-14) to (Y-1)).", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))

stargazer(B3.GJR.2013, B3.GJR.2014, B3.GJR.2015, B3.GJR.2016, B3.GJR.2017, 
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/B3_MZ_gjr",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: B3_MZ_gjr", 
          title = "Regression results: annual augmented MZ-regressions (GJR-GARCH + rolling training set)",
          model.numbers = T,
          notes.align = "l", 
          notes = "Standard errors in parentheses. TS_FC is the 1-week ahead GJR-GARCH(1,1)-forecast. VIBTW weights were estimated using 10-K filings from the past 14 years (i.e., (Y-14) to (Y-1)).", 
          notes.append = T,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))


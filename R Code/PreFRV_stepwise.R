
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: PreFRV_stepwise.R                                                         #                                                                                       #
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

# replace the 5 NAs with the average
common.rhs$prefiling_RV[is.na(common.rhs$prefiling_RV)] <- mean(common.rhs$prefiling_RV, na.rm = T)
sum(is.na(common.rhs$prefiling_RV)); sum(common.rhs$prefiling_RV == 0, na.rm = T)

rhs.vibtw <- MZ_RHS %>% select(ends_with("_VIBTW"))
colnames(rhs.vibtw) <- c("NEG_SENT", "POS_SENT", "ASSERT", "UNCERT", "LITI")

# NS
RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
             rhs.vibtw[,1],
             # GFS = common.rhs$log_GFS,
             # FIN = common.rhs$FIN,
             SIZE = common.rhs$size_ta,
             BTM = common.rhs$log_BTM,
             TRVOL = common.rhs$log_TVOL,
             VIX = common.rhs$med_VIX_pre,
             LEVER = common.rhs$leverage,
             common.rhs[8:159]) %>% as_tibble() # all dummy controls

C1 <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# NS, PS
RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
             rhs.vibtw[,1:2],
             # GFS = common.rhs$log_GFS,
             # FIN = common.rhs$FIN,
             SIZE = common.rhs$size_ta,
             BTM = common.rhs$log_BTM,
             TRVOL = common.rhs$log_TVOL,
             VIX = common.rhs$med_VIX_pre,
             LEVER = common.rhs$leverage,
             common.rhs[8:159]) %>% as_tibble() # all dummy controls

C2 <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# NS, PS, ASSERT
RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
             rhs.vibtw[,1:3],
             # GFS = common.rhs$log_GFS,
             # FIN = common.rhs$FIN,
             SIZE = common.rhs$size_ta,
             BTM = common.rhs$log_BTM,
             TRVOL = common.rhs$log_TVOL,
             VIX = common.rhs$med_VIX_pre,
             LEVER = common.rhs$leverage,
             common.rhs[8:159]) %>% as_tibble() # all dummy controls

C3 <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# NS, PS, ASSERT, UNCERT
RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
             rhs.vibtw[,1:4],
             # GFS = common.rhs$log_GFS,
             # FIN = common.rhs$FIN,
             SIZE = common.rhs$size_ta,
             BTM = common.rhs$log_BTM,
             TRVOL = common.rhs$log_TVOL,
             VIX = common.rhs$med_VIX_pre,
             LEVER = common.rhs$leverage,
             common.rhs[8:159]) %>% as_tibble() # all dummy controls

C4 <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# NS, PS, ASSERT, UNCERT, LITI
RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
             rhs.vibtw,
             # GFS = common.rhs$log_GFS,
             # FIN = common.rhs$FIN,
             SIZE = common.rhs$size_ta,
             BTM = common.rhs$log_BTM,
             TRVOL = common.rhs$log_TVOL,
             VIX = common.rhs$med_VIX_pre,
             LEVER = common.rhs$leverage,
             common.rhs[8:159]) %>% as_tibble() # all dummy controls

C5 <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# NS, PS, ASSERT, UNCERT, LITI, GFS
RHS <- cbind(TS_FC = common.rhs$prefiling_RV, 
             rhs.vibtw,
             GFS = common.rhs$log_GFS,
             # FIN = common.rhs$FIN,
             SIZE = common.rhs$size_ta,
             BTM = common.rhs$log_BTM,
             TRVOL = common.rhs$log_TVOL,
             VIX = common.rhs$med_VIX_pre,
             LEVER = common.rhs$leverage,
             common.rhs[8:159]) %>% as_tibble() # all dummy controls

C6 <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

# NS, PS, ASSERT, UNCERT, LITI, GFS, FIN
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

C7 <- lm(as.matrix(MZ_LHS) ~ as.matrix(RHS))

stargazer(C1, C2, C3, C4, C5, C6, C7,
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/PreFRV_stepwise_regression",
          type = "latex", summary.logical = F,
          omit = ".reptype|sic|repyear|repmth|weekday|repday.",
          align = F, out.header = F, header = F, 
          intercept.bottom = T, initial.zero = F, digits = 3,
          digit.separate = 3, digit.separator = ",", font.size = "small",
          label = "tab: PreFRV_stepwise_regression", 
          title = "Augmented MZ-Regression: Stepwise Addition / Deletion of Textual Variables",
          model.numbers = T,
          notes.align = "l", 
          notes = "***, **, and * denotes statistical significance at the one-, five- and ten-percent level, respectively. Standard errors are displayed in parentheses. Coefficients for boolean control variables (YRDUMMY, MTHDUMMY, WEEKDAYDUMMY, MONTHDAYDUMMY, SECTORDUMMY, and 10KDUMMY) are not displayed in the table.", 
          notes.append = F,
          dep.var.caption = "Dependent variable: PFRV",
          dep.var.labels.include = F, omit.stat = c("ser", "f"))
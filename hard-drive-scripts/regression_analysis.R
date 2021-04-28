
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: regression_analysis                                                       #
#     Description:  1)                                                                    #
#                                                                                         #
#     Date (last updated): June 18th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

f2y_volas <- read.delim(file = "/Users/kevin/Desktop/lm_testdata/price_vola_related_data_9495.txt",
                        sep = ";")

f2y_volas <- as_tibble(f2y_volas)

# augm. MZ
# extract the RHS as a matrix that can be used as single input in the LM command
RHS_vars <- f2y_volas %>% 
  select(., GARCH_1step, GJR_1step, TVOL_med_1w_pre)

RHS_vars <- as.matrix(RHS_vars)

# show regr. results
mz1 <- summary(lm(PFRV_main ~ RHS_vars, data = f2y_volas))
mz1coeffs <- as.data.frame(mz1$coefficients)
mz1coeffs$Estimate

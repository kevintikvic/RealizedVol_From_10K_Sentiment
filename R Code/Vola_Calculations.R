# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: Vola_Calculations                                                         #
#     Description:  1) Load price data around the filing date                             #
#                   2) Calculate PFRV as well as forecasts via GARCH and TVOL (- 5d avg.) #
#                   3) Export table with 5 variables: filename, PFRV, TVOL, GARCH, GJR    #
#                                                                                         #
#     Date (last updated): June 12th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

weekdays(as.Date("2017/03/14"))
repdates <- paste(meta10k$repyear, meta10k$repmth, meta10k$repday, sep = "/")
rep_weekdays <- weekdays(as.Date(repdates))
table(rep_weekdays)

# Data and Package Loading ------------------------------------------------

rm(list=ls()) # clear environment 
options(scipen=999) # disable scientific notation
library(rugarch)
library(zoo)
library(xts)
library(quantmod)

# setwd("/Volumes/LaCie/LM_data/pricedata")

setwd("/Users/kevin/Desktop/lm_testdata/")
pricefiles_namelist <- list.files(pattern = ".\\d+\\.txt$", 
                                  recursive = T)

# Variable Export ---------------------------------------------------------

# All variables we are interested in:
variable_list <- c("txtfilename", 
                   "PFRV_main",
                   "PFRV_sq_ret",
                   "PFRV_abs_ret",
                   "GARCH_1step", 
                   "GJR_1step", 
                   "TVOL_tot_1w_pre",
                   "TVOL_tot_1m_pre",
                   "TVOL_tot_1y_pre",
                   "TVOL_tot_1w_post",
                   "TVOL_med_1w_pre",
                   "TVOL_med_1m_pre",
                   "TVOL_med_1y_pre",
                   "TVOL_med_1w_post")

# How many do we export?
length(variable_list)

# create empty matrix that will contain everything we need: ID, PFRV, GARCH(1,1), GJR GARCH(1,1), TVOL
finallist_PFRV_volas <- as.data.frame(matrix(NA, 
                                             length(pricefiles_namelist), 
                                             length(variable_list)))

colnames(finallist_PFRV_volas) <- variable_list

i <- 1

for (i in 1:length(pricefiles_namelist)) {
  # Calculations:
  tryCatch({
    
    # Load this file i from the folder 
    currfile <- read.zoo(file = pricefiles_namelist[i],
                         index.column = 1,
                         format = "%Y-%m-%d",
                         header = T,
                         sep = ";")
    
    # Convert to time-series object that is suitable for GARCH models & co.
    currfile <- as.xts(currfile)
    
    # Extract from the filename the filing date
    currdate <- substr(pricefiles_namelist[i], 1, 8)
    currdate <- as.Date(currdate, format = "%Y%m%d")
    
    # Extract from the price data all dates -before- the filing date, 
    # this will be the time window on which we will estimate the GARCH
    garch_est_window <- currfile[paste("/", currdate - 1, sep = ""),
                                 (ncol(currfile) - 1):ncol(currfile)]
    
    # compute weekly returns on this time frame
    wkly_returns_GARCHs <- weeklyReturn(garch_est_window[, 2],
                                        type = "log")
    
    # GARCHs ------------------------------------------------------------------
    
    # Define the specs of the GARCH models we want to estimate
    # this will be a) vanilla GARCH, b) GJR
    
    # Note: both will be estimated using normal errors & assuming constant mean
    
    garch_spec_vanilla <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                                     mean.model = list(armaOrder = c(0,0), include.mean = T),
                                     distribution.model =  "norm")
    
    garch_spec_GJR <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                                 mean.model = list(armaOrder = c(0,0), include.mean = T),
                                 distribution.model =  "norm")
    
    # Estimate GARCH based on the wkly return data & the model specs defined just above
    
    garch_fit_vanilla <- ugarchfit(garch_spec_vanilla, wkly_returns_GARCHs)
    garch_fit_GJR <- ugarchfit(garch_spec_GJR, wkly_returns_GARCHs)
    
    # based on those estimtes, obtain the 1-week ahead forecasts
    garch_1step_fc <- sigma(ugarchforecast(garch_fit_vanilla, n.ahead = 1))
    GJR_garch_1step_fc <- sigma(ugarchforecast(garch_fit_GJR, n.ahead = 1))
    
    colnames(garch_1step_fc) <- "vanilla_GARCH"
    colnames(GJR_garch_1step_fc) <- "GJR_GARCH"
    
    # PFRV --------------------------------------------------------------------
    
    # extract the PFRV in 5 day window, i.e., [0-4] window (filing day + 4 days)
    dly_returns_fullwindow <- dailyReturn(currfile[, ncol(currfile)], 
                                          type = "log")
    
    # note: runSD uses the N-1 in denominator approach to correct for one degree of freedom correction
    PFRV <- apply(dly_returns_fullwindow, 2, runSD, n = 5)
    
    # add date column
    PFRV <- as.xts(PFRV, order.by = index(dly_returns_fullwindow))
    
    # rename column
    colnames(PFRV) <- "pfrv_sigma"
    
    # find from the PFRV vector the value that interests us, namely the one 4 days post-filing
    PFRV_lhs <- PFRV[which(index(PFRV) == currdate) + 4, ]
    
    # Define alternative measure for PFRV based on squared returns
    dly_sqrd_returns_fullwindow <- dly_returns_fullwindow^2
    sum_sq_returns <- runSum(dly_sqrd_returns_fullwindow, 5)
    PFRV_sq_ret <- sum_sq_returns[which(index(sum_sq_returns) == currdate) + 4, ]
    PFRV_sq_ret <- sqrt(PFRV_sq_ret)
    colnames(PFRV_sq_ret) <- "pfrv_sigma_sq_ret"
    
    # Define alternative measure for PFRV based on absolute returns
    dly_abs_returns_fullwindow <- abs(dly_returns_fullwindow)
    sum_abs_returns <- runSum(dly_abs_returns_fullwindow, 5)
    PFRV_abs_ret <- sum_abs_returns[which(index(sum_abs_returns) == currdate) + 4, ]
    colnames(PFRV_abs_ret) <- "pfrv_sigma_abs_ret"
    
    # compare those three with the GARCH forecasts
    # PFRV_lhs; PFRV_sq_ret; PFRV_abs_ret; garch_1step_fc; GJR_garch_1step_fc
    
    # TVOL --------------------------------------------------------------------
    
    # Extract TVOL related data
    # I use only pre-filing measures, so as in t we use in the regression only variables already known
    # that way, it is still "forecasting"
    
    # use -1y data as for the GARCH -- take simply the median TVOL
    median_TVOL_1y_pre <- median(garch_est_window[, 1], na.rm = T)
    
    # export also 1M and 1W median values (ROBUSTNESS CHECK) -- month is defined as 22 days before
    median_TVOL_1w_pre <- median(garch_est_window[(nrow(garch_est_window) - 4):nrow(garch_est_window), 1], na.rm = T)
    median_TVOL_1m_pre <- median(garch_est_window[(nrow(garch_est_window) - 22):nrow(garch_est_window), 1], na.rm = T)
    
    # will do the same for sum of TVOLs:
    sum_TVOL_1y_pre <- sum(garch_est_window[, 1], na.rm = T)
    sum_TVOL_1w_pre <- sum(garch_est_window[(nrow(garch_est_window) - 4):nrow(garch_est_window), 1], na.rm = T)
    sum_TVOL_1m_pre <- sum(garch_est_window[(nrow(garch_est_window) - 22):nrow(garch_est_window), 1], na.rm = T)
    
    # export post-filing TVOL as well, could use it for robustness stuff or alternative LHS
    postfil_tvols <- currfile[(which(index(currfile) == currdate)):(which(index(currfile) == currdate) + 4), 5]
    median_TVOL_1w_post <- median(postfil_tvols, na.rm = T)
    sum_TVOL_1w_post <- sum(postfil_tvols, na.rm = T)
    
    # --- --- --- --- 
    # Export:
    finallist_PFRV_volas[i, 1] <- pricefiles_namelist[i] # fill txtfilename
    finallist_PFRV_volas[i, 2] <- PFRV_lhs # fill PFRV (main)
    finallist_PFRV_volas[i, 3] <- PFRV_sq_ret # fill PFRV (sq. returns)
    finallist_PFRV_volas[i, 4] <- PFRV_abs_ret # fill PFRV (abs. returns)
    finallist_PFRV_volas[i, 5] <- garch_1step_fc # fill GARCH(1,1) one-period forecast
    finallist_PFRV_volas[i, 6] <- GJR_garch_1step_fc # fill GJR-GARCH(1,1) one-period forecast
    finallist_PFRV_volas[i, 7] <- sum_TVOL_1w_pre # fill total TVOL 1w before filing
    finallist_PFRV_volas[i, 8] <- sum_TVOL_1m_pre # fill total TVOL 1m before filing
    finallist_PFRV_volas[i, 9] <- sum_TVOL_1y_pre # fill total TVOL 1y before filing
    finallist_PFRV_volas[i, 10] <- sum_TVOL_1w_post # fill total TVOL 1w after filing
    finallist_PFRV_volas[i, 11] <- median_TVOL_1w_pre # fill median TVOL 1w before filing
    finallist_PFRV_volas[i, 12] <- median_TVOL_1m_pre # fill median TVOL 1m before filing
    finallist_PFRV_volas[i, 13] <- median_TVOL_1y_pre # fill median TVOL 1y before filing
    finallist_PFRV_volas[i, 14] <- median_TVOL_1w_post # fill median TVOL 1y before filing
    
    # remove the old variables so as to avoid writing "old" values for next ticker
    rm(list = setdiff(ls(), c("variable_list", "pricefiles_namelist", "i",
                              "finallist_PFRV_volas")))
    
    # increase row counter
    i <- i + 1
    print(i)
    
  }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")})
  
}

# Inspect the final product
head(finallist_PFRV_volas)
tail(finallist_PFRV_volas)

# check for NAs
colSums(is.na(finallist_PFRV_volas))

# Export the final table
setwd("/Users/kevin/Desktop/lm_testdata/")

write.csv2(finallist_PFRV_volas,
           file = "price_vola_related_data.txt", 
           row.names = F)
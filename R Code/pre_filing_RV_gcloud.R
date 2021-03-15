# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: pre_filing_RV_gcloud.R                                                    #
#     Description:  1) Load clean 46,483 price files                                      #
#                   2) instead of PFRV, I will calculate *** pre-filing *** RV            #
#                   3) Export pre-filing RV, so that it can be used offline for MZ again  #
#                                                                                         #
#     Date (last updated): August 5th, 2018                                               #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# --- GOOGLE CLOUD CODE --- # 

# Data and Package Loading ------------------------------------------------

# install.packages("rugarch")
# install.packages("zoo")
# install.packages("xts")
# install.packages("quantmod")

rm(list=ls()); gc() # clear environment 
options(scipen=999) # disable scientific notation
library(rugarch)
library(zoo)
library(xts)
library(quantmod)

# setwd("/Volumes/LaCie/LM_data/pricedata")
setwd("/Users/kevin/Desktop/lm_testdata/pricedata_tesi_synced_46483")
length(list.files())

pricefiles_namelist <- list.files(pattern = ".\\d+\\.txt$", 
                                  recursive = T)

# Variable Export ---------------------------------------------------------

# All variables we are interested in:
variable_list <- c("txtfilename", 
                   "prefiling_RV_main_0_4",
                   "prefiling_RV_main_1_5",
                   "prefiling_sq_ret_0_4",
                   "prefiling_sq_ret_1_5",
                   "prefiling_abs_ret_0_4",
                   "prefiling_abs_ret_1_5")

# How many do we export?
length(variable_list)

# create empty matrix that will contain everything we need: ID, PFRV, GARCH(1,1), GJR GARCH(1,1), TVOL
finallist_PFRV_volas <- as.data.frame(matrix(NA, 
                                             length(pricefiles_namelist), 
                                             length(variable_list)))

colnames(finallist_PFRV_volas) <- variable_list

# overwrite the txtfilenames, those are known ex ante
finallist_PFRV_volas$txtfilename <- pricefiles_namelist

# Start the loop 
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
    
    # PreFRV --------------------------------------------------------------------
    
    # extract the PFRV in 5 day window, i.e., [0-4] window (filing day + 4 days)
    dly_returns_fullwindow <- dailyReturn(currfile[, ncol(currfile)], 
                                          type = "log")
    
    # note: runSD uses the N-1 in denominator approach to correct for one degree of freedom correction
    PFRV <- apply(dly_returns_fullwindow, 2, runSD, n = 5)
    
    # add date column
    PFRV <- as.xts(PFRV, order.by = index(dly_returns_fullwindow))
    
    # rename column
    colnames(PFRV) <- "pfrv_sigma"
    
    # find from the PFRV vector the value that interests us, namely the one on the filing day (or 1 before... )
    PFRV_m4_0 <- PFRV[which(index(PFRV) == currdate) - 0, ]
    PFRV_m5_m1 <- PFRV[which(index(PFRV) == currdate) - 1, ]
    
    # sq. and abs. return variants instead
    # squared returns
    dly_sqrd_returns_fullwindow <- dly_returns_fullwindow^2
    sum_sq_returns <- runSum(dly_sqrd_returns_fullwindow, 5)
    PFRV_sq_m4_0 <- sum_sq_returns[which(index(sum_sq_returns) == currdate) - 0, ]
    PFRV_sq_m4_0 <- sqrt(PFRV_sq_m4_0)
    colnames(PFRV_sq_m4_0) <- "pfrv_sigma_sq_ret"
    PFRV_sq_m5_m1 <- sum_sq_returns[which(index(sum_sq_returns) == currdate) - 1, ]
    PFRV_sq_m5_m1 <- sqrt(PFRV_sq_m5_m1)
    colnames(PFRV_sq_m5_m1) <- "pfrv_sigma_sq_ret"
    
    # absolute returns
    dly_abs_returns_fullwindow <- abs(dly_returns_fullwindow)
    sum_abs_returns <- runSum(dly_abs_returns_fullwindow, 5)
    PFRV_abs_m4_0 <- sum_abs_returns[which(index(sum_abs_returns) == currdate) - 0, ]
    colnames(PFRV_abs_m4_0) <- "pfrv_sigma_abs_ret"
    PFRV_abs_m5_m1 <- sum_abs_returns[which(index(sum_abs_returns) == currdate) - 1, ]
    colnames(PFRV_abs_m5_m1) <- "pfrv_sigma_abs_ret"    
    
    # --- --- --- --- 
    # Export:
    # finallist_PFRV_volas[i, 1] <- pricefiles_namelist[i] # fill txtfilename
    finallist_PFRV_volas[i, 2] <- PFRV_m4_0 
    finallist_PFRV_volas[i, 3] <- PFRV_m5_m1
    finallist_PFRV_volas[i, 4] <- PFRV_sq_m4_0
    finallist_PFRV_volas[i, 5] <- PFRV_sq_m5_m1
    finallist_PFRV_volas[i, 6] <- PFRV_abs_m4_0
    finallist_PFRV_volas[i, 7] <- PFRV_abs_m5_m1
    
    # remove the calculated variables so as to avoid writing "old" values for next ticker
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
write.table(finallist_PFRV_volas,
            file = "/Users/kevin/Desktop/lm_testdata/prefilingvola_46483.txt", 
            row.names = F, 
            sep = ";")

rm(i, pricefiles_namelist, variable_list)

save.image("/Users/kevin/Desktop/lm_testdata/prefilingvola_46483.RData")

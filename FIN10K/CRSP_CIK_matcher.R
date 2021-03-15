# PREFIX ============================================================================= #
#                                                                                      # 
#    Author: Kevin Tikvic                                                              #
#    Title: CRSP_CIK_matcher                                                           #
#                                                                                      #
#    Description:  extracting CIK codes so as to match them to CRSP identifiers        #
#                  URL: wrds.com                                                       #
#                                                                                      #
#   file created on: 4/27/18                                                           # 
#   last updated on: 4/28/18                                                           #
#                                                                                      # 

# PRELIMINARY STEPS ================================================================== #

rm(list=ls()) # clear environment 
setwd("/Users/kevin/Desktop/FIN10K_DATA") # set working directory

merged_total_database <- read.csv("./merged_total_database.txt", 
                                  sep=";")

CIK_list <- as.data.frame(unique(merged_total_database$CIK))
CUSIP_list <- as.data.frame(unique(merged_total_database$CUSIP))

head(CIK_list); head(CUSIP_list)

write.table(CIK_list,
            file = "CIK_list.txt",
            col.names = F,
            row.names = F,
            quote = F)

write.table(CUSIP_list,
            file = "CUSIP_list.txt",
            col.names = F,
            row.names = F,
            quote = F)

# LOAD THE FILES FROM WRDS & MERGE THE TWO DATAFRAMES ===============================================



# END ===============================================
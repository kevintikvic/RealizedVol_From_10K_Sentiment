### CREATED ON: 05/08/2018 ###
# Background: Guidolin feedback in order to test pre-filing RV as RHS variable
# need reduced set of price filings

# rest of calcs will use the reduced folder with 46,483 pricefiles and thereout calculate only pre-fil RV
# the other variables already exist !

library(tidyverse)

# list of survivors I seek to keep (46K)
setwd("/Volumes/LaCie/LM_data/pricedata")
pricefiles_namelist <- list.files(pattern = ".\\d+\\.txt$", 
                                  recursive = T)

# list of the prelim price files I have (77K)
used.in.tesi_namelist <- read.table("/Users/kevin/Desktop/lm_testdata/pfrv_survivors2.txt")

# quick inspect
head(pricefiles_namelist)
head(used.in.tesi_namelist)

# what needs to go:
filings_to_be_deleted <- anti_join(as_tibble(pricefiles_namelist), 
                                   as_tibble(used.in.tesi_namelist),
                                   by = c("value" = "V1"))

# delete the redundants
filings_to_be_deleted_FL <- filings_to_be_deleted %>% 
  mutate(fullink = paste0("/Volumes/LaCie/LM_data/pricedata/", value)) %>% 
  select(fullink)

# delete all elements in this using their URL 
# commented out, as it is a non-reversible, one-shot operation
lapply(filings_to_be_deleted_FL, 
       function(x) unlink(x))

# check the survivors:
pricefiles_namelist_survived <- list.files(pattern = ".\\d+\\.txt$", recursive = T)

used.in.tesi_namelist %>% as_tibble()
pricefiles_namelist_survived %>% as_tibble()

# note: the folder was renamed to: "pricedata_tesi_synced_46483"

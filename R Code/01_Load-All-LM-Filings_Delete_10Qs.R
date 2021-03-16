
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: lm_files                                                                  #
#     Description:  1) inspect all filings downloaded from LM / SRAF ND                   #
#                   2) strip out the non-10-K's (i.e., 10-Q's)                            #
#                   3) delete the 10-Q's from the hard drive                              #
#                                                                                         #
#     Date (last updated): June 11th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

rm(list=ls()) # clear environment 

# load "meta-files" from the names of the txt files in the respective folders

yearlist <- 1994:2017
yr <- 1994
qtrlist <- 1:4
qtr <- 1

for (yr in yearlist) { # loop over years
  for (qtr in qtrlist) { # loop over 4 quarters (four subfolders)
    setwd(paste("/Volumes/LaCie/LM_data/", yr, "/QTR", qtr, sep = "")) # re-set the W.D.
    if (!exists("total_LM")) { # create LM dataframe for the first time
      total_LM <- as.data.frame(list.files(pattern = ".txt$")) # read list of all txt files
    }
    if (exists("total_LM")) { # append the LM dataframe by quarter and year
      temp_dataset <- as.data.frame(list.files(pattern = ".txt$")) # read list of all txt files
      total_LM <- rbind(total_LM, temp_dataset) # add to bottom of exstisting data.frame
      rm(temp_dataset) # remove the list of qq.yyyy again
    }
    qtr <- qtr + 1 # skip to next quarter
  } # loop over 4 quarters (four subfolders)
  yr <- yr + 1 # skip to next year
} # loop over years


# 1 // Inspect, Modify Total Dataset --------------------------------------

# check how many reports we have in total
nrow(total_LM)
head(total_LM)
colnames(total_LM) <- "txtfilename"

# extract some infos from the way the filenames are constructed
total_LM$reportdate <- substr(total_LM$txtfilename, 1, 8)
total_LM$repyear <- substr(total_LM$reportdate, 1, 4)
total_LM$repmth <- substr(total_LM$reportdate, 5, 6)
total_LM$repday <- substr(total_LM$reportdate, 7, 8)
total_LM$id <- seq(1:nrow(total_LM))

# save the environment (STAGE 1)
save.image("/Volumes/LaCie/LM_data/total_LM_txtfilelist.RData")

# Create a matrix that contains report features out of the filenames
decomp_filenames <- strsplit(as.character(total_LM$txtfilename), "_")
decomp_filenames <- matrix(unlist(decomp_filenames), ncol = 7, byrow = T)

head(decomp_filenames) # quick view...

# Assign those features to the "main" dataframe
total_LM$reptype <- decomp_filenames[,2]
total_LM$CIK <- decomp_filenames[,5]
total_LM$accessno <- decomp_filenames[,6]
total_LM$perannumcounter <- decomp_filenames[,7]

# Check how many are of form 10K, Q, KA, etc. 
table(total_LM$reptype)

# check how many unique companies are in the sample
length(unique(total_LM$CIK))

# clean unused variables and save the total, merged df
rm(decomp_filenames)

# save this modified environment (STAGE 2)
save.image("/Volumes/LaCie/LM_data/total_LM_txtfilelist_stripped_info.RData")


# 2 // Split of 10K vs. Q -------------------------------------------------

#- Split the 10Ks from 10Qs in separate dataframes
# load df in case we wanna start from here again
# load("/Volumes/LaCie/LM_data/total_LM_txtfilelist_stripped_info.RData")

# extract a list of 10Q*, based on which we will delete the .txt's in the folders
list_10Qs <- c("10-Q", "10-Q-A", "10-QSB", "10-QSB-A", "10-QT", "10-QT-A", "10QSB", "10QSB-A")
files_to_be_deleted <- total_LM[total_LM$reptype %in% list_10Qs, ]
files_2b_del <- as.character(files_to_be_deleted$txtfilename)
head(files_2b_del) # quick view... 

# keep only the 10-K* in the meta list total_LM
"%ni%" <- Negate("%in%") # creates function for "not in" 
total_LM_keeping <- total_LM[total_LM$reptype %ni% list_10Qs, ]
files_2b_kept <- as.character(total_LM_keeping$txtfilename)
head(files_2b_kept) # quick view... 

# Check the numbers for each of the subframes (2b kept vs. 2b deleted)
nrow(total_LM) # total 
nrow(files_to_be_deleted) # tb deleted
nrow(total_LM) - nrow(files_to_be_deleted) # tb kept
nrow(total_LM_keeping) # tb kept

# Strip out the quarter
files_to_be_deleted$quarter <- quarters(as.Date(files_to_be_deleted$reportdate, 
                                                format = "%Y%m%d"))

# extract the Q number without the Q
files_to_be_deleted$qrtr <- as.factor(substr(files_to_be_deleted$quarter, 2, 2))

# save this modified environment (STAGE 3)
save.image("/Volumes/LaCie/LM_data/total_LM_txtfilelist_stripped_info_tbkept_vs_tbdel.RData")


# 3 // 10Q File Deletion --------------------------------------------------

#- Delete the files from the folders

# create "lists" per year per quarter that contain the .txt files to be deleted
yearlist <- 1994:2017
yr <- 1994
qtrlist <- 1:4
qtr <- 1

for (yr in yearlist) {
  for (qtr in qtrlist) {
    assign(paste("ftbd_", yr , "_QTR", qtr, sep = ""), 
           as.character(files_to_be_deleted[(files_to_be_deleted$repyear == yr) & (files_to_be_deleted$qrtr == qtr), ]$txtfilename))
    qtr <- qtr + 1
  }
  yr <- yr + 1
}

# NB: should have stored them in a list of lists instead of creating 96 "data frames"

# write function that deletes the .txt files in the respective YYYY/QTR# subdirectory
del_files_in_ftbd <- function(ftbdel) {
  
  d_qtr <- substr(deparse(substitute(ftbdel)), 11, 14) # extract the current year
  d_yr <- substr(deparse(substitute(ftbdel)), 6, 9) # extract the current quarter
  
  setwd(paste("/Volumes/LaCie/LM_data/", d_yr, "/", d_qtr, sep = "")) # reset the WD to the subdirectory of the respective y and q
  
  unlink(ftbdel) # delete the whole list from the current WD
}

# Apply the function to all 96 dataframes, as lapply did not work
# a loop did not either ... -- both fail at the stage of resetting the WD within the del_files_in_ftbd function

# 94
del_files_in_ftbd(ftbd_1994_QTR1); del_files_in_ftbd(ftbd_1994_QTR2)
del_files_in_ftbd(ftbd_1994_QTR3); del_files_in_ftbd(ftbd_1994_QTR4)

# 95
del_files_in_ftbd(ftbd_1995_QTR1); del_files_in_ftbd(ftbd_1995_QTR2)
del_files_in_ftbd(ftbd_1995_QTR3); del_files_in_ftbd(ftbd_1995_QTR4)

# 96
del_files_in_ftbd(ftbd_1996_QTR1); del_files_in_ftbd(ftbd_1996_QTR2)
del_files_in_ftbd(ftbd_1996_QTR3); del_files_in_ftbd(ftbd_1996_QTR4)

# 97
del_files_in_ftbd(ftbd_1997_QTR1); del_files_in_ftbd(ftbd_1997_QTR2)
del_files_in_ftbd(ftbd_1997_QTR3); del_files_in_ftbd(ftbd_1997_QTR4)

# 98
del_files_in_ftbd(ftbd_1998_QTR1); del_files_in_ftbd(ftbd_1998_QTR2)
del_files_in_ftbd(ftbd_1998_QTR3); del_files_in_ftbd(ftbd_1998_QTR4)

# 99
del_files_in_ftbd(ftbd_1999_QTR1); del_files_in_ftbd(ftbd_1999_QTR2)
del_files_in_ftbd(ftbd_1999_QTR3); del_files_in_ftbd(ftbd_1999_QTR4)

# 00
del_files_in_ftbd(ftbd_2000_QTR1); del_files_in_ftbd(ftbd_2000_QTR2)
del_files_in_ftbd(ftbd_2000_QTR3); del_files_in_ftbd(ftbd_2000_QTR4)

# 01
del_files_in_ftbd(ftbd_2001_QTR1); del_files_in_ftbd(ftbd_2001_QTR2)
del_files_in_ftbd(ftbd_2001_QTR3); del_files_in_ftbd(ftbd_2001_QTR4)

# 02
del_files_in_ftbd(ftbd_2002_QTR1); del_files_in_ftbd(ftbd_2002_QTR2)
del_files_in_ftbd(ftbd_2002_QTR3); del_files_in_ftbd(ftbd_2002_QTR4)

# 03
del_files_in_ftbd(ftbd_2003_QTR1); del_files_in_ftbd(ftbd_2003_QTR2)
del_files_in_ftbd(ftbd_2003_QTR3); del_files_in_ftbd(ftbd_2003_QTR4)

# 04
del_files_in_ftbd(ftbd_2004_QTR1); del_files_in_ftbd(ftbd_2004_QTR2)
del_files_in_ftbd(ftbd_2004_QTR3); del_files_in_ftbd(ftbd_2004_QTR4)

# 05
del_files_in_ftbd(ftbd_2005_QTR1); del_files_in_ftbd(ftbd_2005_QTR2)
del_files_in_ftbd(ftbd_2005_QTR3); del_files_in_ftbd(ftbd_2005_QTR4)

# 06
del_files_in_ftbd(ftbd_2006_QTR1); del_files_in_ftbd(ftbd_2006_QTR2)
del_files_in_ftbd(ftbd_2006_QTR3); del_files_in_ftbd(ftbd_2006_QTR4)

# 07
del_files_in_ftbd(ftbd_2007_QTR1); del_files_in_ftbd(ftbd_2007_QTR2)
del_files_in_ftbd(ftbd_2007_QTR3); del_files_in_ftbd(ftbd_2007_QTR4)

# 08
del_files_in_ftbd(ftbd_2008_QTR1); del_files_in_ftbd(ftbd_2008_QTR2)
del_files_in_ftbd(ftbd_2008_QTR3); del_files_in_ftbd(ftbd_2008_QTR4)

# 09
del_files_in_ftbd(ftbd_2009_QTR1); del_files_in_ftbd(ftbd_2009_QTR2)
del_files_in_ftbd(ftbd_2009_QTR3); del_files_in_ftbd(ftbd_2009_QTR4)

# 10
del_files_in_ftbd(ftbd_2010_QTR1); del_files_in_ftbd(ftbd_2010_QTR2)
del_files_in_ftbd(ftbd_2010_QTR3); del_files_in_ftbd(ftbd_2010_QTR4)

# 11
del_files_in_ftbd(ftbd_2011_QTR1); del_files_in_ftbd(ftbd_2011_QTR2)
del_files_in_ftbd(ftbd_2011_QTR3); del_files_in_ftbd(ftbd_2011_QTR4)

# 12
del_files_in_ftbd(ftbd_2012_QTR1); del_files_in_ftbd(ftbd_2012_QTR2)
del_files_in_ftbd(ftbd_2012_QTR3); del_files_in_ftbd(ftbd_2012_QTR4)

# 13
del_files_in_ftbd(ftbd_2013_QTR1); del_files_in_ftbd(ftbd_2013_QTR2)
del_files_in_ftbd(ftbd_2013_QTR3); del_files_in_ftbd(ftbd_2013_QTR4)

# 14
del_files_in_ftbd(ftbd_2014_QTR1); del_files_in_ftbd(ftbd_2014_QTR2)
del_files_in_ftbd(ftbd_2014_QTR3); del_files_in_ftbd(ftbd_2014_QTR4)

# 15
del_files_in_ftbd(ftbd_2015_QTR1); del_files_in_ftbd(ftbd_2015_QTR2)
del_files_in_ftbd(ftbd_2015_QTR3); del_files_in_ftbd(ftbd_2015_QTR4)

# 16
del_files_in_ftbd(ftbd_2016_QTR1); del_files_in_ftbd(ftbd_2016_QTR2)
del_files_in_ftbd(ftbd_2016_QTR3); del_files_in_ftbd(ftbd_2016_QTR4)

# 17
del_files_in_ftbd(ftbd_2017_QTR1); del_files_in_ftbd(ftbd_2017_QTR2)
del_files_in_ftbd(ftbd_2017_QTR3); del_files_in_ftbd(ftbd_2017_QTR4)


# 4 // Deletion Functionality Check ---------------------------------------

#- Check functionality of the deletion process

yearlist <- 1994:2017
yr <- 1994
qtrlist <- 1:4
qtr <- 1

# Load the "remaining" files that survived

for (yr in yearlist) { # loop over years
  for (qtr in qtrlist) { # loop over 4 quarters (four subfolders)
    setwd(paste("/Volumes/LaCie/LM_data/", yr, "/QTR", qtr, sep = "")) # re-set the W.D.
    if (!exists("LM_after_del")) { # create LM dataframe for the first time
      LM_after_del <- as.data.frame(list.files(pattern = ".txt$")) # read list of all txt files
    }
    if (exists("LM_after_del")) { # append the LM dataframe by quarter and year
      temp_dataset <- as.data.frame(list.files(pattern = ".txt$")) # read list of all txt files
      LM_after_del <- rbind(LM_after_del, temp_dataset) # add to bottom of exstisting data.frame
      rm(temp_dataset) # remove the list of qq.yyyy again
    }
    qtr <- qtr + 1 # skip to next quarter
  } # loop over 4 quarters (four subfolders)
  yr <- yr + 1 # skip to next year
} # loop over years

# the total length of loaded docs should match the "Keeping" subframe we extracted initially
nrow(LM_after_del); nrow(total_LM_keeping)

# There should be no "non"-10-K files in this newly loaded file
# Use same approach (create temporary DF, split by "_", add as columns, tabulate)

decomp_filenames <- strsplit(as.character(LM_after_del[,1]), "_")
decomp_filenames <- matrix(unlist(decomp_filenames), ncol = 7, byrow = T)

LM_after_del$reptype <- decomp_filenames[,2]
LM_after_del$CIK <- decomp_filenames[,5]
LM_after_del$accessno <- decomp_filenames[,6]
LM_after_del$perannumcounter <- decomp_filenames[,7]

# Check how many are of form 10K, Q, KA, etc. 
table(LM_after_del$reptype) # distribution now
table(total_LM$reptype) # distribution previously

# Successful: all Q-docs are gone / deleted from the hard drive !!!


# 5 // Data Export --------------------------------------------------------

setwd("/Volumes/LaCie/LM_data")

# Export CSV of the total K+Q database
write.csv2(total_LM,
           file = "total_LM_both_kq.txt", 
           row.names = F)

# Export CSV of the total K database
write.csv2(total_LM_keeping,
           file = "total_LM_only_k.txt", 
           row.names = F)

# Export CSV of the total K database
write.csv2(files_to_be_deleted,
           file = "total_LM_only_q.txt", 
           row.names = F)


rm(list=ls()) # clear environment 

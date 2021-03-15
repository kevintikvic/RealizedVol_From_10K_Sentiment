# PREFIX ============================================================================= #
#                                                                                      # 
#    Author: Kevin Tikvic                                                              #
#    Title: meta_10K                                                                   #
#                                                                                      #
#    Description:  loading and describing the meta-files provided by Tsai & Wang       #
#                  That way, we get an overview about the 10K filings they used.       #
#                  URL: https://clip.csie.org/10K/data                                 #
#                                                                                      #
#   file created on: 4/21/18                                                           # 
#   last updated on: 4/28/18                                                           #
#                                                                                      # 
# PRELIMINARY STEPS ================================================================== #

rm(list=ls()) # clear environment 
setwd("/Users/kevin/Desktop/FIN10K_DATA") # set working directory

# Load libraries:
# install.packages("ggplot2")
# install.packages("stargazer")

library(ggplot2)
library(stargazer)

# LOAD META FILES & STORE THEM COMBINED ===============================================

# create list of all "meta"-files from Rekabsaz
setwd("/Users/kevin/Desktop/FIN10K_DATA/all.meta") # set working directory
metalist_complete <- list.files(pattern = ".meta.txt$")
setwd("/Users/kevin/Desktop/FIN10K_DATA") # set working directory

# create loop to read in these meta files contained in the list
# the created data.frames will be named "filings_in_yyyy"
i <- 1996
for (metaindex in metalist_complete) {
  assign(paste("filings_in_", i, sep=""),
  read.delim(paste("~/Desktop/FIN10K_DATA/all.meta/", metaindex, sep=""), 
                header=FALSE,
                stringsAsFactors=FALSE))
  i <- i+1
  metaindex <- paste(i, ".meta.txt", sep="")
}

# create a generic list of month names, always useful for some plots etc.
monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# generic list of years that we have in sample
yrs_sample <- seq(1996, 2013)

# combine all years together, to a large meta-file of 10Ks
allfilings <- rbind(filings_in_1996, filings_in_1997,
                    filings_in_1998, filings_in_1999, filings_in_2000, filings_in_2001,
                    filings_in_2002, filings_in_2003, filings_in_2004, filings_in_2005,
                    filings_in_2006, filings_in_2007, filings_in_2008, filings_in_2009,
                    filings_in_2010, filings_in_2011, filings_in_2012, filings_in_2013)

# drop the redundant "single" data frames
rm(filings_in_1996, filings_in_1997,
      filings_in_1998, filings_in_1999, filings_in_2000, filings_in_2001,
      filings_in_2002, filings_in_2003, filings_in_2004, filings_in_2005,
      filings_in_2006, filings_in_2007, filings_in_2008, filings_in_2009,
      filings_in_2010, filings_in_2011, filings_in_2012, filings_in_2013)

# rename the columns to understand a bit better
colnames(allfilings) <- c("reportid", "issuedate", "url", "co.name")

# strip publication date into DD, MM, YYYY and add those as separate columns
allfilings$pubday   <- as.factor(substring(allfilings$issuedate, 7, 8))
allfilings$pubmonth <- as.factor(substring(allfilings$issuedate, 5, 6))
allfilings$pubyear  <- as.factor(substring(allfilings$issuedate, 1, 4))

# generate an identifier, in order to allow for chronological ordering later on... 
allfilings$id <- seq(1, length(allfilings$reportid))

# extract the CIK and CUSIP
library(stringr)

regexp <- "[[:digit:]]+"
allfilings$CUSIP <- as.factor(substr(allfilings$reportid, 1, 9))
allfilings$CIK <- as.factor(str_extract(allfilings$co.name, regexp))

# Note that these may not be "equivalently unique" (e.g., due to mergers)

# export dataframe as csv for future use
write.csv2(allfilings,
          file = "merged_metaonly.txt", 
          row.names = F)

# VISUALIZATION: TABLES/PLOTS (INCL. EXPORT) ===============================================

# summary stats:
nrow(allfilings) # total number of 10Ks
summary(allfilings$pubyear) # number of 10Ks per year
summary(allfilings$pubmonth) # number of 10Ks per month

# combined table: how many in each y/m combination
yr_vs_month_table <- (table(allfilings$pubyear, 
      allfilings$pubmonth))

yr_vs_month_table <- as.data.frame.matrix(yr_vs_month_table)

colnames(yr_vs_month_table) <- monthnames

stargazer(yr_vs_month_table, summary = FALSE, 
          rownames = TRUE, colnames = TRUE, 
          title = "Temporal Distribution of 10-K Filings (Years and Months)",
          out = "yr_vs_month_meta.tex")

# additional check: how many filings we have per company (i.e., the ColSum)
nrow(unique(as.matrix(allfilings$CIK))); nrow(unique(as.matrix(allfilings$CUSIP)))

counter_per_CIK <- as.data.frame.matrix(table(allfilings$pubyear, allfilings$CIK))
counter_per_CUSIP <- as.data.frame.matrix(table(allfilings$pubyear, allfilings$CUSIP))
any(is.na(counter_per_CIK)); any(is.na(counter_per_CUSIP))

# install.packages("rpgm")
library(rpgm)
mean(colSums(counter_per_CIK)); mean(colSums(counter_per_CUSIP))
mean(colMins(counter_per_CIK)); mean(colMins(counter_per_CUSIP))
mean(colMaxs(counter_per_CIK)); mean(colMaxs(counter_per_CUSIP))

# plot: no. of reports over the years 1996-2013, discrimated by filing month
yr_barplot <- ggplot(allfilings,
                     aes(x = pubyear, colour = pubmonth)) + 
  geom_bar(aes(fill = pubmonth)) + 
  labs(title = "Number of 10-K Filings", 
       subtitle = "1996 - 2013 \n",
       caption = "\nData Source: Tsai et al. (2016)",
       y = "",
       x = "") +
  theme(axis.text.x = element_text(size = 8, margin = margin(t = -10))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(3, "mm")) + 
  theme(legend.text = element_text(size = 6)) +
  theme(legend.title = element_text(size = 6)) + 
  scale_fill_discrete(name = "Filing Month: ",
                      labels = monthnames) +
  guides(fill = guide_legend(nrow = 1), colour = F) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) +
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(size=.1, color="gray"))

yr_barplot

ggsave(file="no_reports_perannum.pdf", width = 297, height = 210, units = "mm")

# MERGING: ===============================================
rm(list=ls())

merged_meta <- read.csv("~/Desktop/FIN10K_DATA/merged_metaonly.txt", 
                        sep=";", 
                        stringsAsFactors=FALSE)

# merge with the (3) vola, abnormal trading volume (1), and excess return (1) databases

# FAMA VOLA
setwd("/Users/kevin/Desktop/FIN10K_DATA/all.logfama") # set working directory
famavola_complete <- list.files(pattern = ".logfama.txt$")

i <- 1996
for (famavolaindex in famavola_complete) {
  assign(paste("famavola_in_", i, sep=""),
         read.delim(paste("~/Desktop/FIN10K_DATA/all.logfama/", famavolaindex, sep=""), 
                    header=FALSE,
                    sep = " ",
                    stringsAsFactors=FALSE))
  i <- i+1
  famavolaindex <- paste(i, ".logfama.txt", sep="")
}

# VOLA PTM
setwd("/Users/kevin/Desktop/FIN10K_DATA/all.logvol/ptm") # set working directory
vola_ptm_complete <- list.files(pattern = ".logvol.-12.txt$")

i <- 1996
for (vola_ptm_index in vola_ptm_complete) {
  assign(paste("vola_ptm_", i, sep=""),
         read.delim(paste("~/Desktop/FIN10K_DATA/all.logvol/ptm/", vola_ptm_index, sep=""), 
                    header=FALSE,
                    sep = " ",
                    stringsAsFactors=FALSE))
  i <- i+1
  vola_ptm_index <- paste(i, ".logvol.-12.txt", sep="")
}

# VOLA NTM
setwd("/Users/kevin/Desktop/FIN10K_DATA/all.logvol/ntm") # set working directory
vola_ntm_complete <- list.files(pattern = ".logvol.+12.txt$")

i <- 1996
for (vola_ntm_index in vola_ntm_complete) {
  assign(paste("vola_ntm_", i, sep=""),
         read.delim(paste("~/Desktop/FIN10K_DATA/all.logvol/ntm/", vola_ntm_index, sep=""), 
                    header=FALSE,
                    sep = " ",
                    stringsAsFactors=FALSE))
  i <- i+1
  vola_ntm_index <- paste(i, ".logvol.+12.txt", sep="")
}

# ABNORMAL TRADING VOLUME (ATV)
setwd("/Users/kevin/Desktop/FIN10K_DATA/all.abnormal") # set working directory
ATV_complete <- list.files(pattern = ".abnormal.txt$")

i <- 1996
for (ATV_index in ATV_complete) {
  assign(paste("ATV_", i, sep=""),
         read.delim(paste("~/Desktop/FIN10K_DATA/all.abnormal/", ATV_index, sep=""), 
                    header=FALSE,
                    sep = " ",
                    stringsAsFactors=FALSE))
  i <- i+1
  ATV_index <- paste(i, ".abnormal.txt", sep="")
}

# EXCESS RETURN (CAR)
setwd("/Users/kevin/Desktop/FIN10K_DATA/all.excess") # set working directory
CAR_complete <- list.files(pattern = ".excess.txt$")

i <- 1996
for (CAR_index in CAR_complete) {
  assign(paste("CAR_", i, sep=""),
         read.delim(paste("~/Desktop/FIN10K_DATA/all.excess/", CAR_index, sep=""), 
                    header=FALSE,
                    sep = " ",
                    stringsAsFactors=FALSE))
  i <- i+1
  CAR_index <- paste(i, ".excess.txt", sep="")
}

# combine all years together
all_famavola <- rbind(famavola_in_1996, famavola_in_1997, famavola_in_1998,
                      famavola_in_1999, famavola_in_2000, famavola_in_2001,
                      famavola_in_2002, famavola_in_2003, famavola_in_2004,
                      famavola_in_2005, famavola_in_2006, famavola_in_2007,
                      famavola_in_2008, famavola_in_2009, famavola_in_2010,
                      famavola_in_2011, famavola_in_2012, famavola_in_2013)

all_vola_ptm <- rbind(vola_ptm_1996, vola_ptm_1997, vola_ptm_1998,
                      vola_ptm_1999, vola_ptm_2000, vola_ptm_2001,
                      vola_ptm_2002, vola_ptm_2003, vola_ptm_2004,
                      vola_ptm_2005, vola_ptm_2006, vola_ptm_2007,
                      vola_ptm_2008, vola_ptm_2009, vola_ptm_2010,
                      vola_ptm_2011, vola_ptm_2012, vola_ptm_2013)

all_vola_ntm <- rbind(vola_ntm_1996, vola_ntm_1997, vola_ntm_1998,
                      vola_ntm_1999, vola_ntm_2000, vola_ntm_2001,
                      vola_ntm_2002, vola_ntm_2003, vola_ntm_2004,
                      vola_ntm_2005, vola_ntm_2006, vola_ntm_2007,
                      vola_ntm_2008, vola_ntm_2009, vola_ntm_2010,
                      vola_ntm_2011, vola_ntm_2012, vola_ntm_2013)

all_ATV <- rbind(ATV_1996, ATV_1997, ATV_1998,
                 ATV_1999, ATV_2000, ATV_2001,
                 ATV_2002, ATV_2003, ATV_2004,
                 ATV_2005, ATV_2006, ATV_2007,
                 ATV_2008, ATV_2009, ATV_2010,
                 ATV_2011, ATV_2012, ATV_2013)

all_CAR <- rbind(CAR_1996, CAR_1997, CAR_1998,
                 CAR_1999, CAR_2000, CAR_2001,
                 CAR_2002, CAR_2003, CAR_2004,
                 CAR_2005, CAR_2006, CAR_2007,
                 CAR_2008, CAR_2009, CAR_2010,
                 CAR_2011, CAR_2012, CAR_2013)

# drop the redundant "single" data frames
rm(famavola_in_1996, famavola_in_1997, famavola_in_1998,
                      famavola_in_1999, famavola_in_2000, famavola_in_2001,
                      famavola_in_2002, famavola_in_2003, famavola_in_2004,
                      famavola_in_2005, famavola_in_2006, famavola_in_2007,
                      famavola_in_2008, famavola_in_2009, famavola_in_2010,
                      famavola_in_2011, famavola_in_2012, famavola_in_2013)

rm(vola_ptm_1996, vola_ptm_1997, vola_ptm_1998,
                      vola_ptm_1999, vola_ptm_2000, vola_ptm_2001,
                      vola_ptm_2002, vola_ptm_2003, vola_ptm_2004,
                      vola_ptm_2005, vola_ptm_2006, vola_ptm_2007,
                      vola_ptm_2008, vola_ptm_2009, vola_ptm_2010,
                      vola_ptm_2011, vola_ptm_2012, vola_ptm_2013)

rm(vola_ntm_1996, vola_ntm_1997, vola_ntm_1998,
                      vola_ntm_1999, vola_ntm_2000, vola_ntm_2001,
                      vola_ntm_2002, vola_ntm_2003, vola_ntm_2004,
                      vola_ntm_2005, vola_ntm_2006, vola_ntm_2007,
                      vola_ntm_2008, vola_ntm_2009, vola_ntm_2010,
                      vola_ntm_2011, vola_ntm_2012, vola_ntm_2013)

rm(ATV_1996, ATV_1997, ATV_1998,
                 ATV_1999, ATV_2000, ATV_2001,
                 ATV_2002, ATV_2003, ATV_2004,
                 ATV_2005, ATV_2006, ATV_2007,
                 ATV_2008, ATV_2009, ATV_2010,
                 ATV_2011, ATV_2012, ATV_2013)

rm(CAR_1996, CAR_1997, CAR_1998,
                 CAR_1999, CAR_2000, CAR_2001,
                 CAR_2002, CAR_2003, CAR_2004,
                 CAR_2005, CAR_2006, CAR_2007,
                 CAR_2008, CAR_2009, CAR_2010,
                 CAR_2011, CAR_2012, CAR_2013)

rm(ATV_complete, ATV_index,
   CAR_complete, CAR_index,
   famavola_complete, famavolaindex,
   vola_ntm_complete, vola_ntm_index,
   vola_ptm_complete, vola_ptm_index, i)

# rename colnames of dataframes
colnames(all_ATV) <- c("ATV","reportid")
colnames(all_CAR) <- c("CAR","reportid")
colnames(all_famavola) <- c("famavola","reportid")
colnames(all_vola_ptm) <- c("vola_ptm","reportid")
colnames(all_vola_ntm) <- c("vola_ntm","reportid")

# merge the combined files based on reportid
merged_meta$CAR <- all_CAR$CAR
merged_meta$ATV <- all_ATV$ATV
merged_meta$famavola <- all_famavola$famavola
merged_meta$vola_ptm <- all_vola_ptm$vola_ptm
merged_meta$vola_ntm <- all_vola_ntm$vola_ntm

rm(all_ATV, all_CAR, all_famavola, all_vola_ntm, all_vola_ptm)

# EXPORT DATA:
setwd("/Users/kevin/Desktop/FIN10K_DATA") # set working directory

# save Data file
save.image("./merged_total_database.RData")

# also export as CSV
write.csv2(merged_meta,
           file = "merged_total_database.txt", 
           row.names = F)

# END ===============================================
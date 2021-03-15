# PREFIX ===============================================

  # Author: Kevin Tikvic
  # Title: meta_10K_kogan

  # Description:  loading and describing the meta-files provided by Kogan et al. (2009).
  #               That way, we get an overview about the 10K filings they used.

  # file created on: 4/16/18
  # last updated on: 4/16/18

# PRELIMINARY STEPS ===============================================

rm(list=ls()) # clear environment 
setwd("/Users/kevintikvic/Desktop/Kogan_Data") # set working directory

# Load libraries:
library(ggplot2)
library(stargazer)
library(plyr)
library(gmodels)

# LOAD META FILES & STORE THEM COMBINED ===============================================

# create list of all "meta"-files from Rekabsaz
kogan_metalists <- list.files(pattern = ".meta.txt$")

# create loop to read in these meta files contained in the list
# the created data.frames will be named "filings_in_yyyy"
i <- 1996
for (metaindex in kogan_metalists) {
  assign(paste("filings_in_", i, sep=""),
  read.delim(paste("~/Desktop/Kogan_Data/", metaindex, sep=""), 
                header=FALSE, 
                stringsAsFactors=FALSE))
  i <- i+1
  metaindex <- paste(i, ".meta.txt", sep="")
}

# create a generic list of month names, always useful for some plots etc.
monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# generic list of years that we have in sample
yrs_sample <- seq(1996, 2006)

# combine all years together, to a large meta-file of 10Ks
allfilings <- rbind(filings_in_1996, filings_in_1997, filings_in_1998, filings_in_1999, filings_in_2000,
                    filings_in_2001, filings_in_2002, filings_in_2003, filings_in_2004, filings_in_2005,
                    filings_in_2006)

# drop the redundant "single" data frames
rm(filings_in_1996, filings_in_1997, filings_in_1998, filings_in_1999, filings_in_2000,
   filings_in_2001, filings_in_2002, filings_in_2003, filings_in_2004, filings_in_2005,
   filings_in_2006)

# rename the columns to understand a bit better
colnames(allfilings) <- c("reportid", "issuedate", "url", "co.name", "CIK")

# strip publication date into DD, MM, YYYY and add those as separate columns
allfilings$pubday   <- as.factor(substring(allfilings$issuedate, 7, 8))
allfilings$pubmonth <- as.factor(substring(allfilings$issuedate, 5, 6))
allfilings$pubyear  <- as.factor(substring(allfilings$issuedate, 1, 4))

# generate an identifier, in order to allow for chronological ordering later on... 
allfilings$id <- seq(1, length(allfilings$reportid))

# export dataframe as csv for future use
write.csv2(allfilings,
          file = "merged_meta_kogan.txt",
          sep = ";",
          row.names = F,
          dec = ".")

# VISUALIZATION: TABLES/PLOTS (INCL. EXPORT) ===============================================

# summary stats:
summary(allfilings$pubyear)

count(allfilings, "pubyear")
count(allfilings, "pubmonth")

write.table(xtabs(~ pubyear + pubmonth, allfilings),
            file = "cross_table_yrmonth.txt")

# plot: no. of reports over the years 06-15, for each sector
gr2 <- ggplot(allfilings, 
             aes(x = pubyear, colour = pubmonth)) + 
  geom_bar(aes(fill=pubmonth)) + 
  facet_wrap(~ pubmonth) + 
  labs(subtitle="1996 - 2006", 
       y="", 
       x="", 
       title="Number of 10-K Filings", 
       caption = "Data Source: Kogan et al. (2009)",
       colour = "") +
  theme(axis.text.x = element_text(size=6, angle=45)) +
  theme(legend.position = "bottom") +
  theme(legend.key.size = unit(3, "mm")) + 
  theme(legend.text = element_text(size=6)) +
  theme(legend.title = element_text(size=6)) +
  guides(colour = F) + 
  scale_fill_discrete(name = "Filing Month",
                           labels = monthnames) +
  guides(fill = guide_legend(nrow = 1))

gr2

ggsave(file="no_reports_per_sector.pdf", width = 297, height = 210, units = "mm")

# END ===============================================

# problem: do not have sector variable

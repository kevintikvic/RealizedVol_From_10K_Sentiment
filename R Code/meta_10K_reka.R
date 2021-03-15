# PREFIX ===============================================

  # Author: Kevin Tikvic
  # Title: meta_10K_reka

  # Description:  loading and describing the meta-files provided by Rekabsaz et al. (2017).
  #               That way, we get an overview about the 10K filings they used.

  # file created on: 4/15/18
  # last updated on: 4/16/18

# PRELIMINARY STEPS ===============================================

rm(list=ls()) # clear environment 
setwd("/Users/kevintikvic/Desktop/Rekabsaz_Data") # set working directory

# Load libraries:
library(ggplot2)
library(stargazer)

# LOAD META FILES & STORE THEM COMBINED ===============================================

# create list of all "meta"-files from Rekabsaz
rekab_metalists <- list.files(pattern = "meta$")

# create loop to read in these meta files contained in the list
# the created data.frames will be named "filings_in_yyyy"
i <- 2006
for (metaindex in rekab_metalists) {
  assign(paste("filings_in_", i, sep=""),
  read.csv(paste("~/Desktop/Rekabsaz_Data/", metaindex, sep=""), 
                header=FALSE, 
                sep=";", 
                stringsAsFactors=FALSE))
  i <- i+1
  metaindex <- paste(i, ".meta", sep="")
}

# create a generic list of month names, always useful for some plots etc.
monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# generic list of years that we have in sample
yrs_sample <- seq(2006, 2015)

# combine all years together, to a large meta-file of 10Ks
allfilings <- rbind(filings_in_2006, filings_in_2007, filings_in_2008, filings_in_2009, filings_in_2010,
                    filings_in_2011, filings_in_2012, filings_in_2013, filings_in_2014, filings_in_2015)

# drop the redundant "single" data frames
rm(filings_in_2006, filings_in_2007, filings_in_2008, filings_in_2009, filings_in_2010,
   filings_in_2011, filings_in_2012, filings_in_2013, filings_in_2014, filings_in_2015)

# rename the columns to understand a bit better
colnames(allfilings) <- c("reportid", "issuedate", "co.name", "CIK", "ticker", "exch",
                          "sector", "ind")

# strip publication date into DD, MM, YYYY and add those as separate columns
allfilings$pubday   <- as.factor(substring(allfilings$issuedate, 7, 8))
allfilings$pubmonth <- as.factor(substring(allfilings$issuedate, 5, 6))
allfilings$pubyear  <- as.factor(substring(allfilings$issuedate, 1, 4))

# generate an identifier, in order to allow for chronological ordering later on... 
allfilings$id <- seq(1, length(allfilings$reportid))

# export dataframe as csv for future use
write.csv2(allfilings,
          file = "merged_meta_reka.txt", 
          sep = ";",
          row.names = F,
          dec = ".")

# VISUALIZATION: TABLES/PLOTS (INCL. EXPORT) ===============================================

# summary stats:
allfilings$sector <- as.factor(allfilings$sector)
summary(allfilings$sector)
summary(allfilings$pubyear)

# plot: no. of reports over the years 06-15, for each sector
gr <- ggplot(allfilings, 
             aes(x = pubyear, colour = pubmonth)) + 
  geom_bar(aes(fill=pubmonth)) + 
  facet_wrap(~ sector) + 
  labs(subtitle="2006 - 2015", 
       y="", 
       x="", 
       title="Number of 10-K Filings", 
       caption = "Data Source: Rekabsaz et al. (2017)",
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

gr

ggsave(file="no_reports_per_sector.pdf", width = 297, height = 210, units = "mm")

# END ===============================================

# problem: much less observations than in Kogan et al. (2009) ?
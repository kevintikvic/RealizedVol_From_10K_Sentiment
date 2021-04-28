
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: lm_files_meta_stats_graphs                                                #
#     Description:  1) inspect 10-K*'s that are left in sample after matching with CRSP   #
#                   2) create some descriptive statistics / distributions                 #
#                   3) create some descriptive, meaningful graphs                         #
#                                                                                         #
#     Date (last updated): June 20th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

rm(list=ls()) # clear environment 
# setwd("/Volumes/LaCie/LM_data/")
setwd("/Users/kevin/Desktop/lm_testdata")

library(ggplot2)
library(stargazer)
library(dplyr)
# install.packages("DescTools")
library(DescTools)

# load the 10-K only file
database_10K_meta <- as_data_frame(read.delim("DB_after_controls_generated_259x67730.txt",
                                              header = T,
                                              sep = ";",
                                              dec = ","))

# To check for the initial steps in sample cleaning
aa <- read.delim("total_LM_only_k.txt", sep = ";", header = T)
nrow(aa); nrow(unique(aa))
length(aa$txtfilename); length(unique(aa$txtfilename))
length(aa$CIK); length(unique(aa$CIK))

bb <- read.delim("meta_having_prices_tickers.csv", sep = ";", header = T)
nrow(bb); nrow(unique(bb))
length(bb$txtfilename); length(unique(bb$txtfilename))
length(bb$CIK); length(unique(bb$CIK))

length(database_10K_meta$cik); length(database_10K_meta$CIK);
length(unique(database_10K_meta$cik)); length(unique(database_10K_meta$CIK)) 

# Sample cleaning in a single dense table:
sample.cleaning.table <- 
  data.frame(Description = c("Initial sample of 10-K*s:", 
                             "Ticker is available:", 
                             "Yahoo Finance price data available (based on ticker):",
                             "COMPUSTAT data available (based on ticker):"), 
             "Filings" = c(295746, 184358, 77951, 67730), 
             "CIKs" = c(40264, 12525, 4934, 4212))

# Auto-Calulate some values in the table
sample.cleaning.table$AvgFilPerCIK <- round(sample.cleaning.table$Filings / sample.cleaning.table$CIKs,
                                            digits = 1)
sample.cleaning.table$PctChange <- round((sample.cleaning.table$Filings / shift(sample.cleaning.table$Filings)) * 100, 
                                         digits = 1)

sample.cleaning.table$PctChange <- paste(sample.cleaning.table$PctChange, "%", sep = "")

# add col names
colnames(sample.cleaning.table) <- c("Description", 
                                     "No. of Filings", 
                                     "No. of Companies", 
                                     "Average No. of Filings per Company", 
                                     "Percentage of Row Above")

# Export the table in LATEX format
stargazer(sample.cleaning.table, 
          type = "latex",
          summary = F, 
          rownames = F, 
          colnames = T, 
          title = "Sample Cleaning and Matching Procedure",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/sample_cleaning_matching",
          digit.separate = 3,
          digit.separator = ",",
          digits = NA,
          initial.zero = F,
          align = F)

# sort the data by CIK and repyear
# database_10K_meta <- database_10K_meta %>% 
#   group_by(., CIK) %>% 
#   arrange(., CIK, repyear)

# Define some lists that are useful in any case (e.g., axis labelling for graphs)
yearlist <- 1994:2017
qtrlist <- 1:4
monthnames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
weekdaynames <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
filingshortcuts <- rownames(as.matrix(table(database_10K_meta$reptype)))

# 1 // Descriptive Stats --------------------------------------------------

# investigate distribution over years
table(database_10K_meta$repyear); barplot(table(database_10K_meta$repyear))
# investigate distribution over months
table(database_10K_meta$repmth); barplot(table(database_10K_meta$repmth))
# investigate distribution over days
table(database_10K_meta$repday); barplot(table(database_10K_meta$repday))
# investigate distribution over 10-K-types
table(database_10K_meta$reptype); barplot(table(database_10K_meta$reptype))

# Create a multiplot of those 4 barplots (Y / M / DoM / WD), coloring within bars being filing type
# install.packages("ggpubr")
library(ggpubr)

p1_y <- ggplot(database_10K_meta,
               aes(x = as.factor(repyear))) + 
  geom_bar(aes(fill = as.factor(reptype))) + 
  labs(title = "Years",
       y = "",
       x = "") +
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), labels = comma, limits = c(0, 4000)) + 
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(2, "mm")) + 
  theme(legend.text = element_text(size = 6)) +
  theme(legend.title = element_text(size = 6)) + 
  scale_fill_discrete(name = "Filing Type: ",
                      labels = filingshortcuts) +
  guides(fill = guide_legend(nrow = 1), colour = F) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) +
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(size = .1, color = "gray"))

p2_m <- ggplot(database_10K_meta,
               aes(x = as.factor(repmth))) + 
  geom_bar(aes(fill = as.factor(reptype))) + 
  labs(title = "Months",
       y = "",
       x = "") +
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 15), labels = comma, limits = c(0, 30000)) + 
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(2, "mm")) + 
  theme(legend.text = element_text(size = 6)) +
  theme(legend.title = element_text(size = 6)) + 
  scale_fill_discrete(name = "Filing Type: ",
                      labels = filingshortcuts) +
  guides(fill = guide_legend(nrow = 1), colour = F) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) +
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(size = .1, color = "gray")) + 
  scale_x_discrete(labels = monthnames)


p3_d <- ggplot(database_10K_meta,
               aes(x = as.factor(repday))) + 
  geom_bar(aes(fill = as.factor(reptype))) + 
  labs(title = "Days of the month",
       y = "",
       x = "") +
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = comma, limits = c(0, 5000)) + 
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(2, "mm")) + 
  theme(legend.text = element_text(size = 6)) +
  theme(legend.title = element_text(size = 6)) + 
  scale_fill_discrete(name = "Filing Type: ",
                      labels = filingshortcuts) +
  guides(fill = guide_legend(nrow = 1), colour = F) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) +
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(size = .1, color = "gray"))

p4_wd <- ggplot(database_10K_meta,
               aes(x = as.factor(weekday))) + 
  geom_bar(aes(fill = as.factor(reptype))) + 
  labs(title = "Weekdays",
       y = "",
       x = "") + 
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 9), labels = comma, limits = c(0, 18000)) + 
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(2, "mm")) + 
  theme(legend.text = element_text(size = 6)) +
  theme(legend.title = element_text(size = 6)) + 
  scale_fill_discrete(name = "Filing Type: ",
                      labels = filingshortcuts) +
  guides(fill = guide_legend(nrow = 1), colour = F) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) +
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(size = .1, color = "gray")) + 
  scale_x_discrete(labels = c("Mo" = "Mon", "Di" = "Tue", "Mi" = "Wed", 
                              "Do" = "Thu", "Fr" = "Fri"), 
                   limits = c("Mo","Di","Mi", "Do", "Fr"))

ggarrange(p1_y, 
          p2_m, 
          p3_d,
          p4_wd,
          ncol = 2, 
          nrow = 2, 
          common.legend = T, 
          legend = "bottom",
          align = "h")

ggsave(file="/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Images/no_reports_Y_M_D_type.pdf", 
       width = 297, 
       height = 210, 
       units = "mm")

# cross count table years vs. months and types vs. months and types vs. years
table(database_10K_meta$repyear, database_10K_meta$repmth)
table(database_10K_meta$reptype, database_10K_meta$repmth)
table(database_10K_meta$reptype, database_10K_meta$repyear)
table(database_10K_meta$reptype, database_10K_meta$weekday)

# Double-Check: There should be no K405's AFTER 2003
# as this "check the box rule" was abandoned by SEC
table(database_10K_meta[database_10K_meta$reptype == "10-K405", ]$repyear)
table(database_10K_meta[database_10K_meta$reptype == "10-K405-A", ]$repyear)
table(database_10K_meta[database_10K_meta$reptype == "10KSB40", ]$repyear)
table(database_10K_meta[database_10K_meta$reptype == "10KSB40-A", ]$repyear)
table(database_10K_meta[database_10K_meta$reptype == "10KT405", ]$repyear)
table(database_10K_meta[database_10K_meta$reptype == "10KT405-A", ]$repyear)

# all good !

# 2 // Stargazer Latex Table Export ---------------------------------------

# Export table years vs. months for LATEX via stargazer
yr_vs_month_table <- as.data.frame.matrix(table(database_10K_meta$repyear, 
                                                database_10K_meta$repmth))

colnames(yr_vs_month_table) <- monthnames

setwd("/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables")

stargazer(yr_vs_month_table, 
          summary = F, 
          rownames = T, 
          colnames = T, 
          title = "Sample Distribution of 10-K* Filings Over Time (Years vs. Months)",
          out = "yr_vs_month_meta",
          digit.separate = 3,
          digit.separator = ",",
          digits = 0)

# Export distribution of filing types
reptype_table <- t(as.matrix(table(database_10K_meta$reptype)))

stargazer(reptype_table, summary = F, 
          rownames = F, colnames = T, 
          column.labels = c("Filing Type", "No. of Observations"),
          title = "Sample Distribution of 13 Different Filing Types",
          out = "filing_types",
          digit.separate = 3,
          digit.separator = ",",
          digits = 0)

# Descriptive Summary Statistics (Mean, SD, etc.)
# subset the "interesting" part of the variables, there are now 215 -- potential PFRV, GARCH & TVOL will need to be added

# RHS variables that need to be inspected and reported a little further:
# trading volume, size, book-to-market, leverage, % of institutional ownership, and % of foreign ownership, plus dummies

desc_stat_10ks <- database_10K_meta %>% 
  select(., "size_ta", "mtb_ratio", "leverage")

# size: 
summary(desc_stat_10ks$size_ta); hist(desc_stat_10ks$size_ta)
summary(database_10K_meta$at); hist(database_10K_meta$at) 

database_10K_meta$at[database_10K_meta$at==0]

# mtb (NB: LM used BTM, so I might consider inverting it)
summary(desc_stat_10ks$mtb_ratio); hist(desc_stat_10ks$mtb_ratio)

mtbs <- desc_stat_10ks$mtb_ratio[desc_stat_10ks$mtb_ratio > 0]

mtbs <- Winsorize(mtbs, 
                  na.rm = T,
                  probs = c(0, 0.99))

summary(mtbs); hist(mtbs)

# Export latex
stargazer(as.data.frame(desc_stat_10ks),
          type = "latex",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/desc_stats",
          summary = T,
          digit.separate = 3,
          digit.separator = ",",
          column.sep.width = "0.5pt",
          font.size = "small",
          initial.zero = F,
          keep = c("ROA", "leverage"), 
          label = "tab: descr_stats", 
          summary.stat = c("n", "mean", "sd", "min", "median", "max"))

# 3 // Graph Export -------------------------------------------------------

# Export graph of distribution over years
setwd("/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Images")

yr_barplot <- ggplot(database_10K_meta,
                     aes(x = as.factor(repyear), colour = as.factor(repmth))) + 
  geom_bar(aes(fill = as.factor(repmth))) + 
  labs(title = "Number of 10-K* Filings \n",
       caption = "\nData Source: https://sraf.nd.edu/",
       y = "",
       x = "") +
  theme(axis.text.x = element_text(size = 8, margin = margin(t = -5), angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), labels = comma, limits = c(0, 4000)) + 
  theme(legend.position = "bottom") + 
  theme(legend.key.size = unit(3, "mm")) + 
  theme(legend.text = element_text(size = 8)) +
  theme(legend.title = element_text(size = 8)) + 
  scale_fill_discrete(name = "Filing Month: ",
                      labels = monthnames) +
  guides(fill = guide_legend(nrow = 1), colour = F) + 
  theme(panel.background = element_blank()) + 
  theme(axis.ticks = element_blank()) +
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(size=.1, color="gray"))

yr_barplot

ggsave(file="no_reports_perannum.pdf", width = 297, height = 210, units = "mm")

# 4 // CIK Investigation --------------------------------------------------

# Check how many reports per company per year
# simple average: 
nrow(database_10K_meta) / length(unique(database_10K_meta$CIK))

# distribution:
CIK_Year_Mapper <- as.data.frame(cbind(database_10K_meta$CIK, database_10K_meta$repyear))
colnames(CIK_Year_Mapper) <- c("CIK", "repyear")

CIK_Year_Mapper <- CIK_Year_Mapper[order(CIK_Year_Mapper$CIK), ]
head(CIK_Year_Mapper, 45)

# Count of filings per CIK
filings_per_CIK <- as.matrix(colSums(table(CIK_Year_Mapper$repyear, CIK_Year_Mapper$CIK)))

# average / median / min / max
mean(filings_per_CIK); median(filings_per_CIK); min(filings_per_CIK); max(filings_per_CIK)

colnames(filings_per_CIK) <- "count"

filings_per_CIK <- as.data.frame(filings_per_CIK)
filings_per_CIK <- filings_per_CIK[order(filings_per_CIK$V1), ]

max(filings_per_CIK) # 163 filings! for 24yrs this are about 7 p.a.
which.max(filings_per_CIK) # find the index of this one
filings_per_CIK[30518,] # it is CIK 1347185 = AFS SenSub Corp. (an ABS structure)

# let's see how many are larger than 24 
# NB: theoretically they can be "a bit" larger than 24 (e.g., due to KT or K-A reports a single CIK can file more than once per year)
length(filings_per_CIK[filings_per_CIK > 24])

# shit... there are 1987 CIKs (!) --> this will be a pretty large number of reports....

# Export CIK list for CRSP matching purposes
write.table(unique(CIK_Year_Mapper$CIK),
           file = "cik_list.txt", 
           row.names = F,
           col.names = F,
           sep = "\n")

rm(list=ls()) # clear environment 

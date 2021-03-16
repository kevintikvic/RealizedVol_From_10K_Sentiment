
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: lm_files_meta_stats_graphs                                                #
#     Description:  1) inspect 10-K*'s that are left in sample after matching with CRSP   #
#                   2) create some descriptive statistics / distributions                 #
#                   3) create some descriptive, meaningful graphs                         #
#                                                                                         #
#     Date (last updated): July 17th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

rm(list=ls()); gc() # clear environment 
# setwd("/Volumes/LaCie/LM_data/")
setwd("/Users/kevin/Desktop/lm_testdata")

library(tidyverse)
library(stargazer)
# install.packages("DescTools")
library(DescTools)
library(data.table)
# install.packages("ggpubr")
library(ggpubr)
library(scales)
# install.packages("sp")
library(sp)
# install.packages("grDevices")
library(grDevices)

# load the 10-K only file
# database_10K_meta <- as_data_frame(read.delim("DB_after_controls_generated_259x67730.txt",
#                                               header = T,
#                                               sep = ";",
#                                               dec = ","))

load("~/Desktop/lm_testdata/DB5_316x46483.RData")
database_10K_meta <- as.data.frame(DB5)

# recode weekdays as they are not ordered:
database_10K_meta$weekday <- factor(database_10K_meta$weekday,
                                    levels = c("Mo", "Di", "Mi", "Do", "Fr"))
levels(database_10K_meta$weekday)

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

# Sample cleaning procedure displayed in a single dense table:
sample.cleaning.table <- 
  data.frame("Description" = c("Initial sample of 10-K*s:", 
                             "Ticker is available:", 
                             "Yahoo Finance price data available (based on ticker):",
                             "COMPUSTAT data available (based on ticker):",
                             "Data is meaningful(*): "), 
             "Filings" = c(295746, 184358, 77951, 64527, 46483), 
             "CIKs" = c(40264, 12525, 4934, 4125, 3736))

# Auto-Calulate some values in the table
sample.cleaning.table$AvgFilPerCIK <- round(sample.cleaning.table$Filings / sample.cleaning.table$CIKs,
                                            digits = 1)
sample.cleaning.table$PctChange <- round((sample.cleaning.table$Filings / 295746) * 100, 
                                         digits = 1)

sample.cleaning.table$PctChange <- paste(sample.cleaning.table$PctChange, "%", sep = "")

# add col names
colnames(sample.cleaning.table) <- c("Description", 
                                     "No. of filings", 
                                     "No. of CIKs", 
                                     "Average no. of filings per CIK", 
                                     "% of 1st row")

# Export the table in LATEX format
stargazer(sample.cleaning.table, type = "latex", summary = F, 
          rownames = F, colnames = T, 
          title = "Sample Cleaning and Matching Procedure",
          label = "tab: sample-cleaning",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/sample_cleaning_matching",
          digit.separate = 3,
          out.header = F, header = F,
          font.size = "small",
          digit.separator = ",", decimal.mark = ".", digits = NA,
          initial.zero = F, align = F)

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
# investigate distribution over month-days
table(database_10K_meta$repday); barplot(table(database_10K_meta$repday))
# investigate distribution over 10-K-types
table(database_10K_meta$reptype); barplot(table(database_10K_meta$reptype))
# investigate distribution over weekdays
table(database_10K_meta$weekday); barplot(table(database_10K_meta$weekday))
# range of first to last date
DB5$reportdate %>% head; DB5$reportdate %>% tail
# how many per year
a <- DB5 %>% group_by(factor(repyear)) %>% summarise(pa.count = n()); a
(a[nrow(a), 2]/a[1, 2] - 1)*100
a$pa.count %>% mean; a$pa.count %>% median 
# how many per month
a <- DB5 %>% group_by(factor(repmth)) %>% summarise(pm.count = n()); a
max(a$pm.count)/sum(a$pm.count)*100

# Multi bar plot ----------------------------------------------------------

# Create a multiplot of those 4 barplots (Y / M / DoM / WD), coloring within bars being filing type
# kevcolors <- gray.colors(13)
# kevcolors <- palette(rainbow(13, alpha = 1))
tol13qualitative <- c("#332288", "#6699CC", "#88CCEE", "#44AA99", 
                     "#117733", "#999933", "#DDCC77", "#661100", 
                     "#CC6677", "#AA4466", "#882255", "#AA4499", 
                     "#ff66ff")
pal <- function(col, border = "light gray", ...){
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "", ...)
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}
pal(tol13qualitative)
kevcolors <- tol13qualitative

common.plot.configs <- 
  theme(axis.text.x = element_text(size = 10, 
                                   angle = 90, 
                                   hjust = 1,
                                   vjust = .5),
        legend.position = "right",
        legend.direction = "horizontal",
        legend.key.size = unit(8, "mm"),
        legend.spacing = unit(10, "mm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(size = .05, 
                                          color = "black"))

p1_y <- ggplot(database_10K_meta,
               aes(x = as.factor(repyear))) + 
  geom_bar(aes(fill = as.factor(reptype))) + 
  labs(title = "(A) Years",
       y = "No. of filings\n",
       x = "") +
  guides(fill = guide_legend("Filing type:\n", 
                             nrow = 13, 
                             title.position = "top")) +
  scale_fill_manual(values = kevcolors) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), 
                     labels = comma, 
                     limits = c(0, 3000)) + 
  common.plot.configs

p2_m <- ggplot(database_10K_meta,
               aes(x = as.factor(repmth))) + 
  geom_bar(aes(fill = as.factor(reptype))) + 
  labs(title = "(B) Months",
       y = "No. of filings\n",
       x = "") +
  guides(fill = guide_legend("Filing type:\n", 
                             nrow = 13, 
                             title.position = "top")) +
  scale_fill_manual(values = kevcolors) +
  scale_y_continuous(breaks = pretty_breaks(n = 10), 
                     labels = comma, 
                     limits = c(0, 20000)) + 
  common.plot.configs

p3_d <- ggplot(database_10K_meta,
               aes(x = as.factor(repday))) + 
  geom_bar(aes(fill = as.factor(reptype))) + 
  labs(title = "(C) Days of the month",
       y = "No. of filings\n",
       x = "") +
  guides(fill = guide_legend("Filing type:\n", 
                             nrow = 13, 
                             title.position = "top")) +
  scale_fill_manual(values = kevcolors) +
  scale_y_continuous(breaks = pretty_breaks(n = 7), 
                     labels = comma, 
                     limits = c(0, 3500)) + 
  common.plot.configs

p4_wd <- ggplot(database_10K_meta,
                aes(x = weekday)) + 
  geom_bar(aes(fill = as.factor(reptype))) + 
  labs(title = "(D) Weekdays",
       y = "No. of filings\n",
       x = "") +
  guides(fill = guide_legend("Filing type:\n", 
                             nrow = 13, 
                             title.position = "top")) +
  scale_fill_manual(values = kevcolors) +
  scale_y_continuous(breaks = pretty_breaks(n = 12), 
                     labels = comma, 
                     limits = c(0, 12000)) + 
  scale_x_discrete(labels = c("Di" = "Tue",
                              "Do" = "Thu",
                              "Fr" = "Fri",
                              "Mi" = "Wed",
                              "Mo" = "Mon")) + 
  common.plot.configs

# create multi-plot
ggarrange(p1_y, p2_m, p3_d, p4_wd, ncol = 2, nrow = 2, 
          common.legend = T, legend = "right", align = "h")
# export
ggsave(file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Images/no_reports_Y_M_D_type.pdf", 
       width = 297, height = 210, units = "mm")
# clear environ
rm(a, p1_y, p2_m, p3_d, p4_wd, common.plot.configs)

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

# Export distribution of filing types
reptype_table <- t(as.matrix(table(database_10K_meta$reptype)))
sum(reptype_table) # all captured !

# how many 10K or 10KA:
sum(reptype_table[1:2])
sum(reptype_table[1:2])/sum(reptype_table)

stargazer(reptype_table, 
          summary = F, type = "latex",
          rownames = F, colnames = T, out.header = F, header = F,
          title = "Sample distribution of filing types",
          label = "tab: type-composition",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/filing_types",
          digit.separate = 3,
          digit.separator = ",",
          digits = 0, decimal.mark = ".", initial.zero = F,
          font.size = "small", align = F)

# Corpus Stats ------------------------------------------------------------
summary(database_10K_meta$nb.tokens);
hist(database_10K_meta$nb.tokens)
summary(database_10K_meta$nb.types); 
hist(database_10K_meta$nb.types)

summary(database_10K_meta$nb.tokens.cleancorp); 
hist(database_10K_meta$nb.tokens.cleancorp)
summary(database_10K_meta$nb.types.cleancorp); 
hist(database_10K_meta$nb.types.cleancorp)

corpus_stats_pa <- DB5 %>% 
  select(repyear, nb.tokens, nb.types, 
         nb.tokens.cleancorp, nb.types.cleancorp,
         CIK) %>% 
  mutate_if(is.integer, as.numeric) %>% 
  group_by(repyear) %>% 
  summarise(no.of.filings = n(),
            sum.no.tokens = sum(nb.tokens),
            sum.no.types = sum(nb.types),
            avg.no.tokens = mean(nb.tokens),
            avg.no.types = mean(nb.types),
            med.no.tokens = median(nb.tokens),
            med.no.types = median(nb.types)) %>% 
  mutate(sum.no.tokens = paste(round(sum.no.tokens/(10^6)),
                               " MM",
                               sep = ""),
         sum.no.types = paste(round(sum.no.types/(10^6)),
                              " MM",
                              sep = ""),
         avg.no.tokens = round(avg.no.tokens),
         avg.no.types = round(avg.no.types),
         med.no.tokens = round(med.no.tokens),
         med.no.types = round(med.no.types))

fullcorp.sumstats <- c("1999-2017",
                       length(DB5$CIK),
                       sum(as.numeric(DB5$nb.tokens)),
                       sum(as.numeric(DB5$nb.types)),
                       mean(as.numeric(DB5$nb.tokens)),
                       mean(as.numeric(DB5$nb.types)),
                       median(as.numeric(DB5$nb.tokens)),
                       median(as.numeric(DB5$nb.types)))

fullcorp.sumstats[3] <- paste(round(as.numeric(fullcorp.sumstats[3]) / 10^6), " MM", sep = "")
fullcorp.sumstats[4] <- paste(round(as.numeric(fullcorp.sumstats[4]) / 10^6), " MM", sep = "")
fullcorp.sumstats[5:8] <- round(as.numeric(fullcorp.sumstats[5:8]))

CSPA <- rbind(corpus_stats_pa,
              fullcorp.sumstats)

CSPA <- CSPA  %>% 
  mutate_at(vars(no.of.filings, avg.no.tokens, 
                 avg.no.types, med.no.tokens, med.no.types),
            funs(as.numeric))

stargazer(as.data.frame(CSPA), 
          summary = F, type = "latex",
          rownames = F, colnames = T, out.header = F, header = F,
          title = "Corpus composition by years",
          label = "tab: corpus-composition-p.a.",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/corpus_composition",
          digit.separate = 3,
          digit.separator = ",",
          decimal.mark = ".", initial.zero = F,
          font.size = "small", align = F)

rm(corpus_stats_pa, CSPA, fullcorp.sumstats)

# regarding readability: ln(GFS) is the measure

# divide by 1MM to obtain GFS in MB, it is given in B
as.numeric(DB4$GFS/10^6) %>% hist(.)
as.numeric(DB4$GFS/10^6) %>% summary(.)

as.numeric(DB4$GFS/10^6) %>% log(.) %>% hist(.)
as.numeric(DB4$GFS/10^6) %>% log(.) %>% summary(.)

# OLD ---------------------------------------------------------------------

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

# oh there are 1987 CIKs (!) --> this will be a pretty large number of reports....

# Export CIK list for CRSP matching purposes
write.table(unique(CIK_Year_Mapper$CIK),
           file = "cik_list.txt", 
           row.names = F,
           col.names = F,
           sep = "\n")

rm(list=ls()) # clear environment 

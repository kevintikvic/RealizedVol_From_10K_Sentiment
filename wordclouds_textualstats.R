
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: wordclouds_textualstats                                                   #
#     Description:  1) load DTMs                                                          #
#                   2) create wordclouds & text-related descriptive stats                 #
#                   3) export graphs / tables in thesis-ready format                      #
#                                                                                         #
#     Date (last updated): July 17th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

rm(list=ls()); gc() # clear environment
options(scipen = 999)
# setwd("/Volumes/LaCie/LM_data/")
setwd("/Users/kevin/Desktop/lm_testdata")

library(tidyverse)
library(stargazer)
# install.packages("DescTools")
library(DescTools)
library(data.table)
library(tm)
# install.packages("ggpubr")
library(ggpubr)
library(scales)
# install.packages("sp")
library(sp)
# install.packages("grDevices")
library(grDevices)
library(wordcloud)

# Load DTMs ---------------------------------------------------------------
load("~/Desktop/lm_testdata/sep_DTMs_RData/DTM_N_46483.RData")
load("~/Desktop/lm_testdata/sep_DTMs_RData/DTM_P_46483.RData")
load("~/Desktop/lm_testdata/sep_DTMs_RData/DTM_U_46483.RData")
load("~/Desktop/lm_testdata/sep_DTMs_RData/DTM_L_46483.RData")
load("~/Desktop/lm_testdata/sep_DTMs_RData/DTM_C_46483.RData")
load("~/Desktop/lm_testdata/sep_DTMs_RData/DTM_SM_46483.RData")
load("~/Desktop/lm_testdata/sep_DTMs_RData/DTM_MM_46483.RData")
load("~/Desktop/lm_testdata/sep_DTMs_RData/DTM_WM_46483.RData")

load("~/Desktop/lm_testdata/DB5_316x46483.RData")

all8_global_DTM <- cbind(DTM_N, DTM_P, DTM_U, DTM_L,
                         DTM_C, DTM_SM, DTM_MM, DTM_WM)

rownames(all8_global_DTM) %>% head
all8_global_DTM %>% colnames %>% length
all8_global_DTM %>% colnames %>% unique %>% length
ncol(DTM_N) + ncol(DTM_P) + ncol(DTM_U) + ncol(DTM_L) + ncol(DTM_C) + ncol(DTM_SM) + ncol(DTM_MM) + ncol(DTM_WM)

# load dicts
load("~/Desktop/lm_testdata/all_LM_wordlists.RData")

# inspect which were in NO doc at all:
not_in_global8_DTM <- anti_join(lm_dict_allcombined, 
                                as_tibble(colnames(all8_global_DTM)), 
                                by = c("word" = "value"))
not_in_global8_DTM
unique(not_in_global8_DTM$word)

# these are used for appendix (italics)

# Most frequent words per class -------------------------------------------

# define 3 groups of years:
g1 <- 1999:2004
g2 <- 2005:2010
g3 <- 2011:2017

# define function to find 4 maxima (1 total, 3x per group)
top5wordfinder <- function(x) {
  DTM_inclyrs <- x %>% as_tibble %>% mutate(filingyear = DB5$repyear)
  DTM_inclyrs$yr_group <- NA
  DTM_inclyrs$yr_group[DTM_inclyrs$filingyear %in% g1] <- "G1"
  DTM_inclyrs$yr_group[DTM_inclyrs$filingyear %in% g2] <- "G2"
  DTM_inclyrs$yr_group[DTM_inclyrs$filingyear %in% g3] <- "G3"
  
  # define total col sums
  N.maxes <- DTM_inclyrs %>% select(-c("filingyear", "yr_group")) %>% 
    summarise_all(sum)
  
  # transpose, add back names:
  N.maxes.top5 <- as.matrix(N.maxes)
  N.maxes.top5 <- t(N.maxes.top5)
  N.maxes.top5 <- N.maxes.top5 %>% 
    as_tibble() %>%
    mutate(word = colnames(x))
  
  # order them, extract top 5
  N.maxes.top5 <- N.maxes.top5 %>% arrange(desc(V1)) %>% 
    top_n(5, V1)
  
  # define col sums per 8y group 
  grp.maxes <- DTM_inclyrs %>% select(-filingyear) %>% 
    group_by(yr_group) %>% 
    summarise_all(sum)
  
  # transpose, add back names:
  grp.maxes.t <- as.matrix(grp.maxes)
  grp.maxes.t <- t(grp.maxes.t)
  grp.maxes.t <- as_tibble(grp.maxes.t)
  
  grp.maxes.t <- grp.maxes.t[-1, ]
  colnames(grp.maxes.t) <- c("G1", "G2", "G3")
  
  grp.maxes.t <- grp.maxes.t %>% 
    mutate_if(is.character, as.numeric) %>% 
    mutate(word = colnames(x))
  
  # order them, extract top 5
  n.maxes.g1.top5 <- grp.maxes.t %>% 
    select(G1, word) %>% 
    arrange(desc(G1)) %>% 
    top_n(5, G1)
  n.maxes.g2.top5 <- grp.maxes.t %>% 
    select(G2, word) %>% 
    arrange(desc(G2)) %>% 
    top_n(5, G2)
  n.maxes.g3.top5 <- grp.maxes.t %>% 
    select(G3, word) %>% 
    arrange(desc(G3)) %>% 
    top_n(5, G3)
  
  # Combine all maxima back together
    top5 <- cbind(N.maxes.top5,
                   n.maxes.g1.top5,
                   n.maxes.g2.top5,
                   n.maxes.g3.top5)
    
    return(top5)
}

# create list of all 8 DTMs
dtmlist <- list(DTM_N, DTM_P, DTM_U, DTM_L,
                DTM_C, DTM_SM, DTM_MM, DTM_WM)

# apply the top5-picker function to the list
top5s.all.DTMs <- lapply(dtmlist, top5wordfinder)

# "unlist" the result of the lapply into a rowbound DF
top5s.all.DTMs <- as.data.frame(do.call(rbind, top5s.all.DTMs))

# assign meaningful colnames
colnames(top5s.all.DTMs) <- c("count.tot", "word.tot",
                              "count.g1", "word.g1",
                              "count.g2", "word.g2",
                              "count.g3", "word.g3")

# define a function to produce LaTeX-ready numbers in 1.05 MM format
tomil <- function(x){
  x <- as.numeric(x/10^6)   # divide by 1 MM
  x <- round(x, digits = 2) # round to 2 digits
  x <- sprintf("%.2f", x)   # display always 2 digits (also trailing 0s)
  return(x)
}

# transform Top-5 list into clean format
top5s.all.DTMs <- top5s.all.DTMs %>% 
  as_tibble() %>% 
  mutate_if(is.integer, as.numeric) %>% 
  mutate_at(vars(count.tot, count.g1, count.g2, count.g3), 
            funs(tomil))

# transform into DF instead of tibble, as stargazer requires this
top5s.all.DTMs <- as.data.frame(top5s.all.DTMs)

# export it:
stargazer(top5s.all.DTMs, 
          type = "latex",
          summary = F, 
          rownames = F, 
          colnames = T, 
          title = "Top-5 words for each LM-lexicon",
          label = "tab: top5_each_LM",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/top5_each_LM",
          digit.separate = 3,
          out.header = F,
          header = F,
          font.size = "small",
          digit.separator = ",",
          decimal.mark = ".",
          digits = NA,
          initial.zero = F,
          align = F)

# LM dict composition / stems ---------------------------------------------

# based on TsaiWangChien2016, p. 7:9

post_stem <- lm_dict_allcombined %>% 
  group_by(senti_cat) %>% 
  summarise(no.of.words.in.k = n()) %>% 
  arrange(senti_cat)

# those are what are left after stemming

# what did we have in the original PDFs? load from old .R script ...
kevinsreader <- readPDF(control = list(text = "-layout"))

lm_neg <- Corpus(URISource("/Users/kevin/Downloads/LM_dict_full_pdfs/LM_Negative.pdf"), 
                 readerControl = list(reader = kevinsreader))
# P
lm_pos <- Corpus(URISource("/Users/kevin/Downloads/LM_dict_full_pdfs/LM_Positive.pdf"), 
                 readerControl = list(reader = kevinsreader))
# U
lm_uncert <- Corpus(URISource("/Users/kevin/Downloads/LM_dict_full_pdfs/LM_Uncertainty.pdf"), 
                    readerControl = list(reader = kevinsreader))
# L
lm_liti <- Corpus(URISource("/Users/kevin/Downloads/LM_dict_full_pdfs/LM_Litigious.pdf"), 
                  readerControl = list(reader = kevinsreader))
# C
lm_constr <- Corpus(URISource("/Users/kevin/Downloads/LM_dict_full_pdfs/LM_Constraining.pdf"), 
                    readerControl = list(reader = kevinsreader))
# M
lm_moda <- Corpus(URISource("/Users/kevin/Downloads/LM_dict_full_pdfs/LM_Modal.pdf"), 
                  readerControl = list(reader = kevinsreader))

# nest a function to do all the cleaning as in the other file,
# avoid copy pasting from .R files
pdf.dict.cleaner <- function(x) {
  x <- content(x[[1]]) %>% strsplit(., "\n") %>% unlist(.) %>% tolower(.)
  x <- x[-1]
  return(x)
}

lm_neg <- pdf.dict.cleaner(lm_neg)
lm_pos <- pdf.dict.cleaner(lm_pos)
lm_uncert <- pdf.dict.cleaner(lm_uncert)
lm_liti <- pdf.dict.cleaner(lm_liti)
lm_constr <- pdf.dict.cleaner(lm_constr)
lm_moda <- pdf.dict.cleaner(lm_moda)

lm_moda_weak <- lm_moda[2:28]; lm_moda_modest <- lm_moda[30:43]; lm_moda_strong <- lm_moda[45:63]

rm(lm_moda, kevinsreader, pdf.dict.cleaner)

lm_neg <- cbind(lm_neg, "negative") %>% as.data.frame
lm_pos <- cbind(lm_pos, "positive") %>% as.data.frame
lm_uncert <- cbind(lm_uncert, "uncertain") %>% as.data.frame
lm_liti <- cbind(lm_liti, "litigious") %>% as.data.frame
lm_constr <- cbind(lm_constr, "constraining") %>% as.data.frame
lm_moda_weak <- cbind(lm_moda_weak, "weakmodal") %>% as.data.frame
lm_moda_modest <- cbind(lm_moda_modest, "modestmodal") %>% as.data.frame
lm_moda_strong <- cbind(lm_moda_strong, "strongmodal") %>% as.data.frame

colnames(lm_neg) <- colnames(lm_pos) <- 
  colnames(lm_uncert) <- colnames(lm_liti) <- 
  colnames(lm_constr) <- colnames(lm_moda_weak) <-
  colnames(lm_moda_modest) <- colnames(lm_moda_strong) <- c("word", "lex.tag")

# bind them
full.orig.LM <- rbind(lm_neg, lm_pos, lm_uncert, lm_liti, lm_constr,
                      lm_moda_strong, lm_moda_modest, lm_moda_weak)

# create counts per category
pre_stem <- full.orig.LM %>% as_tibble() %>% 
  mutate_if(is.factor, as.character) %>% 
  group_by(lex.tag) %>% 
  summarise(no.of.words.in.k = n()) %>% 
  arrange(lex.tag)

# add together pre/post stemmer counts
pre_stem; post_stem
colnames(pre_stem) <- c("LM_k", "Pre_stem")
colnames(post_stem) <- c("LM_k", "Post_stem")

LMs.pre.post.stem <- inner_join(pre_stem, 
                                post_stem, 
                                by = "LM_k")
  # mutate(dropratio = round((1 - Post_stem/Pre_stem) * 100))

# add LM category shortcuts (k)
LMs.pre.post.stem$k <- c("C", "L", "MM", "N", "P", "SM", "U", "WM")

# add the corpus survivors
# can only hope those do not become smaller due to dropping of obs

LMs.pre.post.stem$occ.in.corpus <- c(ncol(DTM_C), ncol(DTM_L), 
                                     ncol(DTM_MM), ncol(DTM_N), 
                                     ncol(DTM_P), ncol(DTM_SM), 
                                     ncol(DTM_U), ncol(DTM_WM))

# reorder cols in table format:
LMs.pre.post.stem <- LMs.pre.post.stem %>% 
  select(LM_k, k, Pre_stem, Post_stem, occ.in.corpus)

# add sums/totals
LMs.pre.post.stem <- rbind(LMs.pre.post.stem,
                           c("Total", "", 
                             sum(LMs.pre.post.stem$Pre_stem),
                             sum(LMs.pre.post.stem$Post_stem),
                             sum(LMs.pre.post.stem$occ.in.corpus)))

# cross-check that the sums are correct:
nrow(full.orig.LM)
nrow(lm_dict_allcombined)
sum(c(ncol(DTM_C), ncol(DTM_L), 
      ncol(DTM_MM), ncol(DTM_N), 
      ncol(DTM_P), ncol(DTM_SM), 
      ncol(DTM_U), ncol(DTM_WM)))

# add the number of uniques as very last row:
length(unique(full.orig.LM$word))
length(unique(lm_dict_allcombined$word))
full.LM.in.corpus <- c(colnames(DTM_C), colnames(DTM_L), 
                       colnames(DTM_MM), colnames(DTM_N), 
                       colnames(DTM_P), colnames(DTM_SM), 
                       colnames(DTM_U), colnames(DTM_WM))
length(full.LM.in.corpus);
length(unique(full.LM.in.corpus))

LMs.pre.post.stem <- rbind(LMs.pre.post.stem,
                           c("Uniques", "", 
                             length(unique(full.orig.LM$word)),
                             length(unique(lm_dict_allcombined$word)),
                             length(unique(full.LM.in.corpus))))

# export via sgzer
stargazer(LMs.pre.post.stem, 
          type = "latex",
          summary = F, 
          rownames = F, 
          colnames = T, 
          title = "Summary statistics about LM lexicon",
          label = "tab: LM_lexicon",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/sumstats_LM_lexicon",
          digit.separate = 3,
          out.header = F,
          header = F,
          font.size = "small",
          digit.separator = ",",
          decimal.mark = ".",
          digits = NA,
          initial.zero = F,
          align = F)
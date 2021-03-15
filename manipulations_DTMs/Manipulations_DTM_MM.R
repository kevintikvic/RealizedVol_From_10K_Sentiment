# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Description:  1) Load DTM_MM and apply different TF/frequency weighting schemes     #
#     Filename: Manipulations_DTM_MM                                                      #
#                                                                                         #
#     Date (last updated): July 17th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries / Settings ----------------------------------------------------

rm(list=ls()) # clear environment 
gc()
options(scipen = 999)
# setwd("/Volumes/LaCie/LM_data/")
setwd("/Users/kevin/Desktop/lm_testdata")
# install.packages("ggExtra")
library(tidyverse)
# install.packages("Matrix")
library(Matrix)

# Data Loading ------------------------------------------------------------
DTM_MM <- read.delim("/Users/kevin/Desktop/lm_testdata/sep_DTMs/DTM_MM.txt",
                     sep = ";", stringsAsFactors = F, dec = ",")

load("~/Desktop/lm_testdata/DB5_316x46483.RData")

# drop those term counts where PFRV was NA
DTM_MM <- DTM_MM[!rownames(DTM_MM) %in% unlist(list(PFRV_diers)), ]

# check that all survived:
sum(rownames(DTM_MM) %in% unlist(list(PFRV_survivors)))

# check that none of the colsums is zero (else this word should be dropped)
# reason: length variables might base upon ncol() count and not the non-zero count
sum(colSums(DTM_MM) == 0)
# remove the zero-counts
DTM_MM <- DTM_MM[, which(colSums(DTM_MM) != 0)]

# Export this environment for VIBTW regressions later on ... 
rm(DB5, PFRV_diers, PFRV_survivors)
save.image("~/Desktop/lm_testdata/sep_DTMs_RData/DTM_MM_46483.RData")

# WF ----------------------------------------------------------------------
# goal: replace all X > 0 values with 1 + log(X), leave zeroes as is
# # could use log(1 + X) instead, because this way I can leave the 0s
# # small example: assume tf = 8
# log(8); log1p(8); log(8+1); 1 + log(8)
# # 1 + log(X) is actually the same as log(e * X)
# # for larger tf's the delta increases, e.g., assume tf = 2 * 10^i
# e <- exp(1); log1p(2); log(2 + 1); log(e * 2); 1 + log(2)
# log1p(2); log(2 + 1); log(e * 2); 1 + log(2)
# log1p(200); log(200 + 1); log(e * 200); 1 + log(200)
# log1p(2000); log(2000 + 1); log(e * 2000); 1 + log(2000)
# log1p(20000); log(20000 + 1); log(e * 20000); 1 + log(20000)
# log1p(200000); log(200000 + 1); log(e * 200000); 1 + log(200000)
# log1p(2000000); log(2000000 + 1); log(e * 2000000); 1 + log(2000000)
# log1p(20000000); log(20000000 + 1); log(e * 20000000); 1 + log(20000000)
# 
# rm(e)

# but the delta is always < 1; and counts > 300 or 500 are very seldom (ca. 5%)
# check colMaxes -- see: median max count is appearance in 81 documents
countmaxes <- sapply(DTM_MM, max)
head(countmaxes); max(countmaxes); summary(countmaxes)
head(sort(countmaxes, decreasing = T), 40) # 40/800 = 5% are larger than 300
rm(countmaxes)

# lets use a) log1p as well as b) 1+log:
# a)
DTM_MM_WF_log1p <- log1p(DTM_MM)
# inspect
DTM_MM_WF_log1p %>% as_tibble %>% head(); DTM_MM %>% as_tibble %>% head()
# all good! 

# b)
# define function to return the according value
one_plus_log_fct <- function(x) {
  if_else(x == 0, 0, 1 + log(x))
}

# quick inspect on the function:
one_plus_log_fct(10); 1 + log(10)
one_plus_log_fct(0); 1 + log(0)
# works ... 

# lapply this function
DTM_MM_WF_1plog <- lapply(DTM_MM, one_plus_log_fct)
DTM_MM_WF_1plog <- as.data.frame(DTM_MM_WF_1plog)
rownames(DTM_MM_WF_1plog) <- rownames(DTM_MM)

# inspect
DTM_MM_WF_1plog %>% as_tibble %>% head(); DTM_MM %>% as_tibble %>% head()
# all good! 

rm(one_plus_log_fct)

# TFIDF -------------------------------------------------------------------

# create document count and IDF weight
df_v <- sapply(DTM_MM, Matrix::nnzero) # count non-zero elements
Ndocs <- rep(nrow(DTM_MM), length(df_v)) # create vector of same N = constant
idf_v <- log(Ndocs / df_v) # create IDF vector for 882 words

rm(df_v, Ndocs)

# Create TF IDF matrix (using trick of diagonal matrix and element-wise multiplication)
DTM_MM_TFIDF <- as.matrix(DTM_MM) %*% diag(idf_v) 

# "format" TF IDF 
DTM_MM_TFIDF <- as.data.frame(DTM_MM_TFIDF)
rownames(DTM_MM_TFIDF) <- rownames(DTM_MM)
colnames(DTM_MM_TFIDF) <- colnames(DTM_MM)

# inspect:
DTM_MM_TFIDF %>% as_tibble %>% head()

# WFIDF -------------------------------------------------------------------
# again, 2 versions:
DTM_MM_WFIDF_p1 <- as.matrix(DTM_MM_WF_log1p) %*% diag(idf_v) 
DTM_MM_WFIDF_p1 <- as.data.frame(DTM_MM_WFIDF_p1)
rownames(DTM_MM_WFIDF_p1) <- rownames(DTM_MM)
colnames(DTM_MM_WFIDF_p1) <- colnames(DTM_MM)
DTM_MM_WFIDF_p1 %>% as_tibble %>% head()

DTM_MM_WFIDF_1p <- as.matrix(DTM_MM_WF_1plog) %*% diag(idf_v) 
DTM_MM_WFIDF_1p <- as.data.frame(DTM_MM_WFIDF_1p)
rownames(DTM_MM_WFIDF_1p) <- rownames(DTM_MM)
colnames(DTM_MM_WFIDF_1p) <- colnames(DTM_MM)
DTM_MM_WFIDF_1p %>% as_tibble %>% head()

# TF-MAX ------------------------------------------------------------------
# normalization that divides each element in row i by the row i's maximum 
tf_jmax <- do.call(pmax, DTM_MM)
tf_jmax <- as.matrix(tf_jmax); dim(tf_jmax)
tf_jmax <- t(tf_jmax)
# problem: there are filings which have TF_max = 0 --> division by zero
sum(tf_jmax == 0)

# can not simply divide: lets use, if TF = 0 then TF_ScaledByMax = 0
# element-wise divison will introduce NaN's but these will be replaced by 0 afterwards
# create fraction / normalization
DTM_MM_TFMAX <- sweep(DTM_MM, 1, tf_jmax, FUN = "/")

# inspect:
DTM_MM_TFMAX %>% as_tibble() %>% head() # so far so good ...

# replace NA/Infs/NaN
sum(is.na(DTM_MM_TFMAX))
sum(!is.finite(as.matrix(DTM_MM_TFMAX))) # all are NAs due to 0 / 0
DTM_MM_TFMAX[is.na(DTM_MM_TFMAX)] <- 0

# scale the whole thing by smoothing constant "a":
a <- 0.4
DTM_MM_TFMAX <- a + (1-a) * DTM_MM_TFMAX

# inspect:
DTM_MM_TFMAX %>% as_tibble() %>% head()

rm(a, tf_jmax)

# TF-AVG ------------------------------------------------------------------
# scale each row not by rowmax but by rowmean
DTM_MM_TFAVG <- sweep(DTM_MM, 1, as.matrix(rowMeans(DTM_MM)), FUN = "/")

DTM_MM_TFAVG %>% as_tibble() %>% head()

# replace NAs by zero , in case mean was 0
DTM_MM_TFAVG[is.na(DTM_MM_TFAVG)] <- 0

# RELFREQ -----------------------------------------------------------------
# create "relative" frequencies, i.e, TF / ∑TF's per row 
DTM_MM_RELFREQ <- sweep(DTM_MM, 1, as.matrix(rowSums(DTM_MM)), FUN = "/")

DTM_MM_RELFREQ %>% as_tibble() %>% head()

# replace NAs by zero , in case mean was 0
DTM_MM_RELFREQ[is.na(DTM_MM_RELFREQ)] <- 0

# RF-IDF ------------------------------------------------------------------
DTM_MM_RFIDF <- as.matrix(DTM_MM_RELFREQ) %*% diag(idf_v) 
DTM_MM_RFIDF <- as.data.frame(DTM_MM_RFIDF)
rownames(DTM_MM_RFIDF) <- rownames(DTM_MM)
colnames(DTM_MM_RFIDF) <- colnames(DTM_MM)

DTM_MM_RFIDF %>% as_tibble() %>% head()

# replace NAs by zero , in case mean was 0
DTM_MM_RFIDF[is.na(DTM_MM_RFIDF)] <- 0

rm(idf_v)

# Export ------------------------------------------------------------------
save.image("~/Desktop/lm_testdata/DTMs_weighted_environment/DTM_MM_weightschemes.RData")

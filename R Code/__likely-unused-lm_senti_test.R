+
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: lm_senti_test                                                             #
#     Description:  1) load report by report in the yr-qtr folders and start "parsing"    #
#                   2) stem them, remove punctuation, remove stopwords, replace numbers   #
#                   3) output a "TDM", to which each report will cbind a column           #
#                                                                                         #
#     Date (last updated): June 22th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# MAIN SOURCE:
# https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/

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







# Corpora --------------------------------------------------------------

# install.packages("tm")
library(tm)

# Load the corpus, based on ALL files in the DirSource
# Recursive = T enables R to scan the sub-folders as well
# this could be a useful feature, if I do it year-wise
AAA <- Corpus(
  DirSource(directory = "/Users/kevin/Desktop/lm_testdata",
            recursive = T,
            mode = "text"))
writeLines(as.character(AAA[[2]]))

# Extract the file names in the folders so as to allow matching once we export DTM/scores
orig_file_names <- names(AAA)

# View a list of pre-defined corpus operations embedded in tm package
getTransformations()

# Try some of them on this small dummy corpus
# NB: the ordering actually DOES matter 
# (e.g., tolower should be run before stemming, stopwords should be removed first to make it faster)

# strip the punctuation
# includes dot, comma, apostroph, minus, etc.
AAA <- tm_map(AAA, removePunctuation)
writeLines(as.character(AAA[[2]]))

# convert all words to lower case letters
# NB: conversely, some prefer "toupper"
AAA <- tm_map(AAA, content_transformer(tolower))
writeLines(as.character(AAA[[2]]))

# Remove stopwords
AAA <- tm_map(AAA, removeWords, stopwords("english"))
writeLines(as.character(AAA[[2]]))

# NB: this is based on stopwords() = pre-defined list of 174 words by tm package in R
# inspect it:
stopwords(); length(stopwords())
# NB: also "no, nor, not" are in this list ("never" is not)
# --> important in case I want to use neg_tagging

# export this stop word list, e.g. for inclusion in thesis appendix
a <- stopwords()
write.table(a, 
            file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/stopwordlist.txt",
            quote = F,
            row.names = F,
            col.names = F,
            eol = ", ")

# stem the words (using wordStem from SnowballC implementation)
# this is based on Porter Stemmer
AAA <- tm_map(AAA, stemDocument)
writeLines(as.character(AAA[[2]]))

# Remove digits
AAA <- tm_map(AAA, removeNumbers)
writeLines(as.character(AAA[[2]]))

# OR
# alternatively, replace them by the hash sign using an own function

toHASH <- content_transformer(function(x, pattern) {return (gsub(pattern, "#", x))})
AAA <- tm_map(AAA, toHASH, "[0-9]+")
writeLines(as.character(AAA[[2]]))

# Note: all of these could/should be bundled into a single transformator function
# sth. like:
# clean.corpus <- function(corpus) {
#   corpus <- tm_map(corpus, removePunctuation)
#   corpus <- tm_map(corpus, stripWhitespace)
#   corpus <- tm_map(corpus, removeNumbers)
#   corpus <- tm_map(corpus, content_transformer(tolower))
#   corpus <- tm_map(corpus, removeWords, stopwords())
# }

# The qdap package offers other text cleaning functions. Each is useful in its own way and is particularly powerful when combined with the others.

# bracketX(): Remove all text within brackets (e.g. “It’s (so) cool” becomes “It’s cool”)
# replace_number(): Replace numbers with their word equivalents (e.g. “2” becomes “two”)
# replace_abbreviation(): Replace abbreviations with their full text equivalents (e.g. “Sr” becomes “Senior”)
# replace_contraction(): Convert contractions back to their base words (e.g. “shouldn’t” becomes “should not”)
# replace_symbol() Replace common symbols with their word equivalents (e.g. “$” becomes “dollar”)

# http://rstudio-pubs-static.s3.amazonaws.com/256588_57b585da6c054349825cba46685d8464.html

# TDM (full) --------------------------------------------------------------------

# Create DTM
dtm <- DocumentTermMatrix(AAA)
inspect(dtm[1:5, 1:5])

# Assign back the original file names as "docnames" in the DTM
# those get somehow lost during the remove/stem/etc. cleaning process
rownames(dtm) <- orig_file_names

# Sum up the word frequencies across docs
freq <- colSums(as.matrix(dtm))
# Order them from most to least common words
ord <- order(freq, decreasing = T)
# View the most / least frequent
freq[head(ord)]; freq[tail(ord)]

# Alternative way to find most frequent terms (instead of using table)
findFreqTerms(dtm, lowfreq = 500, highfreq = Inf)

# find associated words to "inception", with correlation of 0.5, 0.75, 1
findAssocs(dtm, "inception", 0.5)
findAssocs(dtm, "inception", 0.75)
findAssocs(dtm, "inception", 1)

# Create Wordcloud
library(wordcloud)
wordcloud(names(freq), 
          freq, 
          min.freq = 800, 
          colors=brewer.pal(6, "Dark2"))

# side note for JW approach: we will need "a", i.e., the length of each doc
# "problem": we stemmed, removed stopwords, etc. -- so did they ...
# nowhere it is specified to which "number of words" the a_i's refer ...
# let's try with the rowSums for the time being

lengths <- rowSums(as.matrix(dtm))


# LM Dictionaries / Loading & Merging -------------------------------------

# install.packages("SentimentAnalysis")
library(SentimentAnalysis)

# load and view LM lexicon: Loads LM financial dictionary into a standardized dictionary object 
# (here, ONLY categories positive and negative are considered)
# Note: a list of stemmed words in lower case
# lm_dict <- loadDictionaryLM()
# summary(lm_dict)
# 
# # Count no. of terms in the LM dicts
# numEntries(lm_dict); numNegativeEntries(lm_dict); numPositiveEntries(lm_dict)
# # quick inspection:
# head(lm_dict); tail(lm_dict)
# 
# # extract pos & neg (label them 2, as below I will use the "1" for the lists from SRAF)
# lm_pos2 <- unlist(lm_dict[1])
# lm_neg2 <- unlist(lm_dict[2])
# 
# # can "manually" add the uncertainty words, if wished
# lm_uncert2 <- loadDictionaryLM_Uncertainty()
# lm_uncert2 <- unlist(lm_uncert2)

# let's manually load the other lists (litig., modal, etc.) as well
# library(tm)
# kevinsreader <- readPDF(control = list(text = "-layout"))
# lm_constr <- Corpus(URISource("/Users/kevin/Downloads/LM_dict_full_pdfs/LM_Constraining.pdf"), 
#                    readerControl = list(reader = kevinsreader))
# lm_constr <- content(lm_constr[[1]])
# lm_constr <- strsplit(lm_constr, "\n")
# lm_constr <- unlist(lm_constr)
# lm_constr <- tolower(lm_constr)
# lm_constr <- stemDocument(lm_constr)
# lm_constr <- unique(lm_constr)
# lm_constr <- lm_constr[-1]
# 
# lm_liti <- Corpus(URISource("/Users/kevin/Downloads/LM_dict_full_pdfs/LM_Litigious.pdf"), 
#                     readerControl = list(reader = kevinsreader))
# lm_liti <- content(lm_liti[[1]])
# lm_liti <- strsplit(lm_liti, "\n")
# lm_liti <- unlist(lm_liti)
# lm_liti <- tolower(lm_liti)
# lm_liti <- stemDocument(lm_liti)
# lm_liti <- unique(lm_liti)
# lm_liti <- lm_liti[-1]
# 
# lm_moda <- Corpus(URISource("/Users/kevin/Downloads/LM_dict_full_pdfs/LM_Modal.pdf"), 
#                   readerControl = list(reader = kevinsreader))
# lm_moda <- content(lm_moda[[1]])
# lm_moda <- strsplit(lm_moda, "\n")
# lm_moda <- unlist(lm_moda)
# lm_moda <- tolower(lm_moda)
# lm_moda <- stemDocument(lm_moda)
# lm_moda <- unique(lm_moda)
# lm_moda <- lm_moda[-1]

# split into three (weak, modest, strong)
# NB: modest was stemmed to "moder"
# lm_moda_weak <- lm_moda[2:19]
# lm_moda_modest <- lm_moda[21:33]
# lm_moda_strong <- lm_moda[35:51]

# lm_neg <- Corpus(URISource("/Users/kevin/Downloads/LM_dict_full_pdfs/LM_Negative.pdf"), 
#                   readerControl = list(reader = kevinsreader))
# lm_neg <- content(lm_neg[[1]])
# lm_neg <- strsplit(lm_neg, "\n")
# lm_neg <- unlist(lm_neg)
# lm_neg <- tolower(lm_neg)
# lm_neg <- stemDocument(lm_neg)
# lm_neg <- unique(lm_neg)
# lm_neg <- lm_neg[-1]
# 
# lm_pos <- Corpus(URISource("/Users/kevin/Downloads/LM_dict_full_pdfs/LM_Positive.pdf"), 
#                   readerControl = list(reader = kevinsreader))
# lm_pos <- content(lm_pos[[1]])
# lm_pos <- strsplit(lm_pos, "\n")
# lm_pos <- unlist(lm_pos)
# lm_pos <- tolower(lm_pos)
# lm_pos <- stemDocument(lm_pos)
# lm_pos <- unique(lm_pos)
# lm_pos <- lm_pos[-1]
# 
# lm_uncert <- Corpus(URISource("/Users/kevin/Downloads/LM_dict_full_pdfs/LM_Uncertainty.pdf"), 
#                   readerControl = list(reader = kevinsreader))
# lm_uncert <- content(lm_uncert[[1]])
# lm_uncert <- strsplit(lm_uncert, "\n")
# lm_uncert <- unlist(lm_uncert)
# lm_uncert <- tolower(lm_uncert)
# lm_uncert <- stemDocument(lm_uncert)
# lm_uncert <- unique(lm_uncert)
# lm_uncert <- lm_uncert[-1]

# # check overlap of this pos/neg/uncert. list with the R-pre-installed
# lm_pos <- as.matrix(lm_pos)
# lm_pos2 <- as.matrix(lm_pos2)
# row.names(lm_pos) <- seq(1:nrow(lm_pos))
# colnames(lm_pos) <- "pos"
# row.names(lm_pos2) <- seq(1:nrow(lm_pos2))
# colnames(lm_pos2) <- "pos"
# 
# lm_neg <- as.matrix(lm_neg)
# lm_neg2 <- as.matrix(lm_neg2)
# # strip element 314 from the lm2 list, it is a empty "0"
# lm_neg2 <- as.matrix(lm_neg2[-314])
# row.names(lm_neg) <- seq(1:nrow(lm_neg))
# colnames(lm_neg) <- "neg"
# row.names(lm_neg2) <- seq(1:nrow(lm_neg2))
# colnames(lm_neg2) <- "neg"
# 
# lm_uncert <- as.matrix(lm_uncert)
# lm_uncert2 <- as.matrix(lm_uncert2)
# row.names(lm_uncert) <- seq(1:nrow(lm_uncert))
# colnames(lm_uncert) <- "uncert"
# row.names(lm_uncert2) <- seq(1:nrow(lm_uncert2))
# colnames(lm_uncert2) <- "uncert"
# 
# all.equal(lm_pos, lm_pos2)
# all.equal(lm_neg, lm_neg2)
# all.equal(lm_uncert, lm_uncert2)

# convert also the others:
# lm_constr <- as.matrix(lm_constr)
# row.names(lm_constr) <- seq(1:nrow(lm_constr))
# colnames(lm_constr) <- "constr"
# 
# lm_liti <- as.matrix(lm_liti)
# row.names(lm_liti) <- seq(1:nrow(lm_liti))
# colnames(lm_liti) <- "liti"
# 
# lm_moda_weak <- as.matrix(lm_moda_weak)
# row.names(lm_moda_weak) <- seq(1:nrow(lm_moda_weak))
# colnames(lm_moda_weak) <- "modal_weak"
# 
# lm_moda_modest <- as.matrix(lm_moda_modest)
# row.names(lm_moda_modest) <- seq(1:nrow(lm_moda_modest))
# colnames(lm_moda_modest) <- "modal_modest"
# 
# lm_moda_strong <- as.matrix(lm_moda_strong)
# row.names(lm_moda_strong) <- seq(1:nrow(lm_moda_strong))
# colnames(lm_moda_strong) <- "model_strong"

# check if by coincidence any of the lm lists contain stop words
# lm_dict_allcombined <- as.data.frame(rbind(lm_pos,
#                          lm_neg,
#                          lm_uncert,
#                          lm_constr,
#                          lm_liti,
#                          lm_moda_weak,
#                          lm_moda_modest,
#                          lm_moda_strong))

# add indicator that shows to which lexicon word belongs
# # this is tricky, because words belong to more than one list
# nrow(lm_dict_allcombined) - nrow(unique(lm_dict_allcombined)) # 140 words overlap
# 
# # still do it, it will only keeo the "last" list in the loop (ordering matters)
# lm_dict_allcombined$indicator <- NA
# 
# i <- 1
# for (i in 1:nrow(lm_dict_allcombined)) {
#   if(lm_dict_allcombined[i,1] %in% lm_pos) (lm_dict_allcombined[i,2] <- "pos")
#   if(lm_dict_allcombined[i,1] %in% lm_neg) (lm_dict_allcombined[i,2] <- "neg")
#   if(lm_dict_allcombined[i,1] %in% lm_uncert) (lm_dict_allcombined[i,2] <- "uncert")
#   if(lm_dict_allcombined[i,1] %in% lm_constr) (lm_dict_allcombined[i,2] <- "constr")
#   if(lm_dict_allcombined[i,1] %in% lm_liti) (lm_dict_allcombined[i,2] <- "liti")
#   if(lm_dict_allcombined[i,1] %in% lm_moda_weak) (lm_dict_allcombined[i,2] <- "moda_weak")
#   if(lm_dict_allcombined[i,1] %in% lm_moda_modest) (lm_dict_allcombined[i,2] <- "moda_modest")
#   if(lm_dict_allcombined[i,1] %in% lm_moda_strong) (lm_dict_allcombined[i,2] <- "moda_strong")
# }

# export the full (UNIQUE!) dictionary
# write.table(unique(lm_dict_allcombined),
#             file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/LM_dict_full_unique.txt",
#             quote = F,
#             row.names = F,
#             col.names = F,
#             eol = ", ")

# and the separate dictionaries as well:
# write.table(lm_pos,
#             file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/LM_pos.txt",
#             quote = F,
#             row.names = F,
#             col.names = F,
#             eol = ", ")
# 
# write.table(lm_neg,
#             file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/LM_neg.txt",
#             quote = F,
#             row.names = F,
#             col.names = F,
#             eol = ", ")
# 
# write.table(lm_uncert,
#             file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/LM_uncert.txt",
#             quote = F,
#             row.names = F,
#             col.names = F,
#             eol = ", ")
# 
# write.table(lm_constr,
#             file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/LM_constr.txt",
#             quote = F,
#             row.names = F,
#             col.names = F,
#             eol = ", ")
# 
# write.table(lm_liti,
#             file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/LM_liti.txt",
#             quote = F,
#             row.names = F,
#             col.names = F,
#             eol = ", ")
# 
# write.table(lm_moda_weak,
#             file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/LM_wmodal.txt",
#             quote = F,
#             row.names = F,
#             col.names = F,
#             eol = ", ")
# 
# write.table(lm_moda_modest,
#             file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/LM_mmodal.txt",
#             quote = F,
#             row.names = F,
#             col.names = F,
#             eol = ", ")
# 
# write.table(lm_moda_strong,
#             file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/LM_smodal.txt",
#             quote = F,
#             row.names = F,
#             col.names = F,
#             eol = ", ")

lm_sep_howmany <- matrix(NA, 8, 2)

lm_sep_howmany[,1] <- c("Positive", "Negative", "Uncertainty", "Constraint",
                        "Litigious", "Weak Modal", "Modest Modal", "Strong Modal")

lm_sep_howmany[1,2] <- length(lm_pos)
lm_sep_howmany[2,2] <- length(lm_neg)
lm_sep_howmany[3,2] <- length(lm_uncert)
lm_sep_howmany[4,2] <- length(lm_constr)
lm_sep_howmany[5,2] <- length(lm_liti)
lm_sep_howmany[6,2] <- length(lm_moda_weak)
lm_sep_howmany[7,2] <- length(lm_moda_modest)
lm_sep_howmany[8,2] <- length(lm_moda_strong)

library(stargazer)

stargazer(lm_sep_howmany, 
          summary = FALSE, 
          rownames = F, 
          colnames = F, 
          column.labels = c("LM Word List", "No. of Words"),
          title = "Size of LM Word Lists",
          out = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/lm_lists_numbers.tex",
          digit.separate = 3,
          digit.separator = ",",
          digits = 0)

write.table(lm_sep_howmany,
            file = "/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables/lm_lists_numbers.txt", 
            quote = F,
            row.names = F,
            col.names = F,
            sep = " ($V^{}$): ",
            eol = ", ")

sum(as.numeric(lm_sep_howmany[,2]))

# check how many of the words appear in the stop word list
intersect(unlist(lm_dict_ext), a) 
#only 2 words ("against" and "could")

# TDM - LM words only -----------------------------------------------------

# keep from the dtm only the terms that are in the LM dict list of 1030 (both + and - words)
# note that for DTM the terms are in the cols, so if one uses TDM we have to amend accordingly
dtm_LM <- dtm[, colnames(dtm) %in% lm_dict]

inspect(dtm_LM)
# problem: it only has 654 terms instead of the 1,030, because the %in% only takes the ones already existant
# solution noob: cbind the missing and fill their counts with zeros... ? 
# maybe the problem gets mitigated if the corpus grows and all 1,030 will be covered
sum(colnames(dtm) %in% lm_dict) # this gives the 654 that he used

# Adding missing LMs ------------------------------------------------------

# check which ones are missing
intersect <- Terms(dtm[, colnames(dtm) %in% lm_dict])
sum(lm_dict %ni% intersect) # this should give the 376 missing ones...

# extract them:
missingLMs <- lm_dict[lm_dict %ni% intersect]
missingLMs <- unlist(missingLMs)
names(missingLMs) <- NULL
head(missingLMs); tail(missingLMs)
length(missingLMs)

# create zero count df based on this missing list, then cbind it with the DTM list
miss_LMs_add <- matrix(0, length(Docs(dtm_LM)), length(missingLMs))
colnames(miss_LMs_add) <- missingLMs

dtm_LM_allposneg <- as.data.frame(cbind(as.matrix(dtm_LM), miss_LMs_add))
head(dtm_LM_allposneg)

nrow(dtm_LM_allposneg); ncol(dtm_LM_allposneg)
typeof(dtm_LM_allposneg); str(dtm_LM_allposneg)

colnames(dtm_LM_allposneg); rownames(dtm_LM_allposneg)

# Compute tf idf & scores manually

# log dampenening
dtm_LM_allposneg <- (dtm_LM_allposneg != 0) * (1 + log(dtm_LM_allposneg, 10))
dtm_LM_allposneg[is.nan(as.matrix(dtm_LM_allposneg))] <- 0 # convert the infinities to 0

# Count non zero elements per columns - this is document frequency for each word
capN <- ncol(dtm_LM_allposneg)
capV <- nrow(dtm_LM_allposneg)

idf_weights <- log(capV / (colSums(dtm_LM_allposneg != 0) + 1), 10) # used 10-based log
# NB here I used a trick to add 1 in the smoother, as otherwise for OOV words we would devide by infinity

idf_weights <- as.matrix(idf_weights)
nrow(idf_weights); ncol(idf_weights)

# divide each of the rows by this row so as to obtain tf-idf
dtm_LM_allposneg <- dtm_LM_allposneg * idf_weights

# Calculate score by taking rowSums
scores_dummy <- rowSums(dtm_LM_allposneg)

# alternative: scores with k+1 smoothing
dtm_LM_allposneg <- as.data.frame(cbind(as.matrix(dtm_LM), miss_LMs_add))
dtm_LM_allposneg <- dtm_LM_allposneg + 1
dtm_LM_allposneg <- (dtm_LM_allposneg != 0) * (1 + log(dtm_LM_allposneg, 10))
dtm_LM_allposneg[is.nan(as.matrix(dtm_LM_allposneg))] <- 0 
idf_weights <- log(capV / (colSums(dtm_LM_allposneg != 0)) + 1, 10) # again, adjustment by 1
idf_weights <- as.matrix(idf_weights)
dtm_LM_allposneg <- dtm_LM_allposneg * idf_weights
scores_dummy_smooth <- rowSums(dtm_LM_allposneg)

summary(scores_dummy - scores_dummy_smooth)

# Export scores mapped to filenames/ IDs
write.table(scores_dummy,
          file = "/Users/kevin/Desktop/lm_testdata/scores_dummyLM_1405.txt",
          row.names = T,
          sep = ";",
          quote = F,
          col.names = F)

# OLD:
# Sum up the word frequencies across docs
freq_LM <- colSums(as.matrix(dtm_LM_allposneg))
# Order them from most to least common words
ord_LM <- order(freq_LM, decreasing = T)
# View the most / least frequent
freq_LM[head(ord_LM)]; freq_LM[tail(ord_LM)]

# compare the lengths of the 48 docs
lengths_LM <- rowSums(as.matrix(dtm_LM))

# Create Wordcloud
library(wordcloud)
wordcloud(names(freq_LM), 
          freq_LM, 
          min.freq = 100, 
          colors=brewer.pal(6, "Dark2"))

# TDM - tfidf weights -----------------------------------------------------

dtm_tfidf <- DocumentTermMatrix(AAA,
                          control = list(weighting = weightTfIdf))

inspect(dtm_tfidf)

# extract LM only again
dtm_LM_tfidf <- dtm_tfidf[, colnames(dtm_tfidf) %in% lm_dict]

inspect(dtm_LM_tfidf)

# freq counts
freq_LM_tfidf <- colSums(as.matrix(dtm_LM_tfidf))
# Order them from most to least common words
ord_LM_tfidf <- order(freq_LM_tfidf, decreasing = T)
# View the most / least frequent
freq_LM_tfidf[head(ord_LM_tfidf)]; freq_LM_tfidf[tail(ord_LM_tfidf)]

# for comparison, recall the "regular" pure counts
freq_LM[head(ord_LM)]; freq_LM[tail(ord_LM)]

# indeed, the words differ quite drastically...

# side note for tf.idf
# the idf could also be computed using the original DTM and counting the non-zero colSums
dfs <- colSums(as.matrix(dtm_LM) != 0)


rm(list=ls()) # clear environment 
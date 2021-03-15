
# Libraries and Settings --------------------------------------------------

# install.packages("corpus")
library(corpus)
# install.packages("quanteda")
library(quanteda)
# install.packages("tm")
library(tm)
# install.packages("e1071")
library(e1071)
library(stringr)

rm(list=ls()) # clear environment 
setwd("/Users/kevin/Desktop/lm_testdata")

# Using Corpus ------------------------------------------------------------

# This is based on: https://cran.r-project.org/web/packages/corpus/vignettes/corpus.html

# list all txt files, recursive options enables browsing through subfolders
filinglist <- list.files(pattern = ".\\d+\\.txt$", 
                         recursive = T)

# Read the file line by line
file1 <- readLines(filinglist[1])

# Collapse all lines to one single document, replace the line separator by nothing " "
file1 <- paste(file1,
               collapse = " ")

# strip file from extra whitespaces
file1 <- trimws(file1)

# extract the files .txt name so as to allow identification later on
file1name <- substring(filinglist[1], 11)

# create a corpus from it
corp1_doclevel <- corpus_frame(title = file1name, text = file1)

# Loop for test files
i <- 2 # first exists already so as to create corpus

for (i in 2:length(filinglist)) {
  tempfile_text <- readLines(filinglist[i])
  tempfile_text <- paste(tempfile_text,
                         collapse = " ")
  tempfile_text <- trimws(tempfile_text)
  tempfile_title <- substring(filinglist[i], 11)
  corp1_doclevel <- rbind(corp1_doclevel, 
                          c(tempfile_title, tempfile_text))
  i <- i + 1
}

rm(tempfile_text, tempfile_title, i)

# inspect it and show some statistics to the 2 files
corp1_doclevel$title; text_stats(corp1_doclevel)

# Create a TDM
tdm1_doclevel <- term_matrix(corp1_doclevel)
dim(tdm1_doclevel)


# Sentence Level ----------------------------------------------------------

# Split into sentences using corpus::text_split function
corp2_sentlevel <- text_split(corp1_doclevel)
text_stats(corp2_sentlevel)
head(corp2_sentlevel)

# glance / cross check: does doc1 has 508 sentences in this case
tempdoc <- corp2_sentlevel[corp2_sentlevel$parent == 1,]
nrow(tempdoc)
sum(text_ntoken(tempdoc))
text_ntype(tempdoc, 
           collapse = T)

# all good, hence:
rm(tempdoc)

# eliminate all those sentences that have less than 5 words
# other thresholds obviously possible
corp2_sentlevel_larger5w <- corp2_sentlevel[!ntoken(as.character(corp2_sentlevel[,3])) < 5, ]

# Check that none survived somehow
sum(ntoken(as.character(corp2_sentlevel_larger5w[,3])) < 5)

# all good...

# indexing:
corp2_sentlevel_larger5w[,3]
corp2_sentlevel_larger5w[,3][1:10]

# # Using TM ----------------------------------------------------------------
# 
# kiwicorp_TM <- Corpus(
#   DirSource(directory = "/Users/kevin/Desktop/lm_testdata/filings",
#             recursive = T,
#             mode = "text"))
# 
# docnames <- names(kiwicorp_TM)
# 
# tm_to_quanteda <- corpus(kiwicorp_TM$content, docvars = kiwicorp_TM$dmeta)
# 
# kiwicorp_TM_sentlevel <- corpus_reshape(tm_to_quanteda, 
#                                         to = "sentences")
# 
# # tidies ------------------------------------------------------------------
# 
# file1_tid_sent <- file1 %>%
#   as_tibble(.) %>%
#   unnest_tokens(., output = sentences, input = value, token = "sentences")
# 
# aa <- tidy(corp1_doclevel)
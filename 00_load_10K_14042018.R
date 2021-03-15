##############################
   # Title: First Tries using 10K files
   # === === === === === === === === === === === === === === === === 
   # Date: generated on 14.04.2018, last updated on:
   # === === === === === === === === === === === === === === === === 
   # Sources:
             # http://programminghistorian.github.io/ph-submissions/lessons/published/basic-text-processing-in-r
             # https://cran.r-project.org/web/packages/readtext/vignettes/readtext_vignette.html
             # http://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/
##############################

#####
# Install packages                  #
# install.packages("tidyverse")     # 
# install.packages("tokenizers")    #
# install.packages("readtext")      # 
# install.packages("readr")         # 
# install.packages("tm")            # 
#####

#####
# Load packages
library(tidyverse)
library(tokenizers)
library(readtext)
library(readr)
library(tm)
#####

# Load sample txt (for 10K's) + PDF (for Analyst Reports)
setwd("/Users/kevintikvic/Dropbox/Master_Thesis/Code/10K")

# Using readtext package
corpus_10K <- readtext("20170126_10-K_edgar_data_1123316_0001102624-17-000046_1.txt")
corpus_ar <- readtext("cs_amazon_samplefile.pdf")

# Using readr package (returns string character)
mystring.10K <- read_file("20170126_10-K_edgar_data_1123316_0001102624-17-000046_1.txt")
mystring.AR <- read_file("cs_amazon_samplefile.pdf") # does not seem to work very well

# Using tm
txt_files <- list.files(pattern = "txt$")
pdf_files <- list.files(pattern = "pdf$")

# define function for PDF
Rpdf <- readPDF(control = list(text = "-layout"))

opinions <- Corpus(URISource(pdf_files), 
                   readerControl = list(reader = Rpdf))

opinions.tdm <- TermDocumentMatrix(opinions, control = list(removePunctuation = TRUE,
                                                            stopwords = TRUE,
                                                            tolower = TRUE,
                                                            stemming = TRUE,
                                                            removeNumbers = TRUE,
                                                            bounds = list(global = c(3, Inf)))) 

inspect(opinions.tdm[1:5,])
findFreqTerms(opinions.tdm, lowfreq = 20, highfreq = Inf)

sentences_ar <- tokenize_sentences(corpus_ar)
sentences_10K <- tokenize_sentences(corpus_10K)

sentences_ar <- data.frame(Reduce(rbind, sentences_ar))
sentences_10K <- data.frame(Reduce(rbind, sentences_10K))

nrow(sentences_ar)
nrow(sentences_10K)

sentences_10K[1:3,]
# sentences_10K[15:17,]
sentences_10K[55:57,1]
sentences_10K[300:304,1]

sentences_10K <- cbind(sentences_10K, NA)

sentences_10K[55:57,2] <- c("nr", "nr", "r")

### 

corp_10K <- Corpus(URISource(txt_files))
tdm_corpus_10K <- TermDocumentMatrix(corp_10K, control = list(removePunctuation = TRUE,
                                                              stopwords = TRUE,
                                                              tolower = TRUE,
                                                              stemming = TRUE,
                                                              removeNumbers = TRUE,
                                                              bounds = list(global = c(3, Inf)))) 
inspect(tdm_corpus_10K[1:3,])

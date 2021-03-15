# PREFIX ============================================================================= #
#                                                                                      # 
#    Author: Kevin Tikvic                                                              #
#    Title: corpus_10K                                                                 #
#                                                                                      #
#    Description:  loading and inspecting the 10K-txt-corpus provided by Tsai & Wang   #
#                  This will provide an insight about potential issues in NLP.         #
#                  URL:                                                                #
#                                                                                      #
#   file created on: 4/22/18                                                           # 
#   last updated on: 4/28/18                                                           #
#                                                                                      # 
# PRELIMINARY STEPS ================================================================== #

rm(list=ls()) # clear environment 

# Load libraries:
# install.packages("tm")
library(tm)

# LOAD MD&A FILES & STORE THEM INTO A YEAR-WISE CORPUS ===============================================

# create list of all "mda"-files per year
setwd("/Users/kevin/Desktop/FIN10K_DATA/all.mda") # set working directory
MDA_folders <- list.files(pattern = ".mda$")
setwd("/Users/kevin/Desktop/FIN10K_DATA") # set working directory

# create loop to read in the MDA files contained in each yearly sub-folder
# the created data.frames will be named "Corpus_yyyy"
i <- 1996
for (folderindex in MDA_folders) {
  setwd(paste("~/Desktop/FIN10K_DATA/all.mda/", folderindex, sep="")) 
  assign(paste("Corpus_", i, sep=""), Corpus(URISource(list.files(pattern = ".mda$"))))
  i <- i+1
  folderindex <- paste(i, ".mda", sep="")
}

# Save Data Image File to avoid running the loop again
save.image("~/Desktop/FIN10K_DATA/MDA_corpora_per_year.RData")

# Check a sample corpus, in this case the 2013 MD&A files
# install.packages("tokenizers")
library(tokenizers)

# create Term-Doc-Matrix
TDM_2013 <- TermDocumentMatrix(Corpus_2013, control = list(removePunctuation = TRUE,
                                                              stopwords = TRUE,
                                                              tolower = TRUE,
                                                              stemming = TRUE,
                                                              removeNumbers = TRUE,
                                                              bounds = list(global = c(3, Inf)))) 
inspect(TDM_2013[1:30,])
findFreqTerms(TDM_2013, lowfreq = 50000, highfreq = Inf)

# END ===============================================
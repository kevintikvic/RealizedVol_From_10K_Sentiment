
# PREFIX ------------------------------------------------------------------

# --------------------------------------------------------------------------------------- #
#                                                                                         #
#     Author: Kevin Tikvic                                                                #
#     Filename: text_related_RHS_vars                                                     #
#     Description:  1) read all filings and create word counts for each document          #
#                   2) export matrix of 67,730 filings x N/P/U/L/C/SM/WM/MM               #
#                   3) append to this matrix potential other textual RHS vars             #
#                      (e.g., size as proxy for readability, # of exhibits, etc.)         #
#                                                                                         #
#     Date (last updated): June 28th, 2018                                                #
#                                                                                         #
# --------------------------------------------------------------------------------------- #

# Libraries / Settings ----------------------------------------------------
rm(list=ls()) # clear environment 
options(scipen = 999)
# setwd("/Volumes/LaCie/LM_data/")
setwd("/Users/kevin/Desktop/lm_testdata")

library(corpus); library(ggplot2); library(scales); library(quanteda)
library(tm); library(qdap); library(e1071); library(tidytext)
library(dplyr); library(SentimentAnalysis)

# LM Dicts ----------------------------------------------------------------

# load wordlists, provided in PDFs at: https://sraf.nd.edu/textual-analysis/resources/#LM%20Sentiment%20Word%20Lists

# define a reader for PDFs
kevinsreader <- readPDF(control = list(text = "-layout"))

# load them
# N
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

# Transformations: extract text, each string in one line, unlist them, 
#                  lowercase letters, stemming, keep only unique (stems !), dropping 1st element (title)
lm_neg <- content(lm_neg[[1]]); lm_pos <- content(lm_pos[[1]]); lm_uncert <- content(lm_uncert[[1]])
lm_liti <- content(lm_liti[[1]]); lm_constr <- content(lm_constr[[1]]); lm_moda <- content(lm_moda[[1]])

# N
lm_neg <- lm_neg %>% strsplit(., "\n") %>% unlist(.) %>% tolower(.) %>% stemDocument(.) %>% unique(.)
lm_neg <- lm_neg[-1]
# P
lm_pos <- lm_pos %>% strsplit(., "\n") %>% unlist(.) %>% tolower(.) %>% stemDocument(.) %>% unique(.)
lm_pos <- lm_pos[-1]
# U
lm_uncert <- lm_uncert %>% strsplit(., "\n") %>% unlist(.) %>% tolower(.) %>% stemDocument(.) %>% unique(.)
lm_uncert <- lm_uncert[-1]
# L
lm_liti <- lm_liti %>% strsplit(., "\n") %>% unlist(.) %>% tolower(.) %>% stemDocument(.) %>% unique(.)
lm_liti <- lm_liti[-1]
# C
lm_constr <- lm_constr %>% strsplit(., "\n") %>% unlist(.) %>% tolower(.) %>% stemDocument(.) %>% unique(.)
lm_constr <- lm_constr[-1]
# M
lm_moda <- lm_moda %>% strsplit(., "\n") %>% unlist(.) %>% tolower(.) %>% stemDocument(.) %>% unique(.)
lm_moda <- lm_moda[-1]

# Split M into WM, MM, SM
lm_moda_weak <- lm_moda[2:19]; lm_moda_modest <- lm_moda[21:33]; lm_moda_strong <- lm_moda[35:51]
rm(lm_moda)

# quick check on the N, P, U lists that are provided by the SentimentAnalysis package
LM_dicts_SApackage <- loadDictionaryLM(); lm_uncert2 <- loadDictionaryLM_Uncertainty()

# extract N, P, U (label them with suffix 2
lm_pos2 <- unlist(LM_dicts_SApackage[1]); lm_neg2 <- unlist(LM_dicts_SApackage[2])
lm_uncert2 <- unlist(lm_uncert2)

# strip element 314 from the lm2 list, it is an empty "0 / NULL"
lm_neg2 <- as.matrix(lm_neg2[-314])

rm(LM_dicts_SApackage)

# compare them:
# N
length(lm_neg); length(lm_neg2); 
sum(lm_neg == lm_neg2)
# P
length(lm_pos); length(lm_pos2); 
sum(lm_pos == lm_pos2)
# U
length(lm_uncert); length(lm_uncert2); 
sum(lm_uncert == lm_uncert2)

# they are all equal, so I can continue using the SRAF ones with all the other lists
rm(lm_neg2, lm_pos2, lm_uncert2)

# Export the 8 SRAF lists
# put them in a list
LM_lists_combined <- list(lm_neg, lm_pos, lm_uncert, lm_liti, lm_constr, 
                          lm_moda_strong, lm_moda_modest, lm_moda_weak)
# assign names to the list
names(LM_lists_combined) <- c("LM_N", "LM_P", "LM_U", "LM_L",
                              "LM_C", "LM_SM", "LM_MM", "LM_WM")

# re-set wd
setwd("/Users/kevin/Dropbox/Master_Thesis/LaTeX_File/Results_Tables")

# loop to create a TXT file with list of words for each of the 8 LM lexica
for (lm_lex in names(LM_lists_combined)) {
  filename <- paste(lm_lex, ".txt", sep = "")
  write.table(LM_lists_combined[[lm_lex]], 
              file = filename, 
              col.names = F, row.names = F, 
              eol = ", ", quote = F)
}

# also export a TXT with all LM lexica combined into a single list, one with all, one with uniques only
lm_dict_allcombined <- unlist(LM_lists_combined)
write.table(lm_dict_allcombined, 
            file = "./lm_dict_allcombined.txt", 
            col.names = F, row.names = F, 
            eol = ", ", quote = F)

lm_dict_allcombined_uniques <- unique(lm_dict_allcombined)
write.table(lm_dict_allcombined_uniques, 
            file = "./lm_dict_allcombined_uniques.txt", 
            col.names = F, row.names = F, 
            eol = ", ", quote = F)

# transform into tibbles
lm_dict_allcombined <- cbind(names(lm_dict_allcombined), lm_dict_allcombined) %>% as_data_frame(.)
lm_dict_allcombined_uniques <- as_data_frame(lm_dict_allcombined_uniques)

# Export also the list of duplicate words
write.table(lm_dict_allcombined[duplicated(lm_dict_allcombined$lm_dict_allcombined) | duplicated(lm_dict_allcombined$lm_dict_allcombined, fromLast = T), ], 
            file = "./lm_dict_allcombined_dupies.txt", 
            col.names = F, row.names = F, 
            sep = "\t", quote = F)

rm(filename, kevinsreader, LM_lists_combined, lm_lex)

# Compare LM unique list with stop word lists by TM package
kev.stopw <- tm::stopwords() %>% as_tibble() # 174 stop words

# compare with: lm_dict_allcombined_uniques
stoppies.in.LM <- inner_join(lm_dict_allcombined_uniques,
                             kev.stopw)

stoppies.in.LM # we have 6 words co-occurring

# TM Package --------------------------------------------------------------

# Corpus Creation ---------------------------------------------------------
setwd("/Users/kevin/Desktop/lm_testdata")

# Load corpus from all txt files in given DirSource
corp_TM_doclevel <- Corpus(DirSource(directory = "/Users/kevin/Desktop/lm_testdata/filings",
                                     recursive = T,            
                                     mode = "text"))

# summary details about the corpus, like no. of docs in it ... 
corp_TM_doclevel

# access single document in it using DOUBLE SQUARED BRACES (!) 
# print generic info:
corp_TM_doclevel[[1]]
# print meta info:
corp_TM_doclevel[[1]]$meta
# print textual content: 
writeLines(as.character(corp_TM_doclevel[[1]]))
# similar, but losing the "formatting":
corp_TM_doclevel[[1]]$content

# Using the meta information saved by TM, we can access the so-called document ID
# in our case, this is the txtfilename:
corp_TM_doclevel[[1]]$meta$id
# alternatively, we can use the base::names() function:
ctmdl.names <- names(corp_TM_doclevel)

# Extraction length variables ---------------------------------------------
# Extract doc-length variables from corpus, leave 4 cols for DTM related length measures
doclengthvars <- data.frame(txtfilename = ctmdl.names,
                            nb.tokens = NA, # based on corpus
                            nb.types = NA, # based on corpus
                            nb.sentences = NA, # based on corpus
                            full.DTM.counts = NA, # based on DTM 
                            full.DTM.nonzeroes = NA, # based on DTM
                            LM.DTM.counts = NA, # based on DTM
                            LM.DTM.nonzeroes = NA) # based on DTM

# Extract cols 2,3,4 using quanteda functions
doclengthvars$nb.tokens <- lapply(corp_TM_doclevel, ntoken)
doclengthvars$nb.types <- lapply(corp_TM_doclevel, ntype)
doclengthvars$nb.sentences <- lapply(corp_TM_doclevel, nsentence)

# Extraction Header Information -------------------------------------------
    # // this is commented out, as the task was (already) performed in Google Cloude VM Engine // #
    
    # # create empty matrix to receive all those infos, name cols accordingly and fill 1st col with txtfilename
    # gfs.mat <- as.data.frame(matrix(NA, length(corp_TM_doclevel), 5))
    # colnames(gfs.mat) <- c("txtfilename", "GFS", "NFS", "Exhi", "Tabs")
    # gfs.mat$txtfilename <- ctmdl.names
    # 
    # # Loop to extract from each filing in the Corpus the requested infos
    # i <- 1
    # for (i in 1:nrow(gfs.mat)) {
    #   # Declare the content of the doc we want to extract from
    #   file_to_be_analyzed <- corp_TM_doclevel[[i]]$content
    #   
    #   # Extract the 4 vars we want:
    #   # Gross File Size
    #   gfs <- str_match_all(file_to_be_analyzed,
    #                        "\\<GrossFileSize\\>(.*?)\\<\\/GrossFileSize\\>")
    #   gfs <- unlist(gfs)[2]
    #   # Net File Size
    #   nfs <- str_match_all(file_to_be_analyzed,
    #                        "\\<NetFileSize\\>(.*?)\\<\\/NetFileSize\\>")
    #   nfs <- unlist(nfs)[2]
    #   # Number of Tables
    #   tab <- str_match_all(file_to_be_analyzed,
    #                        "\\<N_Tables\\>(.*?)\\<\\/N_Tables\\>")
    #   tab <- unlist(tab)[2]
    #   # Number of Exhibits
    #   exh <- str_match_all(file_to_be_analyzed,
    #                        "\\<N_Exhibits\\>(.*?)\\<\\/N_Exhibits\\>")
    #   exh <- unlist(exh)[2]
    #   
    #   # Fill vars into gfs.mat:
    #   gfs.mat$GFS[i]  <- gfs; gfs.mat$NFS[i]  <- nfs
    #   gfs.mat$Exhi[i] <- exh; gfs.mat$Tabs[i] <- tab
    #   
    #   # Remove 4 var values to avoid carrying them to next line; incr. counter
    #   rm(gfs, nfs, exh, tab); i <- i + 1
    # }
    # 
    # rm(i, file_to_be_analyzed)
    # head(gfs.mat)
    
    # # Export gfs.mat
    # write.table(gfs.mat, 
    #             file = "/Users/kevin/Desktop/lm_testdata/gfs-nfs-tab-exh_header_data.txt", 
    #             col.names = T, row.names = F, 
    #             sep = ";", quote = F)

# Corpus Transformations, Manipulations, Cleaning -------------------------

# here, function tm::tm_map is key; it performs basic transformations on the corpus
tm::getTransformations() # list of "all" available transforms
# but we can define own ones, using a wrapper in content_transformer(.)

# NOTE: the ordering DOES matter for the transformations
# (e.g., tolower should be run before stemming, ...)
      # 1) remove excess spaces, 
      # 2) replace symbols by text equivalents (e.g., dollar for $)
      # 3) replace abbreviations by full expression (e.g., Dr. becomes Doctor, 14 phrases given in qdapDictionaries::abbreviations, can be manually appended)
      # 4) replace contractions by full expression (e.g., isn't becomes is not, 70 phrases given in qdapDictionaries::contractions, can be manually appended)
      # 5) remove punctiation
      # 6) lowercase letters, 
      # 7) remove numbers (as also dates, 10-K* references etc are included)
      # 8) stemming, 
# could also define my own function for task 7:
# toHASH <- content_transformer(function(x, pattern) {return (gsub(pattern, "#", x))})

# for step (3): define additional abbreviations to be replaced, define function to replace
fun.replace_ext_abbrevs <- content_transformer(function(x, custlist_abbrevs) replace_abbreviation(x, abbreviation = custlist_abbrevs))
addit_abbrevs <- data.frame(abv = c("USD", "CHF", "EUR", "GBP", "e.g.", "CAD"),
                            rep = c("US Dollar", "Swiss Franc", "Euro", "British Pound", "eg", "Canadian Dollar"))
mycustlist.abbrevs <- rbind(qdapDictionaries::abbreviations, addit_abbrevs)
rm(addit_abbrevs)

# view doc before:
writeLines(as.character(corp_TM_doclevel[[1]]))

# now transform:
corp_TM_doclevel_cleaned <- corp_TM_doclevel %>% 
  tm_map(., stripWhitespace) %>% # 1
  tm_map(., content_transformer(replace_symbol)) %>% # 2
  tm_map(., replace_abbreviation, mycustlist.abbrevs) %>% # 3
  tm_map(., content_transformer(replace_contraction)) %>% # 4
  tm_map(., removePunctuation) %>% # 5
  tm_map(., content_transformer(tolower)) %>% # 6
  tm_map(., removeNumbers) %>% # 7
  tm_map(., stemDocument) # 8

# can add further Regex to replace, for instance, "USdollar" with "US dollar"

# view doc afterwards:
writeLines(as.character(corp_TM_doclevel_cleaned[[1]]))

# Caution: applying those transformator loses meta information, most importantly the ID/names
# we can restore them -/somehow/- later on, using variable ctmdl.names saved manually above
# a simple re-assignment doesn't work, as object of type corpus does not seem to allow this
names(corp_TM_doclevel_cleaned)

# LM-Subsetting corpus using tidies ---------------------------------------

# first off, define a list of words (which previously were symbols, but got converted using qdap::replace_symbol)
finterms_i_wanna_keep <- c("percent", "percentage", "dollar", "dollars", "euro", 
                           "euros", "pound", "pounds", "yen", "franc", "francs", "usd")

# add senti category to LM list
lm_dict_allcombined <- lm_dict_allcombined %>% mutate(., senti_cat = NA)
lm_dict_allcombined$senti_cat[1:884] <- "negative"
lm_dict_allcombined$senti_cat[885:1029] <- "positive"
lm_dict_allcombined$senti_cat[1030:1158] <- "uncertain"
lm_dict_allcombined$senti_cat[1159:1609] <- "litigious"
lm_dict_allcombined$senti_cat[1610:1666] <- "constraining"
lm_dict_allcombined$senti_cat[1667:1683] <- "strongmodal"
lm_dict_allcombined$senti_cat[1684:1696] <- "modestmodal"
lm_dict_allcombined$senti_cat[1697:1714] <- "weakmodal"

is.na(lm_dict_allcombined$senti_cat) %>% sum(.)

# rename cols
colnames(lm_dict_allcombined) <- c("LM_tag", "word", "senti_cat")
lm_dict_allcombined

# combine LM list and finterms I wish to keep
fins_to_be_kept_tbl <- as_tibble(data.frame(LM_tag = rep("KT_FIN", length(finterms_i_wanna_keep)), 
                                            word = finterms_i_wanna_keep, 
                                            senti_cat = rep("KT", length(finterms_i_wanna_keep))))

# keeping only those tokens that are in LM list (+ $, €, £ and %)
tbl_TM_doclevel <- as_tibble(corp_TM_doclevel_cleaned) %>% 
  unnest_tokens(word, text, token = "words") %>% 
  inner_join(., rbind(lm_dict_allcombined, fins_to_be_kept_tbl), by = "word")

# count how often word W appears in the tibble
W <- "dollar"

tbl_TM_doclevel %>% 
  filter(., word == W) %>% 
  summarise(., counthowmany = n())

# quick count/sort of most common words and most common categories
tbl_TM_doclevel %>% count(., word, sort = T)
tbl_TM_doclevel %>% count(., senti_cat, sort = T)
tbl_TM_doclevel %>% count(., word, senti_cat, sort = T)

# last one shows to be careful, as we need to count per SENTI_CAT
# else it seems as if word "will" appears 1,107 times
# but in fact it appears only 369 times, but in 3 lists (liti, neg, str. mod.)

# DTM Creation ------------------------------------------------------------
# on full, initial corpus
dtm_TM_doclevel <- DocumentTermMatrix(corp_TM_doclevel_cleaned)

# summary stats about DTM, like no. of docs, no. of terms, sparsity, ...
dtm_TM_doclevel
# these figures can also access those separately:
Docs(dtm_TM_doclevel) # Note: retrieves the txtfilenames, similar to the 2 commands on corpus level above
# note the "unsorted" doc names without leading zeros
# to rename, transform -- final -- result below to a matrix and re-set its rownames()
# dtm_as_mat <- as.matrix(dtm_TM_doclevel)
# rownames(dtm_as_mat) <- ctmdl.names
nDocs(dtm_TM_doclevel)
Terms(dtm_TM_doclevel)
nTerms(dtm_TM_doclevel)
# inspect a small sample of the first 10 rows and some cols 
inspect(dtm_TM_doclevel)
# can also use matrix indexing to subset what we seek for
inspect(dtm_TM_doclevel[1:4, 1:10])

# LM-Subset of the DTM ----------------------------------------------------

# subset only those columns of the DTM that are in LM list or finterms, i.e. those in:
terms_tbkept <- rbind(lm_dict_allcombined, fins_to_be_kept_tbl)
terms_tbkept <- terms_tbkept$word

"%ni%" <- Negate("%in%") # creates function for "not in" 

ncol(dtm_TM_doclevel)
sum(colnames(dtm_TM_doclevel) %in% terms_tbkept); sum(colnames(dtm_TM_doclevel) %ni% terms_tbkept)
sum(colnames(dtm_TM_doclevel) %in% terms_tbkept) + sum(colnames(dtm_TM_doclevel) %ni% terms_tbkept)

# subetting in standard matrix index notation
dtm_TM_doclevel_LM_only <- dtm_TM_doclevel[, colnames(dtm_TM_doclevel) %in% terms_tbkept]

inspect(dtm_TM_doclevel_LM_only); ncol(dtm_TM_doclevel_LM_only)

# LM words NOT in initial DTM¨ --------------------------------------------

# NOTE:
# the DTM has 557 words in this case, although we used 1,726 terms to subset columns
# R can obviously only subset those 558 that actually ocurred in DTM
# we could/can manually add the (1,726 - 558) = 1,168 missing using a simple cbind

# inspect the ones NOT in both lists
terms_tbkept %>% as_tibble(.) %>% arrange(value) # 1726
colnames(dtm_TM_doclevel_LM_only) %>% as_tibble(.) %>% arrange(value) # 558

# random check for a word
"addendum" %in% colnames(dtm_TM_doclevel_LM_only); "addendum" %in% terms_tbkept
"abl" %in% colnames(dtm_TM_doclevel_LM_only); "abl" %in% terms_tbkept
"will" %in% colnames(dtm_TM_doclevel_LM_only); "will" %in% terms_tbkept

# the ones not contained in the "full" DTM
not_in_initial_DTM <- anti_join(as_tibble(terms_tbkept), 
                                as_tibble(colnames(dtm_TM_doclevel_LM_only)), 
                                by = "value")

nrow(not_in_initial_DTM); not_in_initial_DTM %>% unique(.) %>% nrow(.)
terms_tbkept %ni% colnames(dtm_TM_doclevel_LM_only) %>% sum

# Export not-in-initial-DTM as list, for potential cbind later
write.table(unlist(not_in_initial_DTM$value),
            file = "/Users/kevin/Desktop/lm_testdata/not-in-initial-DTM.txt",
            eol = ", ", quote = F, row.names = F, col.names = F)

# something APPEARS wrong here... 
# why is the length of the "missings" 1,095 and not 1,726 - 558 = 1,168?
# it is due to dupes in the terms_tbkept
# # try using the unique LM list
# all good ... !!! 

rm(W, fun.replace_ext_abbrevs, mycustlist.abbrevs)

# DTM exports -------------------------------------------------------------

# rename doc names and export
dtm_as_mat <- as.matrix(dtm_TM_doclevel_LM_only)
dtm_as_mat_full <- as.matrix(dtm_TM_doclevel)
rownames(dtm_as_mat) <- ctmdl.names; rownames(dtm_as_mat_full) <- ctmdl.names

write.table(dtm_as_mat, 
            file = "/Users/kevin/Desktop/lm_testdata/DTM_LM_occurring-only.txt", 
            col.names = T, row.names = T, sep = ";", quote = F)

write.table(dtm_as_mat_full, 
            file = "/Users/kevin/Desktop/lm_testdata/DTM_all-orig-terms.txt", 
            col.names = T, row.names = T, sep = ";", quote = F)

# Length vars from DTM ----------------------------------------------------

# doc-lengths based on DTM counts (already stemmed (and) LM-related)
doclengthvars$full.DTM.counts <- rowSums(dtm_as_mat_full)
doclengthvars$full.DTM.nonzeroes <- rowSums(dtm_as_mat_full != 0)
doclengthvars$LM.DTM.counts <- rowSums(dtm_as_mat)
doclengthvars$LM.DTM.nonzeroes <- rowSums(dtm_as_mat != 0)

# Export length-related variables (NB: last 2 cols not really senseful to use)
write.table(as.matrix(doclengthvars), 
            file = "/Users/kevin/Desktop/lm_testdata/doc_length_variables.txt", 
            col.names = T, row.names = F, sep = ";", quote = F)

# DTM manipulations -------------------------------------------------------

# multiply each columns with weight scalar, given in vector
estimated_weights <- rep(0.123, 
                         ncol(dtm_as_mat)) # 558 made up weights for 558 words

# using linear algebra:
# (1. create diagonal matrix from vector, 2. apply element-wise product)
wd <- diag(estimated_weights) # 1
weighted_dtm <- dtm_as_mat %*% wd # 2

# can apply rowsum to compute scores 
scores <- rowSums(weighted_dtm) %>% as.data.frame()
colnames(scores) <- "score.all.LM"

# plot scores over docs (= time, as sequential)
ggplot(data = scores, 
       aes(x = 1:nrow(scores), 
           y = score.all.LM,
           group = 1)) + 
  geom_area() + geom_point() + 
  scale_x_discrete(limits = seq(1, 12, 5)) + 
  scale_y_continuous(labels = comma) + 
  labs(title = "Scores for Filings (sorted by submission date)",
       subtitle = "Scores were computed over full set of words in *all* LM lexica.",
       x = "",
       y = "Sentiment Score") + 
  theme_minimal()

# -- DTM AS TIBBLE -- #
# can transform the (weighted) dtm into tidy format to continue working with it
tidy_dtm <- as_tibble(as.data.frame(dtm_as_mat))
rownames(tidy_dtm) # just to check that those are kept... 
weighted_tidy_dtm <- as_tibble(as.data.frame(weighted_dtm))
rownames(weighted_tidy_dtm) # just to check that those are kept... 

# ************ #
#    FAILED    # 
# ************ #

# Constructing the DTM from the "restricted", cleaned TIDY corpus
tbl_TM_doclevel_withcounts <- tbl_TM_doclevel %>% 
  count(., doc_id, word, sort = T) %>% 
  arrange(doc_id)

# using tidytext::cast_dtm() function
# it requires exactly the inputs like they are in my tbl_TM_doclevel_withcounts
# see: https://www.tidytextmining.com/dtm.html#cast-dtm 
dtm_TM_doclevel_tidy <- tbl_TM_doclevel_withcounts %>% cast_tdm(word, doc_id, n)

# hm, yet that fails ...........

# -- TM TO OTHER PACKAGES -- #
# Transform a TM corpus into a quanteda-type corpus:
# Source: https://github.com/quanteda/quanteda/issues/680
qteda_corpus_fromTM <- corpus(corp_TM_doclevel$content, docvars = corp_TM_doclevel$dmeta)

# 8 separate LM DTMs ------------------------------------------------------
# plus 1 one more for my own finterms!

# quick inspect on what the col-dimension of these matrices should be
rbind(lm_dict_allcombined, fins_to_be_kept_tbl) %>%
  group_by(senti_cat) %>% 
  summarise(percatcount = n()) %>% 
  arrange(desc(percatcount)) 

# the LM and KT dicts already exist from above
# subetting in standard matrix index notation
dtm_TM_N <- dtm_TM_doclevel[, colnames(dtm_TM_doclevel) %in% lm_neg]
dtm_TM_P <- dtm_TM_doclevel[, colnames(dtm_TM_doclevel) %in% lm_pos]
dtm_TM_U <- dtm_TM_doclevel[, colnames(dtm_TM_doclevel) %in% lm_uncert]
dtm_TM_L <- dtm_TM_doclevel[, colnames(dtm_TM_doclevel) %in% lm_liti]
dtm_TM_C <- dtm_TM_doclevel[, colnames(dtm_TM_doclevel) %in% lm_constr]
dtm_TM_SM <- dtm_TM_doclevel[, colnames(dtm_TM_doclevel) %in% lm_moda_strong]
dtm_TM_MM <- dtm_TM_doclevel[, colnames(dtm_TM_doclevel) %in% lm_moda_modest]
dtm_TM_WM <- dtm_TM_doclevel[, colnames(dtm_TM_doclevel) %in% lm_moda_weak]
dtm_TM_KT <- dtm_TM_doclevel[, colnames(dtm_TM_doclevel) %in% finterms_i_wanna_keep]

# we could cbind those 9 back together to check and check their coldims 
ncol(dtm_TM_N); ncol(dtm_TM_P); ncol(dtm_TM_U); ncol(dtm_TM_L); ncol(dtm_TM_C)
ncol(dtm_TM_SM); ncol(dtm_TM_MM); ncol(dtm_TM_WM); ncol(dtm_TM_KT)

ncol(dtm_TM_N) + ncol(dtm_TM_P) + ncol(dtm_TM_U) + ncol(dtm_TM_L) + ncol(dtm_TM_C) + ncol(dtm_TM_SM) + ncol(dtm_TM_MM) + ncol(dtm_TM_WM) + ncol(dtm_TM_KT)
# 631 cols, not 558 as in the "global" case -- this will be due to dupes
# the DTM command can only take the uniques

# Export the 9 DTMs with their counts 
rownames(dtm_TM_N) <- ctmdl.names; rownames(dtm_TM_P) <- ctmdl.names
rownames(dtm_TM_U) <- ctmdl.names; rownames(dtm_TM_L) <- ctmdl.names
rownames(dtm_TM_C) <- ctmdl.names; rownames(dtm_TM_SM) <- ctmdl.names
rownames(dtm_TM_WM) <- ctmdl.names; rownames(dtm_TM_MM) <- ctmdl.names
rownames(dtm_TM_KT) <- ctmdl.names

# put the 9 DTMs in a list
LM_DTMs_list <- list(dtm_TM_N, dtm_TM_P, dtm_TM_U, dtm_TM_L, dtm_TM_C, 
                     dtm_TM_SM, dtm_TM_WM, dtm_TM_MM, dtm_TM_KT)
# assign names to the list
names(LM_DTMs_list) <- c("DTM_N", "DTM_P", "DTM_U", "DTM_L",
                         "DTM_C", "DTM_SM", "DTM_MM", "DTM_WM", "DTM_KT")

# loop to create 9 TXT files with the 9 separate DTMs
for (lm_dtm in names(LM_DTMs_list)) {
  filename <- paste("/Users/kevin/Desktop/lm_testdata/", 
                    lm_dtm, 
                    ".txt", 
                    sep = "")
  write.table(as.matrix(LM_DTMs_list[[lm_dtm]]), 
              file = filename, 
              col.names = T, row.names = T, 
              sep = ";", quote = F)
}

rm(lm_dtm, LM_DTMs_list, filename)

# Export working environment, so as to have them to load in GCE
save.image("~/Desktop/lm_testdata/environ-for-GCE.RData")

# quanteda package --------------------------------------------------------

# -- LOADING A CORPUS -- #
require(readtext)

# Note: this loading method requires explicit filepath
# thus requires loop as no recursion option is available
corp_qtd_doclevel <- readtext("/Users/kevin/Desktop/lm_testdata/filings/1994/QTR1/*.txt")
corp_qtd_doclevel <- quanteda::corpus(corp_qtd_doclevel)
summary(corp_qtd_doclevel)

# To extract texts from a corpus, we use an extractor, called texts():
texts(corp_qtd_doclevel)[2]

# transferring TM corpus to quanteda corpus
corp_TM_doclevel <- Corpus(DirSource(directory = "/Users/kevin/Desktop/lm_testdata/filings",
                                     recursive = T,            
                                     mode = "text"))
qteda_corpus_doclevel_fromTM <- corpus(corp_TM_doclevel$content, docvars = corp_TM_doclevel$dmeta)

# key feature of this package architecture: DOCVARS (meta-variables on document level)
# e.g. create running index and then plot number of sentences per doc
docvars(qteda_corpus_doclevel_fromTM, "run_id") <- 1:ndoc(qteda_corpus_doclevel_fromTM)

# can also export some stats 
qtd_sumstats <- summary(qteda_corpus_doclevel_fromTM)
sum(qtd_sumstats$Sentences) # e.g., to check the total no. of sentences in ALL files
qtd_sumstats$Text # e.g., to retrieve docnames 

# similar equivalents:
ntoken(qteda_corpus_doclevel_fromTM) %>% sum; sum(qtd_sumstats$Tokens)
ntype(qteda_corpus_doclevel_fromTM) %>% sum; sum(qtd_sumstats$Types)

# then plot
ggplot(data = qtd_sumstats, 
       aes(x = run_id, 
           y = Sentences,
           group = 1)) + 
  geom_area() + geom_point() + 
  scale_x_discrete(limits = seq(1, 12, 1)) + 
  scale_y_continuous(labels = comma) + 
  labs(title = "Number of Sentences for Filings (sorted by submission date)",
       subtitle = "Sentence segmentation was performed by the default algorithm of quanteda package in R. \nThe dashed line represents the median number of sentences per filing.",
       x = "",
       y = "No. of Sentences in the Filing",
       caption = "Sample size: 12 filings (1994 - 2017)") + 
  theme_minimal() +
  geom_hline(aes(yintercept = median(Sentences)),
             color = "lightblue", linetype = "dashed", size = 0.5)

# we can subset the whole corpus based on docvars (e.g. sentence number)
corpus_subset(qteda_corpus_doclevel_fromTM, run_id > 10) # only 2 should survive

# the "pre-filled" docvars seem not to be accessible
# but we could simply mirror them from the summary stats DF and then access them
docvars(qteda_corpus_doclevel_fromTM, "no.of.sentences") <- qtd_sumstats$Sentences
corpus_subset(qteda_corpus_doclevel_fromTM, no.of.sentences > 1000) %>% summary()

# corpus Package ----------------------------------------------------------

# -- LOADING A CORPUS -- #
setwd("/Users/kevin/Desktop/lm_testdata/filings") # re-set WD
filinglist <- list.files(pattern = ".\\d+\\.txt$", 
                         recursive = T)

# Read the first file of the filing list into R, performed line by line
file1 <- readLines(filinglist[1])

# Collapse all lines to one single document; 
# done by replacing the line separator by nothing " "
# then trim excess whitespaces
file1 <- file1 %>%  paste(., collapse = " ") %>% trimws(.)

# Extract file name that will serve as document identifier in the corpus file
file1name <- substring(filinglist[1], 11)

# Create a corpus from this one document, then we will add further docs via loop
corp1_doclevel <- corpus_frame(title = file1name, text = file1)

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

rm(file1, file1name, tempfile_text, tempfile_title, i)

# -- FURTHER STUFF WITHIN THE corpus PACKAGE -- #
# ...


# Sentence Level ----------------------------------------------------------

# -- SENTENCE LEVEL -- #

# multiple options: running operation on 
# a) a (quanteda) corpus, 
# b) a tibble (created from a corpus) 
# c) a character string (within a corpus / tibble ???)

# OPTION A: #
quanteda::corpus_reshape()

# short example
corp_TM_doclevel <- Corpus(DirSource(directory = "/Users/kevin/Desktop/lm_testdata/filings",
                                     recursive = T,            
                                     mode = "text"))

qteda_corpus_doclevel_fromTM <- corpus(corp_TM_doclevel$content, docvars = corp_TM_doclevel$dmeta)
sum(nsentence(qteda_corpus_doclevel_fromTM))

# can also export some stats 
qtd_sumstats <- summary(qteda_corpus_doclevel_fromTM)
sum(qtd_sumstats$Sentences) # e.g., to replicate the no. of sentence command above

# as we saw above, we have 8,018 documents in the doclevel corpus -- let's split into those:
qteda_corpus_sentlevel <- corpus_reshape(qteda_corpus_doclevel_fromTM, 
                                         to = "sentences")

# as expected now we have 8,018 DOCUMENTS (as now a document = a sentence) 
# NOTE: the "unit" of a corpus is 1 documents

# key: using this method, each txtfilename gets appended ".number" to indicate the sentence number
docnames(qteda_corpus_sentlevel) %>% head()

# OPTION B: #
tidytext::unnest_tokens()
corp_TM_doclevel <- Corpus(DirSource(directory = "/Users/kevin/Desktop/lm_testdata/filings",
                                     recursive = T,            
                                     mode = "text"))
corp_tidte_fromTM <- corp_TM_doclevel %>% 
  as_tibble() %>% 
  unnest_tokens(sentence, text, token = "sentences")

# here you already see that the separation algo is different;
# we have 8,173 sentences, above with quanteda we had about 170 less

# OPTION C: #
corpus::text_split()

# short example
text_split("This is a sentence. 
           And another on. 
           This one as well? 
           Most def.", units = "sentences")
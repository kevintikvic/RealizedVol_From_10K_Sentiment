# Forecasting  Realized Equity Volatility From Text Sentiment Revealed Revealed by Company Filings

This is a repository containing data- and code files for my 2018 master thesis "Forecasting Forecasting Realized Equity Volatility From Text Sentiment Revealed Revealed by Company Filings", submitted to Università Commerciale Luigi Bocconi. 

The main research question was:

> Can the textual content (sentiment) embedded in 10-Ks help to explain realized return volatility in the week after the submission of the filing?


## Data

The main data structure is as follows:

* 46,483 corporate 10-K filings (median report length: 49,117 words / 4,415 word types; time horizon covered: 1999 – 2017)
* Five so-called «LM» dictionaries (Loughran & McDonald, 2011): Negative, Positive, Uncertainty, Litigious, Strong Modal 
* Price and trading volume data around the filing’s submission date (one year pre- and one week post-filing) used to compute the main variable of interest: post-filing realized volatility (𝑃𝐹𝑅𝑉)
* Control variables: pre-filing realized volatility, firm size, book-to-market ratio, trading volume, VIX, financial leverage, filing year/month/weekday/monthday dummies, filing type dummies, sector dummies


## Sketch of Research Methodology

* For each of the five «LM» dictionaries, the connoted keywords are counted in each 10-K filing, constructing what is referred to as document-term-matrix (DTM).
* Generally, the «raw» term counts are not used for further analysis. Instead, different term weighting schemes are applied. The most common is the term frequency-inverse document frequency (TF-IDF). 
* Instead of conventional term weighting methods, a method similar to the one introduced in Jegadeesh & Wu (2011) was applied. The key idea can be summarised as follows: *all negative words are likely to be «negative» indeed, however, they might not be equally negative*.
* For each sentiment word, a weight is estimated on the basis of the impact the word had on «past» volatility. This is performed by a (pooled OLS) linear regression.
* The sentiment score for each document is then constructed as the simple sum of the weighted relative term frequencies, with the weights being the z-standardized coefficient estimates from the training sample regressions. 
* The application of this procedure for each LM «sentiment category» leads to the construction of the following five textual variables: negative sentiment (`𝑁𝐸𝐺_𝑆𝐸𝑁𝑇`), positive sentiment (`𝑃𝑂𝑆_𝑆𝐸𝑁𝑇`), uncertainty in language (`𝑈𝑁𝐶𝐸𝑅𝑇`), usage of litigious language (`𝐿𝐼𝑇𝐼`), usage of strong modal words (indicating assertiveness, `𝐴𝑆𝑆𝐸𝑅𝑇`).
* Two additional textual variables (`𝐺𝐹𝑆` and `𝐹𝐼𝑁`) were added to the model in order to test for filing readability as well as focus on financial terminology.

## Full Thesis / Executive Presentation

If allowed - attach here the (a) full thesis PDF, and (b) the summary presentation PDF I used in the thesis defense in Milano. 

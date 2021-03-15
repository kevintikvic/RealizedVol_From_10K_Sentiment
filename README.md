# Forecasting  Realized Equity Volatility From Text Sentiment Revealed Revealed by Company Filings

This is a repository containing data- and code files for my 2018 master thesis "Forecasting Forecasting Realized Equity Volatility From Text Sentiment Revealed Revealed by Company Filings", submitted to Università Commerciale Luigi Bocconi. 

The main research question was:

> Can the textual content (sentiment) embedded in 10-Ks help to explain realized return volatility in the week after the submission of the filing?


## Data

The main data structure is as follows:

* 46,483 corporate 10-K filings (median report length: 49,117 words / 4,415 word types; time horizon covered: 1999 – 2017). These can be downloaded by navigating the Google Drive links provided by Bill McDonald on his [Software Repository for Accounting and Finance (SRAF)](https://sraf.nd.edu/data/stage-one-10-x-parse-data/). 
* Five so-called «LM» dictionaries [(Loughran & McDonald, 2011)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1331573): Negative, Positive, Uncertainty, Litigious, Strong Modal 
* Price and trading volume data around the filing’s submission date (one year pre- and one week post-filing) used to compute the main variable of interest: post-filing realized volatility (`𝑃𝐹𝑅𝑉`)
* Control variables: pre-filing realized volatility, firm size, book-to-market ratio, trading volume, VIX, financial leverage, filing year/month/weekday/monthday dummies, filing type dummies, sector dummies


## Sketch of Research Methodology

* For each of the five «LM» dictionaries, the connoted keywords are counted in each 10-K filing, constructing what is referred to as document-term-matrix (DTM).
* Generally, the «raw» term counts are not used for further analysis. Instead, different term weighting schemes are applied. The most common is the term frequency-inverse document frequency (TF-IDF). 
* Instead of conventional term weighting methods, a method similar to the one introduced in [Jegadeesh & Wu (2013)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1787273) was applied. The key idea can be summarised as follows: *all negative words are likely to be «negative» indeed, however, they might not be equally negative*.
* For each sentiment word, a weight is estimated on the basis of the impact the word had on «past» volatility. This is performed by a (pooled OLS) linear regression.
* The sentiment score for each document is then constructed as the simple sum of the weighted relative term frequencies, with the weights being the z-standardized coefficient estimates from the training sample regressions. 
* The application of this procedure for each LM «sentiment category» leads to the construction of the following five textual variables: negative sentiment (`𝑁𝐸𝐺_𝑆𝐸𝑁𝑇`), positive sentiment (`𝑃𝑂𝑆_𝑆𝐸𝑁𝑇`), uncertainty in language (`𝑈𝑁𝐶𝐸𝑅𝑇`), usage of litigious language (`𝐿𝐼𝑇𝐼`), usage of strong modal words (indicating assertiveness, `𝐴𝑆𝑆𝐸𝑅𝑇`).
* Two additional textual variables (`𝐺𝐹𝑆` and `𝐹𝐼𝑁`) were added to the model in order to test for filing readability as well as focus on financial terminology.

Finally, for the out-of sample years, a regression is estimated in order to test the impact of 10-K text content on «unobserved» `PFRV`. Five annual regressions (2013-2017) as well as a single pooled model were estimated. Each of those variants was in turn estimated in three versions, namely using the sentiment scores based on term weights estimated from a static / rolling / extending / training set.


## Research Hypotheses

* Negative sentiment embedded in 10-Ks is associated with higher `𝑃𝐹𝑅𝑉`
* Positive sentiment embedded in 10-Ks is associated with `𝑃𝐹𝑅𝑉`
* Assertiveness in the management’s 10-K writing style is associated with lower `𝑃𝐹𝑅𝑉`
* Usage of language related to uncertainty in 10-Ks is associated with higher `𝑃𝐹𝑅𝑉`
* Litigious language embedded in 10-Ks is associated with higher `𝑃𝐹𝑅𝑉`
* Longer, and consequently less readable 10-Ks (`GFS`), are associated with higher `𝑃𝐹𝑅𝑉`
* The management’s usage of financial keywords (`FIN`) is associated with lower `𝑃𝐹𝑅𝑉`


## Key Findings

* A market-based term weighting scheme was introduced in the field of textual analysis for volatility prediction.
* The key finding of the regression analysis shows that negative and (false) positive language in 10-K text do increase post-filing realized volatility, while other textual aspects are significant only for specific years and/or model choices.
* The most important drivers of volatility appear to be «known» factors, of which the highest relevance can be attributed to the forecasts from time-series models.


## Robustness Checks

* Conventional term weighting schemes, such as the TF-IDF, were used as benchmark models against the volatility-based weighting method.
* Potential mismeasurement of the volatility proxy was tested by using two common alternative proxies (absolute returns and squared returns).
* `𝑃𝑟𝑒𝐹𝑅𝑉` was replaced by two more «powerful» predictors, namely the 1-week ahead volatility forecasts from a GARCH and GJR-GARCH model, respectively. 


## Full Thesis / Executive Summary

If allowed - I will attach here the (a) full thesis PDF, and (b) the summary presentation PDF I used in the thesis defense in Milano. 

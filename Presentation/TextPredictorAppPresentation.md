Presentation for Text Predictor Application
========================================================
author: Justin Z.
date: July 30, 2019
autosize: TRUE

Navigate using arrow keys or clicking arrow buttons in the lower right corner.


Overview
========================================================

This presentation is for the capstone project of the Data Science Specialization
from Johns Hopkins University on Coursera. The goals of the overall project are
listed below:

 - Analyze a large corpus of text documents
 - Discover structure in the data and how words are put together
 - Clean and analyze the text data
 - Build a predictive text model
 - Build a predictive text product that uses the model

To meet the objective an application was created which is now hosted on
shinyapps.io [here][1]. Additional information about the project can be found in
the GitHub repo [here][2].

[1]: https://jtzingsheim.shinyapps.io/TextPredictorApp/  "app on shinyapps.io"
[2]: https://github.com/jtzingsheim1/Text-Prediction-Model-and-App  "proj. repo"


The Data
========================================================

The data for this project were provided by the course, and came from sources
across the web such as: blogs, news articles, and twitter. The training corpus
was assembled by reading in up to 400,000 lines from each of the three sources,
and this works out to be about 56% of the data. This tradeoff was made in the
interest of performance time, which is a key requirement of the app product.

The corpus was preprocessed by removing all of these elements:
 - Numbers
 - Punctuation and Symbols
 - URLs and Twitter handles
 - Profanity


The Algorithm
========================================================

In early phases of the project Katz backoff models were attempted, but they
consumed too many computing resources. Instead a [stupid backoff][3] model was
implemented, and was later modified to further reduce the prediction time.

The algorithm works by looking in a reference table for ngrams and calculating
scores based on their frequencies. The table object is pre-constructed and
simply loaded when the app is loaded to minimize the amount of computation
occuring during the prediction phase. The table object contains ngrams from 1 to
4. Some 5-gram versions were evaluated, but they underperformed 4 gram versions.

[3]: https://www.aclweb.org/anthology/D07-1090 "Googles paper on stupid backoff"


The App
========================================================

<small>
The input text from the user is pre-processed and then sent to the algorithm
where it is trimmed to be no longer than a trigram. The algorithm looks for
the supplied trigram in the table, and if it finds a match it calculates scores
for each of the observed 4-grams. If it finds at least 5 words that complete the
trigram, it processes and returns those results.

If it finds less than 5 words it backs off from trigrams to bigrams, and repeats
as needed until it finds at least 5 five words at one of the levels. This
technique achieves an accuracy of roughly 12% which is about average based on
other's reports in the class. On a local machine the app takes 1-2 seconds to
output a prediction, but on the shiny servers it seems to be taking slightly
longer.

The app includes instructions on how to operate it. The user simply enters text
in the box and clicks the action button. The algorithm returns the prediction
results and an explaination.
</small>


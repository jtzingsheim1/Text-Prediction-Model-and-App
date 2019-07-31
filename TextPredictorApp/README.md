# Text Predictor App README


## Overview

This repo contains my work for the [capstone project][1] of the
[Data Science Specialization][2] from Johns Hopkins University on Coursera. This
folder contains my work for one of the key deliverables: the application.

[1]: https://www.coursera.org/learn/data-science-project  "DS Capstone Site"
[2]: https://www.coursera.org/specializations/jhu-data-science  "DS Spec."
[3]: https://github.com/jtzingsheim1/Text-Prediction-Model-and-App  "project repo"
[4]: https://jtzingsheim.shinyapps.io/TextPredictorApp/  "app on shinyapps.io"


## Detailed Description

The overall [project][3] focuses on natural language processing, with a goal of
building an application which can predict the next word based on a sequence of
words input from a user. The project progresses through several key phases such
as data exploration and cleaning, then model building and optimization, before
wrapping up with application development and an associated "sales pitch"
presentation for the app. Three key deliverables throughout the project are:
a milestone report describing the exploratory data analysis, the application
itself, and a presentation.

The app is hosted on shinyapps.io and can be accessed [here][4]. The files in
this repo are described below:

 - app.R: the application file, use instructions are included in the app itself.
 - global.R: all the function definitions for the app or the precursor script.
 - precursor_script.R: file was used throughout development to simulate the
 results before building the actual application.
 - ngram_table.Rdata: this file accompanies the actual app on the shiny servers,
 but it is not included as part of the GitHub repo. The generate the file
 locally the script configuration should be changed to generate the desired data
 file.

The user provides input to a text box. The app reads in the text and applies the
prediction model to offer up predictions for the next word. The model is based
on a slight modification of the stupid backoff method. The model is modified in
that it does not calculate a score for all words in the vocabulary. Instead it
calculates scores for all observed instances of the supplied ngram (starting
from the 4-gram level) if it does not find five unique occurances of the prefix
then it backs off to the next n-gram level. Using this method it is possible but
unlikely that it would miss a high scoring result at a lower n-gram level. This
could lead to reduced accuracy; however, this tradoff was made to improve
performance time.

The model references data from a training corpus to make its predictions. The
data were provided by the course and came from sources across the web such as:
blogs, news articles, and twitter. The training corpus was assembled by reading
in up to 400,000 lines from each of the sources, and this works out to be ~56%
of the data. This tradeoff was again made in the interest of performance time.
The review criteria below show the emphasis placed on performance for this
project.

The accuracy of this prediction model is about average among the course
projects. One major improvement would be incorporating features to help the
model understand parts of speech. One common area the current model stuggles
with is predicting the correct type of word for the context, for example it may
predict a noun when the context clearly calls for a verb.

>#### Data Product Review Criteria:
>
>* Does the link lead to a Shiny app with a text input box that is running on
shinyapps.io?
>* Does the app load to the point where it can accept input?
>* When you type a phrase in the input box do you get a prediction of a single
word after pressing submit and/or a suitable delay for the model to compute the
answer?
>* Put five phrases drawn from Twitter or news articles in English leaving out
the last word. Did it give a prediction for every one?


# Text Prediction Model and App Project README


## Overview

This repo contains my work for the [capstone project][1] of the
[Data Science Specialization][2] from Johns Hopkins University on Coursera. The
repo contains three folders, one for each of the three key deliverables of the
project: the milestone report, the application, and the presentation. Each of
the project sub folders have their own README files for additional detail. All
aspects of the project were created in RStudio.

[1]: https://www.coursera.org/learn/data-science-project  "DS Capstone Site"
[2]: https://www.coursera.org/specializations/jhu-data-science  "DS Spec."
[3]: https://jtzingsheim1.github.io/Text-Prediction-Model-and-App/Milestone%20Report/milestone_report.html   "milestone report"
[4]: https://jtzingsheim.shinyapps.io/TextPredictorApp/  "app on shinyapps.io"
[5]: http://rpubs.com/jtzingsheim/Text-App-Presentation  "presentation on RPubs"


## Detailed Description

The overall project focuses on natural language processing, with a goal of
building an application that can predict the next word based on a sequence of
words input from a user. The project progresses through several key phases such
as data exploration and cleaning, then model building and optimization, before
wrapping up with application development and an associated "sales pitch"
presentation for the app.

The data were provided by the course, and originally came from sources across
the web such as: blogs, news articles, and twitter. The data were loaded and
explored, and additional details on that can be found in the
[milestone report][3].

The algorithm ultimately used by the app is a modified version of a stupid
backoff model. While average for the course, the accuracy of the method is not
great at about 12%, but it does offer predictions in an acceptable amount of
time. One major improvement would be incorporating features to help the
algorithm understand parts of speech, for example not predicting a noun when the
context clearly calls for a verb. The app is hosted [here][4] on shinyapps.io,
and additional information can be found in the sub folder. The final piece of
the Capstone project is the presentation about the app which can be viewed on
RPubs [here][5].


## Additional Information

The raw and processed data are not uploaded to this repo. If cloning the repo
locally the data can be obtained by accessing functions in the global.R file in
the Text_predictor_App folder. The easiest way would be to call the GetDataFrom
function and specify parameters to load the desired configuration from scratch.
Alternatively the steps within the GetDataFrom function could be reproduced
individually to exercise more control over the data "model". Depending on the
settings selected this could consume a substantial amount of computing
resources.


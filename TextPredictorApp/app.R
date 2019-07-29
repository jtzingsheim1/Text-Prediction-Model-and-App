# Coursera Data Science Specialization Capstone Project Application ------------
# Predicting the next word in a sequence


# The purpose of this app is to complete the requirements for part of the
# capstone project of the Data Science Specialization from Johns Hopkins
# University on Coursera.
#
# The overall project objectives are listed below. This app is the predictive
# text product that uses the model.
# - Analyze a large corpus of text documents
# - Discover structure in the data and how words are put together
# - Clean and analyze the text data
# - Build a predictive text model
# - Build a predictive text product that uses the model
#
# The review criteria specific to the app are listed below:
# - Does the link lead to a Shiny app with a text input box that is running on
# shinyapps.io?
# - Does the app load to the point where it can accept input?
# - When you type a phrase in the input box do you get a prediction of a single
# word after pressing submit and/or a suitable delay for the model to compute
# the answer?
# - Put five phrases drawn from Twitter or news articles in English leaving out
# the last word. Did it give a prediction for every one?
#
# The user provides input to a text box. The app reads in the text and applies
# the prediction model to offer up predictions for the next word. The model is
# based on a slight modification of the stupid backoff method. The model
# references data from a training corpus to make its predictions. The data were
# provided by the course and came from sources across the web such as: blogs,
# news articles, and twitter.

# Load packages and scripts
library(shiny)
library(tidyverse)
library(quanteda)
library(magrittr)
source("global.R")

# Load data table from disk, used to make predictions
ngram.table <- GetDataFrom("saved.object")


# UI ---------------------------------------------------------------------------
ui <- fluidPage(

    # Application title
    titlePanel("Next Word Predictor!"),

    # Sidebar with help text, text box input, and an action button
    sidebarLayout(
        
        sidebarPanel(
            
            # Help text to provide instructions for the user
            helpText("This is an app which can predict which word follows a
                     given string of input text. You can enter text in the box
                     below and click the 'Predict Next Word' button to receive
                     predictions. More information about this app and project
                     can be found in its ",
                     a(href = "https://github.com/jtzingsheim1/Text-Prediction-Model-and-App", 
                     "GitHub Repo.")),
            
            # Line break
            hr(),
            
            # Input text box
            textInput("text.entered", "Your Input Text:"),
            
            # Action button, no calculation happens until button is clicked
            actionButton("predict.word", "Predict Next Word")
            
        ),

        # Main panel with output and details
        mainPanel(
            
            # Help text describing the output results
            helpText("The table below shows prediction information from the
                     model. The word column showns the top five word estimates.
                     The score column is related to model's idea of the
                     likelihood, but it is not an actual probability. The
                     feature column shows the sequence that the model had
                     observed previously which it used to calculate the score.
                     The table truncates scores to 4 digits, so words with very
                     low scores may appear as zero even though they have
                     non-zero values"),
            
            # Line break
            hr(),
            
            # Text output of the top prediction
            textOutput("output.text"),
            
            # Line break
            hr(),
            
            # Table output of the prediction results
            tableOutput("prediction.table")
            
        )
        
    )
    
)


# Server -----------------------------------------------------------------------
server <- function(input, output) {
    
    # MakePrediction action to take when button is clicked
    MakePrediction <- eventReactive(input$predict.word, {
        
        # Profiling message
        message(Sys.time(), " button clicked, collect input and predict")
        
        # Collect input text and prepare it for prediction 
        prefix.words <- PrepareInputText(input$text.entered)
        
        # Make predictions from the input text using the model
        prediction.table <- PredictWords(ngram.table = ngram.table,
                                         prefix.words = prefix.words)
        
        # Extract the word element from the top prediction
        predicted.word <- prediction.table %>%
            slice(1) %$%
            word
        
        # Profiling message
        message(Sys.time(), " prediction complete")
        
        # Prepare all output as a list
        list(predicted.word = predicted.word,
             prediction.table = prediction.table)
        
    })
    
    # Prepare output text to display the top prediction
    output$output.text<- renderText({
        
        # Assemble output text
        c("The model predicts that the most likely next word is: ",
          MakePrediction()$predicted.word)
        
    })
    
    # Prepare output table of the predictions
    output$prediction.table <- renderTable({
        
        # Assemble the output table
        MakePrediction()$prediction.table
        
    }, digits = 4L)
}


# App --------------------------------------------------------------------------
shinyApp(ui = ui, server = server)


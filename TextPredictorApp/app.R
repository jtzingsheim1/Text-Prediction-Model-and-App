# Setup ------------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(quanteda)
library(magrittr)
source("global.R")


# UI ---------------------------------------------------------------------------
ui <- fluidPage(

    # Application title
    titlePanel("Next Word Predictor!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("This is an app which can predict which word follows a
                     given string of input text. You can enter text in the box
                     below and click the 'Predict Next Word' button to receive
                     predictions."),
            textInput("text.entered", "Your Input Text:"),
            actionButton("predict.word", "Predict Next Word")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            helpText("The table below shows prediction information from the
                     model. The word column showns the top five word estimates.
                     The score column is related to model's idea of the
                     likelihood, but it is not an actual probability. The
                     feature column shows the sequence that the model had
                     observed previously which it used to calculate the score"),
            br(),
            textOutput("output.text"),
            br(),
            tableOutput("prediction.table")
        )
    )
)


# Server -----------------------------------------------------------------------
server <- function(input, output) {
    
    RollTheDice <- eventReactive(input$predict.word, {
        
        message(Sys.time(), " button clicked, collect input and predict")
        
        # Collect input text to predict from
        prefix.words <- input$text.entered %>%
            gsub(pattern = '[[:punct:]]', replacement = "", .) %>%
            str_split(pattern = " ") %>%
            unlist()
        
        prediction.table <- PredictWords(ngram.table = ngram.table,
                                    prefix.words = prefix.words)
        
        predicted.word <- prediction.table %>%
            slice(1) %$%
            word
        
        message(Sys.time(), " prediction complete")
        
        list(predicted.word = predicted.word,
             prediction.table = prediction.table)
        
    })
    
    output$output.text<- renderText({
        
        message(Sys.time(), " rendering output text")
        
        c("The model predicts that the most likely next word is: ",
          RollTheDice()$predicted.word)
        
    })
    
    output$prediction.table <- renderTable({
        
        message(Sys.time(), " rendering output table")
        
        RollTheDice()$prediction.table
        
    }, digits = 4L)
}


# App --------------------------------------------------------------------------
shinyApp(ui = ui, server = server)


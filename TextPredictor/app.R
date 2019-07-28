# Setup ------------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(quanteda)
library(magrittr)
source("capstone_functions.R")

# Load data table from disk
ngram.table <- GetDataFrom("saved.object", file.name = "ngram_table.Rdata")


# UI ---------------------------------------------------------------------------
ui <- fluidPage(

    # Application title
    titlePanel("Next Word Predictor!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("text.entered", "Your Text"),
            actionButton("submit.text", "Predict Next Word")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("prediction.output")
        )
    )
)


# Server -----------------------------------------------------------------------
server <- function(input, output) {
    
    RollTheDice <- eventReactive(input$submit.text, {
        
        message(Sys.time(), " button clicked, collect input and predict")
        
        # Collect input text to predict from
        prefix.words <- input$text.entered %>%
            gsub(pattern = '[[:punct:]]', replacement = "", .) %>%
            str_split(pattern = " ") %>%
            unlist()
        
        predictions <- PredictWords(ngram.table = ngram.table,
                                    prefix.words = prefix.words)
        
        message(Sys.time(), " prediction complete")
        
        list(predictions = predictions)
        
    })
    
    output$prediction.output <- renderTable({
        
        message(Sys.time(), " rendering output table")
        
        RollTheDice()$predictions
        
    })
}


# App --------------------------------------------------------------------------
shinyApp(ui = ui, server = server)


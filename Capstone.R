# Coursera Data Science Specialization Capstone Project Script------------------
# Predicting the next word in a sequence


# The purpose of this script is to complete the basic requirements behind the
# capstone project from Johns Hopkins University within the Data Science
# Specialization on Coursera.
#
# The instructions outline the objectives of the project as:
# - Analyze a large corpus of text documents
# - Discover structure in the data and how words are put together
# - Clean and analyze the text data
# - Build a predictive text model
# - Build a predictive text product that uses the model
#
# This project will begin to meet the objectives by building a script to work
# with the data and build a predictive model


library(readtext)


# Part 0) Function definitions--------------------------------------------------

DLAndUnzipData <- function() {
  # Downloads and unzips the capstone dataset if needed, returns folder name
  #
  # Args:
  #   None
  #
  # Returns:
  #   The name of the folder containing the data in the current directory
  data.filename <- "Coursera-SwiftKey.zip"
  # Check if the file already exists, download if it does not
  if (!file.exists(data.filename)) {
    print("Downloading Data File")
    url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(url, data.filename)
  }
  
  # Check if the file is already unzipped, unzip if needed
  data.folder <- "final"
  if (!file.exists(data.folder)) {
    print("Unzipping Data File")
    unzip(data.filename)
  }
  data.folder
}


# Part 1) Load and process the data---------------------------------------------

test <- DLAndUnzipData()
















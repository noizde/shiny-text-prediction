library(shiny)
library(tm)

readNgrams <- function(directory) {
  monogramPath <- paste(directory, "/monograms.csv", sep = "")
  bigramPath <- paste(directory, "/bigrams.csv", sep = "")
  trigramPath <- paste(directory, "/trigrams.csv", sep = "")
  
  monograms <- read.csv(monogramPath)
  bigrams <- read.csv(bigramPath)
  trigrams <- read.csv(trigramPath)
  
  ngrams <- list(monograms = monograms, bigrams = bigrams, trigrams = trigrams)
  
  ngrams
}

ngrams <- readNgrams("resources")

processInput <- function(text) {
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- tolower(text)
  text <- stripWhitespace(text)
  text
}

shinyServer(function(input, output) {
  
  predictWord <- eventReactive(input$predict, {
    ctx <- processInput(input$context)
    if (nchar(ctx) == 0) {
      "Invalid input!"
    } else {
      paste(input$context, ": ", "prediction", sep = "")
    }
  })
  
  output$prediction <- renderText({ 
    predictWord()
  })
})
library(shiny)

shinyUI(fillPage(
  div(textInput("context", "Input:", NULL),
      textOutput("prediction")),
  actionButton("predict", "Predict"),
  hr(),
  div("Depending on the observed frequency of the provided text (and your internet connection), the program may take around a minute to generate a prediction."),
  title = "Text Prediction",
  padding = 100
))
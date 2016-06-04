library(shiny)
library(tm)

# CONSTANT

discount <- 0.75

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

count.kn <- function(context) { # Takes a vector of strings
  order <- length(context)
  
  contextString <- paste(context, collapse = " ")
  inHash <- ngrams$c.kns[[contextString]]
  
  if (is.null(inHash)) {
    if (order == 3) {
      c.kn <- max(ngrams$trigrams[grep(paste("^", contextString, "$", sep = ""), ngrams$trigrams$names), "counts"], 0)
    } else {
      if (order == 2) {
        c.kn <- sum(grepl(paste(" ", contextString, "$", sep = ""), ngrams$trigrams$names))
      } else {
        # meaning order is 1
        c.kn <- sum(grepl(paste(" ", contextString, "$", sep = ""), ngrams$bigrams$names))
      }
    }
    
    ngrams$c.kns[[contextString]] <- c.kn
    c.kn
  } else {
    inHash
  }
}

computeLambda <- function(context){
  
  order <- length(context)
  contextString <- paste(context, collapse = " ")
  
  inHash <- ngrams$lambdas[[contextString]]
  
  if (is.null(inHash)) {
    if (order == 2) {
      nContextStarts <- sum(grepl(paste("^", contextString, " ", sep = ""), ngrams$trigrams$names))
      contextCount  <- max(ngrams$bigrams[ngrams$bigrams$names == contextString,]$counts, 1)
    } else {
      nContextStarts <- sum(grepl(paste("^", contextString, " ", sep = ""), ngrams$bigrams$names))
      contextCount  <- max(ngrams$monograms[ngrams$monograms$names == contextString,]$counts, 1)
    }
    
    nContextStarts <- max(nContextStarts, 1)
    
    ngrams$lambdas[[contextString]] <- (discount/contextCount) * nContextStarts
    (discount/contextCount) * nContextStarts
    
  } else {
    inHash
  }
  
}

p.cont <- function(word) {
  inHash <- ngrams$conts[[word]]
  if (is.null(ngrams$conts[[word]])) {
    completions <- sum(grepl(paste(" ", word, "$", sep = ""), ngrams$bigrams$names))
    bigramTypes <- nrow(ngrams$bigrams)
    ngrams$conts[[word]] <- completions/bigramTypes
    ngrams$conts[[word]]
  } else {
    inHash
  }
}

train.kn <- function(ngrams) {
  
  if (is.null(ngrams$c.kns)) {
    ngrams$c.kns <- hash()
  }
  
  if (is.null(ngrams$lambdas)) {
    ngrams$lambdas <- hash()
  }
  
  if (is.null(ngrams$conts)) {
    ngrams$conts <- hash()
  }
  
  monos <- head(ngrams$monograms$names, 8000)
  # monos <- ngrams$monograms$names
  
  for (word in monos) {
    word <- as.String(word)
    c.kn <- ngrams$c.kns[[word]]
    lambda <- ngrams$lambdas[[word]]
    cont <- ngrams$conts[[word]]
    
    if (is.null(c.kn)) {
      ngrams$c.kns[[word]] <- count.kn(word)
    }
    
    if (is.null(lambda)) {
      ngrams$lambdas[[word]] <- computeLambda(word)
    }
    
    if (is.null(cont)) {
      ngrams$conts[[word]] <- p.cont(word)
    }
  }
  
  ngrams
}

ngrams <- train(ngrams)

p.kn <- function(word, context) {
  normalizer <- computeLambda(context)
  
  if (length(context) == 1) {
    cont <- p.cont(word)
  } else {
    cont <- p.kn(word, tail(context, length(context) - 1))
  }
  
  if (count.kn(context) == 0) {
    wholeProb <- 0
  } else {
    wholeProb <- max(count.kn(c(context, word)) - discount, 0)/count.kn(context)  
  }
  
  wholeProb + normalizer*cont
}

processInput <- function(text) {
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- tolower(text)
  text <- stripWhitespace(text)
  text
}

# ACCESS START

predict.simple3 <- function(text) {
  text <- tail(text, 2)
  textString <- paste(text, collapse = " ")
  predictionTrigram <- as.String(head(ngrams$trigrams[grep(paste("^", textString, " ", sep=""), ngrams$trigrams$names),][,"names"], 1))
  splitTrigram <- strsplit(predictionTrigram, " ")[[1]]
  
  if (length(splitTrigram)) {
    splitTrigram[[3]]
  } else {
    NULL
  }
}

predict.kn <- function(text) {
  
  if (is.null(ngrams$c.kns)) {
    ngrams$c.kns <- hash()
  }
  
  if (is.null(ngrams$lambdas)) {
    ngrams$lambdas <- hash()
  }
  
  if (is.null(ngrams$conts)) {
    ngrams$conts <- hash()
  }
  
  text <- tail(text, 2)
  
  monos <- head(ngrams$monograms$names, 2000)
  
  probs <- sapply(monos, function(word) {
    p.kn(as.String(word), text)
  })
  
  prediction <- data.frame(names = monos, probs = probs)[order(-probs), ][1:10,][1, "names"]
  as.String(prediction)
}

processInput <- function(text) {
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- tolower(text)
  text <- stripWhitespace(text)
  text
}

predict <- function(text) {
  text <- strsplit(processInput(text), " ")[[1]]
  prediction <- predict.simple3(text)
  if (is.null(prediction)) {
    prediction <- predict.kn(text)
  }
  prediction
}

shinyServer(function(input, output) {
  
  predictWord <- eventReactive(input$predict, {
    ctx <- processInput(input$context)
    if (nchar(ctx) == 0) {
      "Invalid input!"
    } else {
      paste(input$context, " ", predict(ctx), sep = "")
    }
  })
  
  output$prediction <- renderText({ 
    predictWord()
  })
})
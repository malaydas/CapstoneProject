options(shiny.maxRequestSize=95*1024^2)

suppressPackageStartupMessages(library(shiny))
source("./Capstone_Utilities.R")
load(file="./MarkovObjects.RData")
load(file="./Quadrigram.RData")
load(file="./Trigram.RData")
load(file="./Bigram.RData")
rm(TransitionMatrix)

shinyServer(function(input, output,session) {
    
    OutputExactWord = eventReactive(input$Submit,NextWord(input$InPhrase,MarkovMatrix,Quadrigram,Trigram,Bigram)$ExactWordPredicted)
    OutputPossibleWord = eventReactive(input$Submit,paste0(as.vector(NextWord(input$InPhrase,MarkovMatrix,Quadrigram,Trigram,Bigram)$PossibleWordPredicted),collapse=","))
            
    output$ExactDisplay <- renderText(paste0("Based on the input provided, the next highly probable word is :   ",OutputExactWord()))
    output$PossibleDisplay <- renderText( if (is.null(OutputPossibleWord())) { " Sorry. couldn't predict "}
                                          else{paste0("Also these are other possible probable words, in order of their probability (Take your pick ) :  " ,OutputPossibleWord())})
    
      
                                      
})
 
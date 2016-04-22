library(shiny)

# Creating UI for App
shinyUI(fluidPage(

  # Application title
  titlePanel("Capstone : Predicting Word"),
  helpText("This application created as part of Capstone Project. This will predict next word, based on the input line/phrase/word etc"),

  # Sidebar (capturing inputs)
  sidebarLayout(
    sidebarPanel(      
      helpText("Please enter the phrase/sentence/word for which you want to predict the next word. Once done, Pls click the button below."),
      
      textInput("InPhrase", "Phrase/Sentence/Word", "Type here"),           
      br(),   
      actionButton("Submit", "Predict Next Word"),      
      br(),   
      br()
    )
    ,

    # Main panel (showing results)
    mainPanel(
      
      h3("Model Output"),
      br(),
      br(),
#      h5("Exact Output"),
      verbatimTextOutput("ExactDisplay")  
#       br(),
#       br(),
#       h5("Possible Output"),
#       verbatimTextOutput("PossibleDisplay")    
    )
  )
))
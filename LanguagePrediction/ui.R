require(shiny)
require(shinythemes)
require(data.table)
options(show.error.messages = FALSE)


shinyUI(navbarPage(strong("Next Word Prediction APP"),
                   tabPanel(strong(""),
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"),
                            fluidPage(
                                fluidRow(
                                    column(5, offset = 1,
                                           h2("Input the first part of phrase/sentence"),
                                           tags$textarea(id = "Phrase", rows=5, cols = 30),
                                           submitButton("Predict next word")
                                    ),
                                    column(5,offset = 1,
                                           h2("Next words"),
                                           h2(strong(textOutput("word_predicted_1")),style="color:green"),
                                           h2(strong(textOutput("word_predicted_2")),style="color:green"),
                                           h2(strong(textOutput("word_predicted_3")),style="color:green"),
                                           h2(strong(textOutput("word_predicted_4")),style="color:green"),
                                           h2(strong(textOutput("word_predicted_5")),style="color:green")
                                    )
                                )
                            )
      
                   
                                
                   )
))
library(shiny)
shinyUI(navbarPage("JHU Capstone project shiny app-NLP in R",
                   tabPanel("page1-ngram prediction",
                            sidebarLayout(
                                    sidebarPanel(
                                            
                                            textInput("text", label=h3("Text input"),value = ""),
                                            radioButtons("radiobutton", label=h3("ngram-prediction:"),
                                                         choice=list("Bigram prediction"="bi",
                                                           "Trigram prediction"="tri",
                                                           "Quadgram prediction"="qua")),
                                            br(),
                                            submitButton("Update"),
                                            helpText("note: type text into text input and select which kind of ngram
                               you want to do prediction with, and then click update button to see.
                               For example: type :
                                                     we went to new york, 
                                                     happi mother, 
                                                     we go to see south carolina gamecock")),
                   
                                    mainPanel(
                                            
                                            h4("N-grams Prediction Output"),
                                            textOutput("textoutput")
                                            
                                    )
                            )
                   ),
                   
                   tabPanel("page2-background of NLP and references", HTML('
                   <ol>
                    <li><a href="http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf">handling string in R</a></li>
                    <li><a href="http://cran.r-project.org/web/views/NaturalLanguageProcessing.html">http://CRAN NLP</a></li>                             
                    </ol>
                    ')
                            )
                   ))




























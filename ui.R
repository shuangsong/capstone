library(shiny)
library(ggplot2)
library(markdown)
shinyUI(navbarPage("JHU Capstone project shiny app-NLP in R",
                   tabPanel("page1-ngram prediction",
                            sidebarLayout(
                                    sidebarPanel(
                                            
                                            textInput("test", label=h3("Text input")),
                                            radioButtons("ngram", "ngram-prediction:",
                                                         c("Bigram prediction"="bi",
                                                           "Trigram prediction"="tri")),
                                            br(),
                                            submitButton("Update"),
                                             helpText("note: type text into text input and select which kind of ngram
                               you want to do prediction with, and then click update button to see.
                               For example: type :we went to new york, happi mother, etc.")),
                   
                                    mainPanel(
                                            
                                            h4("Output"),
                                            verbatimTextOutput("output")
                                            
                                    )
                            )
                   ),
                   tabPanel("page2-Wordcloud visualization",
                            titlePanel("Wordcloud"),
                            sidebarLayout(
                                    sidebarPanel(
                                            selectInput("data", "Choose a data set", c("twitter","blog","news")),
                                            sliderInput("slider",label=h3("Word size"), min=1, max=100, value=40),
                                            br(),
                                            radioButtons("visual", "Visualization options:",
                                                         c("histogram"="hist", "wordcloud"="wc")),
                                            submitButton("Update"),
                                            helpText("By choosing which type of visualization you 
                                                     want, and word size, data set, you can see on the right
                                                     the plot.")
                                    ),
                                    mainPanel(
                                            h4("Plot"),
                                            plotOutput("plot"))
                           )),
                   tabPanel("page3-background of NLP and references", HTML('
                   <ol>
                    <li><a href="http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf">handling string in R</a></li>
                    <li><a href="http://cran.r-project.org/web/views/NaturalLanguageProcessing.html">http://CRAN NLP</a></li>                             
                    </ol>
                    ')
                            )
                   ))




























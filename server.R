
library(tm) 
library(NLP) # for natural language processing
library(stringr) # package for handling string in R
library(RWeka) #for n-gram model
library(ngram) # for n-grams model
library(shiny)
setwd("C:/Users/stephanie song/Desktop/final/en_US")
source(paste(getwd(), "prediction_lib(edited).R", sep="/")) # win8pro 
source(paste(getwd(), "clean_df.R", sep="/"))
load(paste(getwd(), "df_unigram.RData", sep="/"))
load(paste(getwd(), "df_bigram.RData", sep="/"))
load(paste(getwd(), "df_trigram.RData", sep="/"))
load(paste(getwd(), "df_quagram.RData", sep="/"))

shinyServer(function(input, output) {
        output$value <- renderPrint({ 
                
                if (input$radiobutton ==  "bi"){
                        predict_bigram(input$value)
                }
                else if (input$radiobutton = "Trigram prediction"){
                        predict_trigram(input$value)
                }
                else (input$radiobutton = "Quadgram prediction"){
                        predict_quagram(input$value)
                }
                
                
        })
        
     
        
})








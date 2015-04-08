library(shiny)
library(tm)
library(stringr)
library(RWeka)
df_unigram<-read.csv(paste(getwd(), "/data/df_unigram.csv", sep=""))
df_bigram<-read.csv(paste(getwd(), "/data/df_bigram.csv", sep=""))
df_trigram<-read.csv(paste(getwd(), "/data/df_trigram.csv", sep=""))
df_quagram<-read.csv(paste(getwd(), "/data/df_quagram.csv", sep=""))
# add prediction function : (bigram, trigra, and quagram)
source("prediction_lib(edited).R")

#create a funtion that can use to identify radiobutton while do the prediction: 
nextword<-function(radiobutton, inputText,input){
        if (input$radiobutton=="bi"){
                predict_bigram(input$text)
        }else if(input$radiobutton=="tri"){
                predict_trigram(input$text)
        }else if(input$radiobutton=="qua"){
                predict_quagram(input$text)
        }}

shinyServer(function(input, output,session) {
        radiobutton<-reactive({input$radiobutton})
        inputText<-reactive({input$text})
        output$textoutput<-renderText({
                word<-nextword(radiobutton(),inputText,input)
                word
        })
})








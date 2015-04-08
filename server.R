library(shiny)
df_unigram<-read.csv(paste(getwd(), "/data/df_unigram.csv", sep=""))
df_bigram<-read.csv(paste(getwd(), "/data/df_bigram.csv", sep=""))
df_trigram<-read.csv(paste(getwd(), "/data/df_trigram.csv", sep=""))
df_quagram<-read.csv(paste(getwd(), "/data/df_quagram.csv", sep=""))
# add prediction function : (bigram, trigra, and quagram)
predict_bigram<- function(input) {
        if (sapply(gregexpr("\\S+", input), length)<=1){
                return("please enter some words")
        }else {
        input1<-tolower(input)
        input2<-str_replace_all(input1, pattern="[[:punct:]]","") #remove punctuations
        input3<-str_replace_all(input2, pattern="\\s+", " ") # replace whitespace with space
        input_clean<-removeNumbers(input3) 
        clean_tail<-tail(unlist(strsplit(input_clean, " ")), 1)
        start_with_term<-paste("^","\\b", clean_tail,"\\b", sep="")
        find<-df_bigram[grep(start_with_term, df_bigram$terms),]
        if (nrow(find)==0) {
                #warning("There is no prediction in bigram data frame")
                return("no prediction")
        }
        find<-df_bigram[grep(start_with_term, df_bigram$terms),]
        merge<-NULL
        find$terms<-as.character(find$terms)
        find_mat<-matrix(unlist(strsplit(find$terms, " ")), ncol=2, byrow=TRUE)
        find$pred<-find_mat[,2]
        find2<-df_unigram[grep(paste("^","\\b", clean_tail,"\\b", sep=""), df_unigram$terms),]
        if (nrow(find2)==0){
                #warning("There is no prediction in unigram data frame")
                return("no prediction")
        }
        find2<-data.frame(find2)
        for (i in 1: nrow(find)) {
                new_find<-data.frame("pred"=find[i,4], "pred_prob"=find[i,2]/find2[1,2])
                merge<-rbind(merge, data.frame(new_find))
        }
        high_prob<-merge[order(-merge$pred_prob),]
        possible_term<-as.character(high_prob$pred)
        #possible_term<-data.frame(possible_term)
        return(possible_term)
}}

predict_trigram<-function(input){
        if (sapply(gregexpr("\\S+", input), length)<=1){
                return("please enter some words")
        }else {
        input1<-tolower(input)
        input2<-str_replace_all(input1, pattern="[[:punct:]]","") #remove punctuations
        input3<-str_replace_all(input2, pattern="\\s+", " ") # replace whitespace with space
        input_clean<-removeNumbers(input3) 
        clean_tail<-tail(unlist(strsplit(input_clean, " ")), 2)
        words<-paste(clean_tail[1], clean_tail[2], sep=" ")
        start_with_term<-paste("^","\\b", words,"\\b", sep="")
        find<-df_trigram[grep(start_with_term, df_trigram$terms),]
        if (nrow(find)==0) {
                #warning("There is no prediction in trigram data frame")
                return("no prediction")
        }
        merge<-NULL
        find$terms<-as.character(find$terms)
        find_mat<-matrix(unlist(strsplit(find$terms, " ")), ncol=3, byrow=TRUE)
        find$pred<-find_mat[,3]
        find2<-df_bigram[grep(paste("^","\\b",words,"\\b", sep=""), df_bigram$terms),]
        find2<-data.frame(find2)
        if (nrow(find2)==0) {
                #warning("There is no prediction in bigram data frame")
                return("no prediction")
        }
        for (i in 1: nrow(find)) {
                new_find<-data.frame("pred"=find[i,4], "pred_prob"=find[i,2]/find2[1,2])
                merge<-rbind(merge, data.frame(new_find))
        }
        high_prob<-merge[order(-merge$pred_prob),]
        possible_term<-as.character(high_prob$pred)
        return(possible_term)
}}


predict_quagram<-function(input){
        if (sapply(gregexpr("\\S+", input), length)<=1){
                return("please enter some words")
        }else {
        input1<-tolower(input)
        input2<-str_replace_all(input1, pattern="[[:punct:]]","") #remove punctuations
        input3<-str_replace_all(input2, pattern="\\s+", " ") # replace whitespace with space
        input_clean<-removeNumbers(input3) 
        clean_tail<-tail(unlist(strsplit(input_clean, " ")), 3)
        words<-paste(clean_tail[1], clean_tail[2],clean_tail[3], sep=" ")
        start_with_term<-paste("^","\\b",words,"\\b", sep="")
        find<-df_quagram[grep(start_with_term, df_quagram$terms),]
        if (nrow(find)==0) {
                #warning("There is no prediction in quagram data frame")
                return("no prediction")
        }
        merge<-NULL
        find$terms<-as.character(find$terms)
        find_mat<-matrix(unlist(strsplit(find$terms, " ")), ncol=4, byrow=TRUE)
        find$pred<-find_mat[,4]
        find2<-df_trigram[grep(paste("^","\\b",words,"\\b", sep=""), df_trigram$terms),]
        find2<-data.frame(find2)
        if (nrow(find2)==0) {
                #warning("There is no prediction in trigram data frame")
                return("no prediction")
        }
        for (i in 1: nrow(find)) {
                new_find<-data.frame("pred"=find[i,4], "pred_prob"=find[i,2]/find2[1,2])
                merge<-rbind(merge, data.frame(new_find))
        }
        high_prob<-merge[order(-merge$pred_prob),]
        possible_term<-as.character(high_prob$pred)
        return(possible_term)
}}

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
        #output$radiobutton<-render({radiobutton()})
        output$textoutput<-renderText({
                word<-nextword(radiobutton(),inputText,input)
                word
        })
})








#based on ngram markov model: 
#unigram: predict next word with previous one word, random predict.
#bigram: predict next word based on previous one word
#trigram: predict next word based on previous two words
#quadgram: predict next word based on previous three words

library(ggplot2)
library(NLP) # for natural language processing
library(stringr) # package for handling string in R
library(R.utils) # ultils to count lines
library(SnowballC) # for steming words.
library(RWeka) #for n-gram model
library(ngram) # for n-grams model
library(qdap) # count word
library(stringi) # use to count lines fast 
library(pryr) # to see file size with command object_size
library(wordcloud) # for

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
                        return("no prediction")
                }
                find<-df_bigram[grep(start_with_term, df_bigram$terms),]
                merge<-NULL
                find$terms<-as.character(find$terms)
                find_mat<-matrix(unlist(strsplit(find$terms, " ")), ncol=2, byrow=TRUE)
                find$pred<-find_mat[,2]
                find2<-df_unigram[grep(paste("^","\\b", clean_tail,"\\b", sep=""), df_unigram$terms),]
                if (nrow(find2)==0){
                        return("no prediction")
                }
                find2<-data.frame(find2)
                for (i in 1: nrow(find)) {
                        new_find<-data.frame("pred"=find[i,4], "pred_prob"=find[i,2]/find2[1,2])
                        merge<-rbind(merge, data.frame(new_find))
                }
                high_prob<-merge[order(-merge$pred_prob),]
                possible_term<-as.character(high_prob$pred)
                return(possible_term)
        }}
#predict_bigram("last")
#first

predict_trigram<-function(input){
        if (sapply(gregexpr("\\S+", input), length)<=1){
                return("please enter some words")
        }else {
                input1<-tolower(input)
                input2<-str_replace_all(input1, pattern="[[:punct:]]","") 
                input3<-str_replace_all(input2, pattern="\\s+", " ") 
                input_clean<-removeNumbers(input3) 
                clean_tail<-tail(unlist(strsplit(input_clean, " ")), 2)
                words<-paste(clean_tail[1], clean_tail[2], sep=" ")
                start_with_term<-paste("^","\\b", words,"\\b", sep="")
                find<-df_trigram[grep(start_with_term, df_trigram$terms),]
                if (nrow(find)==0) {
                        
                        return("no prediction")
                }
                merge<-NULL
                find$terms<-as.character(find$terms)
                find_mat<-matrix(unlist(strsplit(find$terms, " ")), ncol=3, byrow=TRUE)
                find$pred<-find_mat[,3]
                find2<-df_bigram[grep(paste("^","\\b",words,"\\b", sep=""), df_bigram$terms),]
                find2<-data.frame(find2)
                if (nrow(find2)==0) {
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

#predict_trigram("we went to new york")
#happi mother
predict_quagram<-function(input){
        if (sapply(gregexpr("\\S+", input), length)<=1){
                return("please enter some words")
        }else {
                input1<-tolower(input)
                input2<-str_replace_all(input1, pattern="[[:punct:]]","") 
                input3<-str_replace_all(input2, pattern="\\s+", " ") 
                input_clean<-removeNumbers(input3) 
                clean_tail<-tail(unlist(strsplit(input_clean, " ")), 3)
                words<-paste(clean_tail[1], clean_tail[2],clean_tail[3], sep=" ")
                start_with_term<-paste("^","\\b",words,"\\b", sep="")
                find<-df_quagram[grep(start_with_term, df_quagram$terms),]
                if (nrow(find)==0) {
                        return("no prediction")
                }
                merge<-NULL
                find$terms<-as.character(find$terms)
                find_mat<-matrix(unlist(strsplit(find$terms, " ")), ncol=4, byrow=TRUE)
                find$pred<-find_mat[,4]
                find2<-df_trigram[grep(paste("^","\\b",words,"\\b", sep=""), df_trigram$terms),]
                find2<-data.frame(find2)
                if (nrow(find2)==0) {
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

#predict_quagram("we go to see south carolina gamecock")



#based on ngram markov model: 
#unigram: predict next word with previous one word, random predict.
#bigram: predict next word based on previous one word
#trigram: predict next word based on previous two words
#quadgram: predict next word based on previous three words


#function for bigram prediction:

predict_bigram<- function(input) {
        input1<-tolower(input)
        input2<-str_replace_all(input1, pattern="[[:punct:]]","") #remove punctuations
        input3<-str_replace_all(input2, pattern="\\s+", " ") # replace whitespace with space
        input_clean<-removeNumbers(input3) 
        clean_tail<-tail(unlist(strsplit(input_clean, " ")), 1)
        start_with_term<-paste("^",clean_tail,sep="")
        find<-df_bigram[grep(start_with_term, df_bigram$terms),]
        merge<-NULL
        find$terms<-as.character(find$terms)
        find_mat<-matrix(unlist(strsplit(find$terms, " ")), ncol=2, byrow=TRUE)
        find$pred<-find_mat[,2]
        find2<-df_unigram[grep(paste("^", clean_tail, sep=""), df_unigram$terms),]
        find2<-data.frame(find2)
        for (i in 1: nrow(find)) {
                new_find<-data.frame("pred"=find[i,4], "pred_prob"=find[i,2]/find2[1,2])
                merge<-rbind(merge, data.frame(new_find))
        }
        high_prob<-merge[order(-merge$pred_prob),]
        possible_term<-as.character(high_prob$pred)
        #possible_term<-data.frame(high_prob)
        return(possible_term)
}
#try
predict_bigram("how do you know last")
predict_bigram("first")
predict_bigram("last")

#function for trigram prediction : 

#loop :

predict_trigram<-function(input){
        input1<-tolower(input)
        input2<-str_replace_all(input1, pattern="[[:punct:]]","") #remove punctuations
        input3<-str_replace_all(input2, pattern="\\s+", " ") # replace whitespace with space
        input_clean<-removeNumbers(input3) 
        clean_tail<-tail(unlist(strsplit(input_clean, " ")), 2)
        words<-paste(clean_tail[1], clean_tail[2], sep=" ")
        start_with_term<-paste("^",words,sep="")
        find<-df_trigram[grep(start_with_term, df_trigram$terms),]
        merge<-NULL
        find$terms<-as.character(find$terms)
        find_mat<-matrix(unlist(strsplit(find$terms, " ")), ncol=3, byrow=TRUE)
        find$pred<-find_mat[,3]
        find2<-df_bigram[grep(paste("^", words, sep=""), df_bigram$terms),]
        find2<-data.frame(find2)
        for (i in 1: nrow(find)) {
                new_find<-data.frame("pred"=find[i,4], "pred_prob"=find[i,2]/find2[1,2])
                merge<-rbind(merge, data.frame(new_find))
        }
        high_prob<-merge[order(-merge$pred_prob),]
        possible_term<-as.character(high_prob$pred)
        return(possible_term)
}

predict_trigram("happi new")
predict_trigram("new york")
predict_trigram("we went to the new york")

#function for quagram prediction
#merely use quagram: I will not use it on my shiny app. 
predict_quagram<-function(input){
        input1<-tolower(input)
        input2<-str_replace_all(input1, pattern="[[:punct:]]","") #remove punctuations
        input3<-str_replace_all(input2, pattern="\\s+", " ") # replace whitespace with space
        input_clean<-removeNumbers(input3) 
        clean_tail<-tail(unlist(strsplit(input_clean, " ")), 3)
        words<-paste(clean_tail[1], clean_tail[2],clean_tail[3], sep=" ")
        start_with_term<-paste("^",words,sep="")
        find<-df_quagram[grep(start_with_term, df_quagram$terms),]
        merge<-NULL
        find$terms<-as.character(find$terms)
        find_mat<-matrix(unlist(strsplit(find$terms, " ")), ncol=4, byrow=TRUE)
        find$pred<-find_mat[,4]
        find2<-df_trigram[grep(paste("^", words, sep=""), df_trigram$terms),]
        find2<-data.frame(find2)
        for (i in 1: nrow(find)) {
                new_find<-data.frame("pred"=find[i,4], "pred_prob"=find[i,2]/find2[1,2])
                merge<-rbind(merge, data.frame(new_find))
        }
        high_prob<-merge[order(-merge$pred_prob),]
        possible_term<-as.character(high_prob$pred)
        return(possible_term)
}

predict_quagram("we go to see south carolina gamecock")





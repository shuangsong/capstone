#function for trigram prediction : 

#loop :


predict_trigram<-function(input){
        input1<-tolower(input)
        input2<-str_replace_all(input1, pattern="[[:punct:]]","") #remove punctuations
        input3<-str_replace_all(input2, pattern="\\s+", " ") # replace whitespace with space
        input_clean<-removeNumbers(input3) 
        clean_tail<-tail(unlist(strsplit(input_clean, " ")), 2)
        last_two_term<-tail(clean_tail,2)
        clean_term<-paste(last_two_term[1],last_two_term[2],sep=" ")
        start_with_term<-paste("^",clean_term,sep="")
        find<-df_trigram[grep(start_with_term, df_trigram$terms),]
        merge<-NULL
        for (i in 1: nrow(find)) {
                find$terms<-as.character(find$terms)
                find_mat<-matrix(unlist(strsplit(find$terms, " ")), ncol=3, byrow=TRUE)
                #find_mat<-matrix(tail(unlist(strsplit(find$terms, " ")),2), ncol=3, byrow=TRUE)
                find_vector<-find_mat[,3]
                find2<-df_unigram[grep(find_vector[i], df_unigram$terms),]
                find2<-data.frame(find2)
                ran_prob<-data.frame("terms"=find2[1], "probability"=find[i,3]/find2[3])
                merge<-rbind(merge, data.frame(ran_prob))
        }
        high_prob<-merge[order(-merge$probability),]
        possible_term<-as.character(high_prob$terms)
        #possible_term<-data.frame(high_prob)
        return(possible_term)
}

predict_trigram("happi new")
predict_trigram("new york")
predict_trigram("we went to the new york")






















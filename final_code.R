rm(list=ls())
data_path<-paste(getwd(),"/final/en_US", sep="")
setwd(data_path)
#package I use to do this project：
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
library(wordcloud) # for visualization 
#read in the data and read several lines of data:
con_twitts<- file("en_US.twitter.txt",open="rb") 
con_news<- file("en_US.news.txt", open="rb") 
con_blogs<- file("en_US.blogs.txt", open="rb")

twitts<-readLines(con_twitts,encoding="UTF-8",warn=FALSE)
news<-readLines(con_news,encoding="UTF-8")
blogs<-readLines(con_blogs,encoding="UTF-8")
#close connection: 
close(con_twitts)
close(con_news)
close(con_blogs)
set.seed(1233)
ran_twitts<-sample(twitts, 2000, replace=FALSE)
ran_news<-sample(news, 2000, replace=FALSE)
ran_blogs<-sample(blogs, 2000, replace=FALSE)
#all is the combined data by twitts_part news_part and blogs_part.
all<-paste(ran_twitts, ran_news, ran_blogs, sep=" ")
#all <- sent_detect(all, language = "en", model = NULL) #splitting of text paragraphs into sentences
#count how many words in data: 
#stri_stats_general(all) #it should be 2000.

#sum(sapply(gregexpr("\\W+", all), length))

library(tm)
#tokenization function: 
#create corpus 
my_corp<-VCorpus(VectorSource(all),readerControl=list(language="lat"))
#write corpus on hard drive: 
#writeCorpus(my_corp)
#find more about meta data: 
#inspect(my_corp[1:2])
#do transformation on corpus I made:
my_corp<-tm_map(my_corp, PlainTextDocument)
my_corp<-tm_map(my_corp, content_transformer(removePunctuation))
my_corp<-tm_map(my_corp, stripWhitespace)
my_corp<-tm_map(my_corp, content_transformer(removeNumbers))
my_corp<-tm_map(my_corp, stemDocument)
my_corp<-tm_map(my_corp, content_transformer(tolower)) #convert to lower case
my_corp<-tm_map(my_corp, removeWords, stopwords("english"))
#remove profanity words: (I will upload profanity words in my github)
profane_path<-paste(getwd(), "/profane.txt",sep="")
my_corp<-tm_map(my_corp, removeWords, profane_path)
# corp_dtm<- DocumentTermMatrix(my_corp, control=list(wordLengths=c(1,Inf)))
#n gram : 

gc()
options(mc.cores=1)

UnigramTokenizer<-function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer<-function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer<-function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadgramTokenizer<-function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
tdm_unigram<-TermDocumentMatrix(my_corp, control = list(tokenize = UnigramTokenizer))
tdm_bigram<-TermDocumentMatrix(my_corp, control = list(tokenize = BigramTokenizer))
tdm_trigram<-TermDocumentMatrix(my_corp, control = list(tokenize = TrigramTokenizer))
tdm_quagram<-TermDocumentMatrix(my_corp, control = list(tokenize = QuadgramTokenizer))
#convert to matrix 
#mat_2gram<-as.matrix(tdm_bigram)
#head(sort(rowSums(mat_2gram),decreasing=TRUE),20)
#mat_3gram<-as.matrix(tdm_trigram)
#head(sort(rowSums(mat_3gram),decreasing=TRUE),20)

#function to transform data frame to final data frame for further prediction: 
df_ngram<-function (tdm) {
        df_ngram<-as.data.frame(inspect(tdm))
        df_ngram$count<-rowSums(df_ngram)
        df_ngram<-subset(df_ngram, count> 1)
        df_ngram$terms<-row.names(df_ngram)
        df_ngram<-df_ngram[order(-df_ngram$count),]
        row.names(df_ngram)<-NULL
        df_ngram$probability<-df_ngram$count/sum(df_ngram$count)
        df_ngram_final<-subset(df_ngram, select=c("terms","count","probability"))
        df_ngram_final

}

#return data frame of each tdm gram.


df_unigram<-df_ngram(tdm_unigram)
df_bigram<-df_ngram(tdm_bigram)
df_trigram<-df_ngram(tdm_trigram)
df_quagram<-df_ngram(tdm_quagram)
#function for trigram prediction:

predict_bigram<- function(input) {
        input1<-tolower(input)
        input2<-str_replace_all(input1, pattern="[[:punct:]]","") #remove punctuations
        input3<-str_replace_all(input2, pattern="\\s+", " ") # replace whitespace with space
        input_clean<-removeNumbers(input3) 
        clean_tail<-tail(unlist(strsplit(input_clean, " ")), 1)
        start_with_term<-paste("^",clean_tail,sep="")
        find<-df_bigram[grep(start_with_term, df_bigram$terms),]
        merge<-NULL
        for (i in 1: nrow(find)) {
                find$terms<-as.character(find$terms)
                find_mat<-matrix(unlist(strsplit(find$terms, " ")), ncol=2, byrow=TRUE)
                #find_mat<-matrix(tail(unlist(strsplit(find$terms, " ")),2), ncol=3, byrow=TRUE)
                find_vector<-find_mat[,2]
                find2<-df_unigram[grep(paste("^", find_vector[i], sep=""), df_unigram$terms),]
                find2<-data.frame(find2)
                ran_prob<-data.frame("terms"=find2[1], "probability"=find[i,3]/find2[3])
                merge<-rbind(merge, data.frame(ran_prob))
        }
        high_prob<-merge[order(-merge$probability),]
        possible_term<-as.character(high_prob$terms)
        #possible_term<-data.frame(high_prob)
        return(possible_term)
}
#try
predict_bigram("how do you knwo last")
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
                find2<-df_unigram[grep(paste("^", find_vector[i], sep=""), df_unigram$terms),]
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

#function for quagram prediction
#merely use quagram: I will not use it on my shiny app. 
predict_quagram<-function(input){
        input1<-tolower(input)
        input2<-str_replace_all(input1, pattern="[[:punct:]]","") #remove punctuations
        input3<-str_replace_all(input2, pattern="\\s+", " ") # replace whitespace with space
        input_clean<-removeNumbers(input3) 
        clean_tail<-tail(unlist(strsplit(input_clean, " ")), 3)
        last_three_term<-tail(clean_tail,3)
        clean_term<-paste(last_three_term[1],last_three_term[2],last_three_term[3],sep=" ")
        start_with_term<-paste("^",clean_term,sep="")
        find<-df_quagram[grep(start_with_term, df_quagram$terms),]
        merge<-NULL
        for (i in 1: nrow(find)) {
                find$terms<-as.character(find$terms)
                find_mat<-matrix(unlist(strsplit(find$terms, " ")), ncol=4, byrow=TRUE)
                #find_mat<-matrix(tail(unlist(strsplit(find$terms, " ")),2), ncol=3, byrow=TRUE)
                find_vector<-find_mat[,3]
                find2<-df_unigram[grep(paste("^", find_vector[i], sep=""), df_unigram$terms),]
                find2<-data.frame(find2)
                ran_prob<-data.frame("terms"=find2[1], "probability"=find[i,3]/find2[3])
                merge<-rbind(merge, data.frame(ran_prob))
        }
        high_prob<-merge[order(-merge$probability),]
        possible_term<-as.character(high_prob$terms)
        #possible_term<-data.frame(high_prob)
        return(possible_term)
}

predict_quagram("we went to new york")
















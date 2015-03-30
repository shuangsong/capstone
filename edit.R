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
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

options(mc.cores=1)
tdm1gram <- TermDocumentMatrix(my_corp, control = list(tokenize = UnigramTokenizer))
tdm2gram <- TermDocumentMatrix(my_corp, control = list(tokenize = BigramTokenizer))
tdm3gram <- TermDocumentMatrix(my_corp, control = list(tokenize = TrigramTokenizer))
tdm4gram <- TermDocumentMatrix(my_corp, control = list(tokenize = QuadgramTokenizer))

mat_2gram<-as.matrix(tdm2gram)
head(sort(rowSums(mat_2gram),decreasing=TRUE),20)

mat_3gram<-as.matrix(tdm3gram)
head(sort(rowSums(mat_3gram),decreasing=TRUE),20)


#prepare unigram terms and counts data frame and bigram, trigram df for further lookup:
df_unigram<-as.data.frame(inspect(tdm1gram))
df_unigram$num <- rowSums(df_unigram)
df_unigram <- subset(df_unigram, num > 1)
df_unigram$prediction <- row.names(df_unigram)
df_unigram <- subset(df_unigram, select=c('prediction', 'num'))

df_unigram_final<-df_unigram[order(-df_unigram$num),]
row.names(df_unigram_final) <- NULL
head(df_unigram_final,10)

# df of bigram: (terms and their counts)
df_bigram<-as.data.frame(inspect(tdm2gram))
df_bigram$num <- rowSums(df_bigram)
df_bigram <- subset(df_bigram, num > 1)
df_bigram$prediction <- row.names(df_bigram)
df_bigram <- subset(df_bigram, select=c('prediction', 'num'))

df_bigram_final<-df_bigram[order(-df_bigram$num),]
row.names(df_bigram_final) <- NULL
head(df_bigram_final,10)
# df of trigram: 
df_trigram<-as.data.frame(inspect(tdm3gram))
df_trigram$num <- rowSums(df_trigram)
df_trigram <- subset(df_trigram, num > 1)
df_trigram$prediction <- row.names(df_trigram)
df_trigram <- subset(df_trigram, select=c('prediction', 'num'))

df_trigram_final<-df_trigram[order(-df_trigram$num),]
row.names(df_trigram_final) <- NULL
head(df_trigram_final,10)
#df of quadgram: 
df_quagram<-as.data.frame(inspect(tdm4gram))
df_quagram$num <- rowSums(df_quagram)
df_quagram <- subset(df_quagram, num > 1)
df_quagram$prediction <- row.names(df_quagram)
df_quagram <- subset(df_quagram, select=c('prediction', 'num'))

df_quagram_final<-df_quagram[order(-df_quagram$num),]
row.names(df_quagram_final) <- NULL
head(df_quagram_final,10)


#write prediction function : 
#my try
predict_ngram<- function(input, ngrams) {
        input1<-tolower(input)
        input2<-str_replace_all(input1, pattern="[[:punct:]]","") #remove punctuations
        input3<-str_replace_all(input2, pattern="\\s+", " ") # replace whitespace with space
        input_clean<-removeNumbers(input3) 
        
        
        
        
        
}










































#3  Preprocessing data: 


#random sampling: 
#Since we don't need to load in and use all of the data, I would like to just read 
#several lines of each data and conbine them into one data.
#ramdom select several lines in each data and combine them into one data -all.
set.seed(1233)
ran_twitts<-sample(twitts, 2000, replace=FALSE)
ran_news<-sample(news, 2000, replace=FALSE)
ran_blogs<-sample(blogs, 2000, replace=FALSE)
#all is the combined data by twitts_part news_part and blogs_part.
all<-paste(ran_twitts, ran_news, ran_blogs)
#count how many words in data: 
stri_stats_general(all) #it should be 2000.

sum(sapply(gregexpr("\\W+", all), length))

library(tm)
#tokenization function: 
#create corpus 
my_corp<-Corpus(VectorSource(all), readerControl=list(language="lat"))
#write corpus on hard drive: 
writeCorpus(my_corp)
#find more about meta data: 
inspect(my_corp[1:2])
#do transformation on corpus I made:
my_corp<-tm_map(my_corp, PlainTextDocument)
my_corp<-tm_map(my_corp, removePunctuation)
my_corp<-tm_map(my_corp, stripWhitespace)
my_corp<-tm_map(my_corp, tolower)
my_corp<-tm_map(my_corp, removeNumbers)
my_corp<-tm_map(my_corp, stemDocument)
my_corp<-tm_map(my_corp, tolower) #convert to lower case
my_corp<-tm_map(my_corp, removeWords, stopwords("english"))
#remove profanity words: (I will upload profanity words in my github)
profane_path<-paste(getwd(), "/profane.txt",sep="")
my_corp<-tm_map(my_corp, removeWords, profane_path)
corp_clean<-my_corp
corp_clean<-Corpus(VectorSource(corp_clean))

#generate a document term matrix: 
dtm<-DocumentTermMatrix(corp_clean)
inspect(dtm[1:20, 100:110])
#Frequent terms and associations: 
findFreqTerms(x=dtm, lowfreq=5)# find terms that occurs at least 5 times; 
findAssocs(x=dtm, term="god", corlimit=0.4)
inspect(removeSparseTerms(dtm, 0.4))

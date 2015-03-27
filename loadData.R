#before running, select FILE, save with encoding , UTP-8 please.
#1 load in data set
#create a new working directory that is directly pointing to the folder contains 
#blogs, news and tweets( english data)

rm(list=ls())
getwd()
data_path<-paste(getwd(),"/final/en_US", sep="")
setwd(data_path)
#package I use to do this projectï¼š
library(ggplot2)
library(tm)€€#text mining 
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
##length(readLines(twitts, encoding="UTF-8"))
#length(readLines(news,encoding="UTF-8"))
#length(readLines(blogs,encoding="UTF-8"))
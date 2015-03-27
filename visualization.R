#4 visualization : 
my_dtm<-as.matrix(dtm)
order<-sort(colSums(my_dtm), decreasing=TRUE)[1:100]
order_name <-names(order)
word_freq<-data.frame(order)
df<-data.frame(as.character(rownames(word_freq)), word_freq)
colnames(df)[1]="word_names"
names(df)<-c("word_names","frequency")
rownames(df)<-c(1:nrow(df))
#transformed to a frequency table for plotting easily.
head(df)
#order frequency as decreasing and save to a data frame called df_order
df_order<-df[order(-df$frequency), ]
#df_freq<-as.vector(rep(df_order$word_names, df_order$frequency))
#df_new<-data.frame(df_freq)
#head(df_new)

g<-ggplot(df_order,aes(x=word_names, y=frequency)) + 
        geom_bar(stat="identity",colour="yellow", fill="pink") +
        labs(x="frequency of each word", y="word names") +
        ggtitle("histogram of word frequencies") +
        coord_flip() +
        theme_bw() +
        geom_text(aes(label=frequency))

print(g)

#use word cloud.
v<-sort(colSums(my_dtm), decreasing=TRUE)
words<-names(v)
d<-data.frame(word=words, freq=v)
wordcloud(d$word,d$freq,max.words=150,colors=brewer.pal(5,"Set1"),random.order=FALSE)


#draw a graph:
#source("http://bioconductor.org/biocLite.R")
#biocLite("graph")
library(graph)
#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
library(Rgraphviz)
freq<-findFreqTerms(dtm, lowfreq=15)
plot(dtm, term=freq, corThreshold=0.3, weighting=T)

#cluster:
dtm_rst<-removeSparseTerms(dtm, sparse=0.95)
df_dtm<-as.data.frame(inspect(dtm_rst))
d<-dist(scale(df_dtm), method="euclidean")
fit<-hclust(dist_mat,method="ward.D")
plot(fit)
rect.hclust(fit, k=5)

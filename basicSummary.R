#2 summary of data

#see how many lines of data(run bash command in R script)
#below is a way to count lines ( slower)
#twitts_path<-"~/en_US.twitter.txt"
#R.utils::countLines(twitts_path)
#news_path<-"~/en_US.news.txt"
#R.utils::countLines(news_path)
#blogs_path<-"~/en_US.blogs.txt"
#R.utils::countLines(blogs_path)
#This below is a faster way to count lines: 
stri_stats_general(twitts)[1]
stri_stats_general(news)[1]
stri_stats_general(blogs)[1]
object_size(twitts)
object_size(news)
object_size(blogs)
#count and sum words in each data:
sum(sapply(gregexpr("\\W+", twitts), length))
sum(sapply(gregexpr("\\W+", news), length))
sum(sapply(gregexpr("\\W+", blogs), length))
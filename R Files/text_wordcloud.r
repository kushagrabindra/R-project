library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

tweetsDS<-read.csv("sentiment.csv")
required<-tweetsDS[[11]]
required
tweetsDS.Corpus<-Corpus(VectorSource(required))

tweetsDS.Clean<-tm_map(tweetsDS.Corpus, PlainTextDocument)
tweetsDS.Clean<-tm_map(tweetsDS.Corpus,tolower)
tweetsDS.Clean<-tm_map(tweetsDS.Clean,removeNumbers)
tweetsDS.Clean<-tm_map(tweetsDS.Clean,removeWords,stopwords("english"))
tweetsDS.Clean<-tm_map(tweetsDS.Clean,removePunctuation)
tweetsDS.Clean<-tm_map(tweetsDS.Clean,stripWhitespace)
tweetsDS.Clean<-tm_map(tweetsDS.Clean,stemDocument)

wordcloud(words = tweetsDS.Clean, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(tweetsDS.Clean,max.words = 200,random.color = TRUE,random.order=FALSE)
base_file <- read.csv("sentiment.csv")
base_file

preliminary_dataset <- base_file %>%
  select(AS = airline_sentiment, ASC = airline_sentiment_confidence, NRC = negativereason_confidence)
preliminary_dataset

MeanNRC<-mean(preliminary_dataset$NRC, na.rm=TRUE)
MeanNRC

preliminary_dataset$NRC[which(is.na(preliminary_dataset$NRC))] <- MeanNRC

write.csv(preliminary_dataset,"FinalKMeansDataset.csv",row.names = F)

KMeans<-read.csv("FinalKMeansDataset.csv")
data(KMeans)
head(KMeans)
KMeans_2<-KMeans[-1]
KMeans_2
head(KMeans_2)
KMeans_3<-as.data.frame(scale(KMeans_2))
KMeans_3
head(KMeans_3)

sapply(KMeans_2,mean)
sapply(KMeans_2,sd)
sapply(KMeans_3,mean)
sapply(KMeans_3,sd)

library(NbClust)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(KMeans_3,nc=30,seed=1234)


iris_kmeans<-kmeans(KMeans_3,10)
iris_kmeans
iris_kmeans$centers
iris_kmeans$size
#iris$clstr<-iris_kmeans$cluster


#iris$clstr<-iris_kmeans$cluster
#table(iris$Species,iris$clstr)

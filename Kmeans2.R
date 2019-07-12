###Creating doc corpus and cleaning data
TEXTDOCS <- "ds6/"
install.packages('NLP')
install.packages('SnowballC')
install.packages('ggplot2')

library(NLP)
library(tm)
ds <- DirSource(TEXTDOCS)
doc.corpus <- Corpus(ds)
doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
doc.corpus <- tm_map(doc.corpus, stripWhitespace)
doc.corpus <- tm_map(doc.corpus, stemDocument)


###Creating term doc matrix
TDM <- TermDocumentMatrix(doc.corpus)
dim(TDM)
###Removing relatively infrequent terms
TDM = removeSparseTerms(TDM, 0.75)
dim(TDM)

###Terms used at least 500 times
findFreqTerms(TDM, 500)

###Visuals of frequent terms
tf <- rowSums(as.matrix(TDM))
tf <- subset(tf, tf >= 500)
barplot(tf, las=2)

library(ggplot2)
df <- data.frame(term = names(tf), freq = tf)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip()

install.packages("RColorBrewer")
library(RColorBrewer)

library(wordcloud)
tdm <- as.matrix(TDM)
palette <- brewer.pal(9,"BuGn")[-(1:4)]
wordcloud(rownames(tdm), rowSums(tdm), min.freq = 500, color = palette)

install.packages('DocumentTermMatrix')

###Creating doc term matrix
DTM <- DocumentTermMatrix(doc.corpus)
dim(DTM)
DM <- as.matrix(DTM)

###Measuring similarity between documents
#creating jaccard distance function
jac <- function (A, B) {
  i <- A %*% B
  u <- A %*% A + B %*% B - A %*% B
  j <- i / u
  return (j)
}

#creating a similarity matrix (takes a minute)
sm <- matrix(0, nrow = 265, ncol = 265)
for (i in 1:264) {
  for (j in (i+1):265) {
    sm[i,j] <- jac(DM[i,], DM[j,])
  }
}
sm==57

###Display most/least similar documents
mx = max(sm) #most similar
mx2 = max(sm[sm<mx]) #second most similar
mn = min(sm[sm>0]) #least similar
mn2 = min(sm[sm>mn]) #second least similar
which(sm == mx, arr.ind = TRUE) #7 and 11
which(sm == mx2, arr.ind = TRUE) #2 and 3
which(sm == mn, arr.ind = TRUE) #15 and 86
which(sm == mn2, arr.ind = TRUE) #4 and 58
#NOTE: These documents are not in ascending order (i.e. the document in spot 7 may not be document 7).
#With a dataset this large it would be tedious to order them correctly, but we can do this if you
#it is necessary.




d<-dist(sm, method="euclidean")

#2. K-means
### k-means (this uses euclidean distance)
#number of clusters<-k
k=5
clusters <- kmeans(as.matrix(d),k , nstart = 25)
plot(d, col = clusters$cluster)
points(clusters$centers, col = 1:k, pch = 8)
clusters

#better way to visualize clusters
library(cluster)
clusplot(as.matrix(d), clusters$cluster, color=T, shade=T, labels=2, lines=0)
clust<-clusters$cluster
clust[clust==2]
d5<-labels(clust[clust==2])
d5

d6<-DM[d5,]
n = 1:k
j = 1
for(j in n){
  assign(paste0("kmeans_clust_",j) , which(clust ==j, arr.ind = TRUE))
  j=j+1
}
  


#1. Hierarchical
#run hierarchical clustering using Wardâ€™s method
Hiarc <- hclust(d,method="ward.D") #try method = "ward.D", "complete", "single", "average"
#plot dendogram, use hang to ensure that labels fall below tree
plot(Hiarc, hang=-1)

#creates vector with documents contained in the same branch of the dendrograph
clusth = cutree(Hiarc, 5)
clusth

#creates vectors that contain the documents assigned to each cluster from the cutree function
n = 1:5
j = 1
for(j in n){
  assign(paste0("hiarc_clust_",j) , which(clusth ==j, arr.ind = TRUE))
  j=j+1
}




j=1
m=1
n=1:5
for(j in n){
  for (m in n){ 
    A[j,m]=length(intersect(as.vector(hiarc_clust_m), as.vector(kmeans_clust_j)))
    m=m+1
  }
  j=j+1
}
length(intersect(as.numeric(hiarc_clust_1), as.numeric(kmeans_clust_1)))
length(intersect(as.numeric(hiarc_clust_1), as.numeric(kmeans_clust_2)))
length(intersect(as.numeric(hiarc_clust_1), as.numeric(kmeans_clust_3)))
length(intersect(as.numeric(hiarc_clust_1), as.numeric(kmeans_clust_4)))
length(intersect(as.numeric(hiarc_clust_1), as.numeric(kmeans_clust_5)))
length(intersect(as.numeric(hiarc_clust_2), as.numeric(kmeans_clust_1)))
length(intersect(as.numeric(hiarc_clust_2), as.numeric(kmeans_clust_2)))
length(intersect(as.numeric(hiarc_clust_2), as.numeric(kmeans_clust_3)))
length(intersect(as.numeric(hiarc_clust_2), as.numeric(kmeans_clust_4)))
length(intersect(as.numeric(hiarc_clust_2), as.numeric(kmeans_clust_5)))
length(intersect(as.numeric(hiarc_clust_3), as.numeric(kmeans_clust_1)))
length(intersect(as.numeric(hiarc_clust_3), as.numeric(kmeans_clust_2)))
length(intersect(as.numeric(hiarc_clust_3), as.numeric(kmeans_clust_3)))
length(intersect(as.numeric(hiarc_clust_3), as.numeric(kmeans_clust_4)))
length(intersect(as.numeric(hiarc_clust_3), as.numeric(kmeans_clust_5)))
length(intersect(as.numeric(hiarc_clust_4), as.numeric(kmeans_clust_1)))
length(intersect(as.numeric(hiarc_clust_4), as.numeric(kmeans_clust_2)))
length(intersect(as.numeric(hiarc_clust_4), as.numeric(kmeans_clust_3)))
length(intersect(as.numeric(hiarc_clust_4), as.numeric(kmeans_clust_4)))
length(intersect(as.numeric(hiarc_clust_4), as.numeric(kmeans_clust_5)))
length(intersect(as.numeric(hiarc_clust_5), as.numeric(kmeans_clust_1)))
length(intersect(as.numeric(hiarc_clust_5), as.numeric(kmeans_clust_2)))
length(intersect(as.numeric(hiarc_clust_5), as.numeric(kmeans_clust_3)))
length(intersect(as.numeric(hiarc_clust_5), as.numeric(kmeans_clust_4)))
length(intersect(as.numeric(hiarc_clust_5), as.numeric(kmeans_clust_5)))


length(hiarc_clust_1)
lenght(kmeans_clust_4)
inter

 
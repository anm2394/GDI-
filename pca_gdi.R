gender<-read.csv("C:/Users/anany/OneDrive/Documents/gendercluster.csv")
kmeans(gender$GDI,4,10, nstart = 50)
gdipca<-read.csv("C:/Users/anany/OneDrive/Documents/GDI PCA.csv")
rownames(gdipca)<-gdipca$India.States.UTs
gdi.pca<-prcomp(gdipca[,c(2:7)], center =TRUE, scale. = TRUE)
summary(gdi.pca)
str(gdi.pca)
gdi.pca$rotation
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(gdi.pca, labels = rownames(gdipca))
gdipca<-gdipca[,-1]
gdi.ks<-kmeans(gdipca,3, iter.max=10, nstart = 50, )
summary(gdi.ks)
gdi.ks$cluster
plot<-autoplot(gdi.ks,data=gdipca, frame=TRUE)
print(plot)
write.csv(gdi.ks$cluster, "cluster kmeans")
rownames(gender)<-gender$India.States.UTs
gender<-gender[,-1]
kmeans(gender,4, iter.max = 10, nstart=50)
gdi.ks$cluster
fviz_nbclust(gdipca,kmeans)
gdipca=m(-1)
#optimal number of clusters
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(gdipca)
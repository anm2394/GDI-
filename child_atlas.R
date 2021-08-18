m<-read.csv("C:/Users/anany/OneDrive/Documents/child_atlas.csv")
m[is.na(m)] <- 0
j<-prcomp(m[,3:25])
m1<-m[,-c(1,2)]


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
wssplot(m1)

km<-kmeans(m1,2, nstart=50)
km
#m2<-m
m<- cbind(m,km$cluster)

fviz_cluster(km, data=m1, geom = "text")



install.packages("fs")

library(devtools)

install_github("vqv/ggbiplot")
library(ggbiplot)
library(gridbase)
library(ggplot2)
library(plyr)
library(scales)

ggbiplot(j, ellipse=TRUE,circle=TRUE, labels=m$State...District, groups=m$`km$cluster`)

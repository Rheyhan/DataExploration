#libraries
library(cluster)
library(factoextra)
library(ggplot2)
library(ggsignif)
library(factoextra)
library(pheatmap)
#K means clustering
  #df
df <- USArrests 

  #finding optimum cluster
fviz_nbclust(df, kmeans, method = "silhouette")
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(x = df, FUNcluster = kmeans, method = "gap_stat")

  #setting k-means
set.seed(123)
km <- kmeans(x = df, centers = 2, nstart = 25)
str(km)

  #visualization
fviz_cluster(km, geom = "point", data = df)+ggtitle("k=2")
fviz_cluster(object = km, data = df, palette = "jco", ggtheme = theme_minimal())

clusplot(df,km$cluster)
clusplot(df,km$cluster,color=T,shade=T)

#Hiearchical Clustering 
hc <- hclust(dist(df), method = "ward.D2")

  #visualization
fviz_dend(x = hc, cex = 0.5, k = 4, palette = "jco")

pheatmap(t(df), cutree_cols = 4, fontsize_col = 8.5)


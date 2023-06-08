#install.packages("ggsignif")
install.packages("pheatmap")
library(cluster)
library(factoextra)
library(ggplot2)
library(ggsignif)

data("USArrests")
df <- USArrests 

# Scaling/Standarisasi 
df <- scale(USArrests)

# Penentuan jumlah cluster optimum 
## Koefisien silhoutte dan Elbow

fviz_nbclust(df, kmeans, method = "silhouette")
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(x = df, FUNcluster = kmeans, method = "gap_stat")

# K-Means
set.seed(123)
km <- kmeans(x = df, centers = 2, nstart = 25)
str(km)

# Visualisasi
fviz_cluster(km, geom = "point", data = df)+ggtitle("k=2")
fviz_cluster(object = km, data = df, palette = "jco", ggtheme = theme_minimal())

clusplot(df,km$cluster)
clusplot(df,km$cluster,color=T,shade=T)


# Hiearchical Clustering 
### Contoh 1
hc <- hclust(dist(df), method = "ward.D2")

# Visualisasi
viz_dend(x = hc, cex = 0.5, k = 4, palette = "jco")

library(pheatmap)
pheatmap(t(df), cutree_cols = 4, fontsize_col = 8.5)

### Contoh 2
#define linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

#function to compute agglomerative coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac)

clust <- agnes(df, method = "ward")
clust

#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#compute distance matrix
d <- dist(df, method = "euclidean")

#perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )

#cut the dendrogram into 4 clusters
groups <- cutree(final_clust, k=4)

#find number of observations in each cluster
table(groups)

#append cluster labels to original data
final_data <- cbind(USArrests, cluster = groups)

#display first six rows of final data
head(final_data)

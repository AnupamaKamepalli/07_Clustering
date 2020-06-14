#Import dataset.
getwd()
setwd("C:/Users/anupa/Desktop/ExcelR/Assignment/07_Clustering")
install.packages("readxl")
library(readxl)
d1_air <- read_excel("EastWestAirlines.xlsx")
d2_air <- d1_air[,-1]
View(d1_air)
head(d1_air)

#Check for NA values.
sum(is.na(d1_air)) # Not found any NA values.
any(is.na(d1_air))

#Correlation Check.
install.packages("corrplot")
library(corrplot)
corrplot(cor(d2_air),method = "number",type = "lower")
#so bonus miles vs cc1_miles,Flight_trans_12 VS Flight_miles_12mo are highly correlated.

#Normalization
d2_air_scale <- scale(d2_air)
head(d2_air_scale)

#Determine the no. of clusters using scree plot.
wss_air <- nrow(d2_air_scale)-1*sum(apply(d2_air_scale,2,var))
View(wss_air)
wss_air <- NULL
for (i in 1:20) wss_air[i] <- sum(kmeans(d2_air_scale,centers= i)$withinss)
plot(1:20,wss_air,xlab = "Clusters",ylab = "sum of squares",type = 'b')
# i cant determine due to many clusters.

wss_air <- nrow(d2_air_scale)-1*sum(apply(d2_air_scale,2,var))
View(wss_air)
wss_air <- NULL
for (i in 1:10) wss_air[i] <- sum(kmeans(d2_air_scale,centers= i)$withinss)
plot(1:10,wss_air,xlab = "Clusters",ylab = "sum of squares",type = 'b')

# So i feel optimum number of clusters could be 5.

# K-means clustering technique.
set.seed(123)
fit_air <- kmeans(d2_air_scale,5)
fit_air$cluster
d2_air_K <- cbind(fit_air$cluster,d2_air)
View(d2_air_K)
aggregate(d2_air_K,by=list(fit_air$cluster),FUN = mean)

#Clara function to find the cluster.
install.packages("cluster")
library("cluster")
fit_air_clara <- clara(d2_air_scale,5)
clusplot(fit_air_clara)

#PAM function to find the cluster.

fit_air_Mediods <- pam(d2_air_scale,5)
clusplot(fit_air_Mediods)

#Hierarachial Clustering.

View(d2_air_scale)
d <- dist(d2_air_scale,method = "euclidean")
hir_ward <- hclust(d,method = "complete")
str(hir_ward)
plot(hir_ward,hang = -1)

groups_air <- cutree(hir_ward,k=5)
View(groups_air)

rect.hclust(hir_ward,k=5,border = "red")

Gtp_hir_final <- cbind.data.frame(d1_air,groups_air)
View(Gtp_hir_final)

#Aggregate the data based on cluster wise.
hier_clst_agg <- aggregate(d1_air[,2:12],by=list(groups_air),FUN = "mean")
View(hier_clst_agg)








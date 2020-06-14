# Import the dataset
getwd()
Crime_D1 <- read.csv("crime_data.csv")
View(Crime_D1)
Crime_D2 <- Crime_D1[-1] # Excludes the first column
View(Crime_D2)
head(Crime_D2)

#Check for NA values.
sum(is.na(Crime_D2)) # NA values not found
any(is.na(Crime_D2)) # False : NA values not found.

#Correlation Check.
install.packages("corrplot")
library(corrplot)
corrplot(cor(Crime_D2),type = "lower",method = "number") # Moderate Positive Correlation  
cor(Crime_D2)

#Normalization.
Crime_Norm <- scale(Crime_D2)
head(Crime_Norm,n=3)

#Determine the no. of clusters using scree plot.
wss_cal <- nrow(Crime_Norm)-1 * sum(apply(Crime_Norm,2,var))
wss_cal

wss_cal <- NULL
for(i in 1:10) wss_cal[i] <- sum(kmeans(Crime_Norm,centers = i)$withinss)
plot(1:10,wss_cal,xlab = "clusters",ylab = "sum of withinss",type = "b") # Type=b tells us the circle indicators.
#Conclusion: The optimum number of clusters is 4

#K- means clusters model with K= 4

KM_CR_model1 <- kmeans(Crime_Norm,4)
str(KM_CR_model1)

# Now, i want avg of columns of each 0f 4 cluster for analysis.
crimeopavg <- aggregate(Crime_D2,by=list(KM_CR_model1$cluster),FUN = mean)
write.csv(crimeopavg,file = "crimeopavg.csv")

#Assign rows to its respective cluster.
final_DS <- cbind(Crime_D1,KM_CR_model1$cluster)
View(final_DS)
write.csv(final_DS,file = "CrimeFileop.csv")
getwd()



#----------------------------------
Crime_D <- read.csv("crime_data.csv")
head(Crime_D)

# Created the size of the sample with index positions. Everytime, values should not change.
set.seed(123)
Sample_seg <- sample(1:50,10)  #Index positions
View(Sample_seg)

sample_seg1 <- Crime_D[Sample_seg,-1] # Sample set of records
head(sample_seg1)
View(sample_seg1)
View(Crime_D)

#Calculate the Distance b/w the records.
sample_seg1_dist <- dist(sample_seg1,method = "euclidean")
sample_seg1_dist

#Heat MAps will gelps us to determine optimum number of cluster required
install.packages("factoextra")
library(factoextra)
fviz_dist(sample_seg1_dist) 

#Clusters visualization.
fviz_cluster(KM_CR_model1,data = Crime_D2,
             ellipse.type="euclid",
             star.plot= T,
             ggtheme = theme())





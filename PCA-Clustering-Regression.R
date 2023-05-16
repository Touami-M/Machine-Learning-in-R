library(FactoMineR)
library(cluster)
library(FSelector)
library(mlbench)
library(factoextra)
library(varhandle)

data("Shuttle")

#-----------------------------------------------------------------
#Partie 1: Kmeans sans ACP et sans FS

dataset=Shuttle
dataset$Class<-NULL #Enlever la colonne du sortie
z_scores <- apply(dataset, 2, function(x) abs((x-mean(x))/sd(x)))
outliers <- which(z_scores > 3, arr.ind = TRUE)

# Remove outliers from the dataset
Shuttle_clean <- dataset[-outliers[,1], ]
dataset = Shuttle_clean

# normalisation
dataset=scale(x=dataset,center = TRUE)


#wss pour choisir le facteur K pour faire le clustering
k.max <- 15
wss <- sapply(1:k.max, function(k){kmeans(dataset, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")



#On trouve K=7, on fait le clustering alors
T1<-Sys.time() 
k <- 7 # replace with the optimal value of K=7
cluster <- kmeans(dataset, k)  #on fait le clustering
T2<-Sys.time()
delta_T1=T2-T1
delta_T1 #Temps d'execution sans ACP et sans FS
table(cluster$cluster) / nrow(dataset) * 100 #les pourcentages des clusters
clusplot(dataset,cluster$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)

#----------------------------------------------------------
#Partie 2: Kmeans avec ACP

dataset2=Shuttle_clean


#on effectue l'ACP
dataset2 = PCA(dataset2, scale.unit=TRUE, graph = FALSE)
scores <- predict(dataset2, newdata = Shuttle[, -10]) 
dataset2=scores[["coord"]] #les projections des individus (5 variables gardées)

#wss pour choisir le facteur K pour faire le clustering
k.max <- 15
wss2 <- sapply(1:k.max, function(k){kmeans(dataset2, k, nstart=50,iter.max = 15 )$tot.withinss})
wss2
plot(1:k.max, wss2, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")

#On trouve K=7, on fait le clustering alors
T1<-Sys.time() 
k <- 7 # replace with the optimal value of K=7
cluster <- kmeans(dataset2, k)  #on fait le clustering
T2<-Sys.time()
delta_T2=T2-T1
delta_T2 #Temps d'execution avec ACP
table(cluster$cluster) / nrow(dataset2) * 100 #les pourcentages des clusters
clusplot(dataset2,cluster$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)

#-------------------------------------------------------
#Partie 3: Kmeans avec Feature Selection

dataset3=Shuttle
result <- cfs(Class ~ ., dataset3)
result 
f <- as.simple.formula(result, "Class")
f
dataset3 <- dataset3[, c("V1", "V7", "V9")]


#wss pour choisir le facteur K pour faire le clustering
k.max <- 15
wss3 <- sapply(1:k.max, function(k){kmeans(dataset3, k, nstart=50,iter.max = 15 )$tot.withinss})
wss3
plot(1:k.max, wss3, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")


#On trouve K=2, on fait le clustering alors
T1<-Sys.time() 
k <- 2 # replace with the optimal value of K=2
cluster <- kmeans(dataset3, k)  #on fait le clustering
T2<-Sys.time()
delta_T3=T2-T1
delta_T3 #Temps d'execution avec ACP
table(cluster$cluster) / nrow(dataset3) * 100 #les pourcentages des clusters
clusplot(dataset3,cluster$cluster, color=TRUE, shade=TRUE, labels=0, lines=0)

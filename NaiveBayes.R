#----------------------------------------------------------------------
#Partie Naive Bayes
library(e1071)
library(mlbench)
data=data(Shuttle)
data=Shuttle

#Partitionner le dataset en échantillon d'apprentissage et échantillon test (70%-30%)
n=dim(data)[1]
index = sample(n, 0.7 * n)
Appren = data[index, ]
Test = data[-index, ]
dim(Appren)
dim(Test)
#Effectuer Naive bayes sur le dataset d'apprentissage
T1<-Sys.time()
nb.model<- naiveBayes(Class ~ ., data = Appren)
T2<-Sys.time()
deltaT=T2-T1

nb.model
summary(nb.model)
#Prédire les sorties de l'échantillon de test
Pred=predict(object = nb.model, newdata = Test)

#Concaténer les sorties prédites et les véritable sorties
Test.mod <- cbind(Test, Pred)
head(Test.mod, 5)

#Déterminer la table de confusion
Confusion = table(Test.mod$Class, Test.mod$Pred)
Confusion

#Calculer l'erreur du test
err<- 1-sum(diag(Confusion))/sum(Confusion)
err

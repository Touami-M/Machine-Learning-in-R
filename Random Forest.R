data=data(Shuttle)
data=Shuttle
RF=randomForest(Class~., data=data)
summary(RF)
RF
error = mean(predict(RF, data)!= data$Class)

entree = data[1,]
prediction = predict(RF,entree)
prediction

install.packages("e1071")
library("e1071")
t1=tune.randomForest(Class~., data=data,ntree=c(10,100,200,500))
plot(t1)

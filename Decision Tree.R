data = arbre
library(rpart)
library(rpart.plot)

lapply(data,class)

data$Outlook <- as.factor(data$Outlook)
data$Temperature <- as.factor(data$Temperature)
data$Humidity <- as.factor(data$Humidity)
data$Wind <- as.factor(data$Wind)
data$playTennis <- as.factor(data$playTennis)

Tree <- rpart(playTennis ~ Outlook + Temperature + Humidity + Wind, data = data)
Tree
summary(Tree)
rpart.plot(Tree)

pred=predict(Tree,data, type="class")

tab=table(data$playTennis, pred)

TreeSimple <- prune(Tree,cp=0.066)
summary(TreeSimple)
prp(TreeSimple,extra=1)




predict(TreeSimple,data)
predict(Tree,data)

pred=predict(Tree,data, type="class")

table(data$playTennis, pred)

predict(Tree,data)

prune(Tree,0.02)

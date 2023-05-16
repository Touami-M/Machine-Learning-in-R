dataset = iris
 
is.factor(dataset$species) #Verification

dataset$Species <- factor(dataset$Species) #Factorisation 

#Effectuer One hot encoding
for(unique_value in unique(dataset$Species)){  dataset[paste("Species", unique_value, sep = ".")]<- ifelse(dataset$Species == unique_value, 1, 0)}

#Supression du colonne
dataset$Species <- NULL

#Normalisation MinMax
install.packages("varhandle")
library(varhandle)
dataset[,1:4] = apply(dataset[,1:4], 2, function(x) (x - min(x)) / (max(x) - min(x)) * (1-0) + 0)



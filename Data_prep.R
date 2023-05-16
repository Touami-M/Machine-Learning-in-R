dataset=airquality
sapply(dataset, function(x) length(unique(x)))
sapply(dataset,function(x) sum(is.na(x)))
sum(is.na(dataset$Ozone))
dataset[is.na(dataset$Ozone),]

install.packages("Amelia")
library(Amelia)
missmap(dataset, main = "Missing values vs observed (dataset)")

#supression de la colonne
dataset_1=dataset
dataset_1$Ozone = NULL
dataset_1$Solar.R = NULLS
missmap(dataset_1, main = "Missing values vs observed (dataset_1)")

#Supression des lignes NA
dataset_2=dataset
dataset_2=subset(dataset_2, !is.na(dataset$Ozone) & !is.na(dataset$Solar.R))
missmap(dataset_2, main = "Missing values vs observed (dataset_2)")

#Remplacer les valeurs NA par la moyenne
dataset_3=dataset
dataset_3$Ozone[is.na(dataset_3$Ozone)] <- mean(dataset_3$Ozone,na.rm=T)
dataset_3$Solar.R[is.na(dataset_3$Solar.R)] <- mean(dataset_3$Solar.R,na.rm=T)
missmap(dataset_3, main = "Missing values vs observed (dataset_3)")

#Remplacer les valeurs NA par la médiane
dataset_4=dataset
dataset_4$Ozone[is.na(dataset_4$Ozone)] <- mean(dataset_4$Ozone,na.rm=T)
dataset_4$Solar.R[is.na(dataset_4$Solar.R)] <- mean(dataset_4$Solar.R,na.rm=T)
missmap(dataset_4, main = "Missing values vs observed (dataset_4)")

#Regression lineaire
#loading packages
install.packages("tidyverse")
install.packages("broom")
library(tidyverse)
library(broom)

# Create a copy of the original dataset
dataset_5 <- dataset

# Create a linear regression model for the Ozone column
lm_model_ozone <- lm(Ozone ~ Solar.R + Wind + Temp, data = dataset)

# Use the augment() function from the broom package to create a new dataset with predicted values
dataset_augmented_ozone <- augment(lm_model_ozone, newdata = dataset)


# Replace the NA values in the Ozone column with the predicted values from the augmented dataset
dataset_5$Ozone <- ifelse(is.na(dataset_5$Ozone), dataset_augmented_ozone$Ozone.fitted, dataset_5$Ozone)

# Create a linear regression model for the Solar.R column
lm_model_solar <- lm(Solar.R ~ Ozone + Wind + Temp, data = dataset_5)

# Use the augment() function from the broom package to create a new dataset with predicted values
dataset_augmented_solar <- augment(lm_model_solar, newdata = dataset_5)

# Replace the NA values in the Solar.R column with the predicted values from the augmented dataset
dataset_5$Solar.R <- ifelse(is.na(dataset_5$Solar.R), dataset_augmented_solar$Solar.R.fitted, dataset_5$Solar.R)


#------------------------------------------------------------------------


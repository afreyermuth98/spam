# Adresse du dossier où vous travaillez
#Antho
setwd("F:/Enseirb/AnalyseDeDonnees/spam/Code")
# Clément
#setwd("/Users/luffy/Documents/Cours/ENSEIRB/3ème année/analyse_de_donnees/projet/spam/Code")

# Packages utilisés dans la suite
library(tm)
library(plyr)
library(class)
library(caret)
library(e1071)
library(knitr)
library(kernlab)
library(LiblineaR)

# Supprimer toutes les variables
rm(list=ls(all=TRUE))

# Supprimer tous les graphiques déjà présents
graphics.off()

# Lecture des données d'apprentissage

load(file = "../Data/spam_data_train.rda")
load(file = "../Data/spam_data_test.rda")

fractionTraining   <- 0.80
fractionValidation <- 0.20

sampleSizeTraining   <- floor(fractionTraining   * nrow(data_train))
sampleSizeValidation <- floor(fractionValidation * nrow(data_train))

# Mélange des données (shuffle)
indicesTraining    <- sort(sample(seq_len(nrow(data_train)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(data_train)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))

dataTraining   <- data_train[indicesTraining, ]
dataValidation <- data_train[indicesValidation, ]

data_train_x <- dataTraining[c(0:57)]
data_validate_x <- dataValidation[c(0:57)]

data_train_y <- as.factor(dataTraining$label)
data_validate_y <- as.factor(dataValidation$label)

#svm <- svm(formula = label~.,
#           data = dataTraining,
#           type = 'C-classification',
#           kernel='linear')
#pred_test <- predict(svm, newdata=data_train_x)

#error_rate <- mean(pred_test != dataTraining$label)
#cat("error_rate using test data linear = ",error_rate)

#svm <- svm(formula = label~.,
#           data = dataTraining,
#           type = 'C-classification',
#           kernel='polynomial')
#pred_test <- predict(svm, newdata=data_train_x)

#error_rate <- mean(pred_test != dataTraining$label)
#cat("error_rate using test data polynomial = ",error_rate)

svm <- svm(formula = label~.,
           data = dataTraining,
           type = 'C-classification',
           kernel='radial',
           cost = 1,
           gamma = 0.1)
pred_test <- predict(svm, newdata=data_train_x)
confusionMatrix(pred_test,data_train_y)

error_rate <- mean(pred_test != dataTraining$label)
cat("error_rate using test data radial = ",error_rate)

#svm <- svm(formula = label~.,
#           data = dataTraining,
#           type = 'C-classification',
#           kernel='sigmoid')
#pred_test <- predict(svm, newdata=data_train_x)

#error_rate <- mean(pred_test != dataTraining$label)
#cat("error_rate using test data sigmoid = ",error_rate)

pred_validate <- predict(svm, newdata=data_validate_x)
confusionMatrix(pred_validate,data_validate_y)

error_rate <- mean(pred_validate != dataValidation$label)
cat("error_rate using validate data = ",error_rate)


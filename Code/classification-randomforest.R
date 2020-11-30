# Adresse du dossier o� vous travaillez
#Antho
setwd("F:/Enseirb/AnalyseDeDonnees/spam/Code")
# Cl�ment
#setwd("/Users/luffy/Documents/Cours/ENSEIRB/3�me ann�e/analyse_de_donnees/projet/spam/Code")

# Packages utilis�s dans la suite
library(class)
library(caret)
library(ROCR)
library("FactoMineR")

# Supprimer toutes les variables
rm(list=ls(all=TRUE))

# Supprimer tous les graphiques d�j� pr�sents
graphics.off()

# Lecture des donn�es d'apprentissage

load(file = "../Data/spam_data_train.rda")
load(file = "../Data/spam_data_test.rda")

fractionTraining   <- 0.80
fractionValidation <- 0.20

sampleSizeTraining   <- floor(fractionTraining   * nrow(data_train))
sampleSizeValidation <- floor(fractionValidation * nrow(data_train))

# M�lange des donn�es (shuffle)
indicesTraining    <- sort(sample(seq_len(nrow(data_train)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(data_train)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))

dataTraining   <- data_train[indicesTraining, ]
dataValidation <- data_train[indicesValidation, ]

data_train_x <- dataTraining[c(0:57)]
data_validate_x <- dataValidation[c(0:57)]

data_train_y <- as.factor(dataTraining$label)
data_validate_y <- as.factor(dataValidation$label)

rf <- randomForest(x = data_train_x, y = data_train_y, ntree=200)

# �volution de l'erreur en fonction du nombre d'arbres
# Ici ntree est fix� � la valeur par d�faut = 500
plot(rf$err.rate[,1], type="l")

# Affichage des r�sultats
print(rf)

# Importance des variables
rf$importance
varImpPlot(rf)

# Prediction sur les donn�es test
rf_predict_data_validate <- predict(rf, newdata=data_validate_x)

# Comparaison des valeurs pr�dites et des valeurs observ�es
table(rf_predict_data_validate, dataValidation$label)

# Calcul du taux d'erreur
error_rate <- mean(rf_predict_data_validate != dataValidation$label)
cat("error_rate using test data = ",error_rate)


#R�sultats pour le test :
rf_predict_data_validate <- predict(rf, newdata=data_test)
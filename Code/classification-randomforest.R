# Adresse du dossier où vous travaillez
#Antho
setwd("F:/Enseirb/AnalyseDeDonnees/spam/Code")
# Clément
#setwd("/Users/luffy/Documents/Cours/ENSEIRB/3ème année/analyse_de_donnees/projet/spam/Code")

# Packages utilisés dans la suite
library(class)
library(caret)
library(ROCR)
library("FactoMineR")

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

## TO COMPUTE BEST MTRY
if(FALSE) {
library(caret)
grille.mtry <- data.frame(mtry=seq(1,30,by=1))
ctrl <- trainControl(method="oob")
library(doParallel) ## pour paralléliser
cl <- makePSOCKcluster(4)
registerDoParallel(cl)
set.seed(12345)
sel.mtry <- train(label~.,data=dataTraining,method="rf",trControl=ctrl,
                  tuneGrid=grille.mtry)
on.exit(stopCluster(cl))
sel.mtry
}


rf <- randomForest(x = data_train_x, y = data_train_y, ntree=200, mtry=7)

# Évolution de l'erreur en fonction du nombre d'arbres
# Ici ntree est fixé à la valeur par défaut = 500
plot(rf$err.rate[,1], type="l")

# Affichage des résultats
print(rf)

# Importance des variables
rf$importance
varImpPlot(rf)

# Prediction sur les données test
rf_predict_data_validate <- predict(rf, newdata=data_validate_x)

# Comparaison des valeurs prédites et des valeurs observées
table(rf_predict_data_validate, dataValidation$label)
# Calcul du taux d'erreur
error_rate <- mean(rf_predict_data_validate != dataValidation$label)
cat("error_rate using test data = ",error_rate)

###################################################################???
#Résultats pour le test :
rf_predict_data_test <- predict(rf, newdata=data_test)

###################################################################

# Tests Base de données réduites
# Pas concluants
if (FALSE) {
data_train_reduced <- data_train[c(0,1,3,5,6,7,8,9,10,11,12,16,17,18,23,0,52,53,54,55,56,57,58)]

fractionTraining   <- 0.8
fractionValidation <- 0.2

sampleSizeTraining   <- floor(fractionTraining   * nrow(data_train_reduced))
sampleSizeValidation <- floor(fractionValidation * nrow(data_train_reduced))

# Mélange des données (shuffle)
indicesTraining    <- sort(sample(seq_len(nrow(data_train_reduced)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(data_train_reduced)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))

dataTraining   <- data_train_reduced[indicesTraining, ]
dataValidation <- data_train_reduced[indicesValidation, ]

data_train_x <- dataTraining[c(0:20)]
data_validate_x <- dataValidation[c(0:20)]

data_train_y <- as.factor(dataTraining$label)
data_validate_y <- as.factor(dataValidation$label)

rf <- randomForest(x = data_train_x, y = data_train_y, ntree=100, mtry=7)

# Évolution de l'erreur en fonction du nombre d'arbres
# Ici ntree est fixé à la valeur par défaut = 500
plot(rf$err.rate[,1], type="l")

# Affichage des résultats
print(rf)

# Importance des variables
rf$importance
varImpPlot(rf)

# Prediction sur les données test
rf_predict_data_validate <- predict(rf, newdata=data_validate_x)

# Comparaison des valeurs prédites et des valeurs observées
table(rf_predict_data_validate, dataValidation$label)
# Calcul du taux d'erreur
error_rate <- mean(rf_predict_data_validate != dataValidation$label)
cat("error_rate using test data = ",error_rate)
}


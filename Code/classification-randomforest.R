# Adresse du dossier où vous travaillez
#Antho
setwd("F:/Enseirb/AnalyseDeDonnees/spam/Code")
# Clément
#setwd("/Users/luffy/Documents/Cours/ENSEIRB/3ème année/analyse_de_donnees/projet/spam/Code")

# Packages utilisés dans la suite
library(class)
library(caret)
library(ROCR)
library(randomForest)
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
sel.mtry <- train(label~.,data=dataTraining,method="rf",
                  trControl=ctrl,
                  tuneGrid=grille.mtry)
on.exit(stopCluster(cl))
sel.mtry

}

rf <- randomForest(x = data_train_x, y = data_train_y, ntree=200, mtry=6)

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

###################################################################
#Résultats pour le test :
rf_predict_data_test <- predict(rf, newdata=data_test)

###################################################################
###########################EN DESSOUS D'ICI########################
##############################CA PUE###############################
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

# Tests base de données après ACP
if (FALSE) {
  data_train_reduced_acp <- data.frame(x1=-0.349*data_train[c(1)],x2=-0.162*data_train[c(2)],
                                   x3=-0.006*data_train[c(3)],x4=-0.094*data_train[c(4)],
                                   x5=-0.003*data_train[c(5)],x6=-0.071*data_train[c(6)],
                                   x7=-0.170*data_train[c(7)],x8=-0.110*data_train[c(8)],
                                   x9=-0.077*data_train[c(9)],x10=-0.160*data_train[c(10)],
                                   x11=-0.022*data_train[c(11)],x12=-0.247*data_train[c(12)],
                                   x13=-0.023*data_train[c(13)],x14=-0.130*data_train[c(14)],
                                   x15=-0.080*data_train[c(15)],x16=-0.123*data_train[c(16)],
                                   x17=-0.080*data_train[c(17)],x18=-0.101*data_train[c(18)],
                                   x19=-0.025*data_train[c(19)],x20=-0.046*data_train[c(20)],
                                   x21=-0.082*data_train[c(21)],x22=-0.064*data_train[c(22)],
                                   x23=-0.012*data_train[c(23)],x24=-0.177*data_train[c(24)],
                                   x25=-0.082*data_train[c(25)],x26=0.134*data_train[c(26)],
                                   x27=0.233*data_train[c(27)],x28=0.009*data_train[c(28)],
                                   x29=0.534*data_train[c(29)],x30=0.331*data_train[c(30)],
                                   x31=0.721*data_train[c(31)],x32=0.679*data_train[c(32)],
                                   x33=1.164*data_train[c(33)],x34=0.017*data_train[c(34)],
                                   x35=1.156*data_train[c(35)],x36=0.742*data_train[c(36)],
                                   x37=0.902*data_train[c(37)],x38=0.124*data_train[c(38)],
                                   x39=-0.018*data_train[c(39)],x40=0.072*data_train[c(40)],
                                   x41=0.924*data_train[c(41)],x42=0.007*data_train[c(42)],
                                   x43=0.032*data_train[c(43)],x44=0.308*data_train[c(44)],
                                   x45=0.006*data_train[c(45)],x46=0.013*data_train[c(46)],
                                   x47=-0.009*data_train[c(47)],x48=-0.015*data_train[c(48)],
                                   x49=0.018*data_train[c(49)],x50=-0.002*data_train[c(50)],
                                   x51=0.455*data_train[c(51)],x52=0.171*data_train[c(52)],
                                   x53=-0.042*data_train[c(53)],x54=-0.202*data_train[c(54)],
                                   x55=0.0001*data_train[c(55)],x56=-0.0003*data_train[c(56)],
                                   x55=-0.00009*data_train[c(57)], label=data_train[c(58)])
  
  fractionTraining   <- 0.8
  fractionValidation <- 0.2
  
  sampleSizeTraining   <- floor(fractionTraining   * nrow(data_train_reduced_acp))
  sampleSizeValidation <- floor(fractionValidation * nrow(data_train_reduced_acp))
  
  # Mélange des données (shuffle)
  indicesTraining    <- sort(sample(seq_len(nrow(data_train_reduced_acp)), size=sampleSizeTraining))
  indicesNotTraining <- setdiff(seq_len(nrow(data_train_reduced_acp)), indicesTraining)
  indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
  
  dataTraining   <- data_train_reduced_acp[indicesTraining, ]
  dataValidation <- data_train_reduced_acp[indicesValidation, ]
  
  data_train_x <- dataTraining[c(0:57)]
  data_validate_x <- dataValidation[c(0:57)]
  
  data_train_y <- as.factor(dataTraining$label)
  data_validate_y <- as.factor(dataValidation$label)
  
  rf <- randomForest(x = data_train_x, y = data_train_y, ntree=200, n_samples=10)
  
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


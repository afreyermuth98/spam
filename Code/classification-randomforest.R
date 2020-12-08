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

# Prediction sur les données validate
rf_predict_data_validate <- predict(rf, newdata=data_validate_x)

confusionMatrix(rf_predict_data_validate,data_validate_y)


# Comparaison des valeurs prédites et des valeurs observées
table(rf_predict_data_validate, dataValidation$label)

# Calcul du taux d'erreur
error_rate <- mean(rf_predict_data_validate != dataValidation$label)
cat("error_rate using validate data = ",error_rate)

###################################################################
#Résultats pour le test :
rf_predict_data_test <- predict(rf, newdata=data_test) 
##################################################################

###################################################################
###########################EN DESSOUS D'ICI########################
##############################C'EST PAS TOP###############################
###################################################################


# Tests Base de données réduites en ne gardant que quelques variables
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

###############################
# Tests base de données après ACP
################################

data_train_reduced_acp <- data.frame(word_freq_make=-0.349*data_train[c(1)],
                                     word_freq_address =-0.162*data_train[c(2)],
                                     word_freq_all=-0.006*data_train[c(3)],
                                     word_freq_3d=-0.094*data_train[c(4)],
                                     word_freq_our=-0.071*data_train[c(5)],
                                     word_freq_over=-0.170*data_train[c(6)],
                                     word_freq_remove=-0.110*data_train[c(7)],
                                     word_freq_internet=-0.077*data_train[c(8)],
                                     word_freq_order=-0.160*data_train[c(9)],
                                     word_freq_mail=-0.022*data_train[c(10)],
                                     word_freq_receive=-0.047*data_train[c(11)],
                                     word_freq_will=-0.023*data_train[c(12)],
                                     word_freq_people=-0.130*data_train[c(13)],
                                     word_freq_report=-0.080*data_train[c(14)],
                                     word_freq_addresses=-0.123*data_train[c(15)],
                                     word_freq_free=-0.079*data_train[c(16)],
                                     word_freq_business=-0.101*data_train[c(17)],
                                     word_freq_email=-0.025*data_train[c(18)],
                                     word_freq_you=-0.046*data_train[c(19)],
                                     word_freq_credit=-0.082*data_train[c(20)],
                                     word_freq_your=-0.064*data_train[c(21)],
                                     word_freq_font=-0.012*data_train[c(22)],
                                     word_freq_000=-0.177*data_train[c(23)],
                                     word_freq_money=-0.082*data_train[c(24)],
                                     word_freq_hp=0.134*data_train[c(25)],
                                     word_freq_hpl=0.233*data_train[c(26)],
                                     word_freq_george=0.009*data_train[c(27)],
                                     word_freq_650=0.534*data_train[c(28)],
                                     word_freq_lab=0.331*data_train[c(29)],
                                     word_freq_labs=0.721*data_train[c(30)],
                                     word_freq_telnet=0.679*data_train[c(31)],
                                     word_freq_857=1.164*data_train[c(32)],
                                     word_freq_data=0.017*data_train[c(33)],
                                     word_freq_415=1.156*data_train[c(34)],
                                     word_freq_85=0.742*data_train[c(35)],
                                     word_freq_technology=0.902*data_train[c(36)],
                                     word_freq_1999=0.124*data_train[c(37)],
                                     word_freq_parts=-0.018*data_train[c(38)],
                                     word_freq_pm=0.072*data_train[c(39)],
                                     word_freq_direct=0.924*data_train[c(40)],
                                     word_freq_cs=0.007*data_train[c(41)],
                                     word_freq_meeting=0.032*data_train[c(42)],
                                     word_freq_original=0.308*data_train[c(43)],
                                     word_freq_project=0.006*data_train[c(44)],
                                     word_freq_re=0.013*data_train[c(45)],
                                     word_freq_edu=-0.009*data_train[c(46)],
                                     word_freq_table=-0.015*data_train[c(47)],
                                     word_freq_conference=0.018*data_train[c(48)],
                                     char_freq_.=-0.002*data_train[c(49)],
                                     char_freq_..1=0.455*data_train[c(50)],
                                     char_freq_..2=0.171*data_train[c(51)],
                                     char_freq_..3=-0.042*data_train[c(52)],
                                     char_freq_..4=-0.202*data_train[c(53)],
                                     char_freq_=0.0001*data_train[c(54)],
                                     capital_run_length_average=-0.0003*data_train[c(55)],
                                     capital_run_length_longest=-0.0009*data_train[c(56)],
                                     capital_run_length_total=-0.00005*data_train[c(57)],
                                 label=data_train[c(58)])

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
  
rf_acp <- randomForest(x = data_train_x, y = data_train_y, ntree=200, mtry=6)#n_samples=10)
  
# Évolution de l'erreur en fonction du nombre d'arbres
# Ici ntree est fixé à la valeur par défaut = 500
plot(rf_acp$err.rate[,1], type="l")
  
# Affichage des résultats
print(rf_acp)
  
# Importance des variables
rf_acp$importance
varImpPlot(rf_acp)
  
# Prediction sur les données test
rf_predict_data_validate <- predict(rf_acp, newdata=data_validate_x)
  
confusionMatrix(rf_predict_data_validate,data_validate_y)
  
  
# Comparaison des valeurs prédites et des valeurs observées
table(rf_predict_data_validate, dataValidation$label)
# Calcul du taux d'erreur
error_rate <- mean(rf_predict_data_validate != dataValidation$label)
cat("error_rate using validation data = ",error_rate)
cat("=> accuracy = ", 1-error_rate)


rf_predict_data_test_acp <- predict(rf_acp, newdata=data_test)
  

# Adresse du dossier où vous travaillez
setwd("/Users/adrie/Documents/Scolaire/Enseirb/AnalyseDeDonnees/TP3/Code")
# Packages utilisés dans la suite
library(e1071)
# Supprimer toutes les variables
rm(list=ls(all=TRUE))
# Supprimer tous les graphiques déjà présents
graphics.off()


# Lecture des données d'apprentissage
load("/Users/adrie/Documents/Scolaire/Enseirb/AnalyseDeDonnees/Data/Data/Projets/spam_data_train.rda");
data_train_y <- data_train$label
data_train$label <- as.factor(data_train$label)


#Il faut separer la data_train en data entrainement et data validation
#pour pouvoir faire la suite avec table

#dataTraining <- data_train[1:2329,]
#dataValidation <- data_train[2330:2588,]




fractionTraining   <- 0.8
fractionValidation <- 0.2

sampleSizeTraining   <- floor(fractionTraining   * nrow(data_train))
sampleSizeValidation <- floor(fractionValidation * nrow(data_train))

indicesTraining    <- sort(sample(seq_len(nrow(data_train)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(data_train)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))

dataTraining   <- data_train[indicesTraining, ]
dataValidation <- data_train[indicesValidation, ]
data_train_x <- dataTraining[c(0:57)]
data_validate_x <- dataValidation[c(0:57)]

data_train_y <- as.factor(dataTraining$label)
data_validate_y <- as.factor(dataValidation$label)



# Bayesien Naif
naiv_bayes_train <- naiveBayes(label~., data = dataTraining)


# Prédiction sur les données de train
naiv_bayes_train_predict <- predict(naiv_bayes_train, newdata=data_train_x, type="class")

# Comparaison des valeurs prédites et des valeurs observées
table(naiv_bayes_train_predict, data_train_y)
# Calcul du taux d'erreur
naiv_bayes_error_rate <- mean(naiv_bayes_train_predict != data_train_y)
cat("error rate using train data (Naive bayes) = ", naiv_bayes_error_rate)


# Lecture des données test
load("/Users/adrie/Documents/Scolaire/Enseirb/AnalyseDeDonnees/Data/Data/Projets/spam_data_test.rda");

# Base de validation
naiv_bayes_validation_predict <- predict(naiv_bayes_train, newdata=data_validate_x, type="class")

# Comparaison des valeurs prédites et des valeurs observées
table(naiv_bayes_validation_predict, data_validate_y)

# Calcul du taux d'erreur
naiv_bayes_error_rate <- mean(naiv_bayes_validation_predict != data_validate_y)
cat("error rate using validation data (Naive bayes) = ", naiv_bayes_error_rate)




# Bayesien Naif sur les donnees tests
naiv_bayes_test_predict <- predict(naiv_bayes_train, newdata=data_test, type="class")


# Adresse du dossier o� vous travaillez
setwd("/Users/adrie/Documents/Scolaire/Enseirb/AnalyseDeDonnees/TP3/Code")
# Packages utilis�s dans la suite
library(e1071)
# Supprimer toutes les variables
rm(list=ls(all=TRUE))
# Supprimer tous les graphiques d�j� pr�sents
graphics.off()


# Lecture des donn�es d'apprentissage
load("/Users/adrie/Documents/Scolaire/Enseirb/AnalyseDeDonnees/Data/Data/Projets/spam_data_train.rda");
data_train_y <- data_train$label
data_train$label <- as.factor(data_train$label)


#Il faut separer la data_train en data entrainement et data validation
#pour pouvoir faire la suite avec table

dataTraining <- data_train[1:2329,]
dataValidation <- data_train[2330:2588,]




fractionTraining   <- 0.8
fractionValidation <- 0.2

#sampleSizeTraining   <- floor(fractionTraining   * nrow(data_train))
#sampleSizeValidation <- floor(fractionValidation * nrow(data_train))

#indicesTraining    <- sort(sample(seq_len(nrow(data_train)), size=sampleSizeTraining))
#indicesNotTraining <- setdiff(seq_len(nrow(data_train)), indicesTraining)
#indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))

#dataTraining   <- data_train[indicesTraining, ]
#dataValidation <- data_train[indicesValidation, ]
#data_train_x <- dataTraining[c(0:57)]
#data_validate_x <- dataValidation[c(0:57)]

#data_train_y <- as.factor(dataTraining$label)
#data_validate_y <- as.factor(dataValidation$label)



# Bayesien Naif
naiv_bayes <- naiveBayes(label~., data = dataTraining)


# Pr�diction sur les donn�es de validation
naiv_bayes_predict <- predict(naiv_bayes, newdata=dataValidation, type="class")

# Comparaison des valeurs pr�dites et des valeurs observ�es
table(naiv_bayes_predict, dataValidation$label)
# Calcul du taux d'erreur
naiv_bayes_error_rate <- mean(naiv_bayes_predict != dataValidation$label)
cat("error rate using test data (Naive bayes) = ", naiv_bayes_error_rate)


# Lecture des donn�es test
load("/Users/adrie/Documents/Scolaire/Enseirb/AnalyseDeDonnees/Data/Data/Projets/spam_data_test.rda");

# Bayesien Naif sur les donnees tests
naiv_bayes_test <- predict(naiv_bayes, newdata=data_test, type="class")


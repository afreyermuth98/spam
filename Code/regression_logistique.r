# Adresse du dossier où vous travaillez
setwd("/Users/adrie/Documents/Scolaire/Enseirb/AnalyseDeDonnees/TP3/Code")
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

#data_entrainement <- data_train[1:2000,]
#data_validation <- data_train[2000:2588,]

fractionTraining   <- 0.80
fractionValidation <- 0.20

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

# Régression logistique
glm_train <- glm(label~., data = dataTraining, family=binomial())


# Lecture des données test
load("/Users/adrie/Documents/Scolaire/Enseirb/AnalyseDeDonnees/Data/Data/Projets/spam_data_test.rda");


# Prédiction sur les données de validation
glm_train_predict <- predict(glm_train, newdata=dataValidation, type="response")
result_glm_train_predict <- (glm_train_predict > 0.5)+1


# Comparaison des valeurs prédites et des valeurs observées
table(result_glm_train_predict, dataValidation$label)
# Calcul du taux d'erreur
glm_error_rate <- mean(result_glm_train_predict != dataValidation$label)
cat("error rate using validation data (Logistic regression) = ", glm_error_rate)


#Base de test
# Régression logistique
glm_test_predict <- predict(glm_train, newdata=data_test, type="response")
result_glm_test_predict <- (glm_test_predict > 0.5)+1



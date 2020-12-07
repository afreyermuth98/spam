# Adresse du dossier où vous travaillez
setwd("/Users/adrie/Documents/Scolaire/Enseirb/AnalyseDeDonnees/TP3/Code")
# Packages utilisés dans la suite
library(MASS)
# Supprimer toutes les variables
rm(list=ls(all=TRUE))
# Supprimer tous les graphiques déjà présents
graphics.off()


# Lecture des données d'apprentissage
load("/Users/adrie/Documents/Scolaire/Enseirb/AnalyseDeDonnees/Data/Data/Projets/spam_data_train.rda");
#data_train_y <- data_train$label
#data_train$label <- as.factor(data_train$label)

#Il faut separer la data_train en data entrainement et data validation
#pour pouvoir faire la suite avec table

#dataTraining <- data_train[1:2329,] #1810 / 1941 / 2070
#dataValidation <- data_train[2330:2588,] #2000

fractionTraining   <- 0.90
fractionValidation <- 0.10

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





# Analyse discriminante LINEAIRE
lin_disc_an_train <- lda(label~., data = dataTraining)

# Prédiction sur les données de train
LDA_train_predict <- predict(lin_disc_an_train, newdata=data_train_x, type="class")

# Comparaison des valeurs prédites et des valeurs observées
table(LDA_train_predict$class, data_train_y)
# Calcul du taux d'erreur
lda_error_rate <- mean(LDA_train_predict$class != data_train_y)
cat("error rate using train data (LDA) = ", lda_error_rate)


# Prédiction sur les données de validation
LDA_validation_predict <- predict(lin_disc_an_train, newdata=data_validate_x, type="class")
# Comparaison des valeurs prédites et des valeurs observées
table(LDA_validation_predict$class, data_validate_y)
# Calcul du taux d'erreur
lda_error_rate <- mean(LDA_validation_predict$class != data_validate_y)
cat("error rate using train data (LDA) = ", lda_error_rate)



# Lecture des données test
load("/Users/adrie/Documents/Scolaire/Enseirb/AnalyseDeDonnees/Data/Data/Projets/spam_data_test.rda");

# Prédiction sur les données tests
LDA_test_predict <- predict(lin_disc_an_train, newdata=data_test, type="class")



# Analyse discriminante QUADRATIQUE
quad_disc_an_train <- qda(label~., data = dataTraining)

# Prédiction sur les données de train
QDA_train_predict <- predict(quad_disc_an_train, newdata=data_train_x, type="class")

# Comparaison des valeurs prédites et des valeurs observées
table(QDA_train_predict$class, data_train_y)
# Calcul du taux d'erreur
Qda_error_rate <- mean(QDA_train_predict$class != data_train_y)
cat("error rate using train data QLDA) = ", Qda_error_rate)


# Prédiction sur les données de validation
QDA_validation_predict <- predict(quad_disc_an_train, newdata=data_validate_x, type="class")
# Comparaison des valeurs prédites et des valeurs observées
table(QDA_validation_predict$class, data_validate_y)
# Calcul du taux d'erreur
qda_error_rate <- mean(QDA_validation_predict$class != data_validate_y)
cat("error rate using train data (QDA) = ", qda_error_rate)

# Prédiction sur les données tests
QDA_test_predict <- predict(quad_disc_an_train, newdata=data_test, type="class")


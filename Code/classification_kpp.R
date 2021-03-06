setwd("/Users/luffy/Documents/Cours/ENSEIRB/3A/analyse_de_donnees/projet/spam/Code")


# Supprimer toutes les variables
rm(list=ls(all=TRUE))

# Supprimer tous les graphiques d�j� pr�sents
graphics.off()


# Packages utilis�s dans la suite
library(class)
library(caret)
library(randomForest)
library(ROCR)

# Lecture des donn�es d'apprentissage

load(file = "../Data/spam_data_train.rda")
load(file = "../Data/spam_data_test.rda")

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


# nombre de voisins (par ex proche de la racine carr� du nombre d'obs)
num_of_neigh <- 50
data_train_predict <- knn(train=data_train_x,test=data_train_x, 
                          cl=data_train_y, k=num_of_neigh)

data_validate_predict <- knn(train=data_train_x,test=data_validate_x,
                         cl=data_train_y,k=num_of_neigh)

# Calcul du taux d'erreur
error_rate_train <- mean(data_train_predict != data_train_y)
cat("error_rate train using train data = ",error_rate_train)


# Calcul du taux d'erreur
error_rate_vali <- mean(data_validate_predict != data_validate_y)
cat("error_rate train using train data = ",error_rate_vali)


data_train_x <- data.frame(data_train[,1:57])
# Cr�ation de la sortie (� mettre sous le format facteur sinon
# un mod�le de r�gression est cr��)
data_train_y <- as.factor(data_train[,58])
# For�ts al�atoires
rf <- randomForest(x = data_train_x, y = data_train_y, ntree=500)
# �volution de l'erreur en fonction du nombre d'arbres
# Ici ntree est fix� � la valeur par d�faut = 500
plot(rf$err.rate[,1], type="l")
# Affichage des r�sultats
print(rf)
#Pr�diction
data_test_x <- data.frame(data_test[,1:57])
data_test_y <- data.frame(data_test[,58])
table(rf_predit_data_test, data_test_y)
rf_predit_data_test <- predict(rf, newdata=data_test_x)
# Calcul du taux d'erreur
error_rate <- mean(rf != data_test_y)
cat("error_rate using test data = ",error_rate)

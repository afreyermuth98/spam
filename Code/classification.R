setwd("/Users/luffy/Documents/Cours/ENSEIRB/3A/analyse_de_donnees/projet/spam/Code")


# Supprimer toutes les variables
rm(list=ls(all=TRUE))

# Supprimer tous les graphiques déjà présents
graphics.off()


# Packages utilisés dans la suite
library(class)
library(caret)
library(ROCR)

# Lecture des données d'apprentissage

load(file = "../Data/spam_data_train.rda")
#load(file = "../Data/spam_data_test.rda")

# Séparation des données et de la sortie
# Trouver comment bien séparer les variables
data_train_x <- data.frame(x=data_train$x4,y=data_train$x58)

# nombre de voisins (par ex proche de la racine carré du nombre d'obs)
num_of_neigh <- 10
data_train_predict <- knn(train=data_train_x,test=data_train_x,
                          cl=data_train$x58,k=num_of_neigh)
# Affichage des résultats (étoile)
par(new=T)
plot(data_train$x1,data_train$x58,col=data_train_predict,pch=8)
# Calcul du taux d'erreur
error_rate <- mean(data_train_predict != data_train$y)
cat("error_rate using train data = ",error_rate)

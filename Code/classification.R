setwd("/Users/luffy/Documents/Cours/ENSEIRB/3A/analyse_de_donnees/projet/spam/Code")


# Supprimer toutes les variables
rm(list=ls(all=TRUE))

# Supprimer tous les graphiques déjà présents
graphics.off()


# Packages utilisés dans la suite
library(class)
library(caret)
library(randomForest)
library(ROCR)

# Lecture des données d'apprentissage

load(file = "../Data/spam_data_train.rda")
#load(file = "../Data/spam_data_test.rda")

# Séparation des données et de la sortie
# Trouver comment bien séparer les variables
data_train_x <- data.frame(x1=data_train$word_freq_money,
                           x2=data_train$word_freq_credit)

# nombre de voisins (par ex proche de la racine carré du nombre d'obs)
num_of_neigh <- 2
data_train_predict <- knn(train=data_train[,1:57],test=data_train[,1:57], 
                          cl=data_train[,58], k=num_of_neigh)



# Affichage des résultats (étoile)
#par(new=T)
#plot(data_train$x1,data_train$label,col=data_train_predict,pch=8)
# Calcul du taux d'erreur
error_rate <- mean(data_train_predict != data_train[,58])
cat("error_rate using train data = ",error_rate)


# Forêts aléatoires
rf <- randomForest(x = data_train[,1:57], y = data_train[,58], ntree=500)
# Évolution de l'erreur en fonction du nombre d'arbres
# Ici ntree est fixé à la valeur par défaut = 500
plot(rf$err.rate[,1], type="l")
# Affichage des résultats
print(rf)
# Calcul du taux d'erreur
error_rate <- mean(rf_predit_data_test != data_test$y)
cat("error_rate using test data = ",error_rate)

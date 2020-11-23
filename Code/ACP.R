# Adresse du dossier où vous travaillez
#Antho
#setwd("F:/Enseirb/AnalyseDeDonnees/spam/Code")
# Clément
setwd("/Users/luffy/Documents/Cours/ENSEIRB/3ème année/analyse_de_donnees/projet/spam/Code")

# Packages utilisés dans la suite
library(class)
library(caret)
library(ROCR)

# Supprimer toutes les variables
rm(list=ls(all=TRUE))

# Supprimer tous les graphiques déjà présents
graphics.off()

# Lecture des données d'apprentissage
data_train <- read.table("../Data/spam_data_train.rda",header=T,sep="\t");
print(data_train)

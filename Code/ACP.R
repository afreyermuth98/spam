# Adresse du dossier o� vous travaillez
#Antho
#setwd("F:/Enseirb/AnalyseDeDonnees/spam/Code")
# Cl�ment
setwd("/Users/luffy/Documents/Cours/ENSEIRB/3�me ann�e/analyse_de_donnees/projet/spam/Code")

# Packages utilis�s dans la suite
library(class)
library(caret)
library(ROCR)

# Supprimer toutes les variables
rm(list=ls(all=TRUE))

# Supprimer tous les graphiques d�j� pr�sents
graphics.off()

# Lecture des donn�es d'apprentissage
data_train <- read.table("../Data/spam_data_train.rda",header=T,sep="\t");
print(data_train)
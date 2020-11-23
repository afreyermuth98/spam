
#Change your pwd 
setwd("F:/Enseirb/AnalyseDeDonnees/spam/Code")

# Packages utilisés dans la suite
library(class)
library(caret)
library(ROCR)
library("FactoMineR")



# Supprimer toutes les variables
rm(list=ls(all=TRUE))

# Supprimer tous les graphiques déjà présents
graphics.off()

# Lecture des données d'apprentissage

load(file = "../Data/spam_data_train.rda")
load(file = "../Data/spam_data_test.rda")

mean <- apply(data_train, 2, mean)
std <- apply(data_train, 2,sd)*sqrt(5/6)
stat <- rbind(mean, std)

print(stat, digits=4)
# INNER PRODUCT POUR RECUPERER L'ACP
# ACP sur données d'origine
res <- PCA(data_train[-58], graph=FALSE, scale.unit=FALSE)
plot(res, choix="ind", ces=1.5, title="")
plot(res,choix="var", ces=1.5, title="")

# ACP sur données centrées réduites
resnorm <- PCA(data_train[-58],graph=FALSE)
plot(resnorm,choix="ind",cex=1.5,title="")
plot(resnorm,choix="var",cex=1.5,title="")

res$eig
barplot(res$eig[,1], main = "ACP sur tous les mails")

resnorm$eig
barplot(resnorm$eig[,1], main = "ACP centrées réduites sur tous les mails")

# Projection des variables
resnorm$ind$cos2
# Somme avec les 2 premières
resnorm$var$cos2[,1]+resnorm$var$cos2[,2]
# Et les 3 ?
resnorm$var$cos2[,1]+resnorm$var$cos2[,2]+resnorm$var$cos2[,3]
# Contribution des variables
resnorm$var$contrib

resnorm$var$coord

# Séparation du set de données avec les spamms et pas spamms

#spam <- data_train[data_train$label == 1,]
#ham <- data_train[data_train$label == 0,]

#res_spam <- PCA(spam[-58], graph=FALSE)
#res_ham <- PCA(ham[-58], graph=FALSE)

#res_spam$eig
#barplot(res_spam$eig[,1], main="ACP sur les spamms")
#res_spam$var$contrib

#res_ham$eig
#barplot(res_ham$eig[,1],main="ACP sur les ham")
#res_ham$var$contrib


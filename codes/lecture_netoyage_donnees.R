
rm(list=ls())

# Header ------------------------------------------------------------------

# Work folder
# setwd("C:\\Users\\Sebastien\\Documents\\My WOrk\\2016 02 Text mining députés")
setwd("/home/giuliano/NAS/projets/textmining_deputes")

# Packages ----------------------------------------------------------------

library(jsonlite) # Import Json file
library(plyr) # data manip
library(ggplot2) # Graphics
library(wordcloud) # Nuage de mots
library(RColorBrewer) # COuleurs
library(tm) # Text mining
library(SnowballC)
library(devtools)
library(FactoMineR)
library(tau)
library(dplyr)

# Code --------------------------------------------------------------------

# Importer les données
# json_file <- "http://data.assemblee-nationale.fr/static/openData/repository/QUESTIONS/questions_orales_sans_debat/Questions_orales_sans_debat_XIV.json.zip"
json_file <- "Questions_orales_sans_debat_XIV.json"

json_data <- fromJSON(json_file, flatten=TRUE)
json_data.df <- as.data.frame(json_data)



# Nettoyage des données ---------------------------------------------------

# toutes les colonnes de la table "json_data.df ont un nom qui commence par "questionsOralesSansDebat.question."
# on supprime donc ce préambule pour plus de lisibilité
names(json_data.df) <- gsub("questionsOralesSansDebat.question.","",names(json_data.df))

# on retire les colonnes qui contiennent le texte des questions et le texte des réponses, et qui alourdissent le jeu de données
metadonnees <- select(json_data.df,-minAttribs.minAttrib,-textesQuestion.texteQuestion,-textesReponse.texteReponse.texte) 

# ids <- json_data.df$questionsOralesSansDebat.question.uid


#je sélectionne uniquement le texte des questions.
# Celui-ci est contenu dans la colonne textesQuestion.texteQuestion
text.question <- json_data.df$textesQuestion.texteQuestion
# text.question est une liste de listes. Chaque élément de la liste est une liste qui a trois composantes : 
#   - le type de question
#   - la date de la question
#   - le texte de la question
# certains éléments de cette liste ont plus d'éléments : 
# il s'agit des questions qui ont fait l'objet d'une erreur lors de leur publication au journal officiel.
# ces erreurs ont esuite été rectifiées. Pour ces questions, l'ensemble des versions est fourni.
# il s'agit dans un premier temps de repérer les indices de ces questions qui ont fait l'objet d'une rectification

longueur <- c(NA)
for (i in 1:length(text.question)){
  longueur[i] <- length(text.question[[i]])
}
# indices des questions rectifiées
questions_rectifiees <- c(which(longueur>2))

# indices des questions NON rectifiées
indices <- seq(1,length(text.question))
indices <- indices[-c(which(longueur>3))]
# nombre de questions sans rectifications
length(indices)

questions <- as.data.frame(matrix(unlist(text.question[indices]),ncol=3,byrow=TRUE))
names(questions) <- c("typeJO", "dateJO", "texte")

# sélection des lignes correspondant aux questions non rectifiées
metadonnees <- metadonnees[indices,]


# je rassemble les deux tables (les metadonnées et le texte des questions dans une seule table)
donnees_propres <- cbind(metadonnees,questions)

# je mets les dates en format date : 
donnees_propres$dateJO <- as.POSIXct(donnees_propres$dateJO,format= "%Y-%m-%d")



save(donnees_propres,file="donnees_propres.Rda")
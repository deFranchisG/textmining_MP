rm(list=ls())
gc()

# Header ------------------------------------------------------------------

# Ce programme importe les données de questions ouvertes en format JSON
# Pour les convertir en dataframe

# Work folder : ça devrait etre là où le projet est importé?

# Packages ----------------------------------------------------------------

library(jsonlite) # Import Json file
library(XML) # Import XML file


# Initiation de la randomization
set.seed(142)  

# Importation -------------------------------------------------------------

# Importer les données en format JSON
json.file <- "src\\Questions_orales_sans_debat_XIV.json"
json.data <- fromJSON(json.file, flatten=TRUE)
json.data.df <- as.data.frame(json.data)
head(json.data.df)

# Importer les données en format XML
# xml.file <- "src\\Questions_orales_sans_debat_XIV_xml.xml"
# xml.data.df <- xmlToDataFrame(xml.file)
# head(xml.data.df)

# Cleaning ----------------------------------------------------------------

###
# Simplification des noms de variables
###
noms.var <- names(json.data.df)
noms.var <- gsub("questionsOralesSansDebat.question.", "", noms.var,
                 fixed = T)
names(json.data.df) <- noms.var

###
# Traitement des textes des questions: 
###
# Traitement des exceptions, où il y a plusieurs textes 
# par question
text.question <- json.data.df$textesQuestion.texteQuestion

# Normalement, on a une liste de 2 éléments: "infos JO" et "texte". 
# Dans infoJO on a 2 choses: typeJO et dateJO.
# dans les exceptions on a 4 éléments: texte, infoJO.typeJO, infoJO.dateJO
# et erratum

# Je suppose que il faut considérer la dernière version de la question:
# On garde donc juste la partie corrigée, qui est juste la dernière version
# du texte

# Pour chaque question, on peut donc créer 3 nouvelles variables
# à a place de la variable "texte question" : un texte, et les infos JO

# quelles sont les exceptions
exceptions <- which(unlist(lapply(text.question, length))>2)

# Créations des nouvelles variables
new.var.questions <- c()

for (i in 1:length(text.question)){
  item <- text.question[[i]]  
  if(i %in% exceptions){
    num.erratum <- length(item$erratum)
    q.typeJO <- item$infoJO.typeJO[num.erratum]
    q.dateJO <- item$infoJO.dateJO[num.erratum]
    texte <- item$texte[num.erratum]
    new.question <- c(q.typeJO, q.dateJO, texte)
  }
  else{
    q.typeJO <- item$infoJO$typeJO
    q.dateJO <- item$infoJO$dateJO
    texte <- item$texte
    new.question <- c(q.typeJO, q.dateJO, texte)
  }
  new.var.questions <- rbind(new.var.questions, new.question)
}

new.var.questions <- as.data.frame(new.var.questions, stringsAsFactors = F)

# On utilise les mêmes noms qu'il y a dans la table pour les réponses
names(new.var.questions) <- c("textesQuestion.texteQuestion.infoJO.typeJO", 
                              "textesQuestion.texteQuestion.infoJO.dateJO", 
                              "textesQuestion.texteQuestion.texte")

# On remplace dans la table initiale:
json.data.df$textesQuestion.texteQuestion <- NULL
json.data.df[, names(new.var.questions)] <- new.var.questions

save(json.data.df, file = "tbl\\données propres importées des questions ouvertes.Rdata")


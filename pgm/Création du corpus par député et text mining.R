rm(list=ls())
gc()

# Header ------------------------------------------------------------------

# Ce programme aggrège les textes par intervenant

# Packages ----------------------------------------------------------------

# libraries
library(tm) # Text mining
library(wordcloud) # Wordcloud
library(rpart) # classification tree
library("rattle")  # Fancy tree plot
library("rpart.plot")
library(ggplot2)
library(FactoMineR)


# Initiation de la randomization
set.seed(142)  

# La table de travail
load("tbl\\données propres importées des questions ouvertes.Rdata", verbose = T)

# La table des ids de députés
id.dep <- read.csv("src\\id_deputes.csv", header = T)

# Création du corpus ------------------------------------------------------


#Aggréger les textes par le même député
names(json.data.df)
json.data.df.depute <- subset(json.data.df, 
                              select = c("auteur.identite.acteurRef", 
                                         "textesQuestion.texteQuestion.texte"))

json.data.df.depute <- aggregate(
  textesQuestion.texteQuestion.texte ~ auteur.identite.acteurRef, 
  data=json.data.df.depute, 
  FUN=paste, collapse=' '
)
# Fusionner avec les noms
names(json.data.df.depute) <- c("id_an", "questions") 

id.dep$id_an <- paste0("PA",id.dep$id_an)

json.data.df.depute <- merge(json.data.df.depute, id.dep, by = "id_an")

# Le texte des questions
textdata <- json.data.df.depute$questions
Encoding(textdata) <- "UTF-8"

# Transformation en corpus
mycorpus <- Corpus(VectorSource(textdata))


# Cleaning du texte, depuis 
# https://github.com/cuche27/campagnePresidentielle/blob/master/campagnePresidentielle.R

# Mettre en minuscules
mycorpus <- tm_map(mycorpus, content_transformer(tolower))

# Remplacer les ponctuations par des espaces. tm fournit la fonction
# removePunctuation pour ça, mais elle gère mal les apostrophes.
mycorpus <- tm_map(mycorpus, 
                   content_transformer(function(x) gsub("\\W", " ", x)))

# Supprimer les mots courants
mycorpus <- tm_map(mycorpus, removeWords, words = stopwords("french"))

# Supprimer les terminaisons des mots (ex : e, es, ent...)
mycorpus <- tm_map(mycorpus, stemDocument, language = "french")

# Supprimer les nombres
mycorpus <- tm_map(mycorpus, removeNumbers)

# Supprimer les espaces superflus
mycorpus <- tm_map(mycorpus, stripWhitespace)

# Text mining -------------------------------------------------------------

# Même chose avec scores tf idf
dtm.tfidf <- DocumentTermMatrix(mycorpus, control=list(weighting=weightTfIdf))

# SUr les conseils de François Guillem

# Supprimer les mots trop rares. Si on en garde trop, on obtient des résultats
# anecdotiques, mais si on n'en conserve pas assez, on a des résultats très 
# faibles.
dtm.tfidf <- removeSparseTerms(dtm.tfidf, 0.5)

dtm.df <- as.data.frame(inspect(dtm.tfidf))
rownames(dtm.df) <- paste(json.data.df.depute$nom_de_famille,
                          substr(json.data.df.depute$prenom,1,1))

# res.pca <- PCA(dtm.df)
# dimdesc(res.pca)


dtm.df <- scale(dtm.df)
# Determine number of clusters
wss <- (nrow(dtm.df)-1)*sum(apply(dtm.df,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dtm.df, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# Ward Hierarchical Clustering
d <- dist(dtm.df, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")
head(groups)

head(id.dep)
id.dep.all <- read.csv2("src\\nosdeputes.fr_deputes2016-04-05.csv")
groupe.deputes <- id.dep.all
groupe.deputes$nom.init <- paste(id.dep$nom_de_famille,
                                 substr(id.dep$prenom,1,1))
groups <- data.frame(groups)
groups$nom.init <- row.names(groups)

groupe.deputes <- merge(groupe.deputes, groups, by = "nom.init")
groupe.deputes.clean <- subset(groupe.deputes, select = c("nom", "groupe_sigle",
                                                    "parti_ratt_financier",
                                                    "nom_circo", "id_an",
                                                    "groups"))
write.csv(groupe.deputes.clean, "tbl\\groupes de députés via CAH.csv")

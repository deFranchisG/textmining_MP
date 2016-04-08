rm(list=ls())
gc()

# Header ------------------------------------------------------------------

# Ce programme fait le textmining

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



# Création du corpus ------------------------------------------------------

# Le texte des questions
textdata <- json.data.df$textesQuestion.texteQuestion.texte
Encoding(textdata) <- "UTF-8"

# Transformation en corpus
mycorpus <- Corpus(VectorSource(textdata))

# On regarde pour voir ce qu'il y a au final dans le corpus
writeCorpus(mycorpus, path = "corpus")

# Les méta-données à rajouter
meta(mycorpus, tag = "uid") <- json.data.df[, "uid"]
meta(mycorpus, tag = "groupe") <- json.data.df[, "auteur.groupe.abrege"]

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


##
# Mots les plus utilisés par chaque parti
##

# Matrice des mots utilisés dans chaque document  
dtm <- DocumentTermMatrix(mycorpus)

groupes <- unique(meta(mycorpus, tag = "groupe"))
parti.question <- meta(mycorpus, tag = "groupe")[, 1]

# Mots les plus utilisés par les républicains/UMP
ump <- c("LES-REP", "UMP")
row.ump <- which(parti.question %in% ump)
freq <- colSums(as.matrix(dtm)[row.ump, ])   

png("gph\\Mots les plus utilisés par les partis de droite.png", 
    width = 800, height = 800)
wordcloud(names(freq), freq, min.freq=50)   
dev.off()

# Mots les plus utilisés par les socialistes
ps <- c("SRC")
row.ps <- which(parti.question %in% ps)
freq <- colSums(as.matrix(dtm)[row.ps, ])  

png("gph\\Mots les plus utilisés par les partis de gauche.png", 
    width = 800, height = 800)
wordcloud(names(freq), freq, min.freq=50)   
dev.off()

# Même chose avec scores tf idf
dtm.tfidf <- DocumentTermMatrix(mycorpus, control=list(weighting=weightTfIdf))

# SUr les conseils de François Guillem

# Supprimer les mots trop rares. Si on en garde trop, on obtient des résultats
# anecdotiques, mais si on n'en conserve pas assez, on a des résultats très 
# faibles.
dtm.tfidf <- removeSparseTerms(dtm.tfidf, 0.8)
                                
groupes <- unique(meta(mycorpus, tag = "groupe"))
parti.question <- meta(mycorpus, tag = "groupe")[, 1]
# 100 Mots avec les plus gros scores tf-idf pour les républicains/UMP
ump <- c("LES-REP", "UMP")
row.ump <- which(parti.question %in% ump)
freq <- colSums(as.matrix(dtm.tfidf)[row.ump, ])   
freq.df <- data.frame(freq)
freq.df$terms <- row.names(freq.df)
freq.df <- freq.df[order(-freq.df$freq), ]
freq.df <- freq.df[1:25, ]

p <- ggplot(freq.df, aes(reorder(terms, freq), freq)) + geom_bar(stat = "identity")
p <- p + theme_bw(base_size = 15) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + xlab("Mots avec les scores tf-idf les plus élevés") + ylab("score total")
png("gph\\Mots avec les scores tfidf les plus élevés - droite.png", 
    width = 800, height = 800)
print(p)
dev.off()

# Mots les plus utilisés par les socialistes
ps <- c("SRC")
row.ps <- which(parti.question %in% ps)
freq <- colSums(as.matrix(dtm.tfidf)[row.ps, ])   
freq.df <- data.frame(freq)
freq.df$terms <- row.names(freq.df)
freq.df <- freq.df[order(-freq.df$freq), ]
freq.df <- freq.df[1:25, ]

p <- ggplot(freq.df, aes(reorder(terms, freq), freq)) + geom_bar(stat = "identity")
p <- p + theme_bw(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + xlab("Mots avec les scores tf-idf les plus élevés") + ylab("score total")

png("gph\\Mots avec les scores tfidf les plus élevés - gauche.png", 
    width = 800, height = 800)
print(p)
dev.off()

##
# Classification par score tf-idf 
##
dtm.tfidf <- DocumentTermMatrix(mycorpus, control=list(weighting=weightTfIdf))
parti.question <- meta(mycorpus, tag = "groupe")[, 1]
dtm.df <- as.data.frame(inspect(dtm.tfidf))
rownames(dtm.df) <- 1:nrow(dtm.tfidf)
dtm.df <- cbind(parti.question, dtm.df)

# Gardons les 2 partis principaux
dtm.df$parti <- ""
dtm.df$parti[which(dtm.df$parti.question %in% ump)] <- "Républicains"
dtm.df$parti[which(dtm.df$parti.question %in% ps)] <- "Socialistes"
dtm.df.clean <- subset(dtm.df, parti != "")
dtm.df.clean$parti.question <- NULL
# dtm.df.clean <- dtm.df.clean[, which(names(dtm.df.clean) %in% keep.words)]


# Classification
dt <- rpart(parti ~ ., data = dtm.df.clean)
png("gph\\classification des mots séparants droite et gauche.png", width = 1000, height = 1000)
fancyRpartPlot(dt)
dev.off()

# Analyse en composantes principales

dtm.tfidf <- DocumentTermMatrix(mycorpus, control=list(weighting=weightTfIdf))
# Supprimer les mots trop rares. Si on en garde trop, on obtient des résultats
# anecdotiques, mais si on n'en conserve pas assez, on a des résultats très 
# faibles.
dtm.tfidf <- removeSparseTerms(dtm.tfidf, 0.8)

parti.question <- meta(mycorpus, tag = "groupe")[, 1]
dtm.df <- as.data.frame(inspect(dtm.tfidf))
rownames(dtm.df) <- 1:nrow(dtm.tfidf)
dtm.df <- cbind(parti.question, dtm.df)

res.pca <- PCA(dtm.df, quali.sup = 1)
dimdesc(res.pca, axes=1:2)

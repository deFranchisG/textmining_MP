rm(list=ls())
gc()

# Header ------------------------------------------------------------------

# Ce programme réplique la méthodologie de Jensen et al. 2012

# Packages ----------------------------------------------------------------

# libraries
library(tm) # Text mining
library(RWeka) # Trigrames

library(xlsx) # export Excel

library(ggplot2) # Graphique
library(wesanderson) # Palette graphique
pal <- wes_palette("GrandBudapest", 2)



# Initiation de la randomization
set.seed(142)  

# La table de travail
load("tbl\\données propres importées des questions ouvertes.Rdata", verbose = T)

# La table des ids de députés
id.dep <- read.csv("src\\id_deputes.csv", header = T)

# La table des députés détaillées
id.dep.all <- read.csv2("src\\nosdeputes.fr_deputes2016-04-05.csv")

# Création du corpus ------------------------------------------------------

# Le texte des questions
textdata <- json.data.df$textesQuestion.texteQuestion.texte
Encoding(textdata) <- "UTF-8"

# Transformation en corpus
mycorpus <- Corpus(VectorSource(textdata))

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

# Calcul des chi2 ---------------------------------------------------------

# Les groupes
ump <- c("LES-REP", "UMP")
ps <- c("SRC")

# Matrice des mots utilisés dans chaque document  
dtm <- DocumentTermMatrix(mycorpus)

# Calcul du chi-2 pour chaque mot
groupes <- unique(meta(mycorpus, tag = "groupe"))
parti.question <- meta(mycorpus, tag = "groupe")[, 1]

# Fréquence et ani-fréquence pour chaque mot, UMP
row.ump <- which(parti.question %in% ump)
freq.ump <- colSums(as.matrix(dtm)[row.ump, ])
antifreq.ump <- sum(freq.ump) - freq.ump

# même chose pour le ps
row.ps <- which(parti.question %in% ps)
freq.ps <- colSums(as.matrix(dtm)[row.ps, ])
antifreq.ps <- sum(freq.ps) - freq.ps

# Calcul du chi2
data.chi2 <- cbind(freq.ump, antifreq.ump, freq.ps, antifreq.ps)
data.chi2 <- data.frame(data.chi2)

data.chi2$chi2 <- (data.chi2$freq.ump * data.chi2$antifreq.ps - data.chi2$freq.ps * data.chi2$antifreq.ump)^2/
  ((data.chi2$freq.ump + data.chi2$freq.ps)*(data.chi2$freq.ump + data.chi2$antifreq.ump)*
     (data.chi2$freq.ps + data.chi2$antifreq.ps)*(data.chi2$antifreq.ump + data.chi2$antifreq.ps))

# Regarder quels mots ont le plus large chi2
data.chi2 <- data.chi2[order(- data.chi2$chi2), ]
data.chi2.top <- data.chi2[1:20, ]

p <- ggplot(data.chi2.top, aes(reorder(row.names(data.chi2.top), chi2), chi2))
p <- p + geom_bar(stat = "identity")
p <- p + theme_bw()
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + xlab("Mot") + ylab("Score du Chi2")
p

# Calcul des betas --------------------------------------------------------

#Aggréger les textes par le même député
json.data.df.depute <- subset(json.data.df, 
                              select = c("auteur.identite.acteurRef", "auteur.groupe.abrege", 
                                         "textesQuestion.texteQuestion.texte"))

json.data.df.depute <- aggregate(
  textesQuestion.texteQuestion.texte ~ auteur.identite.acteurRef + auteur.groupe.abrege, 
  data=json.data.df.depute, 
  FUN=paste, collapse=' '
)

# Le texte des questions
textdata <- json.data.df.depute$textesQuestion.texteQuestion.texte
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

# TAbles des fréquences de mots normalisés
dtm.deputes <- DocumentTermMatrix(mycorpus)
dtm.deputes.sc <- scale(dtm.deputes)

# Codage des partis droite gauche
partis <- json.data.df.depute$auteur.groupe.abrege
partis.bin <- rep(0, length(partis))

# Les groupes
ump <- c("LES-REP", "UMP")
ps <- c("SRC")

partis.bin[which(partis%in%ump)] <- 1
partis.bin[which(partis%in%ps)] <- -1

# Les betas
betas <- partis.bin %*% dtm.deputes.sc

# Calculer la mesure de polarisation globale
betas.df <- data.frame(t(betas))
names(betas.df) <- "beta"
data.chi2.df <- data.frame(data.chi2)

beta.chi2.df <- merge(betas.df, data.chi2.df, by = "row.names", 
                      all.x = T, all.y = T)
beta.chi2.df$freqtot <- beta.chi2.df$freq.ps + beta.chi2.df$freq.ump
scorepol <- weighted.mean(beta.chi2.df$beta, w = beta.chi2.df$freqtot)


# Trigrams ----------------------------------------------------------------

# Corpus par députés

#Aggréger les textes par le même député
json.data.df.depute <- subset(json.data.df, 
                              select = c("auteur.identite.acteurRef", "auteur.groupe.abrege", 
                                         "textesQuestion.texteQuestion.texte"))

json.data.df.depute <- aggregate(
  textesQuestion.texteQuestion.texte ~ auteur.identite.acteurRef + auteur.groupe.abrege, 
  data=json.data.df.depute, 
  FUN=paste, collapse=' '
)

# Le texte des questions
textdata <- json.data.df.depute$textesQuestion.texteQuestion.texte
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

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dtm <- DocumentTermMatrix(mycorpus, control = list(tokenize = TrigramTokenizer))

# Codage des partis droite gauche
partis <- json.data.df.depute$auteur.groupe.abrege

# Les groupes
ump <- c("LES-REP", "UMP")
ps <- c("SRC")


# Fréquence et ani-fréquence pour chaque mot, UMP
row.ump <- which(partis %in% ump)
freq.ump <- colSums(as.matrix(dtm)[row.ump, ])
antifreq.ump <- sum(freq.ump) - freq.ump

# même chose pour le ps
row.ps <- which(partis %in% ps)
freq.ps <- colSums(as.matrix(dtm)[row.ps, ])
antifreq.ps <- sum(freq.ps) - freq.ps

# Calcul du chi2
data.chi2 <- cbind(freq.ump, antifreq.ump, freq.ps, antifreq.ps)
data.chi2 <- data.frame(data.chi2)

data.chi2$chi2 <- (data.chi2$freq.ump * data.chi2$antifreq.ps - data.chi2$freq.ps * data.chi2$antifreq.ump)^2/
  ((data.chi2$freq.ump + data.chi2$freq.ps)*(data.chi2$freq.ump + data.chi2$antifreq.ump)*
     (data.chi2$freq.ps + data.chi2$antifreq.ps)*(data.chi2$antifreq.ump + data.chi2$antifreq.ps))

# Regarder quels mots ont le plus large chi2
data.chi2 <- data.chi2[order(- data.chi2$chi2), ]

# Rajouter une couleur pour savoir qui a utilisé le mot le plus souvent
data.chi2$indexparti <- "Droite"
data.chi2$indexparti[which(data.chi2$freq.ps/(data.chi2$freq.ps+data.chi2$antifreq.ps) >
                             data.chi2$freq.ump/(data.chi2$freq.ump+data.chi2$antifreq.ump))] <- "Gauche"

data.chi2.top <- data.chi2[1:50, ]

p <- ggplot(data.chi2.top, aes(reorder(row.names(data.chi2.top), chi2), chi2))
p <- p + geom_bar(aes(fill = indexparti), stat = "identity")
p <- p + theme_bw()
p <- p + scale_fill_manual(values = pal, name = "Hémicycle")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + xlab("Trigram") + ylab("Score du Chi2")
p

write.xlsx(data.chi2[1:200, ], file = "tbl\\Trigrams prononces par les partis par chi2.xlsx")


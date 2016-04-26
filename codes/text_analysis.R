rm(list=ls())
library(dplyr)

load(file="donnees_propres.Rda")
texte_id <- select(donnees_propres,texte,auteur.identite.acteurRef)
table_indiv <- aggregate(texte_id$text, list(texte_id$auteur.identite.acteurRef), paste, collapse="")

# analyse textuelle -------------------------------------------------------
textdata <- as.character(donnees_propres$texte)

Encoding(textdata) <- "UTF-8"

textdata <- gsub("[[:punct:]]", "", textdata)
textdata <- gsub("[[:digit:]]", "", textdata)
textdata <- gsub("http\\w+", "", textdata)
textdata <- gsub("[ \t]{2,}", "", textdata)
textdata <- gsub("^\\s+|\\s+$", "", textdata)
try.error = function(x)
{
  y <- NA
  try_error <- tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y <- tolower(x)
  return(y)
}
textdata <- sapply(textdata, try.error)
textdata <- textdata[!is.na(textdata)]
names(textdata) <- NULL
textdata  <-toupper(textdata)

mycorpus <- Corpus(VectorSource(textdata))
mycorpus <- tm_map(mycorpus, removePunctuation)

dtm <- DocumentTermMatrix(mycorpus)
dtm2 <- as.matrix(dtm)


frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
mots <- frequency[frequency>20]
s <- dtm2[1,which(colnames(dtm2) %in% names(mots))]
for(i in 2:nrow(dtm2)) {
  s <- cbind(s,dtm2[i,which(colnames(dtm2) %in% names(mots))])
}
colnames(s) <- donnees_propres$uid

PCA(s[1:25, ])

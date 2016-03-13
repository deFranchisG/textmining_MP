
# Header ------------------------------------------------------------------

# Work folder
setwd("C:\\Users\\Sebastien\\Documents\\My WOrk\\2016 02 Text mining députés")


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

# Code --------------------------------------------------------------------

# Importer les données
json_file <- "src\\Questions_orales_sans_debat_XIV.json\\Questions_orales_sans_debat_XIV.json"
json_data <- fromJSON(json_file, flatten=TRUE)
json_data.df <- as.data.frame(json_data)
ids <- json_data.df$questionsOralesSansDebat.question.uid


text.question <- json_data.df$questionsOralesSansDebat.question.textesQuestion.texteQuestion
head(text.question)
# observations à pb parce que plus de 3 entrées
notok <- c()
for (i in 1:length(text.question)){
  if (length(unlist(text.question[i])) != 3){
    notok <- c(notok, i)
  }
}
text.question.clean <- text.question[-notok]
ids <- ids[-notok]

d <- as.data.frame(matrix(unlist(text.question.clean), ncol = length(unlist(text.question[1])), byrow = T))
names(d) <- c("typeJO", "dateJO", "texte")
textdata <- d$texte
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
colnames(s) <- ids

PCA(s[1:25, ])

rm(list=ls())
load(file="donnees_propres.Rda")

# statistiques descriptives -----------------------------------------------

# plot du nombre de questions posées par groupe politique. 
# Le groupe "Les républicains", suivi du groupe "Socialistes, radical citoyen" ont posé le plus de questions.
p <- ggplot(donnees_propres)
p <- p + geom_bar(data = donnees_propres, aes(x=auteur.groupe.developpe))
p

# plot du nombre de questions posées par député. Il y a un député qui a posé dix questions.
p <- ggplot(donnees_propres)
p <- p + geom_bar(data = donnees_propres, aes(x=auteur.identite.acteurRef))
p

getwd()
setwd("C:/R")
getwd()

tableau = read.csv("Ski.csv",sep=";",row.names=1,header=TRUE, dec = ",")
# Sep : séparateur du fichier
# row.names : s'il y a le nom de chaque colonne dans le fichier, et si oui, où çà
# header : un fichier en haut
# dec : séparateur décimal
head(tableau) # head() : affichage des premières lignes, à des fins de vérification
summary(tableau) # summary() : affichage de données importantes
tableau$Nombre.de.skieurs # Affichage des valeurs de Nombre.de.skieurs, aussi Ski[,11]
plot(tableau$Nombre.de.skieurs, type = "l")
ForteFreq = ifelse(tableau$Nombre.de.skieurs>10000, "1", "0")
tableau = cbind(tableau, ForteFreq)
tabl = table(ForteFreq)
pie(tabl)
tabl2 = table(tableau$Vacances.Zone.C, ForteFreq) # Dès qu'il y a la zone C, y a que des jours de forte fréquentation
WeekEnd = ifelse((tableau$Jour.de.la.semaine == "Samedi" | tableau$Jour.de.la.semaine == "Dimanche"), "1", "0")
tableau = cbind(tableau, WeekEnd)
tabl3 = table(ForteFreq, WeekEnd)
tabl3
tableau[tableau$Nombre.de.skieurs>20000,]
mean(tableau[tableau$Enneigment.moyen.des.pistes > 1.5,11])
mean(tableau[,11])



# write.table(tableau, file = "Wesh.csv", sep = ";", dec = ".", row.names = 1)
write.csv2(tableau, file = "Wesh.csv")


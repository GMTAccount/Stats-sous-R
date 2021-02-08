getwd()
setwd("C:/R")
getwd()

tableau = read.csv("Ski.csv",sep=";",row.names=1,header=TRUE, dec = ",")
pourcentage = c(length(tableau$Vacances.Zone.A)-sum(tableau$Vacances.Zone.A),sum(tableau$Vacances.Zone.A)/length(tableau$Vacances.Zone.A*100))
pie(table(tableau$Vacances.Zone.A),labels=pourcentage,col=c("red","blue"),main="test")
legend(1,1,legend=c("Hors vacances Zone A","Vacances Zone A"),fill=c("red","blue"),text.width = 3)
head(tableau)
barplot(table(tableau$Jour.de.la.semaine))
plot(tableau$Nombre.de.skieurs, type = "l")
plot(tableau$Neige.Station..m., type = "l")
Hotels=sub("%","",tableau$Reservation.d.hotels)
Hotels=sub(",",".",Hotels)
Hotels
tableau$Reservation.d.hotels=as.numeric(Hotels)/100
Vacances=ifelse(tableau$Vacances.Zone.A==1|tableau$Vacances.Zones.B==1|tableau$Vacances.Zone.C==1,1,0)
hist(tableau$Enneigment.moyen.des.pistes, breaks=c(0,0.5,0.8,1.1,1.4,1.7,2,2.5))
# Barplot : selon des critères, Hist : selon des intervalles
hist(tableau$Enneigment.moyen.des.pistes, breaks=c(0,0.5,2,2.5))
barplot(table(tableau$Heures.de.soleil.prévue.par.la.météo.il.y.a.3.jours))
hist(tableau$Heures.de.soleil.prévue.par.la.météo.il.y.a.3.jours)
mean(tableau$Nombre.de.skieurs)
median(tableau$Nombre.de.skieurs)
max(tableau$Nombre.de.skieurs)-min(tableau$Nombre.de.skieurs)
quantile(tableau$Nombre.de.skieurs,0.75)-quantile(tableau$Nombre.de.skieurs,0.25)
sum(tableau$Nombre.de.skieurs^2)/length(tableau$Nombre.de.skieurs)-mean(tableau$Nombre.de.skieurs)^2
var(tableau$Nombre.de.skieurs)


#Question 4 (en 2 versions)
sum(0:50)
b = 0
for(i in 0:50){
  b = b + i
}

#Question 5
expo=function(x, n) {
  b = 0
  for(i in 0:n) {
    b = b + (x^i)/factorial(i)
  }
  return(b)
}
expo(4,5)


#Question 6
moyenne=function(x){
  b = 0
  for(i in 1:length(x)){
    b = b + x[i]
  }
  return(b/length(x))
}
moyenne(tableau$Nombre.de.skieurs)
mean(tableau$Nombre.de.skieurs)


#Question 7
variance=function(x) {
  b=0
  n = mean(x)
  m = length(x)
  for(i in 1:m){
    b = b + (x[i] - n)^2
  }
  return(b/m)
}
variance(tableau$Nombre.de.skieurs)
-variance(tableau$Nombre.de.skieurs)+var(tableau$Nombre.de.skieurs)


# Question 8 :
mediane=function(x){
  # Ne pas changer x
  n = sort(x)
  a = length(n)
  b = 0
  if(a%%2 != 0){
    b = n[a/2]
  }
  else{
    b=(n[a/2]+n[a/2+1])/2
  }
  return(b)
}
mediane(tableau$Nombre.de.skieurs)
median(tableau$Nombre.de.skieurs)

#Question 9 :
Hauteur.Pistes.Pieds = (tableau$Enneigment.moyen.des.pistes*3.28)
tableau = cbind(tableau, Hauteur.Pistes.Pieds)


#Question 10
Nombre.Centre.Reduit = ((tableau$Nombre.de.skieurs-mean(tableau$Nombre.de.skieurs))/variance(tableau$Nombre.de.skieurs)^0.5)
tableau = cbind(tableau, Nombre.Centre.Reduit)


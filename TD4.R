getwd()
setwd("C:/R")
getwd()

tableau = read.csv("Ski.csv",sep=";",row.names=1,header=TRUE, dec = ",")

plot(tableau$Enneigment.moyen.des.pistes,tableau$Nombre.de.skieurs)

distance=function(x,y,a,b){
  return(sum((y - (a*x + b))^2))
  # pow(y-a*x+b,2)+pow(a*x+b-x,2))
}
distance(tableau$Enneigment.moyen.des.pistes,tableau$Nombre.de.skieurs,10000,0)
abline(0,10000)
plot(tableau$Enneigment.moyen.des.pistes,tableau$Nombre.de.skieurs)
distance(tableau$Enneigment.moyen.des.pistes,tableau$Nombre.de.skieurs,7500,5000)
abline(5000,7500)
me=mean(tableau$Enneigment.moyen.des.pistes)
mn=mean(tableau$Nombre.de.skieurs)
distance(tableau$Enneigment.moyen.des.pistes,tableau$Nombre.de.skieurs,7500,mn-7500*me-1)

A=seq(4864,4865,0.001)
A
DA=rep(0,length(A))
for(i in 0:length(A)){
  #DA[i]=c(DA,distance(tableau$Enneigment.moyen.des.pistes,tableau$Nombre.de.skieurs,i,mn-i*me-1))
  DA[i]=distance(tableau$Enneigment.moyen.des.pistes,tableau$Nombre.de.skieurs,A[i],mn-A[i]*me)
}
DA
A[DA==min(DA)] # Affichage dans A de l'indice minimum
plot(tableau$Enneigment.moyen.des.pistes,tableau$Nombre.de.skieurs)
abline(mn-A[DA==min(DA)]*me,A[DA==min(DA)])


modele=lm(data=tableau,Nombre.de.skieurs~Enneigment.moyen.des.pistes)

summary(modele)
modele2=lm(data=tableau,Nombre.de.skieurs~Vacances.Zone.C)
summary(modele2)
mean(tableau$Enneigment.moyen.des.pistes)
hotels = sub("%","",tableau$Reservation.d.hotels)
hotels = sub(",",".",hotels)
hotels=as.numeric(hotels)
modele3=lm(data=tableau,Nombre.de.skieurs~hotels)
summary(modele3)
prevision=predict(modele3)
plot(tableau$Nombre.de.skieurs,type="l")
lines(prevision,type="l",col="red")
lines(predict(modele2),type="l",col="blue")
modele4=lm(data=tableau,Nombre.de.skieurs~Reservation.d.hotels+Vacances.Zone.C+Heures.de.soleil.prévue.par.la.météo.il.y.a.3.jours)
summary(modele4)
plot(tableau$Nombre.de.skieurs,type="l")
lines(predict(modele4),type="l",col="red")


getwd()
setwd("C:/R")
getwd()

tableau = read.csv("Ski.csv",sep=";",row.names=1,header=TRUE, dec = ",")
Soleil = ifelse(tableau$Heures.de.soleil.prévue.par.la.météo.il.y.a.3.jours <= 2,"Peu",ifelse(tableau$Heures.de.soleil.prévue.par.la.météo.il.y.a.3.jours<=5,"Moyen","Beaucoup"))
affluence = ifelse(tableau$Nombre.de.skieurs<10000,"Peu",ifelse(tableau$Nombre.de.skieurs<20000,"Moyen","Beaucoup"))
affluence = factor(affluence,levels=c("Peu", "Moyen", "Beaucoup"))
Soleil = factor(Soleil,levels=c("Peu", "Moyen", "Beaucoup"))
table(Soleil,affluence)
A = matrix(1:12,nrow=3,ncol=4,byrow=TRUE)
A
apply(A,1,sum)
apply(A,2,sum)
profil=function(x){
  return(x/sum(x))
}
T=table(affluence,Soleil)
T
t(apply(T,1,profil))
apply(T,2,profil)
Khi2=function(T){
  n=sum(T)
  Tl=apply(T,1,sum)
  Tc=apply(T,2,sum)
  Tth = Tl%*%t(Tc)/n
  distance=(T-Tth)^2/Tth
  return(sum(distance))
}
Khi2(T)
chisq.test(T)
#La distance est très élevée, donc les variables sont probablement liés, khi-deux : 17.987
hotels = sub("%","",tableau$Reservation.d.hotels)
hotels = sub(",",".",hotels)
hotels=as.numeric(hotels)
hotel = ifelse(hotels<33,"Peu",ifelse(hotels<66,"Moyen","Beaucoup"))
T=table(affluence,hotel)
Khi2(T)
chisq.test(T)
#La distance est très élevée, donc les variables ne sont très probablement pas liés, khi-deux : 52.703


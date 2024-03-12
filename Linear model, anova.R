#########Aufgabe 1, lineares Modell, Anova

Quecksilber<-read.table(file=stdin(), header=T)
mercury alkalin calcium pH
"
1 1330 2.5 2.9 4.6
2 250 19.6 4.5 7.3
3 450 5.2 2.8 5.4
4 160 71.4 55.2 8.1
5 720 26.4 9.2 5.8
6 810 4.8 4.6 6.4
7 710 6.6 2.7 5.4
8 510 16.5 13.8 7.2
9 1000 7.1 5.2 5.8
10 150 83.7 66.5 8.2
11 190 108.5 35.6 8.7
12 1020 6.4 4.0 5.8
13 450 7.5 2.0 4.4
14 590 17.3 10.7 6.7
15 810 7.0 6.3 6.9
16 420 10.5 6.3 5.5
17 530 30.0 13.9 6.9
18 310 55.4 15.9 7.3
19 470 6.3 3.3 5.8
20 250 67.0 58.6 7.8
21 410 28.8 10.2 7.4
22 160 119.1 38.4 7.9
23 160 25.4 8.8 7.1
24 230 106.5 90.7 6.8
25 560 8.5 2.5 7.0
26 890 87.6 85.5 7.5
27 180 114.0 72.6 7.0
28 190 97.5 45.5 6.8
29 440 11.8 24.2 5.9
30 160 66.5 26.0 8.3
31 670 16.0 41.2 6.7
32 550 5.0 23.6 6.2
33 580 25.6 12.6 6.2
34 980 1.2 2.1 4.3
35 310 34.0 13.1 7.0
36 430 15.5 5.2 6.9
37 280 17.3 3.0 5.2
38 250 71.8 20.5 7.9
"
pairs(Quecksilber)
Quecklog<-Quecksilber
##Im Skript Quecklog[,-4]<-log(Quecklog) aber falsch
Quecklog[,-4]<-log(Quecklog[,-4]) ###
pairs(Quecklog)
fit0<-lm(mercury~alkalin,data=Quecklog)
fit<-lm(mercury~alkalin,data=Quecklog[-26,])
summary(fit)
ab0<-coef(fit0)
ab<-coef(fit)
# Bildbereich
opar<-par(mar=c(4, 4, 1, 1) + 0.1)
#
plot(mercury~alkalin,log="xy",data=Quecksilber,col="blue",pch=21)
curve(exp(ab[1]+ab[2]*log(x)),1,125,add=TRUE,col="darkblue")
points(87.6,890,col="orange",pch=19)
curve(exp(ab0[1]+ab0[2]*log(x)),1,125,add=TRUE,lty=2, col="orange")

plot(mercury~alkalin,data=Quecksilber,col="blue",pch=21)
curve(exp(ab[1]+ab[2]*log(x)),1,125,add=TRUE,col="darkblue")
points(87.6,890,col="orange",pch=19)
curve(exp(ab0[1]+ab0[2]*log(x)),1,125,add=TRUE,lty=2, col="orange")


par(opar)


testfit<-lm(mercury~alkalin+calcium,data=Quecklog[-26,])
anova(fit,testfit)
summary(testfit)

testfit<-lm(mercury~alkalin+pH,data=Quecklog[-26,])
anova(fit,testfit)
summary(testfit)


fitnolog<-lm(mercury~alkalin,data=Quecksilber)
plot(fitnolog)
plot(fit0)
fit1<-lm(mercury~alkalin+calcium,data=Quecklog)
summary(fit1)
fit1<-lm(mercury~alkalin+pH,data=Quecklog)
fit2<-lm(mercury~alkalin+I(alkalin^2),data=Quecklog)
fit1<-lm(mercury~alkalin+calcium+pH,data=Quecklog)
vif(fit1)







#Aufgabe 2
plot(airquality)

sum(is.na(airquality$Ozone))
sum(is.na(airquality$Solar.R))

##schnelle Lösung: werfe die Elemente mit fehlenden Werten weg
airquality2<-na.omit(airquality)
dim(airquality)[1]
dim(airquality)[1]-dim(airquality2)[1]

Nullmod<-lm(Ozone~1,data=airquality2)
Testmod<-lm(Ozone~Solar.R,data=airquality2)
anova(Nullmod,Testmod)
summary(Testmod)

####Ozone~Solar.R wird das aktuelle Modell, und mach weiter
Aktmod<-Testmod
Testmod<-lm(Ozone~Solar.R+Wind,data=airquality2)
anova(Aktmod,Testmod)
summary(Testmod)

####Ozone~Solar.R+Wind wird das aktuelle Modell
Aktmod<-Testmod
Testmod<-lm(Ozone~Solar.R+Wind+Temp,data=airquality2)
anova(Aktmod,Testmod)
summary(Testmod)

####Ozone~Solar.R+Wind+Temp wird das aktuelle Modell
Aktmod<-Testmod
Testmod<-lm(Ozone~Solar.R+Wind+Temp+Month,data=airquality2)
anova(Aktmod,Testmod)
summary(Testmod)

#P=0.051 liegt nah zur Grenze

#Aktmod wird nicht aktualisiert
Testmod<-lm(Ozone~Solar.R+Wind+Temp+Day,data=airquality2)
anova(Aktmod,Testmod)
summary(Testmod)

##Tag ist nicht signifikant (ist sinnvoll)

#Diagnostiken
Testmod<-lm(Ozone~Solar.R+Wind+Temp,data=airquality2)
plot(Testmod)

##evtl quadratische Einfluss
Aktmod<-lm(Ozone~Solar.R+Wind+Temp,data=airquality2)
Testmod<-lm(Ozone~Solar.R+I(Solar.R^2)+Wind+Temp,data=airquality2)
anova(Aktmod,Testmod)

Aktmod<-lm(Ozone~Solar.R+Wind+Temp,data=airquality2)
Testmod<-lm(Ozone~Solar.R+Wind+I(Wind^2)+Temp,data=airquality2)
anova(Aktmod,Testmod)

Aktmod<-Testmod
Testmod<-lm(Ozone~Solar.R+Wind+I(Wind^2)+Temp+I(Temp^2),data=airquality2)
anova(Aktmod,Testmod)

Aktmod<-Testmod
#Untersuche nochmal mit linearem Monatseffekt
Testmod<-lm(Ozone~Solar.R+Wind+I(Wind^2)+Temp+I(Temp^2)+Month,data=airquality2)
anova(Aktmod,Testmod)
summary(Testmod)

#Mit der quadratischen variablen ist Month nicht mehr signifikant 


Endmodell<-lm(Ozone~Solar.R+Wind+I(Wind^2)+Temp+I(Temp^2),data=airquality2)
summary(Endmodell)
plot(Endmodell)
#nicht super aber akzeptzabel




#Aufgabe 3
###Münchner Mietspiegel Daten

Miete<-read.csv2(file="../../../Teaching/Common_Data/Mietspiegel.csv")
str(Miete)

##model Nettomiete abhängig von Wohnfläche
fit1<-lm(nm~wfl,data=Miete)
summary(fit1)
plot(nm~wfl,data=Miete)
abline(fit1)
plot(fit1)
#Scale Location zeigt heterosked...

##benutze add1() und AIC, um das modell zu verbessern

add1(fit1,scope=nm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")
#ww0

Aktmod<-lm(nm~wfl+ww0,data=Miete)
add1(Aktmod,scope=nm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#kueche
Aktmod<-lm(nm~wfl+ww0+kueche,data=Miete)
add1(Aktmod,scope=nm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#bj
Aktmod<-lm(nm~wfl+ww0+kueche+bj,data=Miete)
add1(Aktmod,scope=nm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#wohngut
Aktmod<-lm(nm~wfl+ww0+kueche+bj+wohngut,data=Miete)
add1(Aktmod,scope=nm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#wohnbest
Aktmod<-lm(nm~wfl+ww0+kueche+bj+wohngut+wohnbest,data=Miete)
add1(Aktmod,scope=nm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#badkach0
Aktmod<-lm(nm~wfl+ww0+kueche+bj+wohngut+wohnbest+badkach0,data=Miete)
add1(Aktmod,scope=nm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#zh0
Aktmod<-lm(nm~wfl+ww0+kueche+bj+wohngut+wohnbest+badkach0+zh0,data=Miete)
add1(Aktmod,scope=nm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#rooms
Aktmod<-lm(nm~wfl+ww0+kueche+bj+wohngut+wohnbest+badkach0+zh0+rooms,data=Miete)
add1(Aktmod,scope=nm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#badextra
Aktmod<-lm(nm~wfl+ww0+kueche+bj+wohngut+wohnbest+badkach0+zh0+rooms+badextra,data=Miete)

add1(Aktmod,scope=nm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")


Endmod<-lm(nm~wfl+ww0+kueche+bj+wohngut+wohnbest+badkach0+zh0+rooms+badextra,data=Miete)
summary(Endmod)
plot(Endmod)

#heteroskedastizität ist noch da

##probiere Nettomiete pro Quadratmeter als Zielgröße
fit1<-lm(nmqm~1,data=Miete)
summary(fit1)
plot(nmqm~wfl,data=Miete)

add1(fit1,scope=nmqm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#zh0
Aktmod<-lm(nmqm~zh0,data=Miete)
add1(Aktmod,scope=nmqm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#rooms
Aktmod<-lm(nmqm~zh0+rooms,data=Miete)
add1(Aktmod,scope=nmqm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#kueche
Aktmod<-lm(nmqm~zh0+rooms+kueche,data=Miete)
add1(Aktmod,scope=nmqm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#wohngut
Aktmod<-lm(nmqm~zh0+rooms+kueche+wohngut,data=Miete)
add1(Aktmod,scope=nmqm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#bj
Aktmod<-lm(nmqm~zh0+rooms+kueche+wohngut+bj,data=Miete)
add1(Aktmod,scope=nmqm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#bj
Aktmod<-lm(nmqm~zh0+rooms+kueche+wohngut+bj+ww0,data=Miete)
add1(Aktmod,scope=nmqm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")


#bj
Aktmod<-lm(nmqm~zh0+rooms+kueche+wohngut+bj+ww0+badkach0,data=Miete)
add1(Aktmod,scope=nmqm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#bj
Aktmod<-lm(nmqm~zh0+rooms+kueche+wohngut+bj+ww0+badkach0+wohnbest,data=Miete)
add1(Aktmod,scope=nmqm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#badextra
Aktmod<-lm(nmqm~zh0+rooms+kueche+wohngut+bj+ww0+badkach0+wohnbest+badextra,data=Miete)
add1(Aktmod,scope=nmqm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")

#wfl
Aktmod<-lm(nmqm~bj+rooms+zh0+wohngut+kueche+ww0+badkach0+wohnbest+badextra+wfl,data=Miete)
add1(Aktmod,scope=nmqm~wfl+rooms+bj+wohngut+wohnbest+ww0+zh0+badkach0+badextra+kueche,test="F")
Endmod<-lm(nmqm~bj+rooms+zh0+wohngut+kueche+ww0+badkach0+wohnbest+badextra+wfl,data=Miete)
summary(Endmod)
plot(Endmod)

#modell mit nmqm ist besser als nm als Zielgröße















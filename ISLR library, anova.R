# ISLR Bibliothek, ANOVA

library(ISLR)
?Auto
names(Auto)


plot(Auto$horsepower,Auto$mpg)

#Nullmodel
Nullmod<-lm(mpg~1,data=Auto)
summary(Nullmod)

#Untersuche Variablen 1. Ordnung
Testmod<-lm(mpg~cylinders,data=Auto)
anova(Nullmod,Testmod)
summary(Testmod)
plot(Auto$cylinders,Auto$mpg)
abline(Testmod)

#Testmod wird das aktuelles Modell und füge displacement hinzu
Aktmod<-Testmod
Testmod<-lm(mpg~cylinders+displacement,data=Auto)
anova(Aktmod,Testmod)
summary(Testmod)
##interessant!


#Testmod wird das aktuelles Modell und füge horsepower hinzu
Aktmod<-Testmod
Testmod<-lm(mpg~cylinders+displacement+horsepower,data=Auto)
anova(Aktmod,Testmod)
summary(Testmod)

#Testmod wird das aktuelles Modell und füge weight hinzu
Aktmod<-Testmod
Testmod<-lm(mpg~cylinders+displacement+horsepower+weight,data=Auto)
anova(Aktmod,Testmod)
summary(Testmod)

#Testmod wird das aktuelles Modell und füge acceleration hinzu
Aktmod<-Testmod
Testmod<-lm(mpg~cylinders+displacement+horsepower+weight+acceleration,data=Auto)
anova(Aktmod,Testmod)
summary(Testmod)

#acceleration ist nicht signifikant. 
summary(Aktmod)
### ?lasse cylinders und displacement weg? 
Testmod<-lm(mpg~displacement+horsepower+weight,data=Auto)
anova(Testmod,Aktmod)
Aktmod<-Testmod
Testmod<-lm(mpg~horsepower+weight,data=Auto)
anova(Testmod,Aktmod)
Aktmod<-Testmod

##Aktmod ist unser Modell 1. Ordnung
summary(Aktmod)

plot(Aktmod)
###Zurück zum Skript


###Untersuche Variablen 2. Ordnung
Testmod<-lm(mpg~horsepower+weight+I(horsepower^2),data=Auto)
anova(Aktmod,Testmod)
summary(Testmod)


plot(Auto$horsepower,Auto$mpg)

plot(Auto$weight,Auto$mpg)

Aktmod<-Testmod
Testmod<-lm(mpg~horsepower+weight+I(horsepower^2)+I(weight^2),data=Auto)
anova(Aktmod,Testmod)

Aktmod<-Testmod
Testmod<-lm(mpg~horsepower+weight+I(horsepower^2)+I(weight^2)+horsepower*weight,data=Auto)
anova(Aktmod,Testmod)

Endmodell<-Aktmod
summary(Endmodell)
plot(Endmodell)

#Ende 9.1


###Mit AIC
Nullmod<-lm(mpg~1,data=Auto)
add1(Nullmod,scope=mpg~cylinders+displacement+horsepower+weight+acceleration,test="F")

#weight hat den kleisten AIC-Wert
Aktmod<-lm(mpg~weight,data=Auto)
add1(Aktmod,scope=mpg~cylinders+displacement+horsepower+weight+acceleration,test="F")

#horsepower hat den kleisten AIC-Wert
Aktmod<-lm(mpg~weight+horsepower,data=Auto)
add1(Aktmod,scope=mpg~cylinders+displacement+horsepower+weight+acceleration,test="F")

#cylinders hat den kleisten AIC-Wert aber dieser ist größer als der Aktueller AIC-Wert und
#der p-Wert ist mnicht signifikant


###Effekte 2. Ordnung
add1(Aktmod,scope=mpg~horsepower+I(horsepower^2)+weight+I(weight^2),test="F")

Aktmod<-lm(mpg~weight+horsepower+I(horsepower^2),data=Auto)
add1(Aktmod,scope=mpg~horsepower+I(horsepower^2)+weight+I(weight^2),test="F")

Endmodell<-lm(mpg~weight+I(weight^2)+horsepower+I(horsepower^2),data=Auto)
summary(Endmodell)


##############
##Variance Inflation Factor für das volle Modell 1. Ordnung
Mod1voll<-lm(mpg~cylinders+displacement+horsepower+weight+acceleration,data=Auto)
vif(Mod1voll)

Testmod<-lm(mpg~horsepower+weight,data=Auto)
vif(Testmod)

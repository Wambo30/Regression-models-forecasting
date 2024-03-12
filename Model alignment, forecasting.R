
# Modellanpassung, prognosewert
##Aufgabe 1 
d <- data.frame(
  Geschwindigkeit = rep(c(56.3, 64.4, 72.4, 80.4, 88.5, 96.5), each = 2),
  Verbrauch = c(10.7, 11.8, 8.4, 7.6, 6.4, 6.2, 5.7, 6, 6.9, 6.4, 8.7, 7.8)
)

# Datenpunkte 
plot(Verbrauch~Geschwindigkeit, data=d, col="blue",
xlab = "Geschwindigkeit", ylab = "Verbrauch")
# Modellanpassung
abc<-coef(fit<-lm(Verbrauch~Geschwindigkeit+I(Geschwindigkeit^2),data=d))
abc
# Modellfunktion
curve(abc[1]+abc[2]*x+abc[3]*x^2,col=gray(0.5), lwd=1.5,add=T)

summary(fit)
#Prognosewert bei 70 kmh
abc[1]+abc[2]*70+abc[3]*70^2
#Ableiten und nullstellen
#abc[2]+2*abc[3]*x=0
#Lösung ist
abc[2]/(-2*abc[3])

#Teil b)
tkoeff<-qt(0.975,9)
0.0094573-tkoeff*0.0007702 
0.0094573+tkoeff*0.0007702 

#Teil C) Warum ist der p-Wert der quadratische Parameter klein?

#Teil d)
fit3<-lm(Verbrauch~Geschwindigkeit+I(Geschwindigkeit^2)+I(Geschwindigkeit^3),data=d)
summary(fit3)


#Aufgabe 2
#a)
temp<-c(7,  9,  15,  13,  11,  16,  12,  8,  13,  16, 4,
        5, 6, 3, 8, 10, 3, 9, 8, 12, 7, 10, 11, 12)
Atomkraft<-data.frame(Einstellung=temp,Gruppe=rep(c("A","B","C"),
                                                  c(10,8,6)))

#b)
lm.obj1<-lm(Einstellung~Gruppe,data=Atomkraft)

#c&d)
summary(lm.obj1)
#?Refernzgruppe
#? p-Wert für GruppeC?
'Der Unterschied zwischen Gruppe C und der 
Referenzgruppe ist nicht signifikant 
'
#e)
anova(lm.obj1)

qf(0.95,2,21)
qf(0.99,2,21)

pf(10.125,2,21)
1-pf(10.125,2,21)


#Linieares Modell

load("C:\Users\X220\Documents\EStat\R-Studio\Regressionsmodelle_Downie_SS21\Regressionsmodelle_Üb03\marke.Rda")
names(marke)

plot(marke$Feuchtigkeit,marke$Geschmack)
plot(marke$Suesse,marke$Geschmack)

lm.obj<-lm(Geschmack~Feuchtigkeit,data=marke)
summary(lm.obj1)
options()$contrasts
options(contrasts=c("contr.sum", "contr.poly"))
lm.obj2<-lm(Geschmack~Feuchtigkeit,data=marke)
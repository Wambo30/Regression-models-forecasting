#Aufgabe 2 Battery life: Regression, plot, anova
#b)
Batterylife<- scan(file="Daten/batterylife.dat")
Material<-rep(c("M1","M2","M3"),each=12)
Temp<-as.factor(rep(rep(c(15,70,125),each=2,2),3))
battery<-data.frame(Batterylife,Material,Temp)
#boxplots
boxplot(Batterylife~Temp)
boxplot(Batterylife~Material)
boxplot(Batterylife~Temp*Material)

#c)
lmM<-lm(Batterylife~Material,data=battery)
anova(lmM)
#d)
require(effects)
plot(allEffects(lmM))


#e)
lmT<-lm(Batterylife~Temp,data=battery)
anova(lmT)
#f)
plot(allEffects(lmT))


#g)
lmTMWW<-lm(Batterylife~Temp*Material,data=battery)

#h)
anova(lmTMWW)


#i)
plot(allEffects(lmTMWW))
plot(allEffects(lmTMWW),multiline=TRUE)






#j)
#Benutzen Sie das Effektplot um diese Frage zu beantworten: "gibt es ein Material, dessen
#Nutzungsdauer unempfindlich zur Temperatur ist?"

# M1 schlecht bei Raumtemperatur
# M2 gut bei kalte Temperturen  schlecht bei heißen Temperaturen
# M3 das Beste bei Raumtemperatur und heißen Temperaturen, 
#     und nicht schlecht in kalten Temperaturen 
#M3 ist das unempfindlichsten



###Aufgabe 3 Nebenwirkungen
#a)
load("Daten/Nebenwirkungen.Rdata")

#b)
names(Nebenwirkung)

#c)
lmWW<-lm(Rest~Gewicht*Dosis,data=Nebenwirkung)

#d)
plot(allEffects(lmWW),multiline=TRUE)

#e)
anova(lmWW)

##Wechselwirkung Signifikant?


#f)
lmHE<-lm(Rest~Gewicht+Dosis,data=Nebenwirkung)
anova(lmHE)
plot(allEffects(lmHE),multiline=TRUE)

summary(lmHE)

#Referenzgruppe ist 'Gewicht1' und 'Dosis1' 

#Bestimmen Sie die Modellkoeffizienten das Besten Modells. Wie Groß ist der 
#angepasste Wert für
#Gewichtsstufe 2 und Dosis 3?

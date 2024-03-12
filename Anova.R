#Anova

'#
wir behandeln eine mehrfaktorielle anova mit drei Faktoren.

Keimzahlen == Zielgröße Y

Faktor 1-Molkerei 
mit Faktorgruppe 1 : A
mit Faktorgruppe 2 : B
mit Faktorgruppe 3 : C

Faktor 2-Supermarkt 
mit Faktorgruppe 1 : D
mit Faktorgruppe 2 : E

Faktor 3-Fett
mit Faktorgruppe 1 : Vollmilch
mit Faktorgruppe 2 : Fettarm

Ich bereite die Daten auf um einen dataframe diesbezüglich zu nutzen. Daraufhin
werde ich ein lm Modell mit den drei Faktoren der Form 
Y~Faktor1*Faktor2*Faktor3 , data=Keimzahlen

Der * ist hierfür hinreichend, da es das Haupteffektmodell und das Wechselwirkungs-
modell wiedergibt.

#'


#daten importieren
load("C:/Users/X220/Documents/EStat/R-Studio/Regressionsmodelle_Downie_SS21/Hausaufgabe_Regressionsmodelle/Keimzahlen.Rda")

#Datenaufbereitung für dataframe indem ich die rda betrachtet habe und so den code geschrieben habe

Molkerei<- rep(c("A","B","C"),each=1,8)
#In den Daten herrscht eine A B C Abfolge und das 8 mal im ganzen Datensatz hintereinander weg

Supermarkt<-rep(c("D","E"),each=1,12)
#In den Daten herrscht eine D E Abfolge und das 12 mal im ganzen Datensatz hintereinander weg


Fett<-rep(c("Vollmilch","Fettarm"),each=6,2)
#In den Daten herrscht eine Abfolge  das sechs mal Vollmilch und dann sechsmal 
# Fettarm im Datensatz hintereinander weg auftritt


Keimzahlen<-data.frame(Milch,Molkerei,Supermarkt,Fett)
lmMSF<-lm(Keimzahlen~Molkerei*Supermarkt*Fett, data=Keimzahlen)
#Der * ist hinreichend, da es das Haupteffektmodell und das Wechselwirkungsmodell wiedergibt.

anova(lmMSF)


'#
Ausgabe:
Anhand der Anova können wir folgendes erkennen

- Der Faktor 1 (Molkerei) ist zum Niveau 0.1% signifikant. 

- Der Faktor 3 (Fett) ist zum Niveau 10% signifikant.

- Die Interaktion Molkerei:Fett ist zum Niveau 1% signifikant.

- Die Interaktion Molkerei:Supermarkt:Fett ist zum Niveau 10% signifikant.

Vorschlag für Endmodell == Interaktion Molkerei:Fett
Da man hier eine Wechselwirkung zum Niveau 1% feststellen kann.

#'

require(effects)
plot(allEffects(lmMSF),multiline=TRUE)

'#

Anhand des Effects Plots erkennt man das die Interaktion Molkerei:Fett das
beste Endmodell darstellt. Bei dem Faktor A sieht man das es zu den anderen 
Haupteffektmodellen also Faktor B und Faktor C relativ parallel verläuft.

Da aber eine Wechselwirkung bezüglich Molkerei:Fett und Molkerei:Supermarkt:Fett
existiert, ist diese Parallelität schwächer vorhanden.

Wir können also abschließend sagen , dass die Wechselwirkung Molkerei:Fett das
beste Modell ist. Man kann mit diesem Modell am besten den Ursprung der Keimzahlen
herausfinden. Inwieweit 

#'

plot(Molkerei:Fett,data=Keimzahlen)
boxplot(lmMSF)

#regressionsmodelle blatt 6 aufgabe 2) , Regression,Anova

'#

Ein Ingenieur untersucht den Einfluss von Material und Temperatur auf der 
Nutzungsdauer verschiedener Batterien (Batterylife). Die Hauptfrage ist: 
"gibt es einen Stoff, dessen Nutzungsdauer unempfindlichzum Temperatur ist?"
Die Versuch hat ein sogenannte faktorielle Versuchsplan; Jede Kombination der 
drei Materialien und Temperaturen wurde jeweils viermal wiederholt. 
Die Datendatei (Moodle) beinhaltet nur die Werte der Nutzungsdauer in der 
folgenden Form: siehe tabelle auf blatt 6

Type == Material typ
Temperature == 15 , 70, 125 Fahrenheit jeweils 

Es gibt 3 faktorgruppen == Materialtyp 1, 2 und 3


--->zahlen in der mitte der tabelle scheint die nutzungsdauer darzustellen!

Die folgenden R-Befehle bringt die Werte in einedata.frame-Form, was deutlich 
praktischer ist:
#'

Batterylife<- scan(file="batterylife.dat")
Material<-rep(c("M1","M2","M3"),each=12)
Temperature<-as.factor(rep(rep(c(15,70,125),each=2,2),3))
battery<-data.frame(Batterylife,Material,Temperature)


'#
-------------------------------------------------------------------------------
Regel- nutzung von scan()-befehl um datensatz einzulesen

bsp.obda:
datensatzdatei == batterylife.dat

speichern batterylife.dat auf rechner und finden die datei rechts über das
ordnermenü. wenn datei gefunden , dann links ein häckchen setzen.

--> dann in der mitte rechts auf "More" klicken (da ist ein zahnrad zu sehen)
--> und klicken "Set as working directory"
--> daraufhin kann man den scanbefehl nutzen

Batterylife<- scan(file="batterylife.dat")

==>daten sind hochgeladen und nun nutzbar

-------------------------------------------------------------------------------
Regel- Verständnis datensatz batterylife bzgl anova mit zwei faktoren

faktor 1: Materialtyp x1 variable
Faktorgruppen: Typ 1, Typ2, Typ3

Faktor 2: Temperatur x2 variable
Faktorgruppen: 15 Fahrenheit, 70 Fahrenheit, 125 Fahrenheit

batterylife : Zielvariable y also abhängige variable! dh temperatur und materialtyp
nehmen einfluss auf das batterieleben
==> testergebnisse sind werte bzgl des batterielebens

Ein materialtyp wird in einem durchlauf zweimal bezüglich der selben Temperatur
getestet. Dh ein Durchlauf hat 6 testergebnisse also 2 pro faktorgruppe temperatur.

Es gibts pro materialtyp 2 Durchläufe immer.


bsp obda: 
Batterylife<- scan(file="batterylife.dat")
Material<-rep(c("M1","M2","M3"),each=12)
Temperature<-as.factor(rep(rep(c(15,70,125),each=2,2),3))
battery<-data.frame(Batterylife,Material,Temperature)

jede zeile wird erklärt:


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
zeile 1:
Batterylife<- scan(file="batterylife.dat")

datensatzdatei == batterylife.dat

speichern batterylife.dat auf rechner und finden die datei rechts über das
ordnermenü. wenn datei gefunden , dann links ein häckchen setzen.

--> dann in der mitte rechts auf "More" klicken (da ist ein zahnrad zu sehen)
--> und klicken "Set as working directory"
--> daraufhin kann man den scanbefehl nutzen

Batterylife<- scan(file="batterylife.dat")

==>daten sind hochgeladen und nun nutzbar

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
zeile 2:

Material<-rep(c("M1","M2","M3"),each=12)

==> vektor material gebildet wo hinterander weg 12 mal jeweils M1 dann M2 
dann M3 steht. Ist auch logisch da pro durchlauf 6 testergebnisse pro material-
typ entstehen. Bei zwei durchläufen demnach 12.

ausgabe:
[1] "M1" "M1" "M1" "M1" "M1" "M1" "M1" "M1" "M1" "M1" "M1" "M1" "M2" "M2" "M2" "M2"
[17] "M2" "M2" "M2" "M2" "M2" "M2" "M2" "M2" "M3" "M3" "M3" "M3" "M3" "M3" "M3" "M3"
[33] "M3" "M3" "M3" "M3


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
zeile 3:

Temperature<-as.factor(rep(rep(c(15,70,125),each=2,2),3))

es wird ein vektor erstellt wo die folge genauso 15 15 70 70 125 125
drei mal vorkommt bzw ausgegeben wird!
verständnis---> each=<AnzahlWiederholungZiffer>,<AnzahlWiederholungFolge>

as.factor() == obwohl numerische werte für die testergebnisse rauskommen, sollen
diese aber als faktoren betrachten

Fall 1:

Test3<-as.factor(rep(c(15,70,125),each=1,1)) 

Ausgabe:
[1] 15  70  125
Levels: 15 70 125

==> die angegebenen ziffern werden nur einmal ausgegeben

Fall 2:

Test4<-as.factor(rep(c(15,70,125),each=2,1)) 


Ausgabe:
[1] 15  15  70  70  125 125
Levels: 15 70 125

==> angegebenen ziffern werden in derselben reihenfolge bloß doppelt ausgegeben
da wir also erste ziffer bei each=2,1 stehen haben. ABER die folge 
15 70 125 ist chronologisch so geblieben!


Fall 3:


Test5<-as.factor(rep(c(15,70,125),each=2,2)) 

ausgabe:
[1] 15  15  70  70  125 125 15  15  70  70  125 125
Levels: 15 70 125

==> die folge wo zweimal dieselbe zahl also 15 15 usw ausgegeben wird,
wird einfach zweimal ausgegeben! dh die zweite ziffer legt fest wie oft die 
vorgegebene zahlenfolge , sei sie mit doppelten zahlen oder nicht, ausgegeben wird!


Fall 4:

Test5<-as.factor(rep(c(15,70,125),each=1,3))

ausgabe:
[1] 15  70  125 15  70  125 15  70  125
Levels: 15 70 125

==> die folge wird 3 mal ausgegeben wobei aber jede ziffer NICHT doppelt da steh!
da bei der ersten ziffer bei each=1,3 steht!


Fall 5:

test6<-as.factor(rep(rep(c(15,70,125),each=2,2)))

ausgabe:
[1] 15  15  70  70  125 125 15  15  70  70  125 125
Levels: 15 70 125

==> selbe ausgabe wie davor bloß das wir doppeltes rep() also repeat benutzt
haben! das zweite repeat wird aber nicht verwendet daher tut sich auch nix
neues bei der ausgabe!


fall 6:

test6<-as.factor(rep(rep(c(15,70,125),each=2,2),3))

ausgabe:
[1] 15  15  70  70  125 125 15  15  70  70  125 125 15  15  70  70  125 125 15  15 
[21] 70  70  125 125 15  15  70  70  125 125 15  15  70  70  125 125
Levels: 15 70 125

==> die 3 bei rep() also rep( rep(...  ),3) ist wie ein faktor=3 da davor die
zahlenfolge nur 2 mal ausgegeben wurde und nun 6 mal! somit wurde die anzahl
der ursprünglichen zahlenfolge (2) mit 3 multipliziert also 2*3=6 Zahlenfolgen!


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
zeile 4:

battery<-data.frame(Batterylife,Material,Temperature)

battery == zielvariable y 
material == faktor 1 also x1
temperatur == faktor 2 also x2

==> x1 und x2 nehmen einfluss auf y daher ist die reihenfolge so korrekt!
UND man nutzt hier NICHT lm() da bei lm() nur mit einem faktor gearbeitet wird
der y beeeinflusst!
-------> das ist hier aber nicht der fall!


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-------------------------------------------------------------------------------
#'

#a) Warum wird die Funktionas.factor()für Temperatur aber nicht für Material 
#benutzt?
#Die Temperaturen  sind  als  numerische  Werte  bestimmt,  aber  wir  möchten  
#die  als  3  Gruppen  ohnenumerische Wert modellieren.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#b)Erstellen Sie geeignete Grafiken, um das Verhältnis zwischen Batterylife, 
#Material und Temperatur zu untersuchen



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#c) Erstellen  Sie  ein einfaktorielles Anova-Modell  für  Batterylife  abhängig  
#von  Material lm().Benutzen Sie anova(), um einen F-Test durch zuführen.



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#d)Erstellen  Sie  ein einfaktorielles Anova-Modell  für  Batterylife  abhängig  
#von  Temperaturen.Erstellen Sie eine Anova-Tabelle, um einen F-Test durchzuführen


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#e)  Erstellen Sie einen "Effects-Plot", mit der allEffects()Funktion. Sie ist 
#im Paket namenseffects, das direkt mit der R-Installation kommt. Der erste Befehl
#lädt das Paket und der Zweite ergibt das Diagramm. Sie müssen lmT ersetzen durch 
#den Modell-Objektnamen

'#
ergebnis:

require(effects)
plot(allEffects(lmT)

#'



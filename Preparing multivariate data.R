# vorbereitung der multivariaten daten

'Die Vorbereitung der Daten ist hinreichend damit das Ergebnis der Regression
nicht verzerrt wird. Hierbei untersucht man die X-Variablen und Y-Variablen
jeweils untereinander.

- Bezug auf bevölkerungsdaten in Berlin

X-variablen == 1)Bevölkerungsab-/zunahme, 2) Schüler Berlin, 3)Studierende
4)Übernachtungen

Y-Variablen == 1)Gesamt nach EAV , 2) Monatskarten Berlin ABC,
3)Jahreskarten Berlin ABC , 4)Firmenticket, 5) Semester-Hochschulticket


'
#########################################################################


'
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
regel- Variablen/Spalten mit subset Befehl aus Datensatz filtern:

<NameVar> <- subset(<NameDatensatz>, select = c(<NameSpalte1>,<NameSpalte2>,...) )

==> geht aber nur wenn alle Werte positiv sind!gemischte werte gehen nicht!
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

regel- datensatz filtern mit data.frame()Befehl

syntax: <NameVariable> <- data.frame(<Spalte1>,<Spalte2>,...)

'

# Daten vorbereiten und missing values beseitigen
class(BevolkX)

str(BevolkX)


AltBevolkX<- data.frame (data_Treiber$`Bevölkerungfsab/-zunahme`, data_Treiber$`Schüler Berlin`, data_Treiber$Studierende, data_Treiber$Übernachtungen)

#zeilen löschen die missing values enthalten , von Zeile 85-192 sind Werte

#suche nach missing values und anzeigen im data frame durch TRUE ausgabe
is.na(BevolkX)

#löschen der zeilen mit missing values
BevolkX<-na.omit(BevolkX)

#Spaltennamen ändern
names(BevolkX)[names(BevolkX)=="data_Treiber..Bevölkerungfsab..zunahme."]="Bevoelkerungsab-und zunahme"

names(BevolkX)[names(BevolkX)=="data_Treiber..Schüler.Berlin."]="Schueler Berlin"

names(BevolkX)[names(BevolkX)=="data_Treiber.Studierende"]="Studierende"

names(BevolkX)[names(BevolkX)=="data_Treiber.Übernachtungen"]="Uebernachtungen"


names(BevolkX)
BevolkX
####################################################################################
#Data cleaning machen

install.packages("dplyr")
library(dplyr)

str(BevolkX)
names(BevolkX)

# nutzen box-cox transformation
install.packages("MASS")
library(MASS)




####################################################################################
#Nutzen das vorbereiteten Datensatzes um Tests bzgl Normalverteilung zu machen



#transformieren der daten um shapirotest zu nutzen 
transpose_BevolkX<-t(BevolkX)

# nutzen shapirotest um Normalverteilung festzustellen
mshapiro.test(transpose_BevolkX)


'
Ausgabe:

Shapiro-Wilk normality test

data:  Z
W = 0.86176, p-value = 1.292e-08

Da p-value < 0.05 ==> eine oder mehrere X-Variablen sind NICHT normalverteilt!

'
# wir nutzen zusätzlich den Jarque-Bera Test um das zu bestätigen

install.packages("normtest")
library(normtest)

#nutzen Jarque-Bera Test
jb.norm.test(BevolkX)

jb.norm.test(AltBevolkX)

'
Ausgabe:

Jarque-Bera test for normality

data:  BevolkX
JB = 1.4969, p-value < 2.2e-16

Da p-value<0.05 ==> also ist eine oder mehrere x-variablen NICHT normalverteilt!
Zusätzliche Bestätigung zum Shapiro-Wilk Test

'


install.packages("normwhn.test")
library(normwhn.test)

normality.test1(BevolkX)



#wir nutzen mehrere spezielle tests zur feststellung der Normalverteilung bzw.
# welche variable im Speziellen NICHT normalverteilt ist


install.packages("nortest")
library(nortest)

# der anderson-darling test wird empfohlen nach Q20

ad.test(BevolkX$data_Treiber..Bevölkerungfsab..zunahme.)
ad.test(BevolkX$data_Treiber..Schüler.Berlin.)
ad.test(BevolkX$data_Treiber.Studierende)
ad.test(BevolkX$data_Treiber.Übernachtungen)

#Shapiro Francia Test
sf.test(BevolkX$data_Treiber..Bevölkerungfsab..zunahme.)
sf.test(BevolkX$data_Treiber..Schüler.Berlin.)
sf.test(BevolkX$data_Treiber.Studierende)
sf.test(BevolkX$data_Treiber.Übernachtungen)

'
Ausgabe:
bei shapiro francia stellte sich auch heraus das alle variablen NICHT normalverteilt
sind!

man kann noch folgende Tests für die Normalverteilung ausführen:

Cramer-von-Mises Test == cvm.test("<NameXVariable>")
Kolmogorov-Smirnov Test == lillie.test("<NameXVariable>")
pearson chi quadrat test == pearson.test("<NameXVariable>")

==> Nach Q15 Seite 16 und Q20 Seite 41 muss eine Normalverteilung bei den 
Variablen seperat oder gemeinsam nicht vorherrschen, da bei den meisten
multivariaten Prozeduren die Normalverteilung nicht nötig ist!

'

neu<-log(Neu_BevolkX)

shapiro.test(BevolkX$data_Treiber..Bevölkerungfsab..zunahme.)
shapiro.test(BevolkX$data_Treiber..Schüler.Berlin.)
shapiro.test(BevolkX$data_Treiber.Studierende)
shapiro.test(BevolkX$data_Treiber.Übernachtungen)


#nutzung energy-test um zu prüfen ob normalverteilung vorherrscht

install.packages("energy")
library(energy)
mvnorm.e(BevolkX)


'Ausgabe:


	Energy test of multivariate normality: (Specify R > 0 for MC test)

data:  x, sample size 108, dimension 4, replicates 0
E-statistic = 3.2704, p-value = NA


Erklärung:


'

#Heinze-Zirkler test für multivariate Normalverteilung nutzen
# hier wird für jede Variable seperat die normalverteilung getetest

install.packages("MVN")
library(MVN)
#result <- hz(BevolkX, qqplot=FALSE)

mvn(
  BevolkX,
  subset = NULL,
  mvnTest = "hz",
  covariance = TRUE,
  tol = 1e-25,
  alpha = 0.5,
  scale = FALSE,
  desc = TRUE,
  transform = "none",
  R = 1000,
  univariateTest = "AD",
  univariatePlot = "none",
  multivariatePlot = "qq",
  multivariateOutlierMethod = "none",
  bc = FALSE,
  bcType = "rounded",
  showOutliers= FALSE,
  showNewData = FALSE
)

'==> Ausgabe:

$multivariateNormality
           Test       HZ p value MVN
1 Henze-Zirkler 2.747208       0  NO

$univariateNormality
              Test                                Variable Statistic   p value
1 Anderson-Darling data_Treiber..Bevölkerungfsab..zunahme.    1.3492  0.0016  
2 Anderson-Darling      data_Treiber..Schüler.Berlin.         3.2679  <0.001  
3 Anderson-Darling        data_Treiber.Studierende            2.0126  <0.001  
4 Anderson-Darling       data_Treiber.Übernachtungen          2.6965  <0.001  
  Normality
1    NO    
2    NO    
3    NO    
4    NO    

$Descriptives
                                          n        Mean    Std.Dev  Median
data_Treiber..Bevölkerungfsab..zunahme. 108    3130.426   2481.527    2953
data_Treiber..Schüler.Berlin.           108  344521.880  15612.818  339433
data_Treiber.Studierende                108  177171.750  13740.375  175917
data_Treiber.Übernachtungen             108 2335827.361 680708.462 2479995
                                           Min     Max       25th       75th
data_Treiber..Bevölkerungfsab..zunahme.  -3783    9434    2002.25    4379.75
data_Treiber..Schüler.Berlin.           323724  371869  328186.00  360031.00
data_Treiber.Studierende                153694  199421  165923.00  188982.75
data_Treiber.Übernachtungen             162486 3399163 2028361.50 2830728.00
                                              Skew   Kurtosis
data_Treiber..Bevölkerungfsab..zunahme. -0.1225321  0.5266959
data_Treiber..Schüler.Berlin.            0.1424304 -1.4065373
data_Treiber.Studierende                -0.1015313 -1.2142000
data_Treiber.Übernachtungen             -1.2300973  1.4611266



Erklärung:
Nach den Werten hier sind die Variablen nicht normalverteilt ABER nach dem
Chi-Square QQ plot herrscht eine Normalverteilung!


'



'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

# visueller test um zu prüfen ob Variable normalverteilt ist
'
Nach Q31: The Power to See-A New Graphical Test of Normality
ist es besser die Normalverteilung grafisch also durch den qqplot zu bestimmen!


'



#1)nutzung qqnorm
qqnorm(Neu_BevolkX$dataFrame_Treiber_X.data_Treiber..Bevölkerungfsab..zunahme.)
qqnorm(Neu_BevolkY$dataFrame_Ertrag_Y.data_Ertrag..Monatskarten..Berlin.ABC..450012..)
qqnorm(dataFrame_Ertrag_Y$data_Ertrag..Einzelfahrscheine.Berlin.ABC..450010..)

#z-standardiseren
zBevolk<- scale(Neu_BevolkX$dataFrame_Treiber_X.data_Treiber..Bevölkerungfsab..zunahme.)
zBevolk<- scale(Neu_BevolkY$dataFrame_Ertrag_Y.data_Ertrag..Monatskarten..Berlin.ABC..450012..)
zTest<-scale(dataFrame_Ertrag_Y$data_Ertrag..Einzelfahrscheine.Berlin.ABC..450010..)

qqnorm(zBevolk)
qqnorm(zTest)

#ursprungsgerade einzeichnen wie stark die werte auf gerade liegen
qqline(zBevolk)
qqline(zTest)
'
Man sieht am plot das die Werte ungefähr auf der Gerade liegen zwischen -1 und 1
==> somit ist es ungefähr normalverteilt!
In der Realität gibts keinen datensatz der GENAU normalverteilt ist!

UND
Je größer der Datensatz ist desto unzuverlässiger sind die analytischen tests!
Daher sind die visuellen tests besser dafür geeignet

'

#2)nutzung histogramm um visuell zu prüfen ob werte normaltverteilt sind
hist(zBevolk)
hist(BevolkX$data_Treiber..Bevölkerungfsab..zunahme.)

'
man sieht bei beiden Histogrammen, dass sich die Werte in der Mitte des 
Histogramms mehrheitlich sammeln und an den Seiten weniger werden!
Somit ist es aufjedenfall normalverteilt!

'

#3) Prüfung auf Distanz da man extra prüfen muss ob die Werte gemeinsam
#normalverteilt sind --> AUCH wenn sie es für sich alleine sein sollten!

dist(scale(Neu_BevolkY))




####################################################################################
# Bestimmung der Varianz-Kovarianz Matrix und Prüfung ob die Determinante  dieser 
#Matrix positiv ist


# bilden der Varianz-Kovarianz Matrix von den X-Variablen BevolkX
covBevolkX<- cov(Neu_BevolkX)

# determinante dieser matrix bilden
det(covBevolkX)


'Ausgabe: 2.98712e+36
==> somit ist die Determinante (allgemeine Varianz) der Varianz-Kovarianz Matrix 
von BevolkX positiv und wir können mit multivariante statistischen analyse 
weitermachen!

'

####################################################################################
#Prüfung ob die Varianz-Kovarianz Matrizen der Gruppen gleich sind

'
==> fällt warscheinlich weg da wir diese Eigenschaft später nicht benötigen!

'

#Erstellung einer Gruppen-Variable um die Matrix BevolkX in zwei Gruppen aufzuteilen

group = rep(c("male", "female"), c(10,10))
factor(group)
#newdata = data.frame(BevolkX,group)

install.packages("biotools")
library(biotools)


boxM(Neu_BevolkX,group)
# funktioniert nicht da wohl mind eines der x-variablen nicht normalverteilt
#ist

'
==> klappt nicht da unsere Daten nicht normalverteilt sind! Wir nutzen daher
eine nicht-parametrisierte Methode also die Diskriminanzanalyse
Im Buch Q20 wird das später nochmal behandelt

'


install.packages("ICSNP")
library(ICSNP)
HotellingsT2(Neu_BevolkX[1:54,], Neu_BevolkX[55:108,])


# Schauen ob Varianzen gleich sind

install.packages("psych")
library(psych)

#bei männern und frauen soll eine ähnliche varianz vorherrschen
describeBy(dataFrame_Ertrag_Y$data_Ertrag..Einzelfahrscheine.Berlin.ABC..450010..,dataFrame_Ertrag_Y$data_Ertrag..Monatskarten..Berlin.ABC..450012..)



install.packages("candisc")
install.packages("heplots")
library(candisc)
library(heplots)

####################################################################################
#Ausreißer herausfinden

'
==> wäre wichtig für Hauptkomponentenanalyse! Auch wenn nicht, dann verringert es
dir Korrelation zwischen den Variablen

'
# Bsp Ex1 Neu_BevölkX und Neu_BevolkY

#1)Neu_BevolkX
Neu_BevolkX<-data.frame(dataFrame_Treiber_X$Arbeitslose,dataFrame_Treiber_X$`Azubis insgesamt`,dataFrame_Treiber_X$Studierende,dataFrame_Treiber_X$`Bevoelkerungsab-und zunahme`)
Neu2_BevolkX<-data.frame(dataFrame_Treiber_X$Arbeitslose,dataFrame_Treiber_X$`Azubis insgesamt`,dataFrame_Treiber_X$Studierende,mat3_BevoelkAbZu_X)


names(Neu_BevolkX)

names(Neu_BevolkX)[names(Neu_BevolkX)=="dataFrame_Treiber_X.Arbeitslose"]="Arbeitslose"
names(Neu_BevolkX)[names(Neu_BevolkX)=="dataFrame_Treiber_X..Azubis.insgesamt."]="Azubis insgesamt"
names(Neu_BevolkX)[names(Neu_BevolkX)=="dataFrame_Treiber_X.Studierende"]="Studierende"
names(Neu_BevolkX)[names(Neu_BevolkX)=="dataFrame_Treiber_X..Bevoelkerungsab.und.zunahme."]="Bevoelkerungsab-und zunahme"


#prüfen ob alle Werte als numeric gelten
sapply(Neu_BevolkX,class)

#da die Variable "Arbeitslose" den datentyp factor hat, muss man diesen 
#zuerst zum character umwandeln!
Neu_BevolkX$Arbeitslose<-as.numeric(as.character(Neu_BevolkX$Arbeitslose))


#prüfen ob alle Werte als numeric gelten
sapply(Neu_BevolkX,class)

Cor_BevolkX<-cor(Neu_BevolkX[,c("Arbeitslose", "Azubis insgesamt" ,"Studierende" , "Bevoelkerungsab-und zunahme" )])

sapply(Neu_BevolkX,class)

#dataframe zur Matrix umwandeln
mat_NeuBevolkX<-t(Neu_BevolkX)

# format von matrix anpassen
mat_NeuBevolkX<-t(mat_NeuBevolkX)


#versuch korrelationsmatrix aus der Datenmatrix zu bilden
Cor2_BevolkX<-cor(mat_NeuBevolkX[,c("Arbeitslose", "Azubis insgesamt" ,"Studierende" , "Bevoelkerungsab-und zunahme" )])


boxplot(mat_NeuBevolkX)



#1)Neu_BevolkY
Neu_BevolkY<-data.frame(dataFrame_Ertrag_Y$`Monatskarten ABC`,dataFrame_Ertrag_Y$ABO,dataFrame_Ertrag_Y$Firmenticket,dataFrame_Ertrag_Y$Hochschulticket)


names(Neu_BevolkY)

names(Neu_BevolkY)[names(Neu_BevolkY)=="dataFrame_Ertrag_Y..Monatskarten.ABC."]="Monatskarten ABC"
names(Neu_BevolkY)[names(Neu_BevolkY)=="dataFrame_Ertrag_Y.ABO"]="ABO"
names(Neu_BevolkY)[names(Neu_BevolkY)=="dataFrame_Ertrag_Y.Firmenticket"]="Firmenticket"
names(Neu_BevolkY)[names(Neu_BevolkY)=="dataFrame_Ertrag_Y.Hochschulticket"]="Hochschulticket"


#prüfen ob alle Werte als numeric gelten
sapply(Neu_BevolkY,class)
# ==> ja sind sie

Cor_BevolkY<-cor(Neu_BevolkY[,c("Monatskarten ABC", "ABO" ,"Firmenticket" , "Hochschulticket" )])

sapply(Neu_BevolkY,class)

#dataframe zur Matrix umwandeln
mat_NeuBevolkY<-t(Neu_BevolkY)

# format von matrix anpassen
mat_NeuBevolkY<-t(mat_NeuBevolkY)

#prüfen welchen datentyp existiert
class(mat_NeuBevolkY)


boxplot(mat_NeuBevolkY)

#ausreißer entfernen
Neu_BevolkY_out_rm <- mat_NeuBevolkY[!mat_NeuBevolkY %in% boxplot.stats(mat_NeuBevolkY)$out] 

# angabe zur ausgabe der Ausreißer
length(mat_NeuBevolkY) - length(Neu_BevolkY_out_rm)
'
Ausgabe: 33 dh es existieren 33 Ausreißer die die Korrelation zwischen den Variablen
negativ beeinflussen! '


boxplot(Neu_BevolkY_out_rm)

Cor2_BevolkY<-cor(Neu_BevolkY_out_rm[,c("Monatskarten ABC", "ABO" ,"Firmenticket" , "Hochschulticket" )])




'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
#Methode 3

dataFrame_Y_Monatskarten<- data.frame(dataFrame_Ertrag_Y$`Monatskarten ABC`)

#matrix erstellen und 
mat_Monatskarten_Y<- t(dataFrame_Y_Monatskarten)
mat_Monatskarten_Y<- t(mat_Monatskarten_Y)

#identifizieren von Ausreißern
boxplot(mat_Monatskarten_Y)

#Anzahl der Werte feststellen
length(mat_Monatskarten_Y)

boxplot(mat_Monatskarten_Y, horizontal=TRUE)

#nur werte nehmen die keine ausreißer enthalten
mat2_Monatskarten_Y <- mat_Monatskarten_Y[mat_Monatskarten_Y > 5e+06]

boxplot(mat2_Monatskarten_Y, horizontal=TRUE)

#nur werte nehmen die keine ausreißer enthalten
mat3_Monatskarten_Y<-mat2_Monatskarten_Y[mat2_Monatskarten_Y>5400000]

boxplot(mat3_Monatskarten_Y, horizontal=TRUE)

#anzahl der werte nochmal feststellen ob sich anzahl verringert hat da man die 
#ausreißer aussortiert hat
length(mat3_Monatskarten_Y)

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'


####################################################################################
#Korrelation zwischen den abh. Variablen (Y-Variablen)
#Q15 Seite 14

Neu_BevolkY<- data.frame (dataFrame_Ertrag_Y$data_Ertrag..Monatskarten..Berlin.ABC..450012..,dataFrame_Ertrag_Y$data_Ertrag..ABO..450014..Nutzerfin..,dataFrame_Ertrag_Y$data_Ertrag..Firmenticket..450015..,dataFrame_Ertrag_Y$data_Ertrag..Semester...Hochschulticket..450016..,dataFrame_Ertrag_Y$data_Ertrag..Gesamt.vor.EAVs.)
names(Neu_BevolkY)

#spaltennamen ändern um es besser abzulesen
names(Neu_BevolkY)[names(Neu_BevolkY)=="dataFrame_Ertrag_Y.data_Ertrag..Monatskarten..Berlin.ABC..450012.."]="Monatskarten ABC"
names(Neu_BevolkY)[names(Neu_BevolkY)=="dataFrame_Ertrag_Y.data_Ertrag..ABO..450014..Nutzerfin.."]="Abo"
names(Neu_BevolkY)[names(Neu_BevolkY)=="dataFrame_Ertrag_Y.data_Ertrag..Firmenticket..450015.."]="Firmenticket"
names(Neu_BevolkY)[names(Neu_BevolkY)=="dataFrame_Ertrag_Y.data_Ertrag..Semester...Hochschulticket..450016.."]="Semesterticket"
names(Neu_BevolkY)[names(Neu_BevolkY)=="dataFrame_Ertrag_Y.data_Ertrag..Gesamt.vor.EAVs."]="Gesamt vor EAVs"


#Korrelationsmatrix bestimmen

CorY<-cor(Neu_BevolkY[,c("Monatskarten ABC", "Abo" ,"Firmenticket" , "Semesterticket", "Gesamt vor EAVs" )])

'
Ausgabe: Matrix mit den Werten bezüglich des Grades der Korrelation
die Variable "Abo" würde ich aus der Korrelation entfernen da ihr Wert bei
über 0.7 liegt!
'

#Methode 2 um plot für Korrealtionsmatrix zu erstellen
#Q52
install.packages("ggcorrplot")
library(ggcorrplot)

#angabe der Korrelationswerte mit im Plot
ggcorrplot(CorY,hc.order="TRUE", lab="TRUE")


####################################################################################
#Multikollinearität (Korrelation) zwischen den unabh. Variablen


CorX<-cor(Neu_BevolkX[,c("Arbeitslose", "Azubis insgesamt" ,"Studierende" , "Bevoelkerungsab-und zunahme" )])

#angabe der Korrelationswerte mit im Plot
ggcorrplot(CorX,hc.order="TRUE", lab="TRUE")

boxplot(Neu_BevolkY$ABO)


####################################################################################
#Hauptkomponentenanalyse durchführen um Daten zu reduzieren

' UTUBE Video von Godbensen anschauen was ich runtergeladen habe! Top Erklärung!
Literatur dazu wäre Q52! Brauche aber noch mehr Literatur wegen der Theorie dahinter
--> Q15 wäre vlcht gut. Am besten alle Quellen zu PCA nochmal durchgehen

Udemy Video auch anschauen bzgl PCA!

'


library(psych)
# hiermit prüft man ob die Daten für PCA geeignet sind
KMO(Neu_BevolkY)


'
Ausagbe: 0.6239344
Solange es nicht unter .5 ist, ist es noch für Hauptkomponentenanalyse
geeignet

'

KMO(Neu_BevolkY)$MSA

#schauen wieviele Hauptkomponenten es gibt
scree(Neu_BevolkY)

'--------------------------------------------------------------------------------'

#Methode 2) nutzen Q52 als Quelle
install.packages("FactoMineR")
library(FactoMineR)
install.packages("factoextra")
library(factoextra)

#daten vergleichbar machen
res_BevolkX_pca<-PCA(Neu_BevolkX, scale.unit = TRUE, ncp = 4, graph = TRUE)
#Ausgabe plot als kreis wo die dimensionen irgendwie angezeigt werden

#PCA Anwendung OHNE plot
res_BevolkX_pca<-PCA(Neu_BevolkX, graph = FALSE)

#Eigenwerte ausgeben lassen um zu wissen wieviele Hauptkomponenten wir haben
eig.val <- get_eigenvalue(res_BevolkX_pca)

'==> Ausgabe:

      eigenvalue     variance.percent       cumulative.variance.percent
Dim.1 2.69952988        67.488247                    67.48825
Dim.2 0.94600390        23.650097                    91.13834
Dim.3 0.27418547         6.854637                    97.99298
Dim.4 0.08028075         2.007019                   100.00000


Erklärung: Die Zeilen geben die Hauptkomponenten(=Dimension) wieder.
Dh, Hauptkomponente 1 == Dimension 1 usw.

Die zweite Spalte erklärt der Bezug des Eigenwertes zur Varianz.Die Dritte Spalte 
erklärt die Summe der Prozente, die sich auf die Eigenwerte beziehen. 
Das heißt, hier erklären die ersten beiden Eigenwerte bzw. Hauptkomponenten 
91.14 % der Varianz! Q52,S.49

Q52, S.50: Es gibt keine Faustregel wieviele Hauptkomponenten man wählen soll,
aber über 70% wäre nicht schlecht! Hier sind wir bei den ersten beiden Hauptkomp.
schon bei 91 %!


'
#screeplot erstellen um zu wissen wieviele Hauptkomponenten man hat

fviz_eig(res_BevolkX_pca, addlabels = TRUE, ylim = c(0, 50))

'==> Ausgabe:
Stabdiagramm wo auf jedem einzelnen einen Stab die Prozentangabe bezüglich der
Hauptkomponente(=Dimension) steht. Der erste Balken hat die höchste Prozentanzahl
die man nicht lesen kann, da der Stab zu hoch ist.

'

# relevante Variablen aus dem Datensatz extrahieren

var_pca_BevolkX <- get_pca_var(res_BevolkX_pca)

var_pca_BevolkX$coord
'==>Ausgabe:

                                Dim.1       Dim.2      Dim.3      Dim.4
Arbeitslose                  0.8584069 -0.30033468  0.4157251 0.01045200
Azubis insgesamt             0.9353480 -0.17280297 -0.2481097 0.18358863
Studierende                 -0.9610095 -0.05386354  0.1722792 0.20947396
Bevoelkerungsab-und zunahme  0.4052805  0.90721598  0.1005958 0.05086635

Erklärung:
Korrelation zwischen der Hauptkomponenten und der Variable!
'

#Korrelation zwischen Hauptkomponente und Variable wiedergeben
fviz_pca_var(res_BevolkX_pca, col.var = "black")

'Q52, S.52-54:
positive Korrelation == je mehr Var 1 desto mehr Var 2.In dem Plot haben jene
Variablen eine positive Korrelation die in einem Quadranten gemeinsam eingezeichnet
liegen! 

negative Korrelation == je weniger Var1 desto weniger Var2

Ansonsten sieht man die typischen Korrelationswerte von -1 bis 1 wobei ab >=0.7
eine starke Korrelation existiert! Dh, diese Korrelation besagt das je höher dieser
Wert ist, desto besser wird die Variable DURCH die Hauptkomponente wieder gegeben!
UND
Das heißt aber auch: Je näher die Pfeile der Variablen jeweils an dem Kreisrand
/Kreisumfang sind, desto besser ist die Repräsentanz der Variablen durch 
die Hauptkomponenten!

Es werden hier Dim1 und Dim2 angegeben also zwei Hauptkomponenten!Demnach wählen
wir auch zwei Hauptkomponenten.

'

# Qualität der Repräsentation der Variablen auf dem factor map bzw.
# der Hauptkomponenten bezüglich der Variablen
#Wie gut repräsentieren die Hauptkomponenten die Variablen?

var_pca_BevolkX$cos2
# Je höher der Wert im cos2-plot ist desto besser ist die Repräsentation der 
#Hauptkompenente bezüglich der Variable, Intervall geht von 0 bis 0.92

'==> Ausgabe:
                                Dim.1      Dim.2      Dim.3        Dim.4
Arbeitslose                 0.7368624 0.09020092 0.17282740 0.0001092442
Azubis insgesamt            0.8748759 0.02986087 0.06155841 0.0337047843
Studierende                 0.9235392 0.00290128 0.02968013 0.0438793405
Bevoelkerungsab-und zunahme 0.1642523 0.82304083 0.01011952 0.0025873856

Die Summe aller Zeilen jeweils ist gleich 1.

Werte gehen von null bis 0.92! Bis zur Spalte Dim2 also bis zwei Hauptkomponenten
werden alle Variablen gut vertreten! Daher würden wir hier zwei Hauptkomponenten
wählen!
'

library(corrplot)

corrplot(var_pca_BevolkX$cos2, is.corr=FALSE)
'==> Ausgabe plot:
Das Ergebnis ist dasselbe wie oben bloß das wir einen plot haben, und die 
Repräsentanz durch farbige Kreise wiedergegeben wird!

Werte gehen von null bis 0.92! Bis zur Spalte Dim2 also bis zwei Hauptkomponenten
werden alle Variablen gut vertreten! Daher würden wir hier zwei Hauptkomponenten
wählen!
'

# Repräsentanz durch Balkendiagramm wiedergeben, axes=1:2 da wir nur bis zur
#zweiten Hauptkompente die Repräsentanz betrachten
fviz_cos2(var_pca_BevolkX, choice = "var", axes = 1:2)
#---> geht nicht


#farbiger plot wegen repräsentanz---> geht nicht
fviz_pca_var(var_pca_BevolkX, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE )


#-----------------------------------------------------------------------------#

#Mitwirkung der Variablen bezüglich der Hauptkomponenten
# Kein plan was damit genau gemeint ist

var_pca_BevolkX$contrib
'==>Ausgabe: 

                                Dim.1     Dim.2     Dim.3      Dim.4
Arbeitslose                 27.295954  9.534942 63.033026  0.1360777
Azubis insgesamt            32.408455  3.156527 22.451376 41.9836416
Studierende                 34.211114  0.306688 10.824839 54.6573593
Bevoelkerungsab-und zunahme  6.084476 87.001843  3.690759  3.2229213



Je höher der Wert ist desto stärker korreliert die Variable mit der Hauptkomponente!
Die Werte hier bei der Ausgabe sind aber eher verwirrend!

'

# Plot mit farbigen Kreisen um Ausgabe davor wieder zugeben
corrplot(var_pca_BevolkX$contrib, is.corr=FALSE)

# welche Variablen sind an der Hauptkomponente 1 beteiligt daher axes = 1
# top == Anzahl der Variablen die man betrachten möchte im Datensatz
# man könnte hier top= auch weglassen da wir alle betrachten!
fviz_contrib(res_BevolkX_pca, choice = "var", axes = 1, top = 4)

#axes = 2 ==> Hauptkomponente 2
fviz_contrib(res_BevolkX_pca, choice = "var", axes = 2, top = 4)

'==> Ausgabe:
Balkendiagramme die die Beteiligung der Variablen bezüglich der Hauptkomponente
1 und 2 seperat betrachtet (axes=1 ==> Hauptkomponente 1, axes=2 ==> analog)

die Rote linie im plot gibt die erwartete Durchschnittsbeteiligung der Variable
zur Hauptkomponente wieder. Wenn der Balken pber der roten Line liegt, dann
ist diese Variable wichtig bezüglich der Hauptkomponente für die weitere 
Betrachtung!

ZUSAMMENFASSEND:
Es geht hierbei darum, dass alle Variablen gemeinsam den stärksten Einfluss bezüglich
der Hauptkomponente gemeinsam haben. Wenn eine pder zwei nur über der roten Linie
ist und die anderen nicht, dann ist die Hauptkomponente weniger geeignet dafür!
==> Man betrachtet ja sowieso vorher die anderen plots um zu entscheiden, wieviele
Hauptkomponenten man nimmt! 


'

#farbiges Kennzeichnen jener Variablen die am meisten an der Hauptkomponente
#beteiligt sind
fviz_pca_var(res_BevolkX_pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

#besser
fviz_pca_var(res_BevolkX_pca, col.var = "contrib",
             gradient.cols = c("blue", "green", "red"),
             legend.title = "Cont.Var")


'==> Ausgabe plot:
farbige Kreisplot wieder wo die Pfeile die Variablen darstellen und die Farben
die Beteiligung zu der Hauptkomponente jeweils verdeutlichen!
Man sieht das die Variablen durch zwei Hauptkomponenten relativ gut repräsentiert
werden!

Q52,S 59: farbige Gruppierung ist hier auch möglich aber eher für die Clusteranalyse
geeignet und wäre hier schon zu viel des Guten!

'

# Dimensions erklärung
res.desc <- dimdesc(res_BevolkX_pca, axes = c(1,2), proba = 0.05)

'==> Ausgabe:
$Dim.1
$quanti
                            correlation      p.value
Azubis insgesamt              0.9353480 1.190640e-49
Arbeitslose                   0.8584069 1.670459e-32
Bevoelkerungsab-und zunahme   0.4052805 1.355142e-05
Studierende                  -0.9610095 5.338851e-61

attr(,"class")
[1] "condes" "list"  

$Dim.2
$quanti
                            correlation      p.value
Bevoelkerungsab-und zunahme   0.9072160 1.166714e-41
Arbeitslose                  -0.3003347 1.588044e-03


Erklärung:
axes(1,2) == Hauptkomponenten 1 und 2

Hier sieht man die direkte Einwirkung bzw. Korrelation der Variablen bzgl.
der Hauptkomponenten jeweils! Man könnte hier alle Hauptkomponenten vergleichen
und dann schauen welche am meisten relevant wären!

==> Gute Darstellung die ich nutzn könnte!
'
# ZUSAMMENGEFASST kann man also folgendes nutzen:

#Alle Ergebnisse der Zeilen (Individuals) bzgl aller Hauptkomponenten  wiedergeben
# 

ind <- get_pca_ind(res_BevolkX_pca)

#Koordinaten der Werte aller Zeilen bzgl der Hauptkomponenten
ind$coord

#Qualität der Zeilen bzgl. der Hauptkomponenten
ind$cos2

# Mitwirkung der Hauptkomponenten bzgl der Variablen
ind$contrib

#Alle Zeilen werden in einem Diagramm wiedergegeben
fviz_pca_ind(res_BevolkX_pca)

#farbig alle zeilen --> gute Darstellung
fviz_pca_ind(res_BevolkX_pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# farbig und mit größe der punkte --> weniger geeignet da sich punkte überlappen
fviz_pca_ind(res_BevolkX_pca, pointsize = "cos2",
             pointshape = 21, fill = "#E7B800",
            repel = TRUE # Avoid text overlapping (slow if many points)
)

# farbig und punkte aber immernoch nicht sehr viel besser
fviz_pca_ind(res_BevolkX_pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

# GUTE WAHL! das nehmen
fviz_pca_ind(res_BevolkX_pca, col.ind = "cos2",
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")


#balkendiagramm aber nicht gut!
fviz_cos2(res_BevolkX_pca, choice = "ind")

#balkendiagramm aber nicht gut!
fviz_contrib(res_BevolkX_pca, choice = "ind", axes = 1:2)

#Kreis plot aber nicht gut!
fviz_pca_var(res_BevolkX_pca, geom.var = c("point", "text"))

'--------------------------------------------------------------------------------'

#plot in pdf speichern

pdf("myplot.pdf")
print(myplot)
dev.off()

# Scree plot --> Übernehmen
scree.plot <- fviz_eig(res_BevolkX_pca)

# Plot of individuals-->Übernehmen aber mit farben
ind.plot <- fviz_pca_ind(res_BevolkX_pca)

# Plot of variables--> übernehmen aber davor gabs bessere
var.plot <- fviz_pca_var(res_BevolkX_pca)


pdf("PCA.pdf") # Create a new pdf device
print(scree.plot)
print(ind.plot)
print(var.plot)
dev.off() # Close the pdf device


# Print scree plot to a png file
png("pca-scree-plot.png")
print(scree.plot)
dev.off()
# Print individuals plot to a png file
png("pca-variables.png")
print(var.plot)
dev.off()
# Print variables plot to a png file
png("pca-individuals.png")
print(ind.plot)
dev.off()

#AM BESTEN--> das übernehmen
library(ggpubr)
ggexport(plotlist = list(scree.plot, ind.plot, var.plot),
         filename = "PCA.pdf")

ggexport(plotlist = list(scree.plot, ind.plot, var.plot),
         filename = "PCA.png")


'--------------------------------------------------------------------------------'


####################################################################################

#Multivariate Regression 

' Für jede Y-Variable wird wohl eine seperate multiple Regression angelegt
und dann diese Regressionen miteinander verglichen bzw. verbunden.(Q27 ab S.238)


1)Y und X-Variablen in einem Vektor speichern (kann man machen)
2)Zu jeder Y-Variable jeweils eine multiple Regression (Regressionsmodell) bilden
3) Alle Regressionsmodelle in einem Vektor abspeichern. Man will somit die Resiuden
abspeichern.

4)formaler Test ob Residuen normalverteilt sind obwohl das eigentlich nicht nötig ist!
Es wird dieses nämlich nachfolgend nicht vorausgesetzt!

5) Visuelle Untersuchung Residuen-Normalverteilung durch QQplot
Nutzung vom befehl :
qqplot(rstandard(mlm1))
---> rstandard() == verursacht dass das Residuum durch die Std-Abweichung geteilt
wird! Das passiert aber nur mit diesem Befehl! Dh immer wenn ich die Residuen
betrachte so muss ich rstandard() dafür nutzen!

ABER Normalverteilung ist aber großer Datenmenge unwichtig weswegen das auch wegfällt!
Nach Q15 gilt das bzw. ab wann gilt es als große datenmenge? In Q38 steht das


6)Visuelle Untersuchung der Residuen-Normalverteilung durch Histogramm 
Auch mit rstandard()


7)Test der Normalverteilung durch Shapiro-Wilks
--> bringt sowieso nix da parametrische Tests bei der großen Datenmenge nur
Quatsch ausgeben

8)PCA auf Y-variablen anwenden bzw. auf die linearen Modelle
(hier eher bezug auf das Video bzw. eine andere Quelle außer Q27 nehmen!)

'




#multivariate regression bilden indem man multiple regression ausführt
mlm0<- lm(cbind(Neu_BevolkY$`Monatskarten ABC`, Neu_BevolkY$ABO,Neu_BevolkY$Firmenticket,Neu_BevolkY$Hochschulticket) ~ Neu_BevolkX$Arbeitslose + Neu_BevolkX$`Azubis insgesamt` + Neu_BevolkX$Studierende + Neu_BevolkX$`Bevoelkerungsab-und zunahme` )#= ami_data)

summary(mlm1)

'==> Ausgabe:
Zu jeder Y-Variable wird der Summary bezüglich der X-Variablen ausgegeben!
Das heißt, man könnte demnach pro Y-Variable jeweils eine multiple Regression 
coden. Das Ergebnis wäre dann dasselbe 

Bsp:
mlm1 <- lm(Neu_BevolkY$`Monatskarten ABC` ~ Neu_BevolkX$Arbeitslose + Neu_BevolkX$`Azubis insgesamt` + Neu_BevolkX$Studierende + Neu_BevolkX$`Bevoelkerungsab-und zunahme` )#= ami_data)

So wie man das eben so kennt!

'
# 2) multiple Regressionsmodelle bilden
mlm1 <- lm(Neu_BevolkY$`Monatskarten ABC` ~ Neu_BevolkX$Arbeitslose + Neu_BevolkX$`Azubis insgesamt` + Neu_BevolkX$Studierende + Neu_BevolkX$`Bevoelkerungsab-und zunahme` )#= ami_data)
mlm2 <- lm(Neu_BevolkY$ABO ~ Neu_BevolkX$Arbeitslose + Neu_BevolkX$`Azubis insgesamt` + Neu_BevolkX$Studierende + Neu_BevolkX$`Bevoelkerungsab-und zunahme` )#= ami_data)
mlm3 <- lm(Neu_BevolkY$Firmenticket ~ Neu_BevolkX$Arbeitslose + Neu_BevolkX$`Azubis insgesamt` + Neu_BevolkX$Studierende + Neu_BevolkX$`Bevoelkerungsab-und zunahme` )#= ami_data)
mlm4 <- lm(Neu_BevolkY$Hochschulticket~ Neu_BevolkX$Arbeitslose + Neu_BevolkX$`Azubis insgesamt` + Neu_BevolkX$Studierende + Neu_BevolkX$`Bevoelkerungsab-und zunahme` )#= ami_data)

summary(mlm1)
summary(mlm2)
summary(mlm3)
summary(mlm4)


#3) Residuen der multiplen Regressionsmodelle speichern

Bevolk.res<- cbind(mlm1$residuals,mlm2$residuals,mlm3$residuals,mlm4$residuals)

#anpassen der Variablennamen in der Ausgabe wegen den Residuen
colnames(Bevolk.res)<- c("Monatskarten_res", "ABO_res","Firmenticket_res","Hochschulticket_res" )
Bevolk.res <- data.frame(Bevolk.res)
print(Bevolk.res, digits=2)

'==> Ausgabe:
Alle Residuenwerte bezüglich aller Zeilen und Y-Variablen werden ausgegeben.
Die Werte wurden mithilfe der letzten zwei Nachkommazahlen auf-bzw. abgerundet.
Daher steht da digits=2!



'
#4)
library(mvnormtest)
mshapiro.test( t( Bevolk.res))

'==> Ausgabe:
>  mshapiro.test( t( Bevolk.res))

	Shapiro-Wilk normality test

data:  Z
W = 0.81232, p-value = 2.07e-10


==> Dh eine Normalverteilung liegt also nicht vor! Wir wissen aber, dass es sowieso
nicht vorausgesetzt wird und das diese parametrischen Tests aufgrund der Datenmenge
versagen!


'

#5) QQ-Plot bestimmen für jede X-bzw. Y-Variable seperat

#Lineare Modelle
'Alle Modelle sind normalverteilt da es durch rstandard ausgeglichen wurde'
qqnorm(rstandard(mlm1))
qqline(rstandard(mlm1))


qqnorm(rstandard(mlm2))
qqline(rstandard(mlm2))


qqnorm(rstandard(mlm3))
qqline(rstandard(mlm3))


qqnorm(rstandard(mlm4))
qqline(rstandard(mlm4))

'

Frage: Vlcht sollte man deswegen die PCA für die X-Variablen vorher machen um die 
Daten vorzubereiten und dann erst später für die Y-Variablen?

Da durch die Standardabweichung geteilt wurde scheinen alle Modelle normalverteilt 
zu sein!
'

#6)
hist(rstandard(mlm1))
hist(rstandard(mlm2))
hist(rstandard(mlm3))
hist(rstandard(mlm4))

'==> Ausgabe:
Bei den plots sieht man das um den Wert 0 die Werte sich anhäufen also eine
Normalverteilung demnach vorherrscht! Es existeren keine perfekte Normalverteilungen
aber man muss eine ungefähre Tendenz dahingehend sehen können!

'



#7)
shapiro.test(rstandard(mlm1))
shapiro.test(rstandard(mlm2))
shapiro.test(rstandard(mlm3))#--> nicht normalverteilt nach shapiro
shapiro.test(rstandard(mlm4))#--> nicht normalverteilt nach shapiro


'==> wir wissen das parametrische Tests bei einer großen Datenmenge zu falschen
Ergebnissen führt! Daher nutzen wir eher visuelle Mittel!


'

#8)

print(prcomp(Bevolk.res, scale = TRUE), digits = 3)

'--------------------------------------------------------------------------------'
# Zweites Vorgehen um multiple Regression also eigentlich multivariate Regression
#umzusetzen

' Folgende Voraussetzungen müssen für die multiplen Regression gelten

(Q27 ab S.238 wie man allgemein vorgeht
Q52 plots die man benutzen kann zur darstellung! Q10 top erklärt!

0)multiples Regressionsmodell erstellen
1)Linearität der X-und Y-Variable zueinander (Streudiagramm und R-Quadrat angeben, 
qq-plot)
2)Normalverteilung der Residuen prüfen (qq-plot, histogramm)
3)Korrelation zwischen den X-Variablen prüfen (Multikollinearität)
4)Varianzhomogenität der Residuen muss gegeben sein.Zusätzlich Varianzheterogenität
noch prüfen (Streudiagramm)
5)Autokorrelation aber Herr Winter meinte ist unnötig
6)Ausreißer

'
install.packages("olsrr")
install.packages("jtools")
install.packages("moments")
install.packages("lmtest")

library(olsrr)
library(moments)
library(lmtest)


# test ob man ausreißer entfernen kann Q79,S.197

fitHas <- lm(Neu_BevolkY$`Monatskarten ABC` ~ Neu_BevolkX$Arbeitslose + Neu_BevolkX$`Azubis insgesamt` + Neu_BevolkX$Studierende + Neu_BevolkX$`Bevoelkerungsab-und zunahme` )#= ami_data)

h<- hatvalues(fitHas)

summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte!'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'==>funktioniert! Es werden 10 werte angegeben die als Ausreißer gelten. Diese kann
ich dann löschen! Das passt zu dem Ergebnis was ich damals ungefähr über einen 
Code bestimmt hatte. DA waren es auch um die 10


'


#0)
mlm1 <- lm(Neu_BevolkY$`Monatskarten ABC` ~ Neu_BevolkX$Arbeitslose + Neu_BevolkX$`Azubis insgesamt` + Neu_BevolkX$Studierende + Neu_BevolkX$`Bevoelkerungsab-und zunahme` )#= ami_data)
mlm2 <- lm(Neu_BevolkY$ABO ~ Neu_BevolkX$Arbeitslose + Neu_BevolkX$`Azubis insgesamt` + Neu_BevolkX$Studierende + Neu_BevolkX$`Bevoelkerungsab-und zunahme` )#= ami_data)
mlm3 <- lm(Neu_BevolkY$Firmenticket ~ Neu_BevolkX$Arbeitslose + Neu_BevolkX$`Azubis insgesamt` + Neu_BevolkX$Studierende + Neu_BevolkX$`Bevoelkerungsab-und zunahme` )#= ami_data)
mlm4 <- lm(Neu_BevolkY$Hochschulticket~ Neu_BevolkX$Arbeitslose + Neu_BevolkX$`Azubis insgesamt` + Neu_BevolkX$Studierende + Neu_BevolkX$`Bevoelkerungsab-und zunahme` )#= ami_data)

summary(mlm1)
summary(mlm2)
summary(mlm3)
summary(mlm4)

#1) Linearität

'man muss hier den Datenframe benutzen'
pairs(Neu_BevolkX,pch=19,lower.panel= NULL)


'test für keine Linearität -> p-wert muss über 0.05 sein damit Linear ist'
raintest(mlm1)
raintest(mlm2)
raintest(mlm3)
raintest(mlm4)




'für jede X-Variable bzgl der Y-Variable seperat erstellt
==> beim Plot kommt X-Variable zuerst und DANN die Y-Variable! Sonst entsteht
ein anderes bild!

'
mlm1_1<-lm(Neu_BevolkY$`Monatskarten ABC`~Neu_BevolkX$Arbeitslose)
plot(Neu_BevolkX$Arbeitslose,Neu_BevolkY$`Monatskarten ABC`)#,Neu_BevolkX$`Azubis insgesamt`,Neu_BevolkX$Studierende,Neu_BevolkX$`Bevoelkerungsab-und zunahme`)
abline(lm(Neu_BevolkY$`Monatskarten ABC`~Neu_BevolkX$Arbeitslose))
legend("topright" , bty="n" , legend ="paste" ("R² = " , format(summary(mlm1_1)$r.squared)) )


mlm1_2<-lm(Neu_BevolkY$`Monatskarten ABC`~Neu_BevolkX$`Azubis insgesamt`)
plot(Neu_BevolkX$`Azubis insgesamt`,Neu_BevolkY$`Monatskarten ABC`)
abline(lm(Neu_BevolkY$`Monatskarten ABC`~Neu_BevolkX$`Azubis insgesamt`))
legend("topright" , bty="n" , legend ="paste" ("R² = " , format(summary(mlm1_2)$r.squared)) )


mlm1_3<-lm(Neu_BevolkY$`Monatskarten ABC`~Neu_BevolkX$Studierende)
plot(Neu_BevolkX$Studierende,Neu_BevolkY$`Monatskarten ABC`)
abline(lm(Neu_BevolkY$`Monatskarten ABC`~Neu_BevolkX$Studierende))
legend("topright" , bty="n" , legend ="paste" ("R² = " , format(summary(mlm1_3)$r.squared)) )


mlm1_4<-lm(Neu_BevolkY$`Monatskarten ABC`~Neu_BevolkX$`Bevoelkerungsab-und zunahme`)
plot(Neu_BevolkX$`Bevoelkerungsab-und zunahme`, Neu_BevolkY$`Monatskarten ABC`)
abline(lm(Neu_BevolkY$`Monatskarten ABC`~Neu_BevolkX$`Bevoelkerungsab-und zunahme`))
legend("topright" , bty="n" , legend ="paste" ("R² = " , format(summary(mlm1_4)$r.squared)) )


summary(mlm1)

'==> Ergebnis:
Bei allen Plots sieht man Geraden die durch die Punkte gehen. Das heißt, es existiert
kein linearer Zusammenhang!Das erkennt man auch an dem R-Quadrat was darauf hinweist!


'


#2) Normalität
'Nutzung von rstandard() um die Werte durch z-standardisierung vergleichbar 
zu machen

==> Histogramm allgemein weglassen, da sie bei großen datenmengen unzuverlässig
sind!
'

'qq plot'
qqnorm(rstandard(mlm1))
qqline(rstandard(mlm1))


qqnorm(rstandard(mlm2))
qqline(rstandard(mlm2))


qqnorm(rstandard(mlm3))
qqline(rstandard(mlm3))


qqnorm(rstandard(mlm4))
qqline(rstandard(mlm4))

'Histogramm'
hist(rstandard(mlm1))
hist(rstandard(mlm2))
hist(rstandard(mlm3))
hist(rstandard(mlm4))

'oder mit'

ols_plot_resid_hist(mlm1)
ols_plot_resid_hist(mlm2)
ols_plot_resid_hist(mlm3)
ols_plot_resid_hist(mlm4)

ols_plot_resid_qq(mlm1)
ols_plot_resid_qq(mlm2)
ols_plot_resid_qq(mlm3)
ols_plot_resid_qq(mlm4)

#3) Multikollinearität
'Bsp dazu damit ich für später weiß wie ichs umsetze! Q10'

CorY<-cor(Neu_BevolkY[,c("Monatskarten ABC", "Abo" ,"Firmenticket" , "Semesterticket", "Gesamt vor EAVs" )])

'
Ausgabe: Matrix mit den Werten bezüglich des Grades der Korrelation
die Variable "Abo" würde ich aus der Korrelation entfernen da ihr Wert bei
über 0.7 liegt!
'

'Methode 2 um plot für Korrealtionsmatrix zu erstellen -->Q52'

install.packages("ggcorrplot")
library(ggcorrplot)

'angabe der Korrelationswerte mit im Plot'
ggcorrplot(CorY,hc.order="TRUE", lab="TRUE")


'Oder diese methode um Multikollinearität zu prüfen'

ols_vif_tol(mlm1)
ols_vif_tol(mlm2)
ols_vif_tol(mlm1)
ols_vif_tol(mlm1)



#4) Varianzhomogenität durch Korrektur gegeben
plot(mlm1,1)
plot(mlm2,1)
plot(mlm3,1)
plot(mlm4,1)

'oder'

plot(fitted.values(mlm1), residuals(mlm1))
plot(fitted.values(mlm1), rstandard(mlm1)) #ist ähnlich




'oder mit'

ols_plot_resid_fit(mlm1)
ols_plot_resid_fit(mlm2)
ols_plot_resid_fit(mlm3)
ols_plot_resid_fit(mlm4)

'Varianzheterogenität prüfen'

ols_test_breusch_pagan(mlm1)
ols_test_breusch_pagan(mlm2)
ols_test_breusch_pagan(mlm3)
ols_test_breusch_pagan(mlm4)

'alle modelle scheinen varianzheterogen zu sein! wir beheben das indem wir
robuste Werte bilden: '

install.packages("sandwich")
library(sandwich)
library(lmtest)

coeftest(mlm1,vcov=vcovHC(mlm1,type=c("HC3")))
'Ausgabe: Summary ausgabe wo sich standardfehler erhöhen und dadurch die 
t-werte kleiner werden! Man nutzt diese Werte nun im Folgenden bei der Analyse
bzw. Diagnositk da sie robuste Werte sind!

'

coeftest(mlm2,vcov=vcovHC(mlm1,type=c("HC3")))
coeftest(mlm3,vcov=vcovHC(mlm1,type=c("HC3")))
coeftest(mlm4,vcov=vcovHC(mlm1,type=c("HC3")))


'fehlende Varianzhomogenität gibt Hinweis auf fehlende Linearität!
Es wird hiermit nur die Präzision der Schätzung mit der KQ-Methode verringert. 


'


#5)Autokorrelation


install.packages("sfsmisc")
library(sfsmisc)

'Tukey Anscombe-Plot um Autokorelation festzustellen'
TA.plot(mlm1,
        fit= fitted(lm.res), res= residuals(mlm1, type="pearson"),
        labels= NULL, main= mk.main(), xlab = "Fitted values",
        draw.smooth= n >= 10, show.call = TRUE, show.2sigma= TRUE,
        lo.iter = NULL, lo.cex= NULL,
        par0line  = list(lty = 2, col = "gray"),
        parSmooth = list(lwd = 1.5, lty = 4, col = 2),
        parSigma  = list(lwd = 1.2, lty = 3, col = 4),
        verbose = FALSE,
          )

'habe noch keinen code dazu gefunden aber frage noch wenns sein muss!'


'nutzung durbin-watson test'
library(lmtest)
dwtest(mlm1)

'==> Ausgabe:

Durbin-Watson test

data:  mlm1
DW = 0.75425, p-value = 2.809e-12
alternative hypothesis: true autocorrelation is greater than 0


Erklärung:
nach Q73 S.35 gilt:

dW= 2 ==> Keine Autokorrelation

alles andere was abweicht weist auf eine Autokorrelation hin!es ist aber fraglich
ob bei dieser Datenmenge der Test gültig ist

'



#6)
'Ausreißer müssen nicht alle entfertn werden, da nicht alle negativen Einfluss
haben! Ich würde alle entfernen um das so zu vereinfachen!
--> Habe früheren Code dazu noch!Ansonsten gucke ich bei udemy nochmal

'





'---------------------------------------------------------------------------------'
#vlcht kommt hier noch ein schritt davor wegen Q10 erklärung und Ablauf

'-------------------------------------------------------------------------------'
#Güte der multiplen Regression


data(c)

####################################################################################

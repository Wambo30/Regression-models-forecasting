#Data cleaning und Vorbereitung der Daten - Bezug auf Ex1_Bev�lkerungsentwicklung
# vorbereitung der multivariaten daten f�r die exogene Variable Bev�lkerungsentwicklung

'Die Vorbereitung der Daten ist hinreichend damit das Ergebnis der Regression
nicht verzerrt wird. Hierbei untersucht man die X-Variablen und Y-Variablen
jeweils untereinander.

- Bezug auf bev�lkerungsdaten in Berlin

X-variablen == 1)Bev�lkerungsab-/zunahme, 2) Sch�ler Berlin, 3)Studierende
4)�bernachtungen

Y-Variablen == 1)Gesamt nach EAV , 2) Monatskarten Berlin ABC,
3)Jahreskarten Berlin ABC , 4)Firmenticket, 5) Semester-Hochschulticket



#########################################################################


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
syntax- zeilen in datensatz komplett l�schen

mydataframe[-c(row_index_1, row_index_2),]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'

'-------------------------------------------------------------------------------'
# dataframe f�r Y-Variablen erstellen

dataFrame_Ertrag_Y<-data.frame(data_Ertrag$`Einzelfahrscheine Berlin ABC (450010)`,data_Ertrag$`Tageskarten Berlin ABC (450019)`,data_Ertrag$`Monatskarten  Berlin ABC (450012)`,data_Ertrag$`ABO (450014) Nutzerfin.`,data_Ertrag$`Firmenticket (450015)`,data_Ertrag$`Semester-/ Hochschulticket (450016)`,data_Ertrag$`Berlin Ticket S (450018)`,data_Ertrag$`Gesamt vor EAVs`)
'
Y-Variablen:
Einzelfahrscheine.Berlin.ABC, Tageskarten.Berlin.ABC, Monatskarten..Berlin.ABC,
ABO..450014..Nutzerfin, Firmenticket..450015, Semester...Hochschulticket,
Berlin.Ticket.S, Gesamt.vor.EAVs


'

is.na(dataFrame_Ertrag_Y)

#l�schen der zeilen mit missing values
#dataFrame_Ertrag_Y<-na.omit(dataFrame_Ertrag_Y)

'
das bruache ich warscheinlich nicht mehr! 

# ab zeile 86 (01.01.2012) bis zeile 192 (01.12.2020)
dataFrame_Ertrag_Y<-dataFrame_Ertrag_Y[-c(1:84),]

'
# ab zeile 86 (01.01.2012) bis zeile 192 (01.12.2020)
dataFrame_Ertrag_Y<-dataFrame_Ertrag_Y[-c(1:84),]

names(dataFrame_Ertrag_Y)
is.na(dataFrame_Ertrag_Y)

str(dataFrame_Ertrag_Y)
unique(dataFrame_Ertrag_Y)
class(dataFrame_Ertrag_Y)
names(dataFrame_Ertrag_Y)
sapply(dataFrame_Ertrag_Y,class)

'-------------------------------------------------------------------------------'
#data frame f�r X-variablen erstellen 

dataFrame_Treiber_X<- data.frame(data_Treiber$Arbeitslose, data_Treiber$`Auszubildende Insgesamt`,data_Treiber$Superbenzin,data_Treiber$Dieselkraftstoff, data_Treiber$`Bev�lkerungsstand Monatsanfang`,data_Treiber$`Bev�lkerungfsab/-zunahme`,data_Treiber$Arbeitstage,data_Treiber$Samstage,data_Treiber$`Sonn-/Feiertage`,data_Treiber$Ferientage,data_Treiber$April,data_Treiber$August,data_Treiber$December,data_Treiber$February,data_Treiber$January,data_Treiber$July,data_Treiber$June,data_Treiber$March,data_Treiber$May,data_Treiber$November,data_Treiber$October,data_Treiber$September,data_Treiber$`Tage mit m��igem Regen`,data_Treiber$`Tage mit Schnee`,data_Treiber$`Auspendler Brandenburg`,data_Treiber$`Einpendler insgesamt`,data_Treiber$Studierende,data_Treiber$�bernachtungen,data_Treiber$Corona, data_Treiber$`BVG Streik`,data_Treiber$Stau)
'
X-Variablen:
Arbeitslose, Auszubildende.Insgesamt, Superbenzin, Dieselkraftstoff, 
Bev�lkerungsstand.Monatsanfang., Bev�lkerungfsab..zunahme.,
Arbeitstage, Samstage, Sonn..Feiertage., Ferientage, April, August,
December, February, January, July, June, March, May, November, October,
September, Tage.mit.m��igem.Regen, Tage.mit.Schnee, Auspendler.Brandenburg,
Einpendler.insgesamt, Studierende, �bernachtungen, Corona, BVG.Streik., Stau

'

names(dataFrame_Treiber_X)

'
das brauche ich nicht mehr da es auch mit der na.omit fkt geht!

# ab zeile 86 (01.01.2012) bis zeile 192 (01.12.2020)
dataFrame_Treiber_X<-dataFrame_Treiber_X[-c(1:84),]

'

#zeilen nach 192 l�schen

#suche nach missing values und anzeigen im data frame durch TRUE ausgabe
is.na(dataFrame_Treiber_X)

#l�schen der zeilen mit missing values
dataFrame_Treiber_X<-na.omit(dataFrame_Treiber_X)

dataFrame_Treiber_X

#pr�fen ob alle Variablen datentyp numeric haben
sapply(dataFrame_Treiber_X,class)

#Variable Arbeitslose gilt als factor daher m�ssen wir das in numeric umwandeln

#wenn eine Variable den datentyp factor hat, muss man diesen zuerst zum character umwandeln!
dataFrame_Treiber_X$Arbeitslose<-as.numeric(as.character(dataFrame_Treiber_X$Arbeitslose))

#pr�fen ob alle Variablen datentyp numeric haben
sapply(dataFrame_Treiber_X,class)




'
Literatur wegen na.omit code usw--> tidyverse package pdf

'

#########################################################################
names(dataFrame_Ertrag_Y)

#Spaltennamen �ndern von dataFrame_Ertrag_Y �ndern

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Einzelfahrscheine.Berlin.ABC..450010.."]="Einzelfahrscheine ABC"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Tageskarten.Berlin.ABC..450019.."]="Tageskarten ABC"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Monatskarten..Berlin.ABC..450012.."]="Monatskarten ABC"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..ABO..450014..Nutzerfin.."]="ABO"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Firmenticket..450015.."]="Firmenticket"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Semester...Hochschulticket..450016.."]="Hochschulticket"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Berlin.Ticket.S..450018.."]="Berlin Ticket"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Gesamt.vor.EAVs."]="Gesamt vor EAVs"

'-----------------------------------------------------------------------------------'

names(dataFrame_Treiber_X)

#Spaltennamen �ndern von dataFrame_Treiber_X �ndern

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.Arbeitslose"]="Arbeitslose"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber..Auszubildende.Insgesamt."]="Azubis insgesamt"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.Superbenzin"]="Superbenzin"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.Dieselkraftstoff"]="Dieselkraftstoff"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber..Bev�lkerungsstand.Monatsanfang."]="Bevoelkerungsstand Monatsanfang"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber..Bev�lkerungfsab..zunahme."]="Bevoelkerungsab-und zunahme"


names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.Arbeitstage"]="Arbeitstage"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.Samstage"]="Samstage"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.Ferientage"]="Ferientage"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber..Sonn..Feiertage."]="Sonn-und Feiertage"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.April"]="April"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.August"]="August"
names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.December"]="Dezember"
names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.February"]="Februar"
names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.January"]="Januar"
names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.July"]="Juli"
names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.June"]="Juni"
names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.March"]="Maerz"
names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.October"]="Oktober"
names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.September"]="September"
names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.May"]="Mai"
names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.November"]="November"



names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber..Tage.mit.m��igem.Regen."]="Tage mit m��igem Regen"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber..Tage.mit.Schnee."]="Tage mit Schnee"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber..Auspendler.Brandenburg."]="Auspendler Brandenburg"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber..Einpendler.insgesamt."]="Einpendler insgesamt"


names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.Corona"]="Corona"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.Stau"]="Stau"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.Studierende"]="Studierende"

names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber..BVG.Streik."]="BVG-Streik"


names(dataFrame_Treiber_X)[names(dataFrame_Treiber_X)=="data_Treiber.�bernachtungen"]="Uebernachtungen"

dataFrame_Treiber_X
names(dataFrame_Treiber_X)

'==> Bis hierhin wurde alle Variablen in den Dataframes gespeichert!
'
####################################################################################

#Ausrei�er identifizieren

' Eine Normalverteilung muss NICHT vorliegen, da wir einen gro�en
Datensatz haben und dies auch nicht notwendig ist(nach Q15,Q20)

Ich gehe hierbei jede Variable durch und schaue ob es im Boxplot ausrei�er gibt!
'

boxplot(dataFrame_Treiber_X$Arbeitslose)

boxplot(dataFrame_Treiber_X$`Azubis insgesamt`)

boxplot(dataFrame_Treiber_X$Studierende)

#hat Ausrei�er
boxplot(dataFrame_Treiber_X$`Bevoelkerungsab-und zunahme`, horizontal = TRUE)

'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

#ausrei�er von variable "Bevoelkerungsab-und zunahme" entfernen


#Methode 2)

#identifizieren von Ausrei�ern
boxplot(dataFrame_Treiber_X$`Bevoelkerungsab-und zunahme`, horizontal=TRUE)

#Anzahl der Werte feststellen
length(dataFrame_Treiber_X$`Bevoelkerungsab-und zunahme`)

boxplot(mat_BevoelkAbZu_X)
#nur werte nehmen die keine ausrei�er enthalten
mat2_BevoelkAbZu_X <- mat_BevoelkAbZu_X [ mat_BevoelkAbZu_X < 8000]

boxplot(mat2_BevoelkAbZu_X, horizontal=TRUE)
mat3_BevoelkAbZu_X <- mat2_BevoelkAbZu_X[mat2_BevoelkAbZu_X > -1000]

boxplot(mat3_BevoelkAbZu_X, horizontal=TRUE)

#schauen wonach ich mich bez�glich Quantile orientieren kann
summary(mat3_BevoelkAbZu_X)

length(mat3_BevoelkAbZu_X)


#zum dataframe umwandeln
Ohne_Aus_BevolkAbZu_X<- as.data.frame(t(mat3_BevoelkAbZu_X))
class(Ohne_Aus_BevolkAbZu_X)

dataFrame_Treiber_X$`Bevoelkerungsab-und zunahme`<-Ohne_Aus_BevolkAbZu_X


#Methode 3

boxplot(dataFrame_Treiber_X$`Bevoelkerungsab-und zunahme`,plot=FALSE)$out

Ausreisser_BevoelkAbZu <- boxplot(dataFrame_Treiber_X$`Bevoelkerungsab-und zunahme`,plot=FALSE)$out

#daten aus variable in x speichern
x<-dataFrame_Treiber_X$`Bevoelkerungsab-und zunahme`

boxplot(x)
is.recursive(x)
is.atomic(x)
x<- as.data.frame(dataFrame_Treiber_X$`Bevoelkerungsab-und zunahme`)
class(dataFrame_Treiber_X$`Bevoelkerungsab-und zunahme`)
sapply(dataFrame_Treiber_X$`Bevoelkerungsab-und zunahme`,class)
class(x)
#x<- x[-which(x$breaks %in% Ausreisser_BevoelkAbZu),]
x<- x[-which(x$breaks %in% Ausreisser_BevoelkAbZu),]

x2<-x[which(Ausreisser_BevoelkAbZu)]

boxplot(dataFrame_Treiber_X$`Bevoelkerungsab-und zunahme`)
boxplot(x)

'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'




####################################################################################

#dataframe bilden f�r x-variable von Ex1 Bev�lkerung

Neu_BevolkX<-data.frame(dataFrame_Treiber_X$data_Treiber..Bev�lkerungfsab..zunahme.,dataFrame_Treiber_X$data_Treiber..Sch�ler.Berlin.,dataFrame_Treiber_X$data_Treiber.Studierende,dataFrame_Treiber_X$data_Treiber..Auszubildende.Insgesamt.,dataFrame_Treiber_X$data_Treiber.Arbeitslose)


# test ob normalverteilung vorliegt

#schauen welchen Datentyp die Werte bzw. Spalten haben
sapply(Neu_BevolkX,class)

'
Spalte Bev�lkerungszu-und abnahme wurde als factor gespeichert. somit m�ssen wir
den Datentyp zu numeric umwandeln, ansonsten w�rden die weiteren Tests nicht
funktionieren!


#bsp wie man spalte in numeric umwandelt
Ex1_BevolkX$dataFrame_Treiber_X..Bevoelkerungsab.und.zunahme<-as.numeric(Ex1_BevolkX$dataFrame_Treiber_X..Bevoelkerungsab.und.zunahme.)

#wenn eine Variable den datentyp factor hat, muss man diesen zuerst zum character umwandeln!
Neu_BevolkX$dataFrame_Treiber_X.data_Treiber.Arbeitslose<-as.numeric(as.character(Neu_BevolkX$dataFrame_Treiber_X.data_Treiber.Arbeitslose))

'

#wenn eine Variable den datentyp factor hat, muss man diesen zuerst zum character umwandeln!
Neu_BevolkX$dataFrame_Treiber_X.data_Treiber.Arbeitslose<-as.numeric(as.character(Neu_BevolkX$dataFrame_Treiber_X.data_Treiber.Arbeitslose))

#transformieren der daten in Matrix um shapirotest zu nutzen 
transpose_Neu_BevolkX<-t(Neu_BevolkX)

trans2_Neu_BevolkX<-t(transpose_Neu_BevolkX)
'
Man MUSS die Matrix zweimal transponieren damit die Matrix das richtige Format hat!!
Alle tests au�er shapiro hatten damit funktioniert auch wenn nat�rlich die variablen
nicht normalverteilt waren!

'



class(transpose_Neu_BevolkX)

#schauen welchen Datentyp die Werte bzw. Spalten haben
sapply(trans2_Neu_BevolkX,class)




install.packages("normtest")
library(normtest)

install.packages("mvnormtest")
library(mvnormtest)

# nutzen shapirotest um Normalverteilung festzustellen
mshapiro.test(transpose_Neu_BevolkX)

#nutzen Jarque-Bera Test
jb.norm.test(trans2_Neu_BevolkX)

install.packages("normwhn.test")
library(normwhn.test)

normality.test1(trans2_Neu_BevolkX)


#nutzung energy-test um zu pr�fen ob normalverteilung vorherrscht

install.packages("energy")
library(energy)
mvnorm.e(trans2_Neu_BevolkX)


#Heinze-Zirkler test f�r multivariate Normalverteilung nutzen
# hier wird f�r jede Variable seperat die normalverteilung getetest

install.packages("MVN")
library(MVN)

mvn(
  trans2_Neu_BevolkX,
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

#normalverteilung je variable testen
install.packages("nortest")
library(nortest)

sf.test(Neu_BevolkX$dataFrame_Treiber_X.data_Treiber..Bev�lkerungfsab..zunahme.)
sf.test(Neu_BevolkX$dataFrame_Treiber_X.data_Treiber..Sch�ler.Berlin.)
sf.test(Neu_BevolkX$dataFrame_Treiber_X.data_Treiber.Studierende)
sf.test(Neu_BevolkX$dataFrame_Treiber_X.data_Treiber..Auszubildende.Insgesamt.)
sf.test(Neu_BevolkX$dataFrame_Treiber_X.data_Treiber.Arbeitslose)

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
#data frame bilden f�r Y-Variable Bev�lkerung


Neu_BevolkY<-data.frame(dataFrame_Ertrag_Y$data_Ertrag..Monatskarten..Berlin.ABC..450012..,dataFrame_Ertrag_Y$data_Ertrag..Jahreskarten.Berlin.ABC..450013..,dataFrame_Ertrag_Y$data_Ertrag..Firmenticket..450015..,dataFrame_Ertrag_Y$data_Ertrag..Semester...Hochschulticket..450016..,dataFrame_Ertrag_Y$data_Ertrag..ABO..450014..Nutzerfin..)

#schauen welchen Datentyp die Werte bzw. Spalten haben
sapply(Neu_BevolkY,class)


#transformieren der daten in Matrix 
transpose_Neu_BevolkY<-t(Neu_BevolkY)

trans2_Neu_BevolkX<-t(transpose_Neu_BevolkX)
'doppelte transponierung ist hier sinnvoll, da dann das Format die spalten 
der matrix passt und die tests auch somit funktionieren!
shapiro wilks funktioniert damit nicht aber daf�r alle anderen schon!

'

# nutzen shapirotest um Normalverteilung festzustellen

mshapiro.test(transpose_Neu_BevolkY)



#nutzen Jarque-Bera Test
jb.norm.test(trans2_Neu_BevolkY)

#allgemeiner test wegen normalverteilung
install.packages("normwhn.test")
library(normwhn.test)
trans2_Neu_BevolkY<-t(transpose_Neu_BevolkY)
normality.test1(trans2_Neu_BevolkY)
' Der Test scheint nur zu gehen wenn man die bereits erstellte Matrix nochmal 
transponiert!
'


install.packages("energy")
library(energy)
mvnorm.e(trans2_Neu_BevolkY)
# klappt hier auch nur mit der doppelt transponierten matrix


#Heinze-Zirkler test f�r multivariate Normalverteilung nutzen
# hier wird f�r jede Variable seperat die normalverteilung getetest

install.packages("MVN")
library(MVN)
#funktioniert NUR mit der doppelt transponierten Matrix

mvn(
  trans2_Neu_BevolkY,
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

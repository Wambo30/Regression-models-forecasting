# Umsetzung: Güte, Multivariate Regression, LASSO Regression, Ridge Regression, LARS Regression 

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
'Daten Einlesen aus Excel-Dateien+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

install.packages("readxl")
library(readxl)

#import von datensatz "Treiber ab 2005"
data_Treiber<-read_excel(file.choose(), na="NA")

data_Ertrag<-read_excel(file.choose(), na="NA")

names(data_Treiber)

names(data_Ertrag)

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
'Data Cleaning also Daten vorbereiten+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

# dataframe für Y-Variablen erstellen

dataFrame_Ertrag_Y<-data.frame(data_Ertrag$`Einzelfahrscheine Berlin ABC (450010)`,data_Ertrag$`Tageskarten Berlin ABC (450019)`,data_Ertrag$`Monatskarten  Berlin ABC (450012)`,data_Ertrag$`ABO (450014) Nutzerfin.`,data_Ertrag$`Firmenticket (450015)`,data_Ertrag$`Semester-/ Hochschulticket (450016)`,data_Ertrag$`Berlin Ticket S (450018)`,data_Ertrag$`Gesamt vor EAVs`)

'
Das Datum muss später vlcht hinzugefügt werden um zu schauen wann was passiert! DAs passiert wohl
eher bei der Zeitreihenbetrachtung. Man würde dann folgendes in den DataFrame noch einfügen:

data_Ertrag$...1

'


'
Y-Variablen:
Einzelfahrscheine.Berlin.ABC, Tageskarten.Berlin.ABC, Monatskarten..Berlin.ABC,
ABO..450014..Nutzerfin, Firmenticket..450015, Semester...Hochschulticket,
Berlin.Ticket.S, Gesamt.vor.EAVs


'

is.na(dataFrame_Ertrag_Y)

#löschen der zeilen mit missing values
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


#Spaltennamen ändern

# Datum vlcht hinzufügen später
#names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag....1"]="Datum"


names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Einzelfahrscheine.Berlin.ABC..450010.."]="Einzelfahrscheine ABC"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Tageskarten.Berlin.ABC..450019.."]="Tageskarten ABC"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Monatskarten..Berlin.ABC..450012.."]="Monatskarten_ABC"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..ABO..450014..Nutzerfin.."]="ABO"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Firmenticket..450015.."]="Firmenticket"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Semester...Hochschulticket..450016.."]="Hochschulticket"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Berlin.Ticket.S..450018.."]="Berlin Ticket S"

names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag..Gesamt.vor.EAVs."]="Gesamt vor EAVs"






'
==> Das heißt:

dataFrame_Ertrag_Y== enthält alle relevanten Variablen aus dem Datensatz Ertrag_ohne_Schüler'


#-----------------------------------------------------------------------------------------------------------------#
#data frame für X-variablen erstellen 

dataFrame_Treiber_X<- data.frame(data_Treiber$`Tage mit mäßigem Schnee 6-19`,data_Treiber$`Tage mit mäßigem Regen 6-19`,data_Treiber$`Tage mit mäßigem Schnee`,data_Treiber$`Tage mit starkem Regen`,data_Treiber$`Tage mit starkem Schnee`,data_Treiber$`Tage mit Regen`,data_Treiber$Arbeitslose, data_Treiber$`Auszubildende Insgesamt`,data_Treiber$Superbenzin,data_Treiber$Dieselkraftstoff,data_Treiber$`Bevölkerungfsab/-zunahme`,data_Treiber$Arbeitstage,data_Treiber$Samstage,data_Treiber$`Sonn-/Feiertage`,data_Treiber$Ferientage,data_Treiber$April,data_Treiber$December,data_Treiber$February,data_Treiber$January,data_Treiber$July,data_Treiber$June,data_Treiber$March,data_Treiber$May,data_Treiber$November,data_Treiber$October,data_Treiber$September,data_Treiber$`Tage mit mäßigem Regen`,data_Treiber$`Tage mit Schnee`,data_Treiber$`Auspendler Brandenburg`,data_Treiber$`Einpendler insgesamt`,data_Treiber$Studierende,data_Treiber$Übernachtungen,data_Treiber$Stau)

'
X-Variablen:
Arbeitslose, Auszubildende.Insgesamt, Superbenzin, Dieselkraftstoff, 
, Bevölkerungfsab..zunahme.,Arbeitstage, Samstage, Sonn..Feiertage., Ferientage, April,
December, February, January, July, June, March, May, November, October,
September, Tage.mit.mäßigem.Regen, Tage.mit.Schnee, Auspendler.Brandenburg,
Einpendler.insgesamt, Studierende, Übernachtungen, Stau

'

names(dataFrame_Treiber_X)

'
das brauche ich nicht mehr da es auch mit der na.omit fkt geht!

# ab zeile 86 (01.01.2012) bis zeile 192 (01.12.2020)
dataFrame_Treiber_X<-dataFrame_Treiber_X[-c(1:84),]

'

#zeilen nach 192 löschen

#suche nach missing values und anzeigen im data frame durch TRUE ausgabe
is.na(dataFrame_Treiber_X)

#löschen der zeilen mit missing values
dataFrame_Treiber_X<-na.omit(dataFrame_Treiber_X)

dataFrame_Treiber_X

#prüfen ob alle Variablen datentyp numeric haben
sapply(dataFrame_Treiber_X,class)

#Variable Arbeitslose gilt als factor daher müssen wir das in numeric umwandeln

#wenn eine Variable den datentyp factor hat, muss man diesen zuerst zum character umwandeln!
#dataFrame_Treiber_X$data_Treiber.Arbeitslose<-as.numeric(as.character(dataFrame_Treiber_X$Arbeitslose))

dataFrame_Treiber_X$data_Treiber.Arbeitslose<-as.character(dataFrame_Treiber_X$data_Treiber.Arbeitslose)

dataFrame_Treiber_X$data_Treiber.Arbeitslose<-as.numeric(dataFrame_Treiber_X$data_Treiber.Arbeitslose)


#prüfen ob alle Variablen datentyp numeric haben
sapply(dataFrame_Treiber_X,class)


'
==> Das heißt:

dataFrame_Treiber_X== enthält alle relevanten Variablen aus dem Datensatz Treiber_ab_2005'


'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
'Regressionsmodelle erstellen+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

' Allgemeine Syntax für multiple lineare Regression

<NameRegressionsmodell> <- lm(<Y-Variable> ~ <X-Var1> + <X-Var2> + ... + <X-VarN>)


'

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Ex1- Bevölkerungsentwicklung in Berlin (insgesamt und in Altersgruppen, Schüler, Studierende)

'
X-Variablen für Ex1:
1)Arbeitslose,2)Auszubildende insgesamt, 
3)Bevölkerungsab-und zunahme


Y-Variablen für Ex1:
1)Monatskarten Berlin ABC (450012)  ,2)ABO (450014)  Nutzerfin. ,3)Firmenticket (450015),
4)Gesamt vor EAVs

'

#X-Variablen für Ex1

#Ex1_Treiber_X<-data.frame(dataFrame_Treiber_X$data_Treiber.Arbeitslose,dataFrame_Treiber_X$data_Treiber..Auszubildende.Insgesamt.,dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)

#---------------------------------------------------------------------------------------------------
#Regressionsmodell Ex1 Nr.1- Monatskarten ABC

Ex1_Monatskarten<-lm(dataFrame_Ertrag_Y$Monatskarten_ABC ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose
                     +dataFrame_Treiber_X$data_Treiber..Auszubildende.Insgesamt.+dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)

names(Ex1_Monatskarten$coefficients)<-c('Arbeitslose','Auszubildende_Insgesamt','Bevoelkerungsab-/zunahme')
dataFrame_Ertrag_Y

summary(Ex1_Monatskarten)
'Ausgabe: NA bei Standardfehler und als Koeffizient usw.--> Das ist ein Hinweis auf Multikollinearität'


#Untersuchung auf Korrelation --> Q79,S.179f

cor.test(dataFrame_Treiber_X$data_Treiber.Arbeitslose,dataFrame_Treiber_X$data_Treiber..Auszubildende.Insgesamt.)
'Ausgabe: 0.75 also hohe Korrelation mit p-value < 2.2e-16! Dh alternative Hypoth. wird angenommen also die 
Variablen korrelieren.'

cor.test(dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.,dataFrame_Treiber_X$data_Treiber..Auszubildende.Insgesamt.)
'Ausgabe:0.2 also niedrige Korrelation mit p-value = 0.03186! Somit korrelieren die Variablen!'


cor.test(dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.,dataFrame_Treiber_X$data_Treiber.Arbeitslose)
'Ausgabe:0.11 also niedrige Korrelation aber mit p-value = 0.2248! Dh die Variablen korrelieren NICHT miteinander!'

'
==> das heißt die Variable Auszubildene insgesamt wird aufgrund der hohen Korrelation rausgelassen!

'



#Erstellen von neuem Regressionsmodell also Ex1 Nr1-2 (korrigiert)
'
X-Variablen für Ex1:
1)Arbeitslose, 2)Bevölkerungsab-und zunahme


Y-Variablen für Ex1:
1)Monatskarten Berlin ABC (450012)  ,2)ABO (450014)  Nutzerfin. ,3)Firmenticket (450015),
4)Gesamt vor EAVs

'


Ex1_Monatskarten<-lm(dataFrame_Ertrag_Y$Monatskarten_ABC ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)

#names(Ex1_Monatskarten$coefficients)<-c('Arbeitslose','Bevoelkerungsab-/zunahme')

#rownames(Ex1_Monatskarten$coef) <- c('Arbeitslose', 'Bevoelkerungsab-/zunahme')


###predict

install.packages("stats")
library(stats)

x1<-predict(Ex1_Monatskarten, dataFrame_Ertrag_Y)
cor(x1, dataFrame_Ertrag_Y$Monatskarten_ABC)

'

Prognose mit Modell jetzt effektiv da Effektstärke bei 0.6 ist! Das ist ein sehr starker Effekt nach
Cohen 1992.!

'

summary(Ex1_Monatskarten)

' Ex1-Regressionsmodell 1: 

X-Variablen: Arbeitslose, Bevoelkerungsab-/zunahme
Y-Variable: Monatskarten_ABC

p-value: 4.097e-12
R-squared:  0.3931
Adjusted R-squared:  0.3816 
Residual standard error: 966600
==> keine gute erklärung und standardfehler extrem hoch!


###bp test heteroscad
###reset test linearity

lmtest runterladen



'

#Prognose bestimmen

Ex1_Monatskarten.Prognose<- 1.063*10^-7-Arbeitslose*0.264+Bevoelkerungszunahme * 0.023+e




#MSE bestimmen
Sum_Ex1_Monatskarten<-summary(Ex1_Monatskarten)

MSE_Ex1_Monatskarten<-mean(Sum_Ex1_Monatskarten$residuals^2)
'
MSE bestimmen
908399371769

'

RMSE_Ex1_Monatskarten<-sqrt(MSE_Ex1_Monatskarten)
'
RMSE = 953099.9

'

#MAPE bestimmen
MAPE(Ex1_Monatskarten$residuals)



#T-test der Regressionskoeffizienten über Summary -Befehl

summary(Ex1_Monatskarten)

'
P-Werte der Regressionskoeffizienten:

dataFrame_Treiber_X$data_Treiber.Arbeitslose == 9.35e-09 ***
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 1.89e-08 ***

==> 1)Das heißt, da  für beide P-Werte< alpha=0.05 gilt, leisten beide Variablen 
(Arbeitslose, Bevölkerungsab-und zunahme) einen großen Erklärungsbeitrag zu den
Monatskarten!


'

#Prüfung der Regressionsvoraussetzungen 1)-5)

install.packages("olsrr")
library(olsrr)

#1)Linearität

#nach Q84,S.47 

'residuen des Modells in res gespeichert'
#res<-residuals(Ex1_Monatskarten)
#plot(fitted(Ex1_Monatskarten),res)
#abline(0,0)

plot(fitted(Ex1_Monatskarten), residuals(Ex1_Monatskarten))

ols_plot_resid_fit(Ex1_Monatskarten)

'
==> es ist linear wenn die residuen rein zufällig um die 0-Achse verteilt sind!
Das heißt es darf kein Muster oder Regelmäßigkeit bezüglich der Verteilung 
existieren! 


Ich denke das es nicht linear ist, auch wenn varianzhomogenität vorherrscht!

'


#2)Homoskedastizität

#qqnorm(rstandard(Ex1_Monatskarten))
#qqline(rstandard(Ex1_Monatskarten))

ols_plot_resid_qq(Ex1_Monatskarten)


'
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!

'
# zusätzlicher beusch-pagan test

install.packages("lmtest")
library(lmtest)

bptest(Ex1_Monatskarten)

' wenn p-wert<0.05 dann liegt Varianzheterogenität vor!
==> dh dieser Test sagt dgenau das gegenteil aus, was aber vlcht klarist
da numerische tests bei dieser großen datenmenge unzuverlässig sind!

'

#3)Prüfung auf Normalverteilung


#qqnorm(rstandard(Ex1_Monatskarten))
#qqline(rstandard(Ex1_Monatskarten))

ols_plot_resid_qq(Ex1_Monatskarten)

'
==> Normalverteilung ist gegeben, da die meisten Werte auf der Gerade liegen
oder sehr nah sind!

'


#4)Prüfung auf Autokorrelation mit dem durbin-watson test
library(car)
dwt(Ex1_Monatskarten)


'
Autocorrelation
0.663772 

==> somit gilt eine starke Autokorrelation da d<1 gilt!

Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'




#5)Prüfung auf Multikollinearität

install.packages("car")
library(car)

vif(Ex1_Monatskarten)

'
dataFrame_Treiber_X$data_Treiber.Arbeitslose 
                                                   1.014067 
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. 
                                                   1.014067 

==> geringe Multikollinearität! Zudem ist sie geringer als mit der Variable 
Auszubildende insgesamt.

'

#Ausreißer prüfen)

fitHas <- Ex1_Monatskarten

h<- hatvalues(fitHas)

summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte!
Mean== 0.027778 '

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die doppelt sogroß wie der Mittelwert sind!
Es gibt aber keinen wert größer 1 für die Cook-Distanz.
Somit werden keine Ausreißer aus dem Datensatz hier entfernt!

------------------------------------------------------------------------------------------------------
Auswertung:
Es gilt für das Regressionsmodell Ex1_Monatskarten folgendes:


X-Variablen: Arbeitslose, Bevoelkerungsab-/zunahme
Y-Variable: Monatskarten_ABC


p-wert,standardfehler, korr.Bestimmtheitsmaß:
p-value: 4.097e-12
==> dh stat. signifikanz mit p-value<0.05 ist gegeben!

Adjusted R-squared:  0.3816 
Residual standard error: 966600
==> keine gute erklärung und standardfehler extrem hoch!


T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:

dataFrame_Treiber_X$data_Treiber.Arbeitslose == 9.35e-09 ***
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 1.89e-08 ***

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> 1)Das heißt, da  für beide P-Werte< alpha=0.05 gilt, leisten beide Variablen 
(Arbeitslose, Bevölkerungsab-und zunahme) einen großen Erklärungsbeitrag zu den
Monatskarten!


RMSE = 953099.9

Linearität:
==> es ist linear wenn die residuen rein zufällig um die 0-Achse verteilt sind!
Das heißt es darf kein Muster oder Regelmäßigkeit bezüglich der Verteilung 
existieren! 
Ich denke das es nicht linear ist, auch wenn varianzhomogenität vorherrscht!


Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!


Normalität:
==> Normalverteilung ist gegeben, da die meisten Werte auf der Gerade liegen
oder sehr nah sind!

Autokorrelation:
Autocorrelation
0.663772 

==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!


Multikollinearität:
dataFrame_Treiber_X$data_Treiber.Arbeitslose 
                                                   1.014067 
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. 
                                                   1.014067 

==> geringe Multikollinearität! Zudem ist sie geringer als mit der Variable 
Auszubildende insgesamt.


Ausreißer prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die doppelt sogroß wie der Mittelwert sind!
Es gibt aber keinen wert größer 1 für die Cook-Distanz.
Somit werden keine Ausreißer aus dem Datensatz hier entfernt!

'



#----------------------------------------------------------------------------------------------------------
#Regressionsmodell Ex1.- Nr.2 (ABO (450014)  Nutzerfin.)

Ex1_Abo<-lm(dataFrame_Ertrag_Y$ABO ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)


summary(Ex1_Abo)

' 

p-value: < 2.2e-16
R-squared:  0.7016
Adjusted R-squared:  0.6959 
Residual standard error: 2282000 
==> stat. Siginifikanz ist gegeben da p-value < 0.05 gilt! 
gute erklärung wegen R-squared! Aber standardfehler extrem hoch! Das heißt hinweis auf
Autokorrelation, Multikollinearität und Varianzheterogenität!

'

# T-Test der Regressionskoeffizienten

summary(Ex1_Abo)

'
T-Test der Regressionskoeffizienten also P-Wert der Regressionskoeffizienten:

P-Werte der Regressionskoeffizienten:

dataFrame_Treiber_X$data_Treiber.Arbeitslose == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 3.36e-09 ***

==> 1)Das heißt, da  für beide P-Werte der Regressionskoeffizienten < alpha=0.05 gilt, leisten beide Variablen
(Arbeitslose,Bevölkerungsab-und zunahme) einen großen Erklärungsbeitrag zur Zielvariable Abo!

'

#MSE bestimmen
Sum_Ex1_Abo<-summary(Ex1_Abo)

MSE_Ex1_Abo<-mean(Sum_Ex1_Abo$residuals^2)
'
MSE==
5.063741e+12
'
RMSE_Ex1_Abo<-sqrt(MSE_Ex1_Abo)
'
RMSE = 2250276

'

#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität


'residuen des Modells in res gespeichert'
#res<-residuals(Ex1_Monatskarten)
#plot(fitted(Ex1_Monatskarten),res)
#abline(0,0)

plot(fitted(Ex1_Abo), residuals(Ex1_Abo))

ols_plot_resid_fit(Ex1_Abo)

'
=> im Plot existiert keine regelmäßige Verteilung der Residuen um die 0-Achse!
Somit gilt Nicht-Linearität!


'

#2)Homoskedastizität


ols_plot_resid_qq(Ex1_Abo)


'
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind! Auch wenn es hier einige Ausreißer gibt!
'

#3)Prüfung auf Normalverteilung


ols_plot_resid_qq(Ex1_Abo)


'
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind! Auch wenn es hier einige Ausreißer gibt!
'



#4)Prüfung auf Autokorrelation mit dem durbin-watson test


library(car)

dwt(Ex1_Abo)


'
Autocorrelation
0.8153493 

==> somit gilt eine starke Autokorrelation da d<1 gilt!

Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'


#5)Prüfung auf Multikollinearität


library(car)

vif(Ex1_Abo)

'
 dataFrame_Treiber_X$data_Treiber.Arbeitslose == 1.014067
 
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 1.014067

==> da vif < 10 gilt, haben wir keine Multikollinearität!!

'

#Ausreißer prüfen)

fitHas <- Ex1_Abo

h<- hatvalues(fitHas)

summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte!'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall. Somit werde ich keinen Wert hier löschen!

-------------------------------------------------------------------------------------
Auswertung:
Es gilt für das Regressionsmodell Ex1 Abo:

X-Variablen: 1)Arbeitslose, 2)Bevoelkerungsab-und zunahme
Y-Variable: 1)Abo


p-wert,standardfehler, korr.Bestimmtheitsmaß:

p-value: < 2.2e-16
Adjusted R-squared:  0.6959 
Residual standard error: 2282000 
==> stat. Siginifikanz ist gegeben da p-value < 0.05 gilt! 
gute erklärung wegen R-squared! Aber standardfehler extrem hoch! Das heißt hinweis auf
Autokorrelation, Multikollinearität und Varianzheterogenität!


T-Test der Regressionskoeffizienten also P-Wert der Regressionskoeffizienten:
P-Werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 3.36e-09 ***

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==>Das heißt, da  für beide P-Werte der Regressionskoeffizienten < alpha=0.05 gilt, leisten beide Variablen
(Arbeitslose,Bevölkerungsab-und zunahme) einen großen Erklärungsbeitrag zur Zielvariable Abo!


Linearität:
=> im Plot existiert keine regelmäßige Verteilung der Residuen um die 0-Achse!
Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind! Auch wenn es hier einige Ausreißer gibt!

Normalverteilung:
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind! Auch wenn es hier einige Ausreißer gibt!


Autokorrelation:

Autocorrelation
0.8153493 

==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!


Multikollinearität:
dataFrame_Treiber_X$data_Treiber.Arbeitslose == 1.014067
 
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 1.014067
==> da vif < 10 gilt, haben wir keine Multikollinearität!!


Ausreißer-Prüfung:

mean == 0.027778 
Hat-Wert muss zwei bis dreimal so groß sein wie mean wert! Ist zwar gegegben,
aber diesbezüglich müsste dann auch die Cookdistanz passen.

Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall. Somit werde ich keinen Wert hier löschen!


'


#----------------------------------------------------------------------------------------------------------
#Regressionsmodell Ex1-Nr.3 (Firmenticket)


Ex1_Firmenticket<-lm(dataFrame_Ertrag_Y$Firmenticket ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)

summary(Ex1_Firmenticket)

' 
X-Variablen:1)Arbeitslose, 2)Bevoelkerungsab-und zunahme
Y-Variablen: 1)Firmenticket


p-wert,standardfehler, korr.Bestimmtheitsmaß:
p-value: 1.849e-08
==> dh die stat. Signifikanz ist mit p-value <0.05 für das Modell gegeben!
R-squared:  0.2876
Adjusted R-squared:   0.2741 
Residual standard error: 564600
==> nicht so gute erklärung! Und hoher Standardfehler!

'

#T-Test für Regressionskoeffizienten

summary(Ex1_Firmenticket)

'
T-Test der Regressionskoeffizienten also P-Wert der Regressionskoeffizienten:
P-Wert der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose == 0.307 
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 8.37e-09 ***

==> 1)Das heißt, da  für den P-Wert für den Regressionskoeffizient Bevoelkerungsab-und zunahme 
8.37e-09 ***< alpha=0.05 gilt, so leistet die Variable Bevölkerungsab-und zunahme einen
Erklärungsbeitrag zu Firmenticket!

==>2)Da aber für den P-Wert von Arbeitslose 0.307<alpha=0.05 gilt, leistet die Variable Arbeitslose 
KEINEN Erklärungsbeitrag zur Zielvariable Firmenticket!

'

#MSE bestimmen
Sum_Ex1_Firmenticket<-summary(Ex1_Firmenticket)

MSE_Ex1_Firmenticket<-mean(Sum_Ex1_Firmenticket$residuals^2)
'
MSE==309961436009
'
RMSE_Ex1_Firmenticket<-sqrt(MSE_Ex1_Firmenticket)
'
RMSE = 556741.8

'


#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität


plot(fitted(Ex1_Firmenticket), residuals(Ex1_Firmenticket))

ols_plot_resid_fit(Ex1_Firmenticket)

'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Man kann eher ein Muster erkennen!Somit gilt Nicht-Linearität!
'

#2)Homoskedastizität

ols_plot_resid_qq(Ex1_Firmenticket)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind! Auch wenn es hier einige Ausreißer gibt!
'

#3)Normalverteilung

ols_plot_resid_qq(Ex1_Firmenticket)

'
Normalität:
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind! Auch wenn es hier einige Ausreißer gibt!
'

#4)Autokorrelation
library(car)
dwt(Ex1_Firmenticket)

'
Autokorrelation:
Autocorrelation == 0.8663782 

==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität
library(car)
vif(Ex1_Firmenticket)

'
dataFrame_Treiber_X$data_Treiber.Arbeitslose == 1.014067
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 1.014067

==> da vif < 10 gilt, haben wir keine Multikollinearität!!
'

#Ausreißer prüfen

fitHas <- Ex1_Firmenticket

h<- hatvalues(fitHas)

summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.027778'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!

------------------------------------------------------------------------------------------------------------
Auswertung:
Es gilt für das Regressionsmodell Ex1_Firmenticket folgendes:

X-Variablen: 1)Arbeitslose, 2)Bevoelkerungsab-und zunahme
Y-Variable: 1)Firmenticket

p-wert,standardfehler, korr.Bestimmtheitsmaß:
p-value: 1.849e-08
==> dh die stat. Signifikanz ist mit p-value <0.05 für das Modell gegeben!
Adjusted R-squared:   0.2741 
Residual standard error: 564600
==> nicht so gute erklärung! Und hoher Standardfehler!


T-Test der Regressionskoeffizienten also P-Wert der Regressionskoeffizienten:
P-Wert der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose == 0.307 
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 8.37e-09 ***

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> 1)Das heißt, da  für den P-Wert für den Regressionskoeffizient Bevoelkerungsab-und zunahme 
8.37e-09 ***< alpha=0.05 gilt, so leistet die Variable Bevölkerungsab-und zunahme einen
Erklärungsbeitrag zu Firmenticket!
==>2)Da aber für den P-Wert von Arbeitslose 0.307<alpha=0.05 gilt, leistet die Variable Arbeitslose 
KEINEN Erklärungsbeitrag zur Zielvariable Firmenticket!


Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Man kann eher ein Muster erkennen!Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind! Auch wenn es hier einige Ausreißer gibt!

Normalität:
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind! Auch wenn es hier einige Ausreißer gibt!


Autokorrelation:
Autocorrelation == 0.8663782 
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!


Multikollinearität:
dataFrame_Treiber_X$data_Treiber.Arbeitslose == 1.014067
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 1.014067

==> da vif < 10 gilt, haben wir keine Multikollinearität!!


Ausreißer Prüfung:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!

'


#----------------------------------------------------------------------------------------------------------
#Regressionsmodell Ex1-Nr.4 (Gesamt vor EAVs)


Ex1_GesamtVorEAVs<-lm(dataFrame_Ertrag_Y$`Gesamt vor EAVs` ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)

summary(Ex1_GesamtVorEAVs)


' 
X-Variablen: 1)Arbeitslose,2)Bevoelkerungsab-und zunahme
Y-Variablen: 1)Gesamt vor EAVs


p-Wert,Standardfehler, korr. Bestimmtheitsmaß:
p-value:  < 2.2e-16
==>stat. Signifikanz des Modells mit p-value<0.05 gegeben!

R-squared:  0.8118
Adjusted R-squared:   0.8083 
Residual standard error: 2926000 
==> sehr gute erklärung!Und hoher Standardfehler!

'

#T-Test der Regressionskoeffizienten

summary(Ex1_GesamtVorEAVs)
'
T-Test der Regressionskoeffizienten also P-Wert der Regressionskoeffizienten:
P-Wert der Regressionskoeffizienten:

dataFrame_Treiber_X$data_Treiber.Arbeitslose == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. ==  0.000117 ***

==> 1)Das heißt, da  für beide P-Werte der Regressionskoeffizienten < alpha=0.05  gilt, leistet 
beide Variablen(Arbeitslose, Bevölkerungsab-und zunahme) einen Erklärungsbeitrag zur Zielvariable
GesamtVorEAVs!

'

#MSE bestimmen
Sum_Ex1_GesamtVorEAVs<-summary(Ex1_GesamtVorEAVs)

MSE_Ex1_GesamtVorEAVs<-mean(Sum_Ex1_GesamtVorEAVs$residuals^2)
'
MSE==8.322325e+12
'
RMSE_Ex1_GesamtVorEAVs<-sqrt(MSE_Ex1_GesamtVorEAVs)
'
RMSE = 2884844

'



#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität
plot(fitted(Ex1_GesamtVorEAVs), residuals(Ex1_GesamtVorEAVs))

ols_plot_resid_fit(Ex1_GesamtVorEAVs)

'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Unten sind weniger Residuen als oben!Somit gilt Nicht-Linearität!
'

#2)Homoskedastizität

ols_plot_resid_qq(Ex1_GesamtVorEAVs)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#3)Normalverteilung

ols_plot_resid_qq(Ex1_GesamtVorEAVs)

'
Normalität:
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind! Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#4)Autokorrelation
library(car)
dwt(Ex1_GesamtVorEAVs)

'
Autokorrelation:
Autocorrelation == 0.5341168

==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität
library(car)
vif(Ex1_GesamtVorEAVs)

'
dataFrame_Treiber_X$data_Treiber.Arbeitslose == 1.014067
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 1.014067

==> da vif < 10 gilt, haben wir keine Multikollinearität!!
'

#Ausreißer prüfen

fitHas <- Ex1_GesamtVorEAVs

h<- hatvalues(fitHas)

summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.027778'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!

----------------------------------------------------------------------------------------------
Auswertung:
Es gilt für das Regressionsmodell Ex1_GesamtVorEAVs folgendes:

X-Variablen: 1)Arbeitslose,2)Bevoelkerungsab-und zunahme
Y-Variablen: 1)Gesamt vor EAVs



p-Wert,Standardfehler, korr. Bestimmtheitsmaß:
p-value:  < 2.2e-16
==>stat. Signifikanz des Modells mit p-value<0.05 gegeben!

Adjusted R-squared:   0.8083 
Residual standard error: 2926000 
==> sehr gute erklärung!Und hoher Standardfehler!


T-Test der Regressionskoeffizienten also P-Wert der Regressionskoeffizienten:
P-Wert der Regressionskoeffizienten:

dataFrame_Treiber_X$data_Treiber.Arbeitslose == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. ==  0.000117 ***

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> 1)Das heißt, da  für beide P-Werte der Regressionskoeffizienten < alpha=0.05  gilt, leistet 
beide Variablen(Arbeitslose, Bevölkerungsab-und zunahme) einen Erklärungsbeitrag zur Zielvariable
GesamtVorEAVs!


Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Unten sind weniger Residuen als oben!Somit gilt Nicht-Linearität!


Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!


Normalität:
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind! Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!


Autokorrelation:
Autocorrelation == 0.5341168

==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!


Multikollinearität:
dataFrame_Treiber_X$data_Treiber.Arbeitslose == 1.014067
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 1.014067

==> da vif < 10 gilt, haben wir keine Multikollinearität!!


Ausreißer-Prüfung:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!

'
#----------------------------------------------------------------------------------------------------------
#Regressionsmodell Ex1-Nr.5 (Semester-/ Hochschulticket (450016))


Ex1_Hochschulticket<-lm(dataFrame_Ertrag_Y$Hochschulticket ~ dataFrame_Treiber_X$data_Treiber.Studierende)

summary(Ex1_Hochschulticket)


' 
X-Variablen:1)Studierende
Y-Variablen:1)Hochschulticket

p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
p-value:  < 2.2e-16
==>stat. Signifikanz gegeben da p-value<0.05 gilt!

R-squared:  0.5445
Adjusted R-squared:   0.5402 
Residual standard error: 384600
==> mittelmäßige erklärung!Und hoher Standardfehler!

'

#MSE bestimmen
Sum_Ex1_Hochschulticket<-summary(Ex1_Hochschulticket)

MSE_Ex1_Hochschulticket<-mean(Sum_Ex1_Hochschulticket$residuals^2)
'
MSE == 145195809489
'
RMSE_Ex1_Hochschulticket<-sqrt(MSE_Ex1_Hochschulticket)
'
RMSE == 381045.7

'

#T-Test der Regressionskoeffizienten

summary(Ex1_Hochschulticket)
'
T-Test der Regressionskoeffizienten also P-Wert des Regressionskoeffizienten:
P-Wert-Regressionskoeffizient:

dataFrame_Treiber_X$data_Treiber.Studierende == < 2e-16 *** 

Da für den P-Wert des Regressionskoeffizienten 2e-16 *** <0.05 gilt, leistet die Variable
Studierende einen Erklärungsbeitrag zu den Hochschultickets!

'


#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität
plot(fitted(Ex1_Hochschulticket), residuals(Ex1_Hochschulticket))

ols_plot_resid_fit(Ex1_Hochschulticket)

'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Unten sind mehr Residuen als oben!Somit gilt Nicht-Linearität!
'

#2)Homoskedastizität

ols_plot_resid_qq(Ex1_Hochschulticket)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#3)Normalverteilung

ols_plot_resid_qq(Ex1_Hochschulticket)

'
Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#4)Autokorrelation
library(car)
dwt(Ex1_Hochschulticket)

'
Autokorrelation:
Autocorrelation == -0.05064463 

==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität

'
Ist unnötig da wir nur eine X-Variable betrachten!
'


#Ausreißer prüfen

fitHas <- Ex1_Hochschulticket

h<- hatvalues(fitHas)

summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.018519'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!

---------------------------------------------------------------------------------------------------------
Auswertung:
Es gilt für das Regressionsmodell Ex1_Hochschulticket folgendes:

X-Variablen:1)Studierende
Y-Variablen:1)Hochschulticket



p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
p-value:  < 2.2e-16
==>stat. Signifikanz gegeben da p-value<0.05 gilt!

Adjusted R-squared:   0.5402 
Residual standard error: 384600
==> mittelmäßige erklärung!Und hoher Standardfehler!


T-Test der Regressionskoeffizienten also P-Wert des Regressionskoeffizienten:
P-Wert-Regressionskoeffizient:
dataFrame_Treiber_X$data_Treiber.Studierende == < 2e-16 *** 

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Da für den P-Wert des Regressionskoeffizienten 2e-16 *** <0.05 gilt, leistet die Variable
Studierende einen Erklärungsbeitrag zu den Hochschultickets!



Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Unten sind mehr Residuen als oben!Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!


Autokorrelation:
Autocorrelation == -0.05064463 
==> somit gilt eine SEHR starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!


Multikollinearität:
Ist unnötig da wir nur eine X-Variable betrachten!

Ausreißer-Prüfung:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!


'


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Ex2- Tourismuszahlen

'
X-Variablen für Ex2:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 


Y-Variablen für Ex2:
1)Einzelfahrscheine Berlin ABC (450010), 2)Tageskarten Berlin ABC (450019) 
3)Gesamt vor EAVs

'

#Prüfung auf Multikollinearität da Korrelation paarweise nur gut und es zu aufwändig wäre

Ex2_Einzelfahrscheine<-lm(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC` ~ dataFrame_Treiber_X$data_Treiber.Übernachtungen+dataFrame_Treiber_X$data_Treiber.Samstage+dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.+dataFrame_Treiber_X$data_Treiber.January+dataFrame_Treiber_X$data_Treiber.February+dataFrame_Treiber_X$data_Treiber.March+dataFrame_Treiber_X$data_Treiber.April+dataFrame_Treiber_X$data_Treiber.May+dataFrame_Treiber_X$data_Treiber.June+dataFrame_Treiber_X$data_Treiber.July+dataFrame_Treiber_X$data_Treiber.September+dataFrame_Treiber_X$data_Treiber.October+dataFrame_Treiber_X$data_Treiber.November+dataFrame_Treiber_X$data_Treiber.December)

summary(Ex2_Einzelfahrscheine)

# Multikollinearität prüfen
library(car)
vif(Ex2_Einzelfahrscheine)

'
==> die VIF-werte gehen bis 3.2, somit gilt also eine geringe Multikollinearität!

'

#vereinzelnte Korrelation
cor.test(dataFrame_Treiber_X$data_Treiber.Samstage,dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.)
'Korrelation ist nur stat. signifikant wenn p-value<0.05 gilt! Dann kann man den Korrelationswert ernst nehmen!
Wenn aber p-value>0.05 gilt, so kann man den Korrelationswert nicht ernst nehmen!Dh man ignoriert das!

==> In diesem Fall gilt Korrelationswert==0.06967333 mit p-value=0.4737>0.05=alpha! Dh es existiert keine
stat. signifikante Korrelation!

Zusammenfassend:

Die X-Variablen bleiben also so bestehen und es werden keine aufgrund hoher Korrelation rausgenommen!

Für die Regressionsmodelle von Ex2 Tourmiszahlen gilt also:


X-Variablen für Ex2:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 


Y-Variablen für Ex2:
1)Einzelfahrscheine Berlin ABC (450010), 2)Tageskarten Berlin ABC (450019) 
3)Gesamt vor EAVs

'

#---------------------------------------------------------------------------------------------------
#Regressionsmodell Ex2 Tourimuszahlen Einzelfahrscheine

#Regressionsmodell bilden und schätzen


Ex2_Einzelfahrscheine<-lm(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC` ~ dataFrame_Treiber_X$data_Treiber.Übernachtungen+dataFrame_Treiber_X$data_Treiber.Samstage+dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.+dataFrame_Treiber_X$data_Treiber.January+dataFrame_Treiber_X$data_Treiber.February+dataFrame_Treiber_X$data_Treiber.March+dataFrame_Treiber_X$data_Treiber.April+dataFrame_Treiber_X$data_Treiber.May+dataFrame_Treiber_X$data_Treiber.June+dataFrame_Treiber_X$data_Treiber.July+dataFrame_Treiber_X$data_Treiber.September+dataFrame_Treiber_X$data_Treiber.October+dataFrame_Treiber_X$data_Treiber.November+dataFrame_Treiber_X$data_Treiber.December)

summary(Ex2_Einzelfahrscheine)
#Gucken 2.2

'
X-Variablen für Ex2_Einzelfahrscheine:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 

Y-Variablen:1)Einzelfahrscheine ABC


p-Wert,Standardfehler,korr. Bestimmtheitsmaß:

p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!

R-squared:  0.908
Standardfehler == 787500
Adjusted R-squared:  0.8941 

==> Das Modell erklärt aufgrund von korr. Bestimmtheitsmaß 0.8941 die Zielvariable
Einzelfahrscheine sehr gut! Der Standardfehler ist sehr hoch, was wahrschienlich 
auf die hohe Autokorrelation zurückzuführen ist!

'

#MSE bestimmen
Sum_Ex2_Einzelfahrscheine<-summary(Ex2_Einzelfahrscheine)

MSE_Ex2_Einzelfahrscheine<-mean(Sum_Ex2_Einzelfahrscheine$residuals^2)
'
MSE == 534060109408
'
RMSE_Ex2_Einzelfahrscheine<-sqrt(MSE_Ex2_Einzelfahrscheine)
'
RMSE = 730794.2

'


#T-Test der Regressionskoeffizienten

summary(Ex2_Einzelfahrscheine)

'
T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:

dataFrame_Treiber_X$data_Treiber.Übernachtungen     == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Samstage           ==  0.46959       
dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.  == 0.00456 ** 
dataFrame_Treiber_X$data_Treiber.January            == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.February           ==  1.84e-09 ***
dataFrame_Treiber_X$data_Treiber.March              ==  2.78e-08 ***
dataFrame_Treiber_X$data_Treiber.April              ==  0.00166 **   
dataFrame_Treiber_X$data_Treiber.May                ==  0.00029 ***  
dataFrame_Treiber_X$data_Treiber.June               ==  0.00483 **   
dataFrame_Treiber_X$data_Treiber.July               ==  0.73261    
dataFrame_Treiber_X$data_Treiber.September          ==  0.00219 **   
dataFrame_Treiber_X$data_Treiber.October            ==  2.93e-05 ***
dataFrame_Treiber_X$data_Treiber.November           ==  3.95e-11 ***
dataFrame_Treiber_X$data_Treiber.December           ==  3.03e-11 ***

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> dh wenn für den p-wert den Regressionskoeffizienten p-wert<0.05 gilt, so leistet 
dieser regressionkoeffizient einen Erklärungsbeitrag zu Einzelfahrscheine und ist demnach stat. signifikant! 

Wenn aber p-wertVonRegressionskoeffizient > 0.05 gilt, so leistet dieser Regressionskoeffizient
KEINEN Erklärungsbeitrag zur Zielvariable Einzelfahrscheine und ist NICHT stat. signifikant!

Dh da fast für alle P-Werte der Regressionskoeffizienten <0.05 gilt, sind leisten alle Regressionskoeffizienten
einen Erklärungsbeitrag zu den Einzelfahrscheinen AUßER Samstage(P-Wert=0.46959) und July (P-Wert=0.73261).

'

#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität
plot(fitted(Ex2_Einzelfahrscheine), residuals(Ex2_Einzelfahrscheine))

ols_plot_resid_fit(Ex2_Einzelfahrscheine)

'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!
'


#2)Homoskedastizität

ols_plot_resid_qq(Ex2_Einzelfahrscheine)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'


#3)Normalverteilung

ols_plot_resid_qq(Ex2_Einzelfahrscheine)

'
Normalverteilung:
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#4)Autokorrelation
library(car)
dwt(Ex2_Einzelfahrscheine)

'
Autokorrelation:
Autocorrelation == -0.02989066 

==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität
library(car)
vif(Ex2_Einzelfahrscheine)

'
Multikollinearität:
==> die VIF-werte gehen bis 3.2, somit gilt also eine keine Multikollinearität, 
da die Werte unter 10 sind!

'

#Ausreißer prüfen

fitHas <- Ex2_Einzelfahrscheine

h<- hatvalues(fitHas)

summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.1389'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!

---------------------------------------------------------------------------------------------------------
Auswertung:
Es gilt für das Regressionsmodell Ex2_Einzelfahrscheine folgendes:

X-Variablen für Ex2_Einzelfahrscheine:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 

Y-Variablen:1)Einzelfahrscheine ABC


p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 787500
Adjusted R-squared:  0.8941 

==> Das Modell erklärt aufgrund von korr. Bestimmtheitsmaß 0.8941 die Zielvariable
Einzelfahrscheine sehr gut! Der Standardfehler ist sehr hoch, was wahrschienlich 
auf die hohe Autokorrelation zurückzuführen ist!



T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Übernachtungen     == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Samstage           ==  0.46959       
dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.  == 0.00456 ** 
dataFrame_Treiber_X$data_Treiber.January            == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.February           ==  1.84e-09 ***
dataFrame_Treiber_X$data_Treiber.March              ==  2.78e-08 ***
dataFrame_Treiber_X$data_Treiber.April              ==  0.00166 **   
dataFrame_Treiber_X$data_Treiber.May                ==  0.00029 ***  
dataFrame_Treiber_X$data_Treiber.June               ==  0.00483 **   
dataFrame_Treiber_X$data_Treiber.July               ==  0.73261    
dataFrame_Treiber_X$data_Treiber.September          ==  0.00219 **   
dataFrame_Treiber_X$data_Treiber.October            ==  2.93e-05 ***
dataFrame_Treiber_X$data_Treiber.November           ==  3.95e-11 ***
dataFrame_Treiber_X$data_Treiber.December           ==  3.03e-11 ***

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> dh wenn für den p-wert den Regressionskoeffizienten p-wert<0.05 gilt, so leistet 
dieser regressionkoeffizient einen Erklärungsbeitrag zu Einzelfahrscheine und ist demnach stat. signifikant! 
Wenn aber p-wertVonRegressionskoeffizient > 0.05 gilt, so leistet dieser Regressionskoeffizient
KEINEN Erklärungsbeitrag zur Zielvariable Einzelfahrscheine und ist NICHT stat. signifikant!
Dh da fast für alle P-Werte der Regressionskoeffizienten <0.05 gilt, sind leisten alle Regressionskoeffizienten
einen Erklärungsbeitrag zu den Einzelfahrscheinen AUßER Samstage(P-Wert=0.46959) und July (P-Wert=0.73261).


Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Normalverteilung:
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!


Autokorrelation:
Autocorrelation == -0.02989066 
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!


Multikollinearität:
==> die VIF-werte gehen bis 3.2, somit gilt also eine keine Multikollinearität, 
da die Werte unter 10 sind!


Ausreißer-Prüfung:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!

'

#---------------------------------------------------------------------------------------------------
#Regressionsmodell Ex2 Tourimuszahlen Tageskarten

'
Es gilt für das Regressionsmodell Ex2_Tageskarten folgendes:

X-Variablen für Ex2_Einzelfahrscheine:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 

Y-Variablen:1)Tageskarten ABC
'
#Regressionsmodell bilden und schätzen


Ex2_Tageskarten<-lm(dataFrame_Ertrag_Y$`Tageskarten ABC` ~ dataFrame_Treiber_X$data_Treiber.Übernachtungen+dataFrame_Treiber_X$data_Treiber.Samstage+dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.+dataFrame_Treiber_X$data_Treiber.January+dataFrame_Treiber_X$data_Treiber.February+dataFrame_Treiber_X$data_Treiber.March+dataFrame_Treiber_X$data_Treiber.April+dataFrame_Treiber_X$data_Treiber.May+dataFrame_Treiber_X$data_Treiber.June+dataFrame_Treiber_X$data_Treiber.July+dataFrame_Treiber_X$data_Treiber.September+dataFrame_Treiber_X$data_Treiber.October+dataFrame_Treiber_X$data_Treiber.November+dataFrame_Treiber_X$data_Treiber.December)

summary(Ex2_Tageskarten)
'
p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
R-squared:  0.9791
Adjusted R-squared:  0.976  
Standardfehler == 169100
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
fast perfekt erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!

'

#MSE bestimmen
Sum_Ex2_Tageskarten<-summary(Ex2_Tageskarten)

MSE_Ex2_Tageskarten<-mean(Sum_Ex2_Tageskarten$residuals^2)
'
MSE == 24632970430
'

RMSE_Ex2_Tageskarten<-sqrt(MSE_Ex2_Tageskarten)
'
RMSE = 156948.9

'


#T-Test der Regressionskoeffizienten
summary(Ex2_Tageskarten)


'
T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Übernachtungen     == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Samstage           ==  0.967249       
dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.  == 0.000206 ***
dataFrame_Treiber_X$data_Treiber.January            == 0.036915 * 
dataFrame_Treiber_X$data_Treiber.February           ==  0.845860 
dataFrame_Treiber_X$data_Treiber.March              ==  0.217632 
dataFrame_Treiber_X$data_Treiber.April              ==  0.298765   
dataFrame_Treiber_X$data_Treiber.May                ==  0.057234 . 
dataFrame_Treiber_X$data_Treiber.June               ==  0.260785    
dataFrame_Treiber_X$data_Treiber.July               ==  0.013320 *     
dataFrame_Treiber_X$data_Treiber.September          ==  0.017850 *  
dataFrame_Treiber_X$data_Treiber.October            ==  0.246127 
dataFrame_Treiber_X$data_Treiber.November           ==  0.813583 
dataFrame_Treiber_X$data_Treiber.December           ==  0.004452 ** 

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag zu den Tageskarten. Dieses gilt
für folgende Variablen:
Übernachtungen,Sonn-und Feiertage,Januar, Juli, September,Dezember

==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für Tageskarten da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!

'

#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität
plot(fitted(Ex2_Tageskarten), residuals(Ex2_Tageskarten))

ols_plot_resid_fit(Ex2_Tageskarten)

'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!
'

#2)Homoskedastizität

ols_plot_resid_qq(Ex2_Tageskarten)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#3)Normalverteilung

ols_plot_resid_qq(Ex2_Tageskarten)

'
Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#4)Autokorrelation
library(car)
dwt(Ex2_Tageskarten)

'
Autokorrelation:
Autocorrelation == 0.2384679

==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität
library(car)
vif(Ex2_Tageskarten)

'
Multikollinearität:
==> die VIF-werte gehen bis 3.2, somit gilt also eine keine Multikollinearität, 
da die Werte unter 10 sind!

'

#Ausreißer prüfen

fitHas <- Ex2_Tageskarten

h<- hatvalues(fitHas)

summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.1389'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!

---------------------------------------------------------------------------------------------------------
Auswertung:
Es gilt für das Regressionsmodell Ex2_Tageskarten folgendes:

X-Variablen für Ex2_Einzelfahrscheine:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 

Y-Variablen:1)Tageskarten ABC


p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 169100
Adjusted R-squared:  0.976  
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
fast perfekt erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!


T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Übernachtungen     == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Samstage           ==  0.967249       
dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.  == 0.000206 ***
dataFrame_Treiber_X$data_Treiber.January            == 0.036915 * 
dataFrame_Treiber_X$data_Treiber.February           ==  0.845860 
dataFrame_Treiber_X$data_Treiber.March              ==  0.217632 
dataFrame_Treiber_X$data_Treiber.April              ==  0.298765   
dataFrame_Treiber_X$data_Treiber.May                ==  0.057234 . 
dataFrame_Treiber_X$data_Treiber.June               ==  0.260785    
dataFrame_Treiber_X$data_Treiber.July               ==  0.013320 *     
dataFrame_Treiber_X$data_Treiber.September          ==  0.017850 *  
dataFrame_Treiber_X$data_Treiber.October            ==  0.246127 
dataFrame_Treiber_X$data_Treiber.November           ==  0.813583 
dataFrame_Treiber_X$data_Treiber.December           ==  0.004452 ** 

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag zu den Tageskarten. Dieses gilt
für folgende Variablen:
Übernachtungen,Sonn-und Feiertage,Januar, Juli, September,Dezember
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für Tageskarten da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!


Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Autokorrelation:
Autocorrelation == 0.2384679
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!

Multikollinearität:
==> die VIF-werte gehen bis 3.2, somit gilt also eine keine Multikollinearität, 
da die Werte unter 10 sind!


Ausreißer-Prüfung:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
'


#---------------------------------------------------------------------------------------------------
#Regressionsmodell Ex2 Tourimuszahlen Gesamt vor EAVs

'
Es gilt für das Regressionsmodell Ex2_GesamtVorEAVs folgendes:

X-Variablen für Ex2_Einzelfahrscheine:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 

Y-Variablen:1)Gesamt vor EAVs
'

#Regressionsmodell bilden und schätzen

Ex2_GesamtVorEAVs<-lm(dataFrame_Ertrag_Y$`Gesamt vor EAVs` ~ dataFrame_Treiber_X$data_Treiber.Übernachtungen+dataFrame_Treiber_X$data_Treiber.Samstage+dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.+dataFrame_Treiber_X$data_Treiber.January+dataFrame_Treiber_X$data_Treiber.February+dataFrame_Treiber_X$data_Treiber.March+dataFrame_Treiber_X$data_Treiber.April+dataFrame_Treiber_X$data_Treiber.May+dataFrame_Treiber_X$data_Treiber.June+dataFrame_Treiber_X$data_Treiber.July+dataFrame_Treiber_X$data_Treiber.September+dataFrame_Treiber_X$data_Treiber.October+dataFrame_Treiber_X$data_Treiber.November+dataFrame_Treiber_X$data_Treiber.December)

summary(Ex2_GesamtVorEAVs)

'
p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: 5.002e-09
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 5084000
R-squared:  0.4968
Adjusted R-squared:  0.4211 
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
eher mittelmäßig bis gering erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!
'


#MSE bestimmen
Sum_Ex2_GesamtVorEAVs<-summary(Ex2_GesamtVorEAVs)

MSE_Ex2_GesamtVorEAVs<-mean(Sum_Ex2_GesamtVorEAVs$residuals^2)
'
MSE == 2.225428e+13
'

RMSE_Ex2_GesamtVorEAVs<-sqrt(MSE_Ex2_GesamtVorEAVs)
'
RMSE = 4717444

'

#T-Test der Regressionskoeffizienten
summary(Ex2_GesamtVorEAVs)


'
T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Übernachtungen     == 1.29e-14 ***
dataFrame_Treiber_X$data_Treiber.Samstage           ==  0.74288       
dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.  == 0.87163
dataFrame_Treiber_X$data_Treiber.January            == 0.00467 ** 
dataFrame_Treiber_X$data_Treiber.February           ==  0.04564 *
dataFrame_Treiber_X$data_Treiber.March              ==  0.04404 * 
dataFrame_Treiber_X$data_Treiber.April              ==  0.31361   
dataFrame_Treiber_X$data_Treiber.May                ==  0.42091  
dataFrame_Treiber_X$data_Treiber.June               ==  0.31398    
dataFrame_Treiber_X$data_Treiber.July               ==  0.83330       
dataFrame_Treiber_X$data_Treiber.September          ==  0.14419   
dataFrame_Treiber_X$data_Treiber.October            ==  0.08015 .  
dataFrame_Treiber_X$data_Treiber.November           ==  0.02046 * 
dataFrame_Treiber_X$data_Treiber.December           ==  0.02433 * 

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Übernachtungen,Januar,Februar,März,November,Dezember

==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!

'

#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität
plot(fitted(Ex2_GesamtVorEAVs), residuals(Ex2_GesamtVorEAVs))

ols_plot_resid_fit(Ex2_GesamtVorEAVs)

'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!
'

#2)Homoskedastizität

ols_plot_resid_qq(Ex2_GesamtVorEAVs)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#3)Normalverteilung

ols_plot_resid_qq(Ex2_GesamtVorEAVs)

'
Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#4)Autokorrelation
library(car)
dwt(Ex2_GesamtVorEAVs)

'
Autokorrelation:
Autocorrelation == 0.9141242 

==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität
library(car)
vif(Ex2_GesamtVorEAVs)

'
Multikollinearität:
==> die VIF-werte gehen bis 3.2, somit gilt also eine keine Multikollinearität, 
da die Werte unter 10 sind!
'
#Ausreißer prüfen

fitHas <- Ex2_GesamtVorEAVs
h<- hatvalues(fitHas)
summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.1389'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
-------------------------------------------------------------------------------------------------------

Auswertung:
Es gilt für das Regressionsmodell Ex2_GesamtVorEAVs folgendes:

X-Variablen für Ex2_Einzelfahrscheine:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 

Y-Variablen:1)Gesamt Vor EAVs


p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: 5.002e-09
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 5084000
Adjusted R-squared:  0.4211 
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
eher mittelmäßig bis gering erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!


T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Übernachtungen     == 1.29e-14 ***
dataFrame_Treiber_X$data_Treiber.Samstage           ==  0.74288       
dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.  == 0.87163
dataFrame_Treiber_X$data_Treiber.January            == 0.00467 ** 
dataFrame_Treiber_X$data_Treiber.February           ==  0.04564 *
dataFrame_Treiber_X$data_Treiber.March              ==  0.04404 * 
dataFrame_Treiber_X$data_Treiber.April              ==  0.31361   
dataFrame_Treiber_X$data_Treiber.May                ==  0.42091  
dataFrame_Treiber_X$data_Treiber.June               ==  0.31398    
dataFrame_Treiber_X$data_Treiber.July               ==  0.83330       
dataFrame_Treiber_X$data_Treiber.September          ==  0.14419   
dataFrame_Treiber_X$data_Treiber.October            ==  0.08015 .  
dataFrame_Treiber_X$data_Treiber.November           ==  0.02046 * 
dataFrame_Treiber_X$data_Treiber.December           ==  0.02433 * 

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag zu der Zielvariable. Dieses gilt
für folgende Variablen:
Übernachtungen,Januar,Februar,März,November,Dezember
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!


Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Autokorrelation:
Autocorrelation == 0.9141242 
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!

Multikollinearität:
==> die VIF-werte gehen bis 3.2, somit gilt also eine keine Multikollinearität, 
da die Werte unter 10 sind!

Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
'

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Ex3- Einflüsse vom Arbeitsmarkt (Erwerbstätige und Pendler)

'
X-Variablen für Ex3:
1)Arbeitslose ,2)Auszubildene insgesamt,3)Ferientage,4)Arbeitstage,
5)Auspendler Brandenburg ,6)Einpendler insgesamt


Y-Variablen für Ex3:
1)Einzelfahrscheine Berlin ABC (450010), 2)Tageskarten Berlin ABC (450019) 
3)Firmenticket (450015), 4)Gesamt vor EAVs

'

#Prüfung auf Multikollinearität da Korrelation paarweise nur gut und es zu aufwändig wäre

Ex3_Einzelfahrscheine<-lm(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`~dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Ferientage+dataFrame_Treiber_X$data_Treiber.Arbeitstage+dataFrame_Treiber_X$data_Treiber..Auspendler.Brandenburg.+dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.)
summary(Ex3_Einzelfahrscheine)

# Multikollinearität prüfen
library(car)
vif(Ex3_Einzelfahrscheine)


cor.test(dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.,dataFrame_Treiber_X$data_Treiber..Auspendler.Brandenburg.)
'Ausgabe: 0.8551339  also sehr hohe Korrelation mit p-value < 2.2e-16! Dh alternative Hypoth. wird angenommen also die 
Variablen korrelieren und es ist stat. signifikant'


cor.test(dataFrame_Treiber_X$data_Treiber.Arbeitslose,dataFrame_Treiber_X$data_Treiber.Arbeitstage)
'Ausgabe: 0.05160755  also mittelmäßige Korrelation die aber nicht stat. signifikant ist da p-value = 0.5958>0.05 gilt!
Das heißt man kann die Variablen so lassen!
'

'==> die Variable "Auspendler Brandenburg" aus dem Modell entfernen da es stark mit Einpendler insgesamt korreliert!
Zudem wird "Auszubildende insgesamt" entfernt da es stark mit Arbeitslose korreliert!
'

#Neue Bildung vom Regressionsmodell
Ex3_Einzelfahrscheine<-lm(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`~dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Ferientage+dataFrame_Treiber_X$data_Treiber.Arbeitstage+dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.)
summary(Ex3_Einzelfahrscheine)
vif(Ex3_Einzelfahrscheine)
'==> Standardfehler ist kleiner geworden und das korr. Bestimmtheitsmaß hat sich bissel verbessert!
Zudem hat sich die Multikollinearität bei allen Variablen wesentlich verbessert und liegt unter 2.1!
---------------------------------------------------------------------------------------------------------
Insgesamt gilt für die Exogene Variable Einflüsse vom Arbeitsmarkt (Erwerbstätige und Pendler) folgendes:

X-Variablen für Ex3:
1)Arbeitslose,2)Ferientage,3)Arbeitstage,4)Einpendler insgesamt


Y-Variablen für Ex3:
1)Einzelfahrscheine Berlin ABC (450010), 2)Tageskarten Berlin ABC (450019) 
3)Firmenticket (450015), 4)Gesamt vor EAVs

'

#Regressionsmodell bilden und schätzen
'
Für das Regressionsmodell Ex3_Einzelfahrscheine gilt:

X-Variablen für Ex3:
1)Arbeitslose,2)Ferientage,3)Arbeitstage,4)Einpendler insgesamt


Y-Variablen für Ex3:
1)Einzelfahrscheine Berlin ABC (450010)
'

Ex3_Einzelfahrscheine<-lm(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`~dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Ferientage+dataFrame_Treiber_X$data_Treiber.Arbeitstage+dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.)
summary(Ex3_Einzelfahrscheine)

'
p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 1475000
R-squared:  0.6424
Adjusted R-squared:  0.6286 
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
ganz gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!
'

#MSE bestimmen
Sum_Ex3_Einzelfahrscheine<-summary(Ex3_Einzelfahrscheine)

MSE_Ex3_Einzelfahrscheine<-mean(Sum_Ex3_Einzelfahrscheine$residuals^2)
'
MSE == 2.074887e+12
'

RMSE_Ex3_Einzelfahrscheine<-sqrt(MSE_Ex3_Einzelfahrscheine)

'
RMSE = 1440447

'


#T-Test der Regressionskoeffizienten
summary(Ex3_Einzelfahrscheine)


'
T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             ==  < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Ferientage              ==  0.32156         
dataFrame_Treiber_X$data_Treiber.Arbeitstage             == 0.00153 ** 
dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.  == < 2e-16 *** 


Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag zu den Einzelfahrscheine. Dieses gilt
für folgende Variablen:
Arbeitslose,Arbeitstage,Einpendler insgesamt

==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für Einzelfahrscheine da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!

'

#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität
plot(fitted(Ex3_Einzelfahrscheine), residuals(Ex3_Einzelfahrscheine))

ols_plot_resid_fit(Ex3_Einzelfahrscheine)

'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!
'

#2)Homoskedastizität

ols_plot_resid_qq(Ex3_Einzelfahrscheine)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#3)Normalverteilung

ols_plot_resid_qq(Ex3_Einzelfahrscheine)

'
Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#4)Autokorrelation
library(car)
dwt(Ex3_Einzelfahrscheine)

'
Autokorrelation:
Autocorrelation == 0.317548  
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität
library(car)
vif(Ex3_Einzelfahrscheine)

'
Multikollinearität:
==> die VIF-werte sind unter 2.1, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!
'

#Ausreißer prüfen

fitHas <- Ex3_Einzelfahrscheine
h<- hatvalues(fitHas)
summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.04630'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
-------------------------------------------------------------------------------------------------------
Auswertung:
Es gilt für das Regressionsmodell Ex3_Einzelfahrscheine folgendes:

X-Variablen für Ex3:
1)Arbeitslose,2)Ferientage,3)Arbeitstage,4)Einpendler insgesamt


Y-Variablen für Ex3:
1)Einzelfahrscheine Berlin ABC (450010)


p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 1475000
Adjusted R-squared:  0.6286 
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
ganz gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!


T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             ==  < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Ferientage              ==  0.32156         
dataFrame_Treiber_X$data_Treiber.Arbeitstage             == 0.00153 ** 
dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.  == < 2e-16 *** 

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag zu den Einzelfahrscheinen. Dieses gilt
für folgende Variablen:
Arbeitslose,Arbeitstage,Einpendler insgesamt
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für Einzelfahrscheine da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!


Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Autokorrelation:
Autocorrelation == 0.317548  
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!

Multikollinearität:
==> die VIF-werte sind unter 2.1, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!

Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
'

#---------------------------------------------------------------------------------------------------
#Regressionsmodell Ex3 Einflüsse vom Arbeitsmarkt (Erwerbstätige und Pendler) - Tageskarten


#Regressionsmodell bilden und schätzen

'
Es gilt für das Regressionsmodell Ex3_Tageskarten folgendes:

X-Variablen für Ex3:
1)Arbeitslose,2)Ferientage,3)Arbeitstage,4)Einpendler insgesamt


Y-Variablen für Ex3:
1)Tageskarten Berlin ABC (450019)
'

Ex3_Tageskarten<-lm(dataFrame_Ertrag_Y$`Tageskarten ABC`~dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Ferientage+dataFrame_Treiber_X$data_Treiber.Arbeitstage+dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.)
summary(Ex3_Tageskarten)

'
p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 622500 
R-squared:  0.6867
Adjusted R-squared:  0.6745 
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
ganz gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!
'

#MSE bestimmen
Sum_Ex3_Tageskarten<-summary(Ex3_Tageskarten)

MSE_Ex3_Tageskarten<-mean(Sum_Ex3_Tageskarten$residuals^2)
'
MSE == 369552944986
'

RMSE_Ex3_Tageskarten<-sqrt(MSE_Ex3_Tageskarten)
'
RMSE = 607908.7

'


#T-Test der Regressionskoeffizienten
summary(Ex3_Tageskarten)

'
T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             ==  < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Ferientage              ==  4.78e-05 ***        
dataFrame_Treiber_X$data_Treiber.Arbeitstage             ==  0.631 
dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.  == < 2e-16 *** 

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Ferientage,Einpendler insgesamt
==>ALLE anderen Variablen (Arbeitstage) leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!

'

#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität
plot(fitted(Ex3_Tageskarten), residuals(Ex3_Tageskarten))
ols_plot_resid_fit(Ex3_Tageskarten)
'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!
'

#2)Homoskedastizität

ols_plot_resid_qq(Ex3_Tageskarten)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#3)Normalverteilung

ols_plot_resid_qq(Ex3_Tageskarten)

'
Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#4)Autokorrelation
library(car)
dwt(Ex3_Tageskarten)

'
Autokorrelation:
Autocorrelation == 0.4371336  
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität
library(car)
vif(Ex3_Tageskarten)

'
Multikollinearität:
==> die VIF-werte sind unter 2.1, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!
'

#Ausreißer prüfen

fitHas <- Ex3_Tageskarten
h<- hatvalues(fitHas)
summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.04630'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
-------------------------------------------------------------------------------------------------------
Auswertung:
Es gilt für das Regressionsmodell Ex3_Tageskarten folgendes:
X-Variablen für Ex3:
1)Arbeitslose,2)Ferientage,3)Arbeitstage,4)Einpendler insgesamt


Y-Variablen für Ex3:
1)Tageskarten Berlin ABC (450019)


p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 622500 
Adjusted R-squared:  0.6745 
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
ganz gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!


T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             ==  < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Ferientage              ==  4.78e-05 ***        
dataFrame_Treiber_X$data_Treiber.Arbeitstage             ==  0.631 
dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.  == < 2e-16 *** 

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Ferientage,Einpendler insgesamt
==>ALLE anderen Variablen (Arbeitstage) leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!


Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Autokorrelation:
Autocorrelation == 0.4371336  
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!

Multikollinearität:
==> die VIF-werte sind unter 2.1, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!

Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
'
#--------------------------------------------------------------------------------------------------------------
#Regressionsmodell Ex3 Einflüsse vom Arbeitsmarkt (Erwerbstätige und Pendler) - Firmenticket


#Regressionsmodell bilden und schätzen

'
Es gilt für das Regressionsmodell Ex3_Firmenticket folgendes:

X-Variablen für Ex3:
1)Arbeitslose,2)Ferientage,3)Einpendler insgesamt


Y-Variablen für Ex3:
1)Firmenticket (450015)
'

Ex3_Firmenticket<-lm(dataFrame_Ertrag_Y$Firmenticket ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Ferientage+dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.)
summary(Ex3_Firmenticket)

'
p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 287200
R-squared:  0.8175
Adjusted R-squared:  0.8122
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
sehr gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!
'

#MSE bestimmen
Sum_Ex3_Firmenticket<-summary(Ex3_Firmenticket)

MSE_Ex3_Firmenticket<-mean(Sum_Ex3_Firmenticket$residuals^2)
'
MSE == 79402405838
'

RMSE_Ex3_Firmenticket<-sqrt(MSE_Ex3_Firmenticket)
'
RMSE = 281784.3

'

#T-test der Regressionskoeffizienten

summary(Ex3_Firmenticket)

'
T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             ==  < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Ferientage              ==  0.179       
dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.  == < 2e-16 *** 

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Einpendler insgesamt
==>ALLE anderen Variablen (Ferientage) leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'

#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität
plot(fitted(Ex3_Firmenticket), residuals(Ex3_Firmenticket))
ols_plot_resid_fit(Ex3_Firmenticket)
'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!
'

#2)Homoskedastizität

ols_plot_resid_qq(Ex3_Firmenticket)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#3)Normalverteilung

ols_plot_resid_qq(Ex3_Firmenticket)

'
Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#4)Autokorrelation
library(car)
dwt(Ex3_Firmenticket)

'
Autokorrelation:
Autocorrelation == 0.8194063  
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität
library(car)
vif(Ex3_Firmenticket)

'
Multikollinearität:
==> die VIF-werte sind unter 2.03, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!
'

#Ausreißer prüfen

fitHas <- Ex3_Firmenticket
h<- hatvalues(fitHas)
summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.03704'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
-------------------------------------------------------------------------------------------------------
Auswertung:
Es gilt für das Regressionsmodell Ex3_Firmenticket folgendes:

X-Variablen für Ex3:
1)Arbeitslose,2)Ferientage,3)Einpendler insgesamt


Y-Variablen für Ex3:
1)Firmenticket (450015)


p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 287200
Adjusted R-squared:  0.8122
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
sehr gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!


T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             ==  < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Ferientage              ==  0.179       
dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.  == < 2e-16 *** 

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Einpendler insgesamt
==>ALLE anderen Variablen (Ferientage) leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!


Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Autokorrelation:
Autocorrelation == 0.8194063  
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!

Multikollinearität:
==> die VIF-werte sind unter 2.03, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!

Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
'
#--------------------------------------------------------------------------------------------------------------
#Regressionsmodell Ex3 Einflüsse vom Arbeitsmarkt (Erwerbstätige und Pendler) - Gesamt vor EAVs

#Regressionsmodell bilden und schätzen

'
Es gilt für das Regressionsmodell Ex3_GesamtVorEAVs folgendes:

X-Variablen für Ex3:
1)Arbeitslose,2)Ferientage,3)Arbeitstage,4)Einpendler insgesamt


Y-Variablen für Ex3:
1)Gesamt vor EAVs
'

Ex3_GesamtVorEAVs<-lm(dataFrame_Ertrag_Y$`Gesamt vor EAVs` ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Ferientage+dataFrame_Treiber_X$data_Treiber.Arbeitstage+dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.)

summary(Ex3_GesamtVorEAVs)

'
p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 3019000
R-squared:  0.8035
Adjusted R-squared:  0.7959
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
sehr gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!
'

#MSE bestimmen
Sum_Ex3_GesamtVorEAVs<-summary(Ex3_GesamtVorEAVs)

MSE_Ex3_GesamtVorEAVs<-mean(Sum_Ex3_GesamtVorEAVs$residuals^2)
'
MSE == 8.690941e+12
'

RMSE_Ex3_GesamtVorEAVs<-sqrt(MSE_Ex3_GesamtVorEAVs)
'
RMSE = 2948040

'

#T-test der Regressionskoeffizienten

summary(Ex3_GesamtVorEAVs)
'
T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
  P-werte der Regressionskoeffizienten:
  dataFrame_Treiber_X$data_Treiber.Arbeitslose             ==  < 2e-16 ***
  dataFrame_Treiber_X$data_Treiber.Ferientage              ==  0.8136   
  dataFrame_Treiber_X$data_Treiber.Arbeitstage             ==  0.0190 * 
dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.    ==  0.0227 *  
  
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
  Arbeitslose,Arbeitstage,Einpendler insgesamt
==>ALLE anderen Variablen (Ferientage) leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'

#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität
plot(fitted(Ex3_GesamtVorEAVs), residuals(Ex3_GesamtVorEAVs))
ols_plot_resid_fit(Ex3_GesamtVorEAVs)
'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!
'

#2)Homoskedastizität

ols_plot_resid_qq(Ex3_GesamtVorEAVs)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#3)Normalverteilung

ols_plot_resid_qq(Ex3_GesamtVorEAVs)

'
Normalverteilung:
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#4)Autokorrelation
library(car)
dwt(Ex3_GesamtVorEAVs)

'
Autokorrelation:
Autocorrelation ==  0.5307851
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität
library(car)
vif(Ex3_GesamtVorEAVs)

'
Multikollinearität:
==> die VIF-werte sind unter 2.04, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!
'

#Ausreißer prüfen

fitHas <- Ex3_GesamtVorEAVs
h<- hatvalues(fitHas)
summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.04630'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
-------------------------------------------------------------------------------------------------------
Auswertung:
Es gilt für das Regressionsmodell Ex3_GesamtVorEAVs folgendes:

X-Variablen für Ex3:
1)Arbeitslose,2)Ferientage,3)Arbeitstage,4)Einpendler insgesamt

Y-Variablen für Ex3:
1)Gesamt vor EAVs


p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 3019000
Adjusted R-squared:  0.7959
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
sehr gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!


T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             ==  < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Ferientage              ==  0.8136   
dataFrame_Treiber_X$data_Treiber.Arbeitstage             ==  0.0190 * 
dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.  ==  0.0227 *  
  
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
  Arbeitslose,Arbeitstage,Einpendler insgesamt
==>ALLE anderen Variablen (Ferientage) leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!


Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Normalverteilung:
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Autokorrelation:
Autocorrelation ==  0.5307851
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!

Multikollinearität:
==> die VIF-werte sind unter 2.04, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!

Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Regressionsmodell Ex4 Wetter-/Witterungsdaten

'
X-Variablen für Ex. 4:
1)Tage mit mäßigem Regen,2)Tage mit Schnee,3)Januar,4)Februar,5)März, 
6)April,7)Mai,8)Juni,9)Juli,10)September,11)Oktober,12)November,13)Dezember


Y-Variablen für Ex. 4:
1)Einzelfahrscheine Berlin ABC (450010), 2)Tageskarten Berlin ABC (450019)
3)Gesamt vor EAVs
'

#Prüfung auf Multikollinearität 
Ex4_Einzelfahrscheine<-lm(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC` ~ dataFrame_Treiber_X$data_Treiber..Tage.mit.mäßigem.Regen.+dataFrame_Treiber_X$data_Treiber..Tage.mit.Schnee.+dataFrame_Treiber_X$data_Treiber.January+dataFrame_Treiber_X$data_Treiber.February+dataFrame_Treiber_X$data_Treiber.March+dataFrame_Treiber_X$data_Treiber.April+dataFrame_Treiber_X$data_Treiber.May+dataFrame_Treiber_X$data_Treiber.June+dataFrame_Treiber_X$data_Treiber.July+dataFrame_Treiber_X$data_Treiber.September+dataFrame_Treiber_X$data_Treiber.October+dataFrame_Treiber_X$data_Treiber.November+dataFrame_Treiber_X$data_Treiber.December)
summary(Ex4_Einzelfahrscheine)

vif(Ex4_Einzelfahrscheine)

cor.test(dataFrame_Treiber_X$data_Treiber..Tage.mit.mäßigem.Regen.,dataFrame_Treiber_X$data_Treiber..Tage.mit.Schnee.)

dwt(Ex4_Einzelfahrscheine)


Ex4No2_Einzelfahrscheine<-lm(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC` ~ dataFrame_Treiber_X$data_Treiber.Ferientage+dataFrame_Treiber_X$data_Treiber.Arbeitstage+dataFrame_Treiber_X$data_Treiber..Tage.mit.mäßigem.Regen.+ dataFrame_Treiber_X$data_Treiber.January+dataFrame_Treiber_X$data_Treiber.February+dataFrame_Treiber_X$data_Treiber.March+dataFrame_Treiber_X$data_Treiber.April+dataFrame_Treiber_X$data_Treiber.May+dataFrame_Treiber_X$data_Treiber.June+dataFrame_Treiber_X$data_Treiber.July+dataFrame_Treiber_X$data_Treiber.September+dataFrame_Treiber_X$data_Treiber.October+dataFrame_Treiber_X$data_Treiber.November+dataFrame_Treiber_X$data_Treiber.December)
summary(Ex4No2_Einzelfahrscheine)

vif(Ex4No2_Einzelfahrscheine)


'==> Egal welche kombination ich mit den Wetterdaten nutze, das modell ist nicht ausreichend genug!
'
#--------------------------------------------------------------------------------------------------
#Regressionsmodell Ex4 Wetter-/Witterungsdaten-Einzelfahrscheine

Ex4_Einzelfahrscheine<-lm(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC` ~ dataFrame_Treiber_X$data_Treiber..Tage.mit.mäßigem.Regen.+dataFrame_Treiber_X$data_Treiber..Tage.mit.Schnee.+dataFrame_Treiber_X$data_Treiber.January+dataFrame_Treiber_X$data_Treiber.February+dataFrame_Treiber_X$data_Treiber.March+dataFrame_Treiber_X$data_Treiber.April+dataFrame_Treiber_X$data_Treiber.May+dataFrame_Treiber_X$data_Treiber.June+dataFrame_Treiber_X$data_Treiber.July+dataFrame_Treiber_X$data_Treiber.September+dataFrame_Treiber_X$data_Treiber.October+dataFrame_Treiber_X$data_Treiber.November+dataFrame_Treiber_X$data_Treiber.December)
summary(Ex4_Einzelfahrscheine)

'
==>p-value: 0.8082>0.05 somit ist das Modell nicht stat. signifikant und kann nict genutzt werden!
Ich habe auch schon andere Kombinationen (Andere Wettervariablen usw.) versucht, was das selbe
Ergebnis auslöste!

'
#--------------------------------------------------------------------------------------------------
#Regressionsmodell Ex4 Wetter-/Witterungsdaten-Tageskarten

Ex4_Tageskarten<-lm(dataFrame_Ertrag_Y$`Tageskarten ABC` ~ dataFrame_Treiber_X$data_Treiber..Tage.mit.mäßigem.Regen.+dataFrame_Treiber_X$data_Treiber..Tage.mit.Schnee.+dataFrame_Treiber_X$data_Treiber.January+dataFrame_Treiber_X$data_Treiber.February+dataFrame_Treiber_X$data_Treiber.March+dataFrame_Treiber_X$data_Treiber.April+dataFrame_Treiber_X$data_Treiber.May+dataFrame_Treiber_X$data_Treiber.June+dataFrame_Treiber_X$data_Treiber.July+dataFrame_Treiber_X$data_Treiber.September+dataFrame_Treiber_X$data_Treiber.October+dataFrame_Treiber_X$data_Treiber.November+dataFrame_Treiber_X$data_Treiber.December)
summary(Ex4_Tageskarten)

'
==>p-value: 0.08814>0.05 somit ist das Modell nicht stat. signifikant und kann nict genutzt werden!
Ich habe auch schon andere Kombinationen (Andere Wettervariablen usw.) versucht, was das selbe
Ergebnis auslöste!

'
#--------------------------------------------------------------------------------------------------
#Regressionsmodell Ex4 Wetter-/Witterungsdaten-Gesamt vor EAVs


Ex4_GesamtVorEAVs<-lm(dataFrame_Ertrag_Y$`Gesamt vor EAVs` ~ dataFrame_Treiber_X$data_Treiber..Tage.mit.mäßigem.Regen.+dataFrame_Treiber_X$data_Treiber..Tage.mit.Schnee.+dataFrame_Treiber_X$data_Treiber.January+dataFrame_Treiber_X$data_Treiber.February+dataFrame_Treiber_X$data_Treiber.March+dataFrame_Treiber_X$data_Treiber.April+dataFrame_Treiber_X$data_Treiber.May+dataFrame_Treiber_X$data_Treiber.June+dataFrame_Treiber_X$data_Treiber.July+dataFrame_Treiber_X$data_Treiber.September+dataFrame_Treiber_X$data_Treiber.October+dataFrame_Treiber_X$data_Treiber.November+dataFrame_Treiber_X$data_Treiber.December)
summary(Ex4_GesamtVorEAVs)
'
==>p-value: 0.8907>0.05 somit ist das Modell nicht stat. signifikant und kann nict genutzt werden!
Ich habe auch schon andere Kombinationen (Andere Wettervariablen usw.) versucht, was das selbe
Ergebnis auslöste!

'

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Exogene Variable 5: Preisentwicklung im ÖPNV (für einzelne Produktgruppen)

'
X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Dieselkraftstoff,4)Stau

Y-Variablen für Ex. 5:
1)Einzelfahrscheine Berlin ABC (450010)
2)ABO (450014) Nutzerfin.
3)Firmenticket (450015)
4)Berlin Ticket S (450018)
'

#Multikollinearität und Korrelation prüfen

Ex5_Einzelfahrscheine<-lm(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC` ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Superbenzin+dataFrame_Treiber_X$data_Treiber.Dieselkraftstoff+dataFrame_Treiber_X$data_Treiber.Stau)

summary(Ex5_Einzelfahrscheine)

library(car)
vif(Ex5_Einzelfahrscheine)
' 
Dieselkraftstoff == 28.74
Superbenzin == 26.38
==> werde dieselkraftstoff aus dem Modell entfernen!
'

Ex5No2_Einzelfahrscheine<-lm(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC` ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Superbenzin+dataFrame_Treiber_X$data_Treiber.Stau)
summary(Ex5No2_Einzelfahrscheine)

vif(Ex5No2_Einzelfahrscheine)

cor.test(dataFrame_Treiber_X$data_Treiber.Superbenzin,dataFrame_Treiber_X$data_Treiber.Dieselkraftstoff)
'
starke Korrelation von 0.963022 zwischen Dieselkraftstoff und Superbenzin vorhanden die auch noch
stat. signifikant ist! Somit muss eins der Variablen raus.


==> Multikollinearität hat sich stark gesenkt und liegt bei allen variablen bei unter 2!
Zudem hat sich der Standardfehler und das korr. Bestimmtheitsmaß verbessert.
------------------------------------------------------------------------------------------------
'

#Exogene Variable 5: Preisentwicklung im ÖPNV (für einzelne Produktgruppen)-Einzelfahrscheine

'
Für Ex5 gilt im allgemeinen:

X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5:
1)Einzelfahrscheine Berlin ABC (450010)
2)ABO (450014) Nutzerfin.
3)Firmenticket (450015)
4)Berlin Ticket S (450018)

'

#Regressionsmodell Ex5-Einzelfahrscheine

'
Für Ex5_Einzelfahrscheine gilt:

X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5:
1)Einzelfahrscheine Berlin ABC (450010)
'

#Regressionsmodell Ex5_Einzelfahrscheine bilden und schätzen

Ex5_Einzelfahrscheine<-lm(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC` ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Superbenzin+dataFrame_Treiber_X$data_Treiber.Stau)
summary(Ex5_Einzelfahrscheine)

'
p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: 2.052e-12
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 1865000 
R-squared:  0.423
Adjusted R-squared:  0.4064 
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
eher weniger gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!
'

#MSE bestimmen
Sum_Ex5_Einzelfahrscheine<-summary(Ex5_Einzelfahrscheine)

MSE_Ex5_Einzelfahrscheine<-mean(Sum_Ex5_Einzelfahrscheine$residuals^2)
'
MSE == 3.348116e+12
'

RMSE_Ex5_Einzelfahrscheine<-sqrt(MSE_Ex5_Einzelfahrscheine)
'
RMSE = 1829786

'

#T-Test der Regressionskoeffizienten
summary(Ex5_Einzelfahrscheine)

'
T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             == 1.10e-09 ***
dataFrame_Treiber_X$data_Treiber.Superbenzin             ==  2.22e-07 *** 
dataFrame_Treiber_X$data_Treiber.Stau                    ==  5.03e-07 ***
 
  
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für alle Variablen!
'

#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität
plot(fitted(Ex5_Einzelfahrscheine), residuals(Ex5_Einzelfahrscheine))
ols_plot_resid_fit(Ex5_Einzelfahrscheine)
'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!
'

#2)Homoskedastizität

ols_plot_resid_qq(Ex5_Einzelfahrscheine)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#3)Normalverteilung

ols_plot_resid_qq(Ex5_Einzelfahrscheine)

'
Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#4)Autokorrelation
library(car)
dwt(Ex5_Einzelfahrscheine)

'
Autokorrelation:
Autocorrelation ==  0.5460055 
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität
library(car)
vif(Ex5_Einzelfahrscheine)

'
Multikollinearität:
==> die VIF-werte sind unter 1.2, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!
'

#Ausreißer prüfen

fitHas <- Ex5_Einzelfahrscheine
h<- hatvalues(fitHas)
summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.03704'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
-----------------------------------------------------------------------------------------------------
Auswertung:
Es gilt für das Regressionsmodell Ex5_Einzelfahrscheine folgendes:

X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5:
1)Einzelfahrscheine Berlin ABC (450010)



p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: 2.052e-12
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 1865000 
Adjusted R-squared:  0.4064 
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
eher weniger gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!


T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             == 1.10e-09 ***
dataFrame_Treiber_X$data_Treiber.Superbenzin             ==  2.22e-07 *** 
dataFrame_Treiber_X$data_Treiber.Stau                    ==  5.03e-07 ***
 
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für alle Variablen!

Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Autokorrelation:
Autocorrelation ==  0.5460055 
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!

Multikollinearität:
==> die VIF-werte sind unter 1.2, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!

Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
'
#----------------------------------------------------------------------------------------------
#Regressionsmodell Ex5 Preisentwicklung im ÖPNV (für einzelne Produktgruppen) - Abo

'
Es gilt für das Regressionsmodell Ex5_Abo folgendes:

X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5:
1)ABO (450014) Nutzerfin

'

#Regressionsmodell bilden und schätzen

Ex5_Abo<-lm(dataFrame_Ertrag_Y$ABO ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Superbenzin+dataFrame_Treiber_X$data_Treiber.Stau)
summary(Ex5_Abo)

'
p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 1891000 
R-squared:  0.7971
Adjusted R-squared:  0.7913 
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
sehr gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!
'

#MSE bestimmen
Sum_Ex5_Abo<-summary(Ex5_Abo)

MSE_Ex5_Abo<-mean(Sum_Ex5_Abo$residuals^2)
'
MSE == 3.442437e+12
'

RMSE_Ex5_Abo<-sqrt(MSE_Ex5_Abo)
'
RMSE = 1855381

'

#T-Test der Regressionskoeffizienten
summary(Ex5_Abo)

'
T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Superbenzin             ==  < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Stau                    ==  2.81e-06 ***
 
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für alle Variablen!
'

#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität
plot(fitted(Ex5_Abo), residuals(Ex5_Abo))
ols_plot_resid_fit(Ex5_Abo)
'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!
'

#2)Homoskedastizität

ols_plot_resid_qq(Ex5_Abo)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#3)Normalverteilung

ols_plot_resid_qq(Ex5_Abo)

'
Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#4)Autokorrelation
library(car)
dwt(Ex5_Abo)

'
Autokorrelation:
Autocorrelation ==   0.851267
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität
library(car)
vif(Ex5_Abo)

'
Multikollinearität:
==> die VIF-werte sind unter 1.2, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!
'

#Ausreißer prüfen

fitHas <- Ex5_Abo
h<- hatvalues(fitHas)
summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.03704'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
-----------------------------------------------------------------------------------------------------
Auswertung:

Es gilt für das Regressionsmodell Ex5_Abo folgendes:

X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5:
1)ABO (450014) Nutzerfin


p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: < 2.2e-16
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 1891000 
Adjusted R-squared:  0.7913 
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
sehr gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!


T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Superbenzin             ==  < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Stau                    ==  2.81e-06 ***
 
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für alle Variablen!



Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Autokorrelation:
Autocorrelation ==   0.851267
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!

Multikollinearität:
==> die VIF-werte sind unter 1.2, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!

Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
'

#-------------------------------------------------------------------------------------------------
#Regressionsmodell Ex5 Preisentwicklung im ÖPNV (für einzelne Produktgruppen) - Firmenticket (450015)

'
Es gilt für das Regressionsmodell Ex5_Firmenticket folgendes:

X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5:
1)Firmenticket (450015)

'

#Regressionsmodell bilden und schätzen
Ex5_Firmenticket<-lm(dataFrame_Ertrag_Y$Firmenticket ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Superbenzin+dataFrame_Treiber_X$data_Treiber.Stau)
summary(Ex5_Firmenticket)

'
p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: 7.028e-15
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 483200
R-squared:  0.4833
Adjusted R-squared:  0.4684 
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
sehr gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!
'

#MSE bestimmen
Sum_Ex5_Firmenticket<-summary(Ex5_Firmenticket)

MSE_Ex5_Firmenticket<-mean(Sum_Ex5_Firmenticket$residuals^2)
'
MSE == 224808306567
'

RMSE_Ex5_Firmenticket<-sqrt(MSE_Ex5_Firmenticket)
'
RMSE = 474139.5

'

#T-Test der Regressionskoeffizienten
summary(Ex5_Firmenticket)

'
T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             ==  0.193 
dataFrame_Treiber_X$data_Treiber.Superbenzin             ==  4.04e-14 ***
dataFrame_Treiber_X$data_Treiber.Stau                    ==  2.73e-07 ***
 
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen: Superbenzin,Stau.
==>ALLE anderen Variablen (Arbeitslose) leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'

#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität
plot(fitted(Ex5_Firmenticket), residuals(Ex5_Firmenticket))
ols_plot_resid_fit(Ex5_Firmenticket)
'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!
'

#2)Homoskedastizität

ols_plot_resid_qq(Ex5_Firmenticket)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#3)Normalverteilung

ols_plot_resid_qq(Ex5_Firmenticket)

'
Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#4)Autokorrelation
library(car)
dwt(Ex5_Firmenticket)

'
Autokorrelation:
Autocorrelation ==   0.849446 
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität
library(car)
vif(Ex5_Firmenticket)

'
Multikollinearität:
==> die VIF-werte sind unter 1.2, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!
'

#Ausreißer prüfen

fitHas <- Ex5_Firmenticket
h<- hatvalues(fitHas)
summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.03704'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
-----------------------------------------------------------------------------------------------------
Auswertung:
Es gilt für das Regressionsmodell Ex5_Firmenticket folgendes:

X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5:
1)Firmenticket (450015)


p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: 7.028e-15
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 483200
Adjusted R-squared:  0.4684 
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
sehr gut erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!


T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             ==  0.193 
dataFrame_Treiber_X$data_Treiber.Superbenzin             ==  4.04e-14 ***
dataFrame_Treiber_X$data_Treiber.Stau                    ==  2.73e-07 ***
 
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen: Superbenzin,Stau.
==>ALLE anderen Variablen (Arbeitslose) leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!


Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Autokorrelation:
Autocorrelation ==   0.849446 
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!

Multikollinearität:
==> die VIF-werte sind unter 1.2, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!

Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
'
#--------------------------------------------------------------------------------------------------
#Regressionsmodell Ex.5 Preisentwicklung im ÖPNV (für einzelne Produktgruppen) - Berlin Ticket S (450018) 


'
Es gilt für das Regressionsmodell Ex5_BerlinTicketS folgendes:

X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5:
1)Berlin Ticket S (450018) 
'

Ex5_BerlinTicketS<-lm(dataFrame_Ertrag_Y$`Berlin Ticket S` ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Superbenzin+dataFrame_Treiber_X$data_Treiber.Stau)
summary(Ex5_BerlinTicketS)

'
p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: 4.599e-06
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 528000
R-squared:  0.2315
Adjusted R-squared:   0.2093  
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
gering erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!
'



#MSE bestimmen
Sum_Ex5_BerlinTicketS<-summary(Ex5_BerlinTicketS)

MSE_Ex5_BerlinTicketS<-mean(Sum_Ex5_BerlinTicketS$residuals^2)
'
MSE == 268507132229
'

RMSE_Ex5_BerlinTicketS<-sqrt(MSE_Ex5_BerlinTicketS)
'
RMSE = 518176.7

'


#T-Test der Regressionskoeffizienten

summary(Ex5_BerlinTicketS)

'
T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             ==  0.1053
dataFrame_Treiber_X$data_Treiber.Superbenzin             ==  0.0654 . 
dataFrame_Treiber_X$data_Treiber.Stau                    ==  4.09e-07 ***
 
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen: Stau
==>ALLE anderen Variablen (Arbeitslose,Superbenzin) leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'

#Prüfung der Regressionsvoraussetzungen 1)-5)

library(olsrr)

#1)Linearität
plot(fitted(Ex5_BerlinTicketS), residuals(Ex5_BerlinTicketS))
ols_plot_resid_fit(Ex5_BerlinTicketS)

'
Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!
'

#2)Homoskedastizität

ols_plot_resid_qq(Ex5_BerlinTicketS)

'
Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'


#3)Normalverteilung

ols_plot_resid_qq(Ex5_BerlinTicketS)

'
Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!
'

#4)Autokorrelation
library(car)
dwt(Ex5_BerlinTicketS)

'
Autokorrelation:
Autocorrelation ==   0.802911 
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!
'

#5)Multikollinearität
library(car)
vif(Ex5_BerlinTicketS)

'
Multikollinearität:
==> die VIF-werte sind unter 1.2, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!
'

#Ausreißer prüfen

fitHas <- Ex5_BerlinTicketS
h<- hatvalues(fitHas)
summary(h)
'das nutzen und Mean ablesen! Wenn in der nächsten summary der hat-wert 
zwei bis dreifach größer als der mean ist, dann ist das ein hebelwert den man
entfernen sollte! Mean == 0.03704'

inflRes<-influence.measures(fitHas)
summary(inflRes)

'
Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
-----------------------------------------------------------------------------------------------------
Auswertung:

Es gilt für das Regressionsmodell Ex5_BerlinTicketS folgendes:
X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5:
 1)Berlin Ticket S (450018) 



p-Wert,Standardfehler,korr. Bestimmtheitsmaß:
  p-value: 4.599e-06
==>dh das Modell ist stat. signifikant da p-value<0.05 gilt!
Standardfehler == 528000
Adjusted R-squared:   0.2093  
==>Das Modell wird durch die Variablen bzw. den korr.Bestimmtheitsmaß
gering erklärt!Der hohe Standardfehler existiert wegen der hohen 
Autokorrelation!


T-Test der Regressionskoeffizienten also P-Werte der Regressionskoeffizienten:
P-werte der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose             ==  0.1053
dataFrame_Treiber_X$data_Treiber.Superbenzin             ==  0.0654 . 
dataFrame_Treiber_X$data_Treiber.Stau                    ==  4.09e-07 ***
 
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen: Stau
==>ALLE anderen Variablen (Arbeitslose,Superbenzin) leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!


Linearität:
=> im Plot existiert keine regelmäßige /zufällige Verteilung der Residuen um die 0-Achse!
Viele Residuen häufen sich in einem Bereich an!Somit gilt Nicht-Linearität!

Homoskedastizität:
==> Varianzhomogenität gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Normalverteilung
==> Normalverteilung gegeben, da die Werte sehr nah an der Regressionsgerade sind
bzw. die meisten nah genug oder drauf sind!Am Ende und Anfang sind die Residuen weniger
auf der Geraden drauf, was aber normal ist!

Autokorrelation:
Autocorrelation ==   0.802911 
==> somit gilt eine starke Autokorrelation da d<1 gilt!
Wenn Autokorrelation gilt, dann ist das starker Verdacht auf Nicht-Linearität!

Multikollinearität:
==> die VIF-werte sind unter 1.2, somit gilt also keine Multikollinearität, 
da die Werte unter 10 sind!

Ausreißer-Prüfen:
Wenn nun die Cooksche Distanz also cook.d >1 gilt, dann ist dieser Ausreißer schädlich!
Hier ist es nicht der Fall, auch wenn es Hat-Werte gibt die das Doppelte vom Mean darstellen. 
Somit werde ich keinen Wert hier löschen!
'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#Umsetzung Ridge-Regression

'
Die Daten wurden bereits vorbereitet weswegen wir direkt mit dem Code beginnen können.

'

install.packages("MASS")
library(MASS)

#--------------------------------------------------------------------------------------------
#Ex1 Bevölkerungsentwicklung in Berlin (insgesamt und in Altersgruppen, Schüler, Studierende)
'
X-Variablen für Ex1:
1)Arbeitslose, 2)Bevölkerungsab-und zunahme


Y-Variablen für Ex1:
1)Monatskarten Berlin ABC (450012)  ,2)ABO (450014)  Nutzerfin. ,3)Firmenticket (450015),
4)Gesamt vor EAVs

'

#Regressionsmodell Ex1-Monatskarten

Ex1_Ridge_Monatskarten<-lm.ridge(dataFrame_Ertrag_Y$Monatskarten_ABC ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)
summary(Ex1_Ridge_Monatskarten)

'==> diese Methode bringt mir nix! Daher bleibt das weg!'


#---------------------------------------------------------------
#Regressionsmodell Ex1-Monatskarten -zweiter versuch mit glmnet

'
X-Variablen für Ex1:
1)Arbeitslose, 2)Bevölkerungsab-und zunahme

Y-Variablen für Ex1:
1)Monatskarten Berlin ABC (450012)
'


library(glmnet)

'
lieber aus beiden variablen jeweils einen Vektor bilden und das dann in einer Matrix
zusammenfassen!
'

'Dataframe als Vektoren jeweils speichern'

x_Arbeitslose_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Arbeitslose)
x_BevoelkAbZu_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)

'Alle Vektoren in einer Variable speichern'
Ex1_Ridge_XVek<-c(x_Arbeitslose_Vek,x_BevoelkAbZu_Vek)

'Variable zu einer Matrix umwandeln'
Ex1_Ridge_XMatrix<-matrix(Ex1_Ridge_XVek,ncol=2)


#Ex1_Ridge_Monatskarten Regressionsmodell mit X-Datenmatrix aus den Variablen bilden

Ex1_Ridge_Monatskarten<-glmnet(Ex1_Ridge_XMatrix,dataFrame_Ertrag_Y$Monatskarten_ABC,alpha=0)

summary(Ex1_Ridge_Monatskarten)


#Bestes Lambda für Ex1_Ridge_Monatskarten bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex1_Ridge_Monatskarten<-cv.glmnet(Ex1_Ridge_XMatrix,dataFrame_Ertrag_Y$Monatskarten_ABC,alpha=0)

bestes_Lambda<-cv_Ex1_Ridge_Monatskarten$lambda.min

'Ausgabe:51735.45 '

#Plot mit MSE und bestem Lambda
plot(cv_Ex1_Ridge_Monatskarten)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex1_Ridge_Monatskarten<-glmnet(Ex1_Ridge_XMatrix,dataFrame_Ertrag_Y$Monatskarten_ABC,alpha=0,lambda=bestes_Lambda)

#Ausgabe Koeffizienten vom Regressionsmodell mit bestem Lambda
coef(Best_Ex1_Ridge_Monatskarten)

'
3 x 1 sparse Matrix of class "dgCMatrix"
                       s0
(Intercept)  1.044966e+07
V1          -2.352067e+01
V2           2.203288e+02

'

#Bestimmtheitsmaß bestimmen

y_predicted_Ex1_Ridge_Monatskarten<-predict(Ex1_Ridge_Monatskarten,s=bestes_Lambda,newx=Ex1_Ridge_XMatrix)

'SST und SSE bestimmen'

sst<-sum((dataFrame_Ertrag_Y$Monatskarten_ABC-mean(dataFrame_Ertrag_Y$Monatskarten_ABC))^2)
sse<-sum((y_predicted_Ex1_Ridge_Monatskarten - dataFrame_Ertrag_Y$Monatskarten_ABC)^2)

rsq<-1- (sse/sst)

'
R-Quadrat bestimmen:
rsq == R-Quadrat == Bestimmtheitsmaß== 0.3923086
==> somit wird das Modell eher gering durch die Variablen erklärt!
'

#MSE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)


mse=(sum((y_predicted_Ex1_Ridge_Monatskarten-dataFrame_Ertrag_Y$Monatskarten_ABC)^2)/length(y_predicted_Ex1_Ridge_Monatskarten))
'
MSE bestimmen für Ridge Regression
MSE = 909630388712

'



#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex1_Ridge_Monatskarten<-linearRidge(dataFrame_Ertrag_Y$Monatskarten_ABC ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)

summary(Ex1_Ridge_Monatskarten)


'
T-Test der Regressionskoeffizienten

dataFrame_Treiber_X$data_Treiber.Arbeitslose                == 4.04e-10 ***
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 1.06e-09 ***

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Bevölkerungsab-und zunahme

==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!

'
library(MASS)

#Multikollinearität

library(car)
vif(Ex1_Ridge_Monatskarten)

'
geht nur wenn man linearRidge( )-Regressionsmodell hat!
ist bei beiden Variablen bei unter 2!
==> da vif < 10 gilt, haben wir keine Multikollinearität!!
'

'
Um festzustellen welches von den Regressionsmethoden also ridge,lasso oder lars
besser ist, nutzt man deren bestimmtes lambda!Je kleiner das lambda ist desto besser
ist dieses Verfahren für den datensatz geeignet!

'
'
# MSE und MAE berechnen
install.packages("caret")
library(caret)

confusionMatrix(Ex1_Ridge_Monatskarten)
==> wäre anderer Ansatz aber fällt erstmal weg!
'

#MSE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)


mse=(sum((y_predicted_Ex1_Ridge_Monatskarten-dataFrame_Ertrag_Y$Monatskarten_ABC)^2)/length(y_predicted_Ex1_Ridge_Monatskarten))

'
MSE bestimmen für Ridge Regression

man muss ein predicted regressionsmodell erstellen um MSE zu bestimmen!Daher geht es nur so
da ridge ein lambda verlangt!

MSE=909630388712
-------------------------------------------------------------------------------------------
Auswertung
Regressionsmodell Ex1-Ridge-Monatskarten
X-Variablen für Ex1:
1)Arbeitslose, 2)Bevölkerungsab-und zunahme

Y-Variablen für Ex1:
1)Monatskarten Berlin ABC (450012)


R-Quadrat bestimmen:
rsq == R-Quadrat == Bestimmtheitsmaß== 0.3923086
==> somit wird das Modell eher gering durch die Variablen erklärt!



T-Test der Regressionskoeffizienten
dataFrame_Treiber_X$data_Treiber.Arbeitslose                == 4.04e-10 ***
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. == 1.06e-09 ***

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Bevölkerungsab-und zunahme
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!


MSE bestimmen für Ridge Regression
man muss ein predicted regressionsmodell erstellen um MSE zu bestimmen!Daher geht es nur so
da ridge ein lambda verlangt!

MSE=909630388712
'
#-------------------------------------------------------------------------------------------
#Regressionsmodell Ex1-Ridge-Abo

'
Regressionsmodell Ex1-Ridge-Abo
X-Variablen für Ex1:
  1)Arbeitslose, 2)Bevölkerungsab-und zunahme

Y-Variablen für Ex1:
  1)ABO (450014)  Nutzerfin.
'

#Regressionsmodell erstellen und schätzen

'Matrix für X-Variablen'
Ex1_Ridge_XMatrix<-matrix(Ex1_Ridge_XVek,ncol=2)


Ex1_Ridge_Abo<-glmnet(Ex1_Ridge_XMatrix,dataFrame_Ertrag_Y$ABO,alpha=0)
summary(Ex1_Ridge_Abo)

#Bestes Lambda für Ex1_Ridge_Abo bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex1_Ridge_Abo<-cv.glmnet(Ex1_Ridge_XMatrix,dataFrame_Ertrag_Y$ABO,alpha=0)

bestes_Lambda<-cv_Ex1_Ridge_Abo$lambda.min

'Ausgabe:314532.5 '

#Plot mit MSE und bestem Lambda
plot(cv_Ex1_Ridge_Monatskarten)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex1_Ridge_Abo<-glmnet(Ex1_Ridge_XMatrix,dataFrame_Ertrag_Y$ABO,alpha=0,lambda=bestes_Lambda)




#Bestimmtheitsmaß bestimmen
y_predicted_Ex1_Ridge_Abo<-predict(Ex1_Ridge_Abo,s=bestes_Lambda,newx=Ex1_Ridge_XMatrix)

'SST und SSE bestimmen'

sst<-sum((dataFrame_Ertrag_Y$ABO-mean(dataFrame_Ertrag_Y$ABO))^2)
sse<-sum((y_predicted_Ex1_Ridge_Abo - dataFrame_Ertrag_Y$ABO)^2)

rsq<-1- (sse/sst)

'
R-Quadrat bestimmen:
0.6985696

==> gute erklärung der Zielvariable durch die X-Variablen
'

#MSE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)


mse=(sum((y_predicted_Ex1_Ridge_Abo-dataFrame_Ertrag_Y$ABO)^2)/length(y_predicted_Ex1_Ridge_Abo))
'
MSE bestimmen für Ridge Regression
MSE = 5.114803e+12

'


#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex1_Ridge_Abo_TTest<-linearRidge(dataFrame_Ertrag_Y$ABO ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)

summary(Ex1_Ridge_Abo_TTest)


'
T-Test der Regressionskoeffizienten

dataFrame_Treiber_X$data_Treiber.Arbeitslose                 < 2e-16 ***
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. 7.84e-11 ***

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Bevölkerungsab-und zunahme
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!

--------------------------------------------------------------------------------------------
Auswertung:
Regressionsmodell Ex1-Ridge-Abo
X-Variablen für Ex1:
  1)Arbeitslose, 2)Bevölkerungsab-und zunahme

Y-Variablen für Ex1:
  1)ABO (450014)  Nutzerfin.
  


R-Quadrat bestimmen:
0.6985696
==> gute erklärung der Zielvariable durch die X-Variablen


MSE bestimmen für Ridge Regression:
MSE = 5.114803e+12


T-Test der Regressionskoeffizienten
dataFrame_Treiber_X$data_Treiber.Arbeitslose                 < 2e-16 ***
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. 7.84e-11 ***

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Bevölkerungsab-und zunahme
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!

'
#-----------------------------------------------------------------------------------------
#Regressionmodell Ex1-Ridge-Firmenticket (450015)

'
Regressionsmodell Ex1-Ridge-Firmenticket
X-Variablen für Ex1:
  1)Arbeitslose, 2)Bevölkerungsab-und zunahme

Y-Variablen für Ex1:
  1)Firmenticket (450015)
'

#Regressionsmodell erstellen und schätzen

'Matrix für X-Variablen'
Ex1_Ridge_XMatrix<-matrix(Ex1_Ridge_XVek,ncol=2)


Ex1_Ridge_Firmenticket<-glmnet(Ex1_Ridge_XMatrix,dataFrame_Ertrag_Y$Firmenticket,alpha=0)
summary(Ex1_Ridge_Firmenticket)

#Bestes Lambda für Ex1_Ridge_Firmenticket bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex1_Ridge_Firmenticket<-cv.glmnet(Ex1_Ridge_XMatrix,dataFrame_Ertrag_Y$Firmenticket,alpha=0)

bestes_Lambda<-cv_Ex1_Ridge_Firmenticket$lambda.min

'Ausgabe:88573.21 '


#Plot mit MSE und bestem Lambda
plot(cv_Ex1_Ridge_Firmenticket)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex1_Ridge_Firmenticket<-glmnet(Ex1_Ridge_XMatrix,dataFrame_Ertrag_Y$Firmenticket,alpha=0,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex1_Ridge_Firmenticket<-predict(Ex1_Ridge_Firmenticket,s=bestes_Lambda,newx=Ex1_Ridge_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$Firmenticket-mean(dataFrame_Ertrag_Y$Firmenticket))^2)
sse<-sum((y_predicted_Ex1_Ridge_Firmenticket - dataFrame_Ertrag_Y$Firmenticket)^2)

rsq<-1- (sse/sst)

'
R-Quadrat bestimmen:
0.2838293
==> schlechte erklärung der Zielvariable durch die X-Variablen
'

#MSE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)


mse=(sum((y_predicted_Ex1_Ridge_Firmenticket-dataFrame_Ertrag_Y$Firmenticket)^2)/length(y_predicted_Ex1_Ridge_Firmenticket))
'
MSE bestimmen für Ridge Regression
MSE = 311616414265
'

#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex1_Ridge_Firmenticket_TTest<-linearRidge(dataFrame_Ertrag_Y$Firmenticket ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)

summary(Ex1_Ridge_Firmenticket_TTest)


'
T-Test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose                   0.287    
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. 2.95e-10 ***

Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Bevölkerungsab-und zunahme
==>ALLE anderen Variablen (Arbeitslose)leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
--------------------------------------------------------------------------------------------
Auswertung:
Regressionsmodell Ex1-Ridge-Firmenticket
X-Variablen für Ex1:
  1)Arbeitslose, 2)Bevölkerungsab-und zunahme

Y-Variablen für Ex1:
  1)Firmenticket (450015)


R-Quadrat bestimmen:
0.2838293
==> schlechte erklärung der Zielvariable durch die X-Variablen


MSE bestimmen für Ridge Regression:
MSE = 311616414265


T-Test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose                   0.287    
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. 2.95e-10 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Bevölkerungsab-und zunahme
==>ALLE anderen Variablen (Arbeitslose)leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'
#--------------------------------------------------------------------------------------------
#Regressionsmodell Ex1-Ridge-Gesamt vor EAVs

'
Regressionsmodell Ex1-Ridge-GesamtVorEAVs
X-Variablen für Ex1:
  1)Arbeitslose, 2)Bevölkerungsab-und zunahme

Y-Variablen für Ex1:
  1)GesamtVorEAVs
'

#Regressionsmodell bilden und schätzen

'Matrix für X-Variablen'
Ex1_Ridge_XMatrix<-matrix(Ex1_Ridge_XVek,ncol=2)


Ex1_Ridge_GesamtVorEAVs<-glmnet(Ex1_Ridge_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=0)
summary(Ex1_Ridge_GesamtVorEAVs)

#Bestes Lambda für Ex1_Ridge_GesamtVorEAVs bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex1_Ridge_GesamtVorEAVs<-cv.glmnet(Ex1_Ridge_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=0)

bestes_Lambda<-cv_Ex1_Ridge_GesamtVorEAVs$lambda.min

'Ausgabe:588534.8 '


#Plot mit MSE und bestem Lambda
plot(cv_Ex1_Ridge_GesamtVorEAVs)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex1_Ridge_GesamtVorEAVs<-glmnet(Ex1_Ridge_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=0,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex1_Ridge_GesamtVorEAVs<-predict(Ex1_Ridge_GesamtVorEAVs,s=bestes_Lambda,newx=Ex1_Ridge_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Gesamt vor EAVs`-mean(dataFrame_Ertrag_Y$`Gesamt vor EAVs`))^2)
sse<-sum((y_predicted_Ex1_Ridge_GesamtVorEAVs - dataFrame_Ertrag_Y$`Gesamt vor EAVs`)^2)

rsq<-1- (sse/sst)

'
R-Quadrat bestimmen
==>0.8059744
gute Erklärung der Zielvariable durch die X-Variablen
'

#MSE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex1_Ridge_GesamtVorEAVs-dataFrame_Ertrag_Y$`Gesamt vor EAVs`)^2)/length(y_predicted_Ex1_Ridge_GesamtVorEAVs))
'
MSE bestimmen für Ridge Regression
MSE = 8.581697e+12
'

#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex1_Ridge_GesamtVorEAVs_TTest<-linearRidge(dataFrame_Ertrag_Y$`Gesamt vor EAVs` ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)

summary(Ex1_Ridge_GesamtVorEAVs_TTest)

'
T-Test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose                 < 2e-16 ***
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. 6.03e-05 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Bevölkerungsab-und zunahme,Arbeitslose
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
----------------------------------------------------------------------------------------------------------
Auswertung:
Regressionsmodell Ex1-Ridge-GesamtVorEAVs
X-Variablen für Ex1:
  1)Arbeitslose, 2)Bevölkerungsab-und zunahme

Y-Variablen für Ex1:
  1)GesamtVorEAVs
  
  
R-Quadrat bestimmen:
==>0.8059744
gute Erklärung der Zielvariable durch die X-Variablen


MSE bestimmen für Ridge Regression:
MSE = 8.581697e+12


T-Test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose                 < 2e-16 ***
dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme. 6.03e-05 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Bevölkerungsab-und zunahme,Arbeitslose
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!

'
#--------------------------------------------------------------------------------------------
#Regressionsmodell Ex1-Ridge-Hochschulticket (Nur mit Studierende als X-Variable)

'
Regressionsmodell Ex1-Ridge-Hochschulticket
X-Variablen für Ex1:
  1)Studierende

Y-Variablen für Ex1:
  1)Hochschulticket
'


'Dataframe als Vektoren jeweils speichern'

x_Studierende_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Studierende)

'Alle Vektoren in einer Variable speichern'
Ex1_Ridge_XVek_Studierende<-c(x_Studierende_Vek)

'Variable zu einer Matrix umwandeln'
Ex1_Ridge_XMatrix_Studierende<-matrix(Ex1_Ridge_XVek_Studierende,ncol=1)


#Ex1_Ridge_Hochschulticket Regressionsmodell mit Studierende als X-Variable
library(glmnet)

'geht nicht da von der X-Datenmatrix mindestens zwei Variablen verlangt wird!!'
Ex1_Ridge_Hochschulticket<-glmnet(Ex1_Ridge_XMatrix_Studierende,dataFrame_Ertrag_Y$Hochschulticket,alpha=0)
summary(Ex1_Ridge_Hochschulticket)

'nutze daher linear ridge'
library(ridge)
Ex1_Ridge_Hochschulticket<-linearRidge(dataFrame_Ertrag_Y$Hochschulticket ~ dataFrame_Treiber_X$data_Treiber.Studierende)
'
==> FEHLER! Es werden mindestens zwei X-Variablen gefordert! Somit kann man KEIN modell bilden
für Ridge!
'




#Bestes Lambda für Ex1_Ridge_GesamtVorEAVs bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex1_Ridge_Hochschulticket<-cv.glmnet(Ex1_Ridge_XMatrix_Studierende,dataFrame_Ertrag_Y$Hochschulticket,alpha=0)

bestes_Lambda<-cv_Ex1_Ridge_GesamtVorEAVs$lambda.min
'
==> NICHT möglich zu bilden! Man braucht mindestens zwei X-Variablen!!
'


'Ausgabe:588534.8 '

'
==> für Hochschulticket mit nur einer X-Variable ist es nicht möglic Ridge und LASSO
zu bilden!!!
'

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Regressionsmodell Ex.2-Ridge-Tourismuszahlen 

'
X-Variablen für Ex2-Ridge:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 


Y-Variablen für Ex2-Ridge:
1)Einzelfahrscheine Berlin ABC (450010), 2)Tageskarten Berlin ABC (450019) 
3)Gesamt vor EAVs
'


#Regressionsmodell Ex2-Ridge-Einzelfahrscheine bilden und schätzen

'
Ex2-Ridge-Einzelfahrscheine

X-Variablen für Ex2-Ridge:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 


Y-Variablen für Ex2-Ridge:
1)Einzelfahrscheine Berlin ABC (450010)
'

#X-Matrix erstellen

'Dataframe als Vektoren jeweils speichern'

x_Uebernachtungen_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Übernachtungen)
x_Samstage_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Samstage)
x_SonnFeiertage_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.)
x_Januar_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.January)
x_Februar_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.February)
x_Maerz_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.March)
x_April_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.April)
x_Mai_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.May)
x_Juni_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.June)
x_Juli_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.July)
x_September_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.September)
x_Oktober_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.October)
x_November_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.November)
x_Dezember_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.December)


'Alle Vektoren in einer Variable speichern'
Ex2_Ridge_XVek<-c(x_Uebernachtungen_Vek,x_Samstage_Vek,x_SonnFeiertage_Vek,x_Januar_Vek,x_Februar_Vek,x_Maerz_Vek,x_April_Vek,x_Mai_Vek,x_Juni_Vek,x_Juli_Vek,x_September_Vek,x_Oktober_Vek,x_November_Vek,x_Dezember_Vek)

'Variable zu einer Matrix umwandeln'
Ex2_Ridge_XMatrix<-matrix(Ex2_Ridge_XVek,ncol=14)


#Regressionsmodell Ex2-Ridge-Einzelfahrscheine bilden und schätzen

Ex2_Ridge_Einzelfahrscheine<-glmnet(Ex2_Ridge_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=0)
summary(Ex2_Ridge_Einzelfahrscheine)

#Bestes Lambda für Ex2_Ridge_Einzelfahrscheine bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex2_Ridge_Einzelfahrscheine<-cv.glmnet(Ex2_Ridge_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=0)

bestes_Lambda<-cv_Ex2_Ridge_Einzelfahrscheine$lambda.min
'Bestes lambda: 187913.3'

#Plot mit MSE und bestem Lambda
plot(cv_Ex2_Ridge_Einzelfahrscheine)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex2_Ridge_Einzelfahrscheine<-glmnet(Ex2_Ridge_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=0,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex2_Ridge_Einzelfahrscheine<-predict(Ex2_Ridge_Einzelfahrscheine,s=bestes_Lambda,newx=Ex2_Ridge_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`-mean(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`))^2)
sse<-sum((y_predicted_Ex2_Ridge_Einzelfahrscheine - dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.8814305

sehr gute erklärung der Zielvariable durch die X-Variablen!
'

#MSE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex2_Ridge_Einzelfahrscheine-dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)^2)/length(y_predicted_Ex2_Ridge_Einzelfahrscheine))
'
MSE bestimmen für Ridge Regression
MSE = 688049431901
'

#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex2_Ridge_Einzelfahrscheine_TTest<-linearRidge(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC` ~ dataFrame_Treiber_X$data_Treiber.Samstage+dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.+dataFrame_Treiber_X$data_Treiber.Übernachtungen+dataFrame_Treiber_X$data_Treiber.January+dataFrame_Treiber_X$data_Treiber.February+dataFrame_Treiber_X$data_Treiber.March+dataFrame_Treiber_X$data_Treiber.April+dataFrame_Treiber_X$data_Treiber.May+dataFrame_Treiber_X$data_Treiber.June+dataFrame_Treiber_X$data_Treiber.July+dataFrame_Treiber_X$data_Treiber.September+dataFrame_Treiber_X$data_Treiber.October+dataFrame_Treiber_X$data_Treiber.November+dataFrame_Treiber_X$data_Treiber.December)

summary(Ex2_Ridge_Einzelfahrscheine_TTest)


'
T-test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Samstage             ==0.3418   
dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.    == 0.1126
dataFrame_Treiber_X$data_Treiber.Übernachtungen       ==< 2e-16 ***
dataFrame_Treiber_X$data_Treiber.January              == 7.43e-13 ***
dataFrame_Treiber_X$data_Treiber.February             == 0.2695
dataFrame_Treiber_X$data_Treiber.March                == 0.4906
dataFrame_Treiber_X$data_Treiber.April                == 0.0942   .  
dataFrame_Treiber_X$data_Treiber.May                  == 0.5086
dataFrame_Treiber_X$data_Treiber.June                 == 0.2420
dataFrame_Treiber_X$data_Treiber.July                 == 0.0333  *  
dataFrame_Treiber_X$data_Treiber.September            == 0.7164
dataFrame_Treiber_X$data_Treiber.October              == 0.4847   
dataFrame_Treiber_X$data_Treiber.November             == 0.0451 *  
dataFrame_Treiber_X$data_Treiber.December             == 0.0168           *  
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Übernachtungen,Januar,Juli,November,Dezember
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!

---------------------------------------------------------------------------------------------------
Auswertung:
Ex2-Ridge-Einzelfahrscheine

X-Variablen für Ex2-Ridge:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 


Y-Variablen für Ex2-Ridge:
1)Einzelfahrscheine Berlin ABC (450010)


R-Quadrat bestimmen:
==>0.8814305
sehr gute erklärung der Zielvariable durch die X-Variablen!

MSE bestimmen für Ridge Regression:
MSE = 688049431901


T-test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Samstage             ==0.3418   
dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.    == 0.1126
dataFrame_Treiber_X$data_Treiber.Übernachtungen       ==< 2e-16 ***
dataFrame_Treiber_X$data_Treiber.January              == 7.43e-13 ***
dataFrame_Treiber_X$data_Treiber.February             == 0.2695
dataFrame_Treiber_X$data_Treiber.March                == 0.4906
dataFrame_Treiber_X$data_Treiber.April                == 0.0942   .  
dataFrame_Treiber_X$data_Treiber.May                  == 0.5086
dataFrame_Treiber_X$data_Treiber.June                 == 0.2420
dataFrame_Treiber_X$data_Treiber.July                 == 0.0333  *  
dataFrame_Treiber_X$data_Treiber.September            == 0.7164
dataFrame_Treiber_X$data_Treiber.October              == 0.4847   
dataFrame_Treiber_X$data_Treiber.November             == 0.0451 *  
dataFrame_Treiber_X$data_Treiber.December             == 0.0168           *  
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Übernachtungen,Januar,Juli,November,Dezember
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'
#-----------------------------------------------------------------------------------------------------------
#Regressionsmodell Ex2-Ridge-Tageskarten

'
Ex2-Ridge-Tageskarten

X-Variablen für Ex2-Ridge:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 


Y-Variablen für Ex2-Ridge:
1) Tageskarten Berlin ABC (450019)
'


'Variable zu einer Matrix umwandeln'
Ex2_Ridge_XMatrix<-matrix(Ex2_Ridge_XVek,ncol=14)


#Regressionsmodell Ex2-Ridge-Tageskarten bilden und schätzen
library(glmnet)
Ex2_Ridge_Tageskarten<-glmnet(Ex2_Ridge_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,alpha=0)
summary(Ex2_Ridge_Tageskarten)


#Bestes Lambda für Ex2_Ridge_Tageskarten bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex2_Ridge_Tageskarten<-cv.glmnet(Ex2_Ridge_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,alpha=0)

bestes_Lambda<-cv_Ex2_Ridge_Tageskarten$lambda.min
'Bestes lambda: 105792.3'


#Plot mit MSE und bestem Lambda
plot(cv_Ex2_Ridge_Tageskarten)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex2_Ridge_Tageskarten<-glmnet(Ex2_Ridge_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,alpha=0,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex2_Ridge_Tageskarten<-predict(Ex2_Ridge_Tageskarten,s=bestes_Lambda,newx=Ex2_Ridge_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Tageskarten ABC`-mean(dataFrame_Ertrag_Y$`Tageskarten ABC`))^2)
sse<-sum((y_predicted_Ex2_Ridge_Tageskarten - dataFrame_Ertrag_Y$`Tageskarten ABC`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.9684188

fast perfekte erklärung der Zielvariable durch die X-Variablen!
'


#MSE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex2_Ridge_Tageskarten-dataFrame_Ertrag_Y$`Tageskarten ABC`)^2)/length(y_predicted_Ex2_Ridge_Tageskarten))
'
MSE bestimmen für Ridge Regression
MSE = 37246138443
'


#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex2_Ridge_Tageskarten_TTest<-linearRidge(dataFrame_Ertrag_Y$`Tageskarten ABC` ~ dataFrame_Treiber_X$data_Treiber.Samstage+dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.+dataFrame_Treiber_X$data_Treiber.Übernachtungen+dataFrame_Treiber_X$data_Treiber.January+dataFrame_Treiber_X$data_Treiber.February+dataFrame_Treiber_X$data_Treiber.March+dataFrame_Treiber_X$data_Treiber.April+dataFrame_Treiber_X$data_Treiber.May+dataFrame_Treiber_X$data_Treiber.June+dataFrame_Treiber_X$data_Treiber.July+dataFrame_Treiber_X$data_Treiber.September+dataFrame_Treiber_X$data_Treiber.October+dataFrame_Treiber_X$data_Treiber.November+dataFrame_Treiber_X$data_Treiber.December)

summary(Ex2_Ridge_Tageskarten_TTest)

'

T-test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Samstage             ==0.277326 
dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.    == 0.000929 ***
dataFrame_Treiber_X$data_Treiber.Übernachtungen       ==< 2e-16 ***
dataFrame_Treiber_X$data_Treiber.January              == 0.225000
dataFrame_Treiber_X$data_Treiber.February             == 0.114248
dataFrame_Treiber_X$data_Treiber.March                == 0.044265 *  
dataFrame_Treiber_X$data_Treiber.April                == 0.301286   
dataFrame_Treiber_X$data_Treiber.May                  == 0.084627 . 
dataFrame_Treiber_X$data_Treiber.June                 == 0.206185
dataFrame_Treiber_X$data_Treiber.July                 == 0.044311 *  
dataFrame_Treiber_X$data_Treiber.September            == 0.015026 * 
dataFrame_Treiber_X$data_Treiber.October              == 0.558247   
dataFrame_Treiber_X$data_Treiber.November             == 0.435019
dataFrame_Treiber_X$data_Treiber.December             == 0.000305  *** 
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Sonn-und Feiertage,Übernachtungen,März,Juli,September,Dezember
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!

--------------------------------------------------------------------------------------------------------
Auswertung:
Ex2-Ridge-Tageskarten

X-Variablen für Ex2-Ridge:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 


Y-Variablen für Ex2-Ridge:
1) Tageskarten Berlin ABC (450019)


R-Quadrat bestimmen:
==>0.9684188
fast perfekte erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für Ridge Regression:
MSE = 37246138443


T-test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Samstage             ==0.277326 
dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.    == 0.000929 ***
dataFrame_Treiber_X$data_Treiber.Übernachtungen       ==< 2e-16 ***
dataFrame_Treiber_X$data_Treiber.January              == 0.225000
dataFrame_Treiber_X$data_Treiber.February             == 0.114248
dataFrame_Treiber_X$data_Treiber.March                == 0.044265 *  
dataFrame_Treiber_X$data_Treiber.April                == 0.301286   
dataFrame_Treiber_X$data_Treiber.May                  == 0.084627 . 
dataFrame_Treiber_X$data_Treiber.June                 == 0.206185
dataFrame_Treiber_X$data_Treiber.July                 == 0.044311 *  
dataFrame_Treiber_X$data_Treiber.September            == 0.015026 * 
dataFrame_Treiber_X$data_Treiber.October              == 0.558247   
dataFrame_Treiber_X$data_Treiber.November             == 0.435019
dataFrame_Treiber_X$data_Treiber.December             == 0.000305  *** 
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Sonn-und Feiertage,Übernachtungen,März,Juli,September,Dezember
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'
#----------------------------------------------------------------------------------------------------------------
#Regressionsmodell Ex2-Ridge-GesamtVorEAVs

'
Ex2-Ridge-GesamtVorEAVs

X-Variablen für Ex2-Ridge:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 

Y-Variablen für Ex2-Ridge:
1) Gesamt Vor EAVs
'

'Variable zu einer Matrix umwandeln'
Ex2_Ridge_XMatrix<-matrix(Ex2_Ridge_XVek,ncol=14)


#Regressionsmodell Ex2-Ridge-GesamtVorEAVs bilden und schätzen
library(glmnet)
Ex2_Ridge_GesamtVorEAVs<-glmnet(Ex2_Ridge_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=0)
summary(Ex2_Ridge_GesamtVorEAVs)


#Bestes Lambda für Ex2_Ridge_Tageskarten bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex2_Ridge_GesamtVorEAVs<-cv.glmnet(Ex2_Ridge_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=0)

bestes_Lambda<-cv_Ex2_Ridge_GesamtVorEAVs$lambda.min
'Bestes lambda: 420402.4'


#Plot mit MSE und bestem Lambda
plot(cv_Ex2_Ridge_GesamtVorEAVs)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex2_Ridge_GesamtVorEAVs<-glmnet(Ex2_Ridge_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=0,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex2_Ridge_GesamtVorEAVs<-predict(Ex2_Ridge_GesamtVorEAVs,s=bestes_Lambda,newx=Ex2_Ridge_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Gesamt vor EAVs`-mean(dataFrame_Ertrag_Y$`Gesamt vor EAVs`))^2)
sse<-sum((y_predicted_Ex2_Ridge_GesamtVorEAVs - dataFrame_Ertrag_Y$`Gesamt vor EAVs`)^2)

rsq<-1- (sse/sst)

'
R-Quadrat bestimmen:
==>0.4869708
geringe bis mittelmäßige Erklärung der Zielvariable durch die X-Variablen!
'

#MSE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex2_Ridge_GesamtVorEAVs-dataFrame_Ertrag_Y$`Gesamt vor EAVs`)^2)/length(y_predicted_Ex2_Ridge_GesamtVorEAVs))
'
MSE bestimmen für Ridge Regression
MSE = 2.269114e+13
'



#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex2_Ridge_GesamtVorEAVs_TTest<-linearRidge(dataFrame_Ertrag_Y$`Gesamt vor EAVs` ~ dataFrame_Treiber_X$data_Treiber.Samstage+dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.+dataFrame_Treiber_X$data_Treiber.Übernachtungen+dataFrame_Treiber_X$data_Treiber.January+dataFrame_Treiber_X$data_Treiber.February+dataFrame_Treiber_X$data_Treiber.March+dataFrame_Treiber_X$data_Treiber.April+dataFrame_Treiber_X$data_Treiber.May+dataFrame_Treiber_X$data_Treiber.June+dataFrame_Treiber_X$data_Treiber.July+dataFrame_Treiber_X$data_Treiber.September+dataFrame_Treiber_X$data_Treiber.October+dataFrame_Treiber_X$data_Treiber.November+dataFrame_Treiber_X$data_Treiber.December)

summary(Ex2_Ridge_GesamtVorEAVs_TTest)

'

T-test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Samstage             ==0.996
dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.    == 0.963
dataFrame_Treiber_X$data_Treiber.Übernachtungen       ==< 2e-16 ***
dataFrame_Treiber_X$data_Treiber.January              == 0.153
dataFrame_Treiber_X$data_Treiber.February             == 0.742
dataFrame_Treiber_X$data_Treiber.March                == 0.555 
dataFrame_Treiber_X$data_Treiber.April                == 0.611  
dataFrame_Treiber_X$data_Treiber.May                  == 0.579  
dataFrame_Treiber_X$data_Treiber.June                 == 0.701
dataFrame_Treiber_X$data_Treiber.July                 == 0.386   
dataFrame_Treiber_X$data_Treiber.September            == 0.639
dataFrame_Treiber_X$data_Treiber.October              == 0.269   
dataFrame_Treiber_X$data_Treiber.November             == 0.363
dataFrame_Treiber_X$data_Treiber.December             == 0.165 
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Übernachtungen
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
-----------------------------------------------------------------------------------------------------------------
Auswertung:

Ex2-Ridge-GesamtVorEAVs

X-Variablen für Ex2-Ridge:
1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,
12)Oktober, 13)November,14)Dezember 

Y-Variablen für Ex2-Ridge:
1) Gesamt Vor EAVs


R-Quadrat bestimmen:
==>0.4869708
geringe bis mittelmäßige Erklärung der Zielvariable durch die X-Variablen!

MSE bestimmen für Ridge Regression:
MSE = 2.269114e+13


T-test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Samstage             ==0.996
dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.    == 0.963
dataFrame_Treiber_X$data_Treiber.Übernachtungen       ==< 2e-16 ***
dataFrame_Treiber_X$data_Treiber.January              == 0.153
dataFrame_Treiber_X$data_Treiber.February             == 0.742
dataFrame_Treiber_X$data_Treiber.March                == 0.555 
dataFrame_Treiber_X$data_Treiber.April                == 0.611  
dataFrame_Treiber_X$data_Treiber.May                  == 0.579  
dataFrame_Treiber_X$data_Treiber.June                 == 0.701
dataFrame_Treiber_X$data_Treiber.July                 == 0.386   
dataFrame_Treiber_X$data_Treiber.September            == 0.639
dataFrame_Treiber_X$data_Treiber.October              == 0.269   
dataFrame_Treiber_X$data_Treiber.November             == 0.363
dataFrame_Treiber_X$data_Treiber.December             == 0.165 
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Übernachtungen
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Ex3-Ridge:Exogene Variable: Einflüsse vom Arbeitsmarkt (Erwerbstätige und Pendler)

'
X-Variablen für Ex. 3:
 1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
  4)Einpendler insgesamt


Y-Variablen für Ex.3-Ridge:
1)Einzelfahrscheine Berlin ABC (450010)
2)Tageskarten Berlin ABC (450019)
3)Firmenticket (450015) [Ohne Arbeitstage in X-Variablen]
4)Gesamt vor EAVs
'

#Regressionsmodell Ex3-Ridge-Einzelfahrscheine


'Dataframe als Vektoren jeweils speichern'

x_Arbeitslose_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Arbeitslose)
x_Ferientage_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Ferientage)
x_Arbeitstage_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Arbeitstage)
x_EinpendlerInsgesamt<-as.vector(dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.)



'Alle Vektoren in einer Variable speichern'
Ex3_Ridge_XVek<-c(x_Arbeitslose_Vek,x_Ferientage_Vek,x_Arbeitstage_Vek,x_EinpendlerInsgesamt)

Ex3_Ridge_XVek_OhneArbeitstage<-c(x_Arbeitslose_Vek,x_Ferientage_Vek,x_EinpendlerInsgesamt)

'Variable zu einer Matrix umwandeln'
Ex3_Ridge_XMatrix<-matrix(Ex3_Ridge_XVek,ncol=4)
Ex3_Ridge_XMatrix_OhneArbeitstage<-matrix(Ex3_Ridge_XVek_OhneArbeitstage,ncol=3)


#Regressionsmodell Ex3-Ridge-Einzelfahrscheine bilden und schätzen
'
Regressionsmodell Ex3-Ridge-Einzelfahrscheine
X-Variablen für Ex. 3:
1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
4)Einpendler insgesamt


Y-Variablen für Ex.3-Ridge:
1)Einzelfahrscheine Berlin ABC (450010)
'

library(glmnet)
Ex3_Ridge_Einzelfahrscheine<-glmnet(Ex3_Ridge_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=0)
summary(Ex3_Ridge_Einzelfahrscheine)


#Bestes Lambda für Ex3_Ridge_Einzelfahrscheine bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex3_Ridge_Einzelfahrscheine<-cv.glmnet(Ex3_Ridge_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=0)

bestes_Lambda<-cv_Ex3_Ridge_Einzelfahrscheine$lambda.min
'Bestes lambda: 90654.21'



#Plot mit MSE und bestem Lambda
plot(cv_Ex3_Ridge_Einzelfahrscheine)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex3_Ridge_Einzelfahrscheine<-glmnet(Ex3_Ridge_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=0,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex3_Ridge_Einzelfahrscheine<-predict(Ex3_Ridge_Einzelfahrscheine,s=bestes_Lambda,newx=Ex3_Ridge_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`-mean(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`))^2)
sse<-sum((y_predicted_Ex3_Ridge_Einzelfahrscheine - dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)^2)

rsq<-1- (sse/sst)

'
R-Quadrat bestimmen:
==>0.6341508
gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex3_Ridge_Einzelfahrscheine-dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)^2)/length(y_predicted_Ex3_Ridge_Einzelfahrscheine))
'
MSE bestimmen für Ridge Regression
MSE = 2.122993e+12
'

mae=(sum(abs(y_predicted_Ex3_Ridge_Einzelfahrscheine-dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)/length(y_predicted_Ex3_Ridge_Einzelfahrscheine)  ))
'
MAE bestimmen für Ridge-Regression:
MAE=884080.4
'


#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex3_Ridge_Einzelfahrscheine_TTest<-linearRidge(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC` ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Ferientage+dataFrame_Treiber_X$data_Treiber.Arbeitstage+dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.)

summary(Ex3_Ridge_Einzelfahrscheine_TTest)

'


T-Test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose                  ==       3.3e-08 ***
dataFrame_Treiber_X$data_Treiber.Ferientage                   ==       0.47444    
dataFrame_Treiber_X$data_Treiber.Arbeitstage                  ==       0.11629    
dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.       ==       0.00013 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Einpendler insgesamt
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
----------------------------------------------------------------------------------------------
Auswertung:
Regressionsmodell Ex3-Ridge-Einzelfahrscheine
X-Variablen für Ex. 3:
1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
4)Einpendler insgesamt

Y-Variablen für Ex.3-Ridge:
1)Einzelfahrscheine Berlin ABC (450010)


R-Quadrat bestimmen:
==>0.6341508
gute Erklärung der Zielvariable durch die X-Variablen!

MSE bestimmen für Ridge Regression
MSE = 2.122993e+12

MAE bestimmen für Ridge-Regression:
MAE=884080.4


T-Test der Regressionskoeffizienten:
==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Einpendler insgesamt
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'
#----------------------------------------------------------------------------------------------
#Regressionsmodell Ex3-Ridge-Tageskarten

'
Regressionsmodell Ex3-Ridge-Tageskarten
X-Variablen für Ex. 3:
1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
4)Einpendler insgesamt

Y-Variablen für Ex.3-Ridge:
1)Tageskarten Berlin ABC (450019)

'

'Variable zu einer Matrix umwandeln'
Ex3_Ridge_XMatrix<-matrix(Ex3_Ridge_XVek,ncol=4)
Ex3_Ridge_XMatrix_OhneArbeitstage<-matrix(Ex3_Ridge_XVek_OhneArbeitstage,ncol=3)

#Regressionsmodell Ex3-Ridge-Tageskarten bilden und schätzen

library(glmnet)
Ex3_Ridge_Tageskarten<-glmnet(Ex3_Ridge_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,alpha=0)
summary(Ex3_Ridge_Tageskarten)


#Bestes Lambda für Ex3_Ridge_Einzelfahrscheine bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex3_Ridge_Tageskarten<-cv.glmnet(Ex3_Ridge_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,alpha=0)

bestes_Lambda<-cv_Ex3_Ridge_Tageskarten$lambda.min
'Bestes lambda: 52294.28'


#Plot mit MSE und bestem Lambda
plot(cv_Ex3_Ridge_Tageskarten)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex3_Ridge_Tageskarten<-glmnet(Ex3_Ridge_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,alpha=0,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex3_Ridge_Tageskarten<-predict(Ex3_Ridge_Tageskarten,s=bestes_Lambda,newx=Ex3_Ridge_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Tageskarten ABC`-mean(dataFrame_Ertrag_Y$`Tageskarten ABC`))^2)
sse<-sum((y_predicted_Ex3_Ridge_Tageskarten - dataFrame_Ertrag_Y$`Tageskarten ABC`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.6739603
gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex3_Ridge_Tageskarten-dataFrame_Ertrag_Y$`Tageskarten ABC`)^2)/length(y_predicted_Ex3_Ridge_Tageskarten))
'
MSE bestimmen für Ridge Regression
MSE = 384523784662
'

mae=(sum(abs(y_predicted_Ex3_Ridge_Tageskarten-dataFrame_Ertrag_Y$`Tageskarten ABC`)/length(y_predicted_Ex3_Ridge_Tageskarten)  ))
'
MAE bestimmen für Ridge-Regression:
MAE= 463443.2
'

#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex3_Ridge_Tageskarten_TTest<-linearRidge(dataFrame_Ertrag_Y$`Tageskarten ABC` ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Ferientage+dataFrame_Treiber_X$data_Treiber.Arbeitstage+dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.)

summary(Ex3_Ridge_Tageskarten_TTest)

'
T-Test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose                  ==       < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Ferientage                   ==       0.00258 **    
dataFrame_Treiber_X$data_Treiber.Arbeitstage                  ==       0.81144    
dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.       ==      1.27e-10 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Ferientage,Einpendler insgesamt
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
----------------------------------------------------------------------------------------------
Auswertung:
Regressionsmodell Ex3-Ridge-Tageskarten
X-Variablen für Ex. 3:
1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
4)Einpendler insgesamt

Y-Variablen für Ex.3-Ridge:
1)Tageskarten Berlin ABC (450019)


R-Quadrat bestimmen:
==>0.6739603
gute Erklärung der Zielvariable durch die X-Variablen!

MSE bestimmen für Ridge Regression:
MSE = 384523784662

MAE bestimmen für Ridge-Regression:
MAE= 463443.2

T-Test der Regressionskoeffizienten:
==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Ferientage,Einpendler insgesamt
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'
#---------------------------------------------------------------------------------------------------
#Regressionsmodell Ex3-Ridge-Firmenticket (Ohne Arbeitstage)

'
Regressionsmodell Ex3-Ridge-Tageskarten (Ohne Arbeitstage als X-Variable)
X-Variablen für Ex. 3:
1)Arbeitslose ,2)Ferientage, 3)Einpendler insgesamt

Y-Variablen für Ex.3-Ridge:
1)Firmenticket (450015)
'

'Variable zu einer Matrix umwandeln'
Ex3_Ridge_XMatrix<-matrix(Ex3_Ridge_XVek,ncol=4)
Ex3_Ridge_XMatrix_OhneArbeitstage<-matrix(Ex3_Ridge_XVek_OhneArbeitstage,ncol=3)

#Regressionsmodell Ex3-Ridge-Tageskarten bilden und schätzen

library(glmnet)
Ex3_Ridge_Firmenticket<-glmnet(Ex3_Ridge_XMatrix_OhneArbeitstage,dataFrame_Ertrag_Y$Firmenticket,alpha=0)
summary(Ex3_Ridge_Firmenticket)


#Bestes Lambda für Ex3_Ridge_Einzelfahrscheine bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex3_Ridge_Firmenticket<-cv.glmnet(Ex3_Ridge_XMatrix_OhneArbeitstage,dataFrame_Ertrag_Y$Firmenticket,alpha=0)

bestes_Lambda<-cv_Ex3_Ridge_Firmenticket$lambda.min
'Bestes lambda: 48180.38'


#Plot mit MSE und bestem Lambda
plot(cv_Ex3_Ridge_Firmenticket)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex3_Ridge_Firmenticket<-glmnet(Ex3_Ridge_XMatrix_OhneArbeitstage,dataFrame_Ertrag_Y$Firmenticket,alpha=0,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex3_Ridge_Firmenticket<-predict(Ex3_Ridge_Firmenticket,s=bestes_Lambda,newx=Ex3_Ridge_XMatrix_OhneArbeitstage)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$Firmenticket-mean(dataFrame_Ertrag_Y$Firmenticket))^2)
sse<-sum((y_predicted_Ex3_Ridge_Firmenticket - dataFrame_Ertrag_Y$Firmenticket)^2)
rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.7928274
gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex3_Ridge_Firmenticket-dataFrame_Ertrag_Y$Firmenticket)^2)/length(y_predicted_Ex3_Ridge_Firmenticket))
'
MSE bestimmen für Ridge Regression
MSE = 90143835208
'

mae=(sum(abs(y_predicted_Ex3_Ridge_Firmenticket-dataFrame_Ertrag_Y$Firmenticket)/length(y_predicted_Ex3_Ridge_Firmenticket)  ))
'
MAE bestimmen für Ridge-Regression:
MAE= 463443.2
'

#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex3_Ridge_Firmenticket_TTest<-linearRidge(dataFrame_Ertrag_Y$Firmenticket ~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Ferientage+dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.)

summary(Ex3_Ridge_Firmenticket_TTest)

'
T-Test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose                  ==       < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Ferientage                   ==       0.354    
dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.       ==      <2e-16 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Einpendler insgesamt
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
--------------------------------------------------------------------------------------------------
Auswertung:
Regressionsmodell Ex3-Ridge-Tageskarten (Ohne Arbeitstage als X-Variable)
X-Variablen für Ex. 3:
1)Arbeitslose ,2)Ferientage, 3)Einpendler insgesamt

Y-Variablen für Ex.3-Ridge:
1)Firmenticket (450015)


R-Quadrat bestimmen:
==>0.7928274
gute Erklärung der Zielvariable durch die X-Variablen!

MSE bestimmen für Ridge Regression
MSE = 90143835208

MAE bestimmen für Ridge-Regression:
MAE= 463443.2


T-Test der Regressionskoeffizienten:
==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Einpendler insgesamt
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'
#-----------------------------------------------------------------------------------------------
#Regressionsmodell Ex.3-Ridge-Gesamt Vor EAVs
'
Regressionsmodell Ex3-Ridge-Gesamt EAVs
X-Variablen für Ex. 3:
1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
4)Einpendler insgesamt

Y-Variablen für Ex.3-Ridge:
1)Gesamt Vor EAVs
'

#Regressionsmodell Ex.3-Ridge-Gesamt EAVs bilden und schätzen
'Variable zu einer Matrix umwandeln'
Ex3_Ridge_XMatrix<-matrix(Ex3_Ridge_XVek,ncol=4)


library(glmnet)
Ex3_Ridge_GesamtVorEAVs<-glmnet(Ex3_Ridge_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=0)
summary(Ex3_Ridge_GesamtVorEAVs)


#Bestes Lambda für Ex3_Ridge_GesamtVorEAVsbestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex3_Ridge_GesamtVorEAVs<-cv.glmnet(Ex3_Ridge_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=0)

bestes_Lambda<-cv_Ex3_Ridge_GesamtVorEAVs$lambda.min
'Bestes lambda: 588534.8'


#Plot mit MSE und bestem Lambda
plot(cv_Ex3_Ridge_GesamtVorEAVs)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex3_Ridge_GesamtVorEAVs<-glmnet(Ex3_Ridge_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=0,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex3_Ridge_GesamtVorEAVs<-predict(Ex3_Ridge_GesamtVorEAVs,s=bestes_Lambda,newx=Ex3_Ridge_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Gesamt vor EAVs`-mean(dataFrame_Ertrag_Y$`Gesamt vor EAVs`))^2)
sse<-sum((y_predicted_Ex3_Ridge_GesamtVorEAVs - dataFrame_Ertrag_Y$`Gesamt vor EAVs`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.7915416
gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex3_Ridge_GesamtVorEAVs-dataFrame_Ertrag_Y$`Gesamt vor EAVs`)^2)/length(y_predicted_Ex3_Ridge_GesamtVorEAVs))

'
MSE bestimmen für Ridge Regression
MSE = 9.220054e+12
'

mae=(sum(abs(y_predicted_Ex3_Ridge_GesamtVorEAVs-dataFrame_Ertrag_Y$`Gesamt vor EAVs`)/length(y_predicted_Ex3_Ridge_GesamtVorEAVs)  ))

'
MAE bestimmen für Ridge-Regression:
MAE= 2188024
'

#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex3_Ridge_GesamtVorEAVS_TTest<-linearRidge(dataFrame_Ertrag_Y$`Gesamt vor EAVs`~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Ferientage+dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.+dataFrame_Treiber_X$data_Treiber.Arbeitstage)

summary(Ex3_Ridge_GesamtVorEAVS_TTest)

'
T-test der Regressionskoeffizienten
dataFrame_Treiber_X$data_Treiber.Arbeitslose                      ==   <2e-16 ***
dataFrame_Treiber_X$data_Treiber.Ferientage                       ==    0.7976    
dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.           ==   0.0871 .  
dataFrame_Treiber_X$data_Treiber.Arbeitstage                      ==   0.0200 *  
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Arbeitstage
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
---------------------------------------------------------------------------------------------
Auswertung:
Regressionsmodell Ex3-Ridge-Gesamt EAVs
X-Variablen für Ex. 3:
1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
4)Einpendler insgesamt

Y-Variablen für Ex.3-Ridge:
1)Gesamt Vor EAVs


R-Quadrat bestimmen:
==>0.7915416
gute Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für Ridge Regression
MSE = 9.220054e+12


MAE bestimmen für Ridge-Regression:
MAE= 2188024


T-test der Regressionskoeffizienten
dataFrame_Treiber_X$data_Treiber.Arbeitslose                      ==   <2e-16 ***
dataFrame_Treiber_X$data_Treiber.Ferientage                       ==    0.7976    
dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.           ==   0.0871 .  
dataFrame_Treiber_X$data_Treiber.Arbeitstage                      ==   0.0200 *  
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Arbeitstage
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
-------------------------------------------------------------------------------------------------
'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'
==>Ex4 wird komplett weggelassen, da wir bei der multivariaten Regression festgestellt hatten, dass
alle Regressionsmodelle von Ex4 nicht stat. signifikant sind! Diese Feststellung gilt auch für alle
anderen Regressionsmethoden!
'


#Regressionsmodelle Exogene Variable 5-Ridge: Preisentwicklung im ÖPNV (für einzelne Produktgruppen)

'
X-Variablen für Ex. 5-Ridge:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5-Ridge:
1)Einzelfahrscheine Berlin ABC (450010)
2)ABO (450014) Nutzerfin.
3)Firmenticket (450015)
4)Berlin Ticket S (450018)
'

'Dataframe als Vektoren jeweils speichern'

x_Arbeitslose_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Arbeitslose)
x_Superbenzin_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Superbenzin)
x_Stau_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Stau)

'Alle Vektoren in einer Variable speichern'
Ex5_Ridge_XVek<-c(x_Arbeitslose_Vek,x_Superbenzin_Vek,x_Stau_Vek)

'Variable zu einer Matrix umwandeln'
Ex5_Ridge_XMatrix<-matrix(Ex5_Ridge_XVek,ncol=3)



#Regressionsmodell Ex5-Ridge-Einzelfahrscheine - bilden und schätzen
'
Regressionsmodell Ex5-Ridge-Einzelfahrscheine
X-Variablen für Ex. 5-Ridge:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5-Ridge:
1)Einzelfahrscheine Berlin ABC (450010)

'

Ex5_Ridge_XMatrix<-matrix(Ex5_Ridge_XVek,ncol=3)

library(glmnet)
Ex5_Ridge_Einzelfahrscheine<-glmnet(Ex5_Ridge_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=0)
summary(Ex5_Ridge_Einzelfahrscheine)


#Bestes Lambda für Ex5_Ridge_Eizelfahrscheine bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex5_Ridge_Einzelfahrscheine<-cv.glmnet(Ex5_Ridge_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=0)

bestes_Lambda<-cv_Ex5_Ridge_Einzelfahrscheine$lambda.min
'Bestes lambda: 90654.21'


#Plot mit MSE und bestem Lambda
plot(cv_Ex5_Ridge_Einzelfahrscheine)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex5_Ridge_Einzelfahrscheine<-glmnet(Ex5_Ridge_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=0,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex5_Ridge_Einzelfahrscheine<-predict(Ex5_Ridge_Einzelfahrscheine,s=bestes_Lambda,newx=Ex5_Ridge_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`-mean(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`))^2)
sse<-sum((y_predicted_Ex5_Ridge_Einzelfahrscheine - dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.4216922
geringe bis mittelmäßig gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex5_Ridge_Einzelfahrscheine-dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)^2)/length(y_predicted_Ex5_Ridge_Einzelfahrscheine))

'
MSE bestimmen für Ridge Regression
MSE = 3.355874e+12
'

mae=(sum(abs(y_predicted_Ex5_Ridge_Einzelfahrscheine-dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)/length(y_predicted_Ex5_Ridge_Einzelfahrscheine)  ))

'
MAE bestimmen für Ridge-Regression:
MAE= 1275679
'

#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex5_Ridge_Einzelfahrscheine_TTest<-linearRidge(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Superbenzin+dataFrame_Treiber_X$data_Treiber.Stau)

summary(Ex5_Ridge_Einzelfahrscheine_TTest)

'
T-test der Regressionskoeffizienten
dataFrame_Treiber_X$data_Treiber.Arbeitslose                      ==   <2e-16 ***
dataFrame_Treiber_X$data_Treiber.Superbenzin                      == 3.76e-08 ***
dataFrame_Treiber_X$data_Treiber.Stau                             == 8.73e-08 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Superbenzin,Stau
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
--------------------------------------------------------------------------------------------------
Auswertung:
Regressionsmodell Ex5-Ridge-Einzelfahrscheine
X-Variablen für Ex. 5-Ridge:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5-Ridge:
1)Einzelfahrscheine Berlin ABC (450010)


R-Quadrat bestimmen:
==>0.4216922
geringe bis mittelmäßig gute Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für Ridge Regression
MSE = 3.355874e+12


MAE bestimmen für Ridge-Regression:
MAE= 1275679


T-test der Regressionskoeffizienten
dataFrame_Treiber_X$data_Treiber.Arbeitslose                      ==   <2e-16 ***
dataFrame_Treiber_X$data_Treiber.Superbenzin                      == 3.76e-08 ***
dataFrame_Treiber_X$data_Treiber.Stau                             == 8.73e-08 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Superbenzin,Stau
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'
#-------------------------------------------------------------------------------------------------
#Regressionsmodell Ex5-Ridge-ABO (450014) Nutzerfin.

'
Regressionsmodell Ex5-Ridge-Einzelfahrscheine
X-Variablen für Ex. 5-Ridge:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5-Ridge:
1)ABO (450014) Nutzerfin.
'

#Regressionsmodell Ex5-Ridge-Abo- bilden und schätzen

Ex5_Ridge_XMatrix<-matrix(Ex5_Ridge_XVek,ncol=3)

library(glmnet)
Ex5_Ridge_Abo<-glmnet(Ex5_Ridge_XMatrix,dataFrame_Ertrag_Y$ABO,alpha=0)
summary(Ex5_Ridge_Abo)


#Bestes Lambda für Ex5_Ridge_Eizelfahrscheine bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex5_Ridge_Abo<-cv.glmnet(Ex5_Ridge_XMatrix,dataFrame_Ertrag_Y$ABO,alpha=0)

bestes_Lambda<-cv_Ex5_Ridge_Abo$lambda.min
'Bestes lambda: 90654.21'


#Plot mit MSE und bestem Lambda
plot(cv_Ex5_Ridge_Abo)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex5_Ridge_Abo<-glmnet(Ex5_Ridge_XMatrix,dataFrame_Ertrag_Y$ABO,alpha=0,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex5_Ridge_Abo<-predict(Ex5_Ridge_Abo,s=bestes_Lambda,newx=Ex5_Ridge_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$ABO-mean(dataFrame_Ertrag_Y$ABO))^2)
sse<-sum((y_predicted_Ex5_Ridge_Abo - dataFrame_Ertrag_Y$ABO)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.7945193
geringe bis mittelmäßig gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex5_Ridge_Abo-dataFrame_Ertrag_Y$ABO)^2)/length(y_predicted_Ex5_Ridge_Abo))

'
MSE bestimmen für Ridge Regression
MSE = 3.486686e+12
'

mae=(sum(abs(y_predicted_Ex5_Ridge_Abo-dataFrame_Ertrag_Y$ABO)/length(y_predicted_Ex5_Ridge_Abo)  ))

'
MAE bestimmen für Ridge-Regression:
MAE= 1442645
'

#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex5_Ridge_Abo_TTest<-linearRidge(dataFrame_Ertrag_Y$ABO~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Superbenzin+dataFrame_Treiber_X$data_Treiber.Stau)

summary(Ex5_Ridge_Abo_TTest)

'
T-test der Regressionskoeffizienten
dataFrame_Treiber_X$data_Treiber.Arbeitslose                      ==   <2e-16 ***
dataFrame_Treiber_X$data_Treiber.Superbenzin                      == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Stau                             == 6.77e-07 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Superbenzin,Stau
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
--------------------------------------------------------------------------------------------------
Auswertung:
Regressionsmodell Ex5-Ridge-Einzelfahrscheine
X-Variablen für Ex. 5-Ridge:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5-Ridge:
1)ABO (450014) Nutzerfin.


R-Quadrat bestimmen:
==>0.7945193
gute bis sehr gute Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für Ridge Regression
MSE = 3.486686e+12


MAE bestimmen für Ridge-Regression:
MAE= 1442645


T-test der Regressionskoeffizienten
dataFrame_Treiber_X$data_Treiber.Arbeitslose                      ==   <2e-16 ***
dataFrame_Treiber_X$data_Treiber.Superbenzin                      == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Stau                             == 6.77e-07 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Arbeitslose,Superbenzin,Stau
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'
#-------------------------------------------------------------------------------------------------
#Regressionsmodell Ex.5-Ridge-Firmenticket (450015)

'
Regressionsmodell Ex5-Ridge-Firmenticket

X-Variablen für Ex. 5-Ridge:
1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5-Ridge:
1)Firmenticket (450015)
'

#Regressionsmodell Ex5-Ridge-Firmenticket- bilden und schätzen

Ex5_Ridge_XMatrix<-matrix(Ex5_Ridge_XVek,ncol=3)

library(glmnet)
Ex5_Ridge_Firmenticket<-glmnet(Ex5_Ridge_XMatrix,dataFrame_Ertrag_Y$Firmenticket,alpha=0)
summary(Ex5_Ridge_Firmenticket)


#Bestes Lambda für Ex5_Ridge_Eizelfahrscheine bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex5_Ridge_Firmenticket<-cv.glmnet(Ex5_Ridge_XMatrix,dataFrame_Ertrag_Y$Firmenticket,alpha=0)

bestes_Lambda<-cv_Ex5_Ridge_Firmenticket$lambda.min
'Bestes lambda: 37762.06'


#Plot mit MSE und bestem Lambda
plot(cv_Ex5_Ridge_Firmenticket)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex5_Ridge_Firmenticket<-glmnet(Ex5_Ridge_XMatrix,dataFrame_Ertrag_Y$Firmenticket,alpha=0,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex5_Ridge_Firmenticket<-predict(Ex5_Ridge_Firmenticket,s=bestes_Lambda,newx=Ex5_Ridge_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$Firmenticket-mean(dataFrame_Ertrag_Y$Firmenticket ))^2)
sse<-sum((y_predicted_Ex5_Ridge_Firmenticket - dataFrame_Ertrag_Y$Firmenticket)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.480881
geringe bis mittelmäßig gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex5_Ridge_Firmenticket-dataFrame_Ertrag_Y$Firmenticket)^2)/length(y_predicted_Ex5_Ridge_Firmenticket))

'
MSE bestimmen für Ridge Regression
MSE = 225876349100
'

mae=(sum(abs(y_predicted_Ex5_Ridge_Firmenticket-dataFrame_Ertrag_Y$Firmenticket)/length(y_predicted_Ex5_Ridge_Firmenticket)  ))

'
MAE bestimmen für Ridge-Regression:
MAE= 355921.7
'

#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex5_Ridge_Firmenticket_TTest<-linearRidge(dataFrame_Ertrag_Y$Firmenticket~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Superbenzin+dataFrame_Treiber_X$data_Treiber.Stau)

summary(Ex5_Ridge_Firmenticket_TTest)

'
T-test der Regressionskoeffizienten
dataFrame_Treiber_X$data_Treiber.Arbeitslose                      ==   0.222 
dataFrame_Treiber_X$data_Treiber.Superbenzin                      == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Stau                             == 4.13e-08 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Superbenzin,Stau
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
--------------------------------------------------------------------------------------------------
Auswertung:
Regressionsmodell Ex5-Ridge-Firmenticket

X-Variablen für Ex. 5-Ridge:
1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5-Ridge:
1)Firmenticket (450015)


R-Quadrat bestimmen:
==>0.480881
geringe bis mittelmäßig gute Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für Ridge Regression
MSE = 225876349100


MAE bestimmen für Ridge-Regression:
MAE= 355921.7


T-test der Regressionskoeffizienten:
dataFrame_Treiber_X$data_Treiber.Arbeitslose                      ==   0.222 
dataFrame_Treiber_X$data_Treiber.Superbenzin                      == < 2e-16 ***
dataFrame_Treiber_X$data_Treiber.Stau                             == 4.13e-08 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Superbenzin,Stau
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'
#--------------------------------------------------------------------------------------------------
#Regressionsmodell Ex.5-Ridge-Berlin Ticket S (450018)

'
Regressionsmodell Ex5-Ridge-Berlin Ticket S (450018)

X-Variablen für Ex. 5-Ridge:
1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5-Ridge:
1)Berlin Ticket S (450018)
'

#Regressionsmodell Ex5-Ridge-Berlin Ticket S- bilden und schätzen

Ex5_Ridge_XMatrix<-matrix(Ex5_Ridge_XVek,ncol=3)

library(glmnet)
Ex5_Ridge_BerlinTicketS<-glmnet(Ex5_Ridge_XMatrix,dataFrame_Ertrag_Y$`Berlin Ticket S`,alpha=0)
summary(Ex5_Ridge_BerlinTicketS)


#Bestes Lambda für Ex5_Ridge_Berlin Ticket S bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex5_Ridge_BerlinTicketS<-cv.glmnet(Ex5_Ridge_XMatrix,dataFrame_Ertrag_Y$`Berlin Ticket S`,alpha=0)

bestes_Lambda<-cv_Ex5_Ridge_BerlinTicketS$lambda.min
'Bestes lambda: 128015.4'


#Plot mit MSE und bestem Lambda
plot(cv_Ex5_Ridge_BerlinTicketS)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex5_Ridge_BerlinTicketS<-glmnet(Ex5_Ridge_XMatrix,dataFrame_Ertrag_Y$`Berlin Ticket S`,alpha=0,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex5_Ridge_BerlinTicketS<-predict(Ex5_Ridge_BerlinTicketS,s=bestes_Lambda,newx=Ex5_Ridge_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Berlin Ticket S`-mean(dataFrame_Ertrag_Y$`Berlin Ticket S` ))^2)
sse<-sum((y_predicted_Ex5_Ridge_BerlinTicketS - dataFrame_Ertrag_Y$`Berlin Ticket S`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.2214869
schlechte (geringe) Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für Ridge Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex5_Ridge_BerlinTicketS-dataFrame_Ertrag_Y$`Berlin Ticket S`)^2)/length(y_predicted_Ex5_Ridge_BerlinTicketS))

'
MSE bestimmen für Ridge Regression
MSE = 272002110297
'

mae=(sum(abs(y_predicted_Ex5_Ridge_BerlinTicketS-dataFrame_Ertrag_Y$`Berlin Ticket S`)/length(y_predicted_Ex5_Ridge_BerlinTicketS)  ))

'
MAE bestimmen für Ridge-Regression:
MAE= 390614.1
'

#T-Test der Regressionskoeffizienten
install.packages("ridge")
library(ridge)

Ex5_Ridge_BerlinTicketS_TTest<-linearRidge(dataFrame_Ertrag_Y$`Berlin Ticket S`~ dataFrame_Treiber_X$data_Treiber.Arbeitslose+dataFrame_Treiber_X$data_Treiber.Superbenzin+dataFrame_Treiber_X$data_Treiber.Stau)

summary(Ex5_Ridge_BerlinTicketS_TTest)

'
T-test der Regressionskoeffizienten
dataFrame_Treiber_X$data_Treiber.Arbeitslose                      ==   0.185 
dataFrame_Treiber_X$data_Treiber.Superbenzin                      == 0.195
dataFrame_Treiber_X$data_Treiber.Stau                             == 1.93e-07 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Stau
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
--------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex5-Ridge-Berlin Ticket S (450018)

X-Variablen für Ex. 5-Ridge:
1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex. 5-Ridge:
1)Berlin Ticket S (450018)

R-Quadrat bestimmen:
==>0.2214869
schlechte (geringe) Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für Ridge Regression
MSE = 272002110297


MAE bestimmen für Ridge-Regression:
MAE= 390614.1


T-test der Regressionskoeffizienten
dataFrame_Treiber_X$data_Treiber.Arbeitslose                      ==   0.185 
dataFrame_Treiber_X$data_Treiber.Superbenzin                      == 0.195
dataFrame_Treiber_X$data_Treiber.Stau                             == 1.93e-07 ***
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

==> Wenn der p-wert des Regressionskoeffizienten <0.05 ist, dann leistet diese Variable
bzw. Regressionskoeffizient einen Erklärungsbeitrag für die Zielvariable. Dieses gilt
für folgende Variablen:
Stau
==>ALLE anderen Variablen leisten KEINEN Erklärungsbeitrag für die Zielvariable da bei denen
p-WertRegressionskoeffizient > 0.05 gilt!
'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Regressionsmodelle Exogene Variable 1: Bevölkerungsentwicklung in Berlin (insgesamt und in Altersgruppen, Schüler, Studierende)
#mit LASSO

'
Regressionsmodelle Ex1-LASSO-Bevölkerungsentwicklung in Berlin

X-Variablen für Ex. 1-LASSO:
1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LASSO:
1)Monatskarten Berlin ABC (450012)
2)ABO (450014)  Nutzerfin.
3)Firmenticket (450015)  
4)Gesamt vor EAVs

'

#Regressionsmodell Ex1-LASSO-Monatskarten 
'
Regressionsmodelle Ex1-LASSO-Bevölkerungsentwicklung in Berlin-Monatskarten

X-Variablen für Ex. 1-LASSO-Monatskarten:
1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LASSO-Monatskarten:
1)Monatskarten Berlin ABC (450012)
'


'Dataframe als Vektoren jeweils speichern'

x_Arbeitslose_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Arbeitslose)
x_BevoelkAbZu_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)

'Alle Vektoren in einer Variable speichern'
Ex1_LASSO_XVek<-c(x_Arbeitslose_Vek,x_BevoelkAbZu_Vek)

'Variable zu einer Matrix umwandeln'
Ex1_LASSO_XMatrix<-matrix(Ex1_LASSO_XVek,ncol=2)


#Regresssionsmodell Ex1-LASSO-Monatskarten bilden und schätzen

library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex1_LASSO_Monatskarten<-glmnet(Ex1_LASSO_XMatrix,dataFrame_Ertrag_Y$Monatskarten_ABC,alpha=1)

summary(Ex1_LASSO_Monatskarten)


#Bestes Lambda für Ex1_LASSO_Monatskarten bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex1_LASSO_Monatskarten<-cv.glmnet(Ex1_LASSO_XMatrix,dataFrame_Ertrag_Y$Monatskarten_ABC,alpha=1)

bestes_Lambda<-cv_Ex1_LASSO_Monatskarten$lambda.min

'Ausgabe:3403.847 '



#Plot mit MSE und bestem Lambda
plot(cv_Ex1_LASSO_Monatskarten)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex1_LASSO_Monatskarten<-glmnet(Ex1_LASSO_XMatrix,dataFrame_Ertrag_Y$Monatskarten_ABC,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex1_LASSO_Monatskarten<-predict(Ex1_LASSO_Monatskarten,s=bestes_Lambda,newx=Ex1_LASSO_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$Monatskarten_ABC-mean(dataFrame_Ertrag_Y$Monatskarten_ABC ))^2)
sse<-sum((y_predicted_Ex1_LASSO_Monatskarten - dataFrame_Ertrag_Y$Monatskarten_ABC)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.3931135
geringe Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex1_LASSO_Monatskarten-dataFrame_Ertrag_Y$Monatskarten_ABC)^2)/length(y_predicted_Ex1_LASSO_Monatskarten))

'
MSE bestimmen für LASSO Regression
MSE = 908425642375
'

mae=(sum(abs(y_predicted_Ex1_LASSO_Monatskarten-dataFrame_Ertrag_Y$Monatskarten_ABC)/length(y_predicted_Ex1_LASSO_Monatskarten)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 753371.7
'
'
-----------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodelle Ex1-LASSO-Bevölkerungsentwicklung in Berlin-Monatskarten

X-Variablen für Ex. 1-LASSO-Monatskarten:
1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LASSO-Monatskarten:
1)Monatskarten Berlin ABC (450012)


R-Quadrat bestimmen:
==>0.3931135
geringe Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für LASSO Regression
MSE = 908425642375


MAE bestimmen für LASSO Regression:
MAE= 753371.7
---------------------------------------------------------------------------------------------------
'
#--------------------------------------------------------------------------------------------------
#Regressionsmodell Ex1-LASSO-ABO (450014)  Nutzerfin.

'
Regressionsmodell Ex1-LASSO-Bevölkerungsentwicklung in Berlin-ABO (450014)  Nutzerfin.

X-Variablen für Ex. 1-LASSO-Abo:
1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LASSO-Abo:
1)ABO (450014)  Nutzerfin.
'

#Regressionsmodell Ex1-LASSO-Abo - bilden und schätzen

'Variable zu einer Matrix umwandeln'
Ex1_LASSO_XMatrix<-matrix(Ex1_LASSO_XVek,ncol=2)

library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex1_LASSO_Abo<-glmnet(Ex1_LASSO_XMatrix,dataFrame_Ertrag_Y$ABO,alpha=1)

summary(Ex1_LASSO_Abo)


#Bestes Lambda für Ex1_LASSO_Abo bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex1_LASSO_Abo<-cv.glmnet(Ex1_LASSO_XMatrix,dataFrame_Ertrag_Y$ABO,alpha=1)

bestes_Lambda<-cv_Ex1_LASSO_Abo$lambda.min

'Ausgabe:36163.59 '


#Plot mit MSE und bestem Lambda
plot(cv_Ex1_LASSO_Abo)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex1_LASSO_Abo<-glmnet(Ex1_LASSO_XMatrix,dataFrame_Ertrag_Y$ABO,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex1_LASSO_Abo<-predict(Ex1_LASSO_Abo,s=bestes_Lambda,newx=Ex1_LASSO_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$ABO-mean(dataFrame_Ertrag_Y$ABO))^2)
sse<-sum((y_predicted_Ex1_LASSO_Abo - dataFrame_Ertrag_Y$ABO)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.7014409
gute bis sehr gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex1_LASSO_Abo-dataFrame_Ertrag_Y$ABO)^2)/length(y_predicted_Ex1_LASSO_Abo))

'
MSE bestimmen für LASSO Regression
MSE = 5.066081e+12
'

mae=(sum(abs(y_predicted_Ex1_LASSO_Abo-dataFrame_Ertrag_Y$ABO)/length(y_predicted_Ex1_LASSO_Abo)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 1596814

----------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex1-LASSO-Bevölkerungsentwicklung in Berlin-ABO (450014)  Nutzerfin.

X-Variablen für Ex. 1-LASSO-Abo:
1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LASSO-Abo:
1)ABO (450014)  Nutzerfin.


R-Quadrat bestimmen:
==>0.7014409
gute bis sehr gute Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für LASSO Regression
MSE = 5.066081e+12


MAE bestimmen für LASSO Regression:
MAE= 1596814
---------------------------------------------------------------------------------------------------
'
#-------------------------------------------------------------------------------------------------
#Regressionsmodell Ex1-LASSO-Firmenticket (450015)

'
Regressionsmodell Ex1-LASSO-Bevölkerungsentwicklung in Berlin -Firmenticket (450015)

X-Variablen für Ex. 1-LASSO-Firmenticket:
1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LASSO-Firmenticket:
1)Firmenticket (450015)
'

#Regressionsmodell Ex1-LASSO-Firmenticket - bilden und schätzen

'Variable zu einer Matrix umwandeln'
Ex1_LASSO_XMatrix<-matrix(Ex1_LASSO_XVek,ncol=2)

library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex1_LASSO_Firmenticket<-glmnet(Ex1_LASSO_XMatrix,dataFrame_Ertrag_Y$Firmenticket,alpha=1)

summary(Ex1_LASSO_Firmenticket)


#Bestes Lambda für Ex1_LASSO_Firmenticket bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex1_LASSO_Firmenticket<-cv.glmnet(Ex1_LASSO_XMatrix,dataFrame_Ertrag_Y$Firmenticket,alpha=1)

bestes_Lambda<-cv_Ex1_LASSO_Firmenticket$lambda.min

'Ausgabe:86536.91'


#Plot mit MSE und bestem Lambda
plot(cv_Ex1_LASSO_Firmenticket)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex1_LASSO_Firmenticket<-glmnet(Ex1_LASSO_XMatrix,dataFrame_Ertrag_Y$Firmenticket,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex1_LASSO_Firmenticket<-predict(Ex1_LASSO_Firmenticket,s=bestes_Lambda,newx=Ex1_LASSO_XMatrix)


#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$Firmenticket-mean(dataFrame_Ertrag_Y$Firmenticket))^2)
sse<-sum((y_predicted_Ex1_LASSO_Firmenticket - dataFrame_Ertrag_Y$Firmenticket)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.2876204
schlechte Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex1_LASSO_Firmenticket-dataFrame_Ertrag_Y$Firmenticket)^2)/length(y_predicted_Ex1_LASSO_Firmenticket))

'
MSE bestimmen für LASSO Regression
MSE = 309966851822
'

mae=(sum(abs(y_predicted_Ex1_LASSO_Firmenticket-dataFrame_Ertrag_Y$Firmenticket)/length(y_predicted_Ex1_LASSO_Firmenticket)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 420161.1

----------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex1-LASSO-Bevölkerungsentwicklung in Berlin -Firmenticket (450015)

X-Variablen für Ex. 1-LASSO-Firmenticket:
1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LASSO-Firmenticket:
1)Firmenticket (450015)


R-Quadrat bestimmen:
==>0.2876204
schlechte Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für LASSO Regression
MSE = 309966851822

MAE bestimmen für LASSO Regression:
MAE= 420161.1


----------------------------------------------------------------------------------------------------
'
#----------------------------------------------------------------------------------------------------
#Regressionsmodell Ex1-LASSO-GesamtVorEAVs


'
Regressionsmodell Ex1-LASSO-Bevölkerungsentwicklung in Berlin -GesamtVorEAVs

X-Variablen für Ex. 1-LASSO-GesamtVorEAVs:
1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LASSO-GesamtVorEAVs:
1)GesamtVorEAVs
'

#Regressionsmodell Ex1-LASSO-GesamtVorEAVs - bilden und schätzen

'Variable zu einer Matrix umwandeln'
Ex1_LASSO_XMatrix<-matrix(Ex1_LASSO_XVek,ncol=2)

library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex1_LASSO_GesamtVorEAVs<-glmnet(Ex1_LASSO_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=1)

summary(Ex1_LASSO_GesamtVorEAVs)


#Bestes Lambda für Ex1_LASSO_GesamtVorEAVs bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex1_LASSO_GesamtVorEAVs<-cv.glmnet(Ex1_LASSO_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=1)

bestes_Lambda<-cv_Ex1_LASSO_GesamtVorEAVs$lambda.min

'Ausgabe:26689.34'

#Plot mit MSE und bestem Lambda
plot(cv_Ex1_LASSO_GesamtVorEAVs)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex1_LASSO_GesamtVorEAVs<-glmnet(Ex1_LASSO_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex1_LASSO_GesamtVorEAVs<-predict(Ex1_LASSO_GesamtVorEAVs,s=bestes_Lambda,newx=Ex1_LASSO_XMatrix)

#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Gesamt vor EAVs`-mean(dataFrame_Ertrag_Y$`Gesamt vor EAVs`))^2)
sse<-sum((y_predicted_Ex1_LASSO_GesamtVorEAVs - dataFrame_Ertrag_Y$`Gesamt vor EAVs`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.8118021
sehr gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex1_LASSO_GesamtVorEAVs-dataFrame_Ertrag_Y$`Gesamt vor EAVs`)^2)/length(y_predicted_Ex1_LASSO_GesamtVorEAVs))

'
MSE bestimmen für LASSO Regression
MSE = 8.32394e+12
'

mae=(sum(abs(y_predicted_Ex1_LASSO_GesamtVorEAVs-dataFrame_Ertrag_Y$`Gesamt vor EAVs`)/length(y_predicted_Ex1_LASSO_GesamtVorEAVs)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 2193886
-----------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex1-LASSO-Bevölkerungsentwicklung in Berlin -GesamtVorEAVs

X-Variablen für Ex. 1-LASSO-GesamtVorEAVs:
1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LASSO-GesamtVorEAVs:
1)GesamtVorEAVs


R-Quadrat bestimmen:
==>0.8118021
sehr gute Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für LASSO Regression
MSE = 8.32394e+12


MAE bestimmen für LASSO Regression:
MAE= 2193886

---------------------------------------------------------------------------------------------------
'
#--------------------------------------------------------------------------------------------
#Regressionsmodell Ex1-LASSO-Hochschulticket
'
Regressionsmodell Ex1-LASSO-Hochschulticket:

X-Variablen für Ex. 1-LASSO:
1)Studierende

Y-Variablen für Ex.1-LASSO:
1)Hochschulticket

==> Kann man nicht umsetzen, da es auch dieselben Funktionen nutzt wie bei Ridge!
Somit ist es für LASSO aufgrund der Funktionsbedingungen nicht möglich!Man bräuchte
unter anderem mindestens zwei X-Variablen!
---------------------------------------------------------------------------------------------
'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Regressionsmodelle Ex2-LASSO-Tourismuszahlen

'
X-Variablen für Ex. 2-LASSO-Tourismuszahlen:
 1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
 13)November,14)Dezember 

Y-Variablen für Ex.2-LASSO-Tourismuszahlen:
1)Einzelfahrscheine Berlin ABC (450010)
2) Tageskarten Berlin ABC (450019)
3)Gesamt vor EAVs
'

#Regressionsmodell Ex2-LASSO-Einzelfahrscheine

'
Regressionsmodell Ex2-LASSO-Einzelfahrscheine:

X-Variablen für Ex. 2-LASSO-Tourismuszahlen:
 1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
 13)November,14)Dezember 

Y-Variablen für Ex.2-LASSO-Tourismuszahlen:
1)Einzelfahrscheine Berlin ABC (450010)

'



#X-Matrix erstellen

'Dataframe als Vektoren jeweils speichern'

x_Uebernachtungen_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Übernachtungen)
x_Samstage_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Samstage)
x_SonnFeiertage_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.)
x_Januar_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.January)
x_Februar_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.February)
x_Maerz_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.March)
x_April_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.April)
x_Mai_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.May)
x_Juni_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.June)
x_Juli_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.July)
x_September_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.September)
x_Oktober_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.October)
x_November_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.November)
x_Dezember_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.December)


'Alle Vektoren in einer Variable speichern'
Ex2_LASSO_XVek<-c(x_Uebernachtungen_Vek,x_Samstage_Vek,x_SonnFeiertage_Vek,x_Januar_Vek,x_Februar_Vek,x_Maerz_Vek,x_April_Vek,x_Mai_Vek,x_Juni_Vek,x_Juli_Vek,x_September_Vek,x_Oktober_Vek,x_November_Vek,x_Dezember_Vek)

'Variable zu einer Matrix umwandeln'
Ex2_LASSO_XMatrix<-matrix(Ex2_LASSO_XVek,ncol=14)


#Regressionsmodell Ex2-LASSO-Einzelfahrscheine - bilden und schätzen

library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex2_LASSO_Einzelfahrscheine<-glmnet(Ex2_LASSO_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=1)

summary(Ex2_LASSO_Einzelfahrscheine)


#Bestes Lambda für Ex2_LASSO_Einzelfahrscheine bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex2_LASSO_Einzelfahrscheine<-cv.glmnet(Ex2_LASSO_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=1)

bestes_Lambda<-cv_Ex2_LASSO_Einzelfahrscheine$lambda.min

'Ausgabe:1325.691'


#Plot mit MSE und bestem Lambda
plot(cv_Ex2_LASSO_Einzelfahrscheine)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex2_LASSO_Einzelfahrscheine<-glmnet(Ex2_LASSO_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex2_LASSO_Einzelfahrscheine<-predict(Ex2_LASSO_Einzelfahrscheine,s=bestes_Lambda,newx=Ex2_LASSO_XMatrix)

#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`-mean(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`))^2)
sse<-sum((y_predicted_Ex2_LASSO_Einzelfahrscheine - dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.9078979
sehr sehr gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex2_LASSO_Einzelfahrscheine-dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)^2)/length(y_predicted_Ex2_LASSO_Einzelfahrscheine))

'
MSE bestimmen für LASSO Regression
MSE = 534461302360
'

mae=(sum(abs(y_predicted_Ex2_LASSO_Einzelfahrscheine-dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)/length(y_predicted_Ex2_LASSO_Einzelfahrscheine)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 512004.9

---------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex2-LASSO-Einzelfahrscheine:

X-Variablen für Ex. 2-LASSO-Tourismuszahlen:
 1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
 13)November,14)Dezember 

Y-Variablen für Ex.2-LASSO-Tourismuszahlen:
1)Einzelfahrscheine Berlin ABC (450010)



R-Quadrat bestimmen:
==>0.9078979
sehr sehr gute Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für LASSO Regression
MSE = 534461302360

MAE bestimmen für LASSO Regression:
MAE= 512004.9
----------------------------------------------------------------------------------------------------
'
#---------------------------------------------------------------------------------------------------
#Regressionsmodell Ex2-LASSO-Tageskarten Berlin ABC (450019)

'
Regressionsmodell Ex2-LASSO-Tageskarten:

X-Variablen für Ex. 2-LASSO-Tageskarten:
 1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
 13)November,14)Dezember 

Y-Variablen für Ex.2-LASSO-Tageskarten:
1)Tageskarten Berlin ABC (450019)
'

'Variable zu einer Matrix umwandeln'
Ex2_LASSO_XMatrix<-matrix(Ex2_LASSO_XVek,ncol=14)


#Regressionsmodell Ex2-LASSO-Tageskarten - bilden und schätzen

library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex2_LASSO_Tageskarten<-glmnet(Ex2_LASSO_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,alpha=1)

summary(Ex2_LASSO_Tageskarten)


#Bestes Lambda für Ex2_LASSO_Tageskarten bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex2_LASSO_Tageskarten<-cv.glmnet(Ex2_LASSO_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,alpha=1)

bestes_Lambda<-cv_Ex2_LASSO_Tageskarten$lambda.min

'Ausgabe:10098.39'


#Plot mit MSE und bestem Lambda
plot(cv_Ex2_LASSO_Tageskarten)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex2_LASSO_Tageskarten<-glmnet(Ex2_LASSO_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex2_LASSO_Tageskarten<-predict(Ex2_LASSO_Tageskarten,s=bestes_Lambda,newx=Ex2_LASSO_XMatrix)

#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Tageskarten ABC`-mean(dataFrame_Ertrag_Y$`Tageskarten ABC`))^2)
sse<-sum((y_predicted_Ex2_LASSO_Tageskarten - dataFrame_Ertrag_Y$`Tageskarten ABC`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.9775896
sehr sehr gute Erklärung der Zielvariable durch die X-Variablen!
'


#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex2_LASSO_Tageskarten-dataFrame_Ertrag_Y$`Tageskarten ABC`)^2)/length(y_predicted_Ex2_LASSO_Tageskarten))

'
MSE bestimmen für LASSO Regression
MSE = 26430308585
'

mae=(sum(abs(y_predicted_Ex2_LASSO_Tageskarten-dataFrame_Ertrag_Y$`Tageskarten ABC`)/length(y_predicted_Ex2_LASSO_Tageskarten)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 130531.2
-----------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex2-LASSO-Tageskarten:

X-Variablen für Ex. 2-LASSO-Tageskarten:
 1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
 13)November,14)Dezember 

Y-Variablen für Ex.2-LASSO-Tageskarten:
1)Tageskarten Berlin ABC (450019)


R-Quadrat bestimmen:
==>0.9775896
sehr sehr gute Erklärung der Zielvariable durch die X-Variablen!

MSE bestimmen für LASSO Regression
MSE = 26430308585

MAE bestimmen für LASSO Regression:
MAE= 130531.2

--------------------------------------------------------------------------------------------------
'
#--------------------------------------------------------------------------------------------------
#Regressionsmodell Ex2-LASSO-Gesamt vor EAVs

'
Regressionsmodell Ex2-LASSO-GesamtVorEAVs:

X-Variablen für Ex. 2-LASSO-GesamtVorEAVs:
 1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
 13)November,14)Dezember 

Y-Variablen für Ex.2-LASSO-GesamtVorEAVs:
1)GesamtVorEAVs

'


'Variable zu einer Matrix umwandeln'
Ex2_LASSO_XMatrix<-matrix(Ex2_LASSO_XVek,ncol=14)


#Regressionsmodell Ex2-LASSO-GesamtVorEAVs bilden und schätzen

library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex2_LASSO_GesamtVorEAVs<-glmnet(Ex2_LASSO_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=1)

summary(Ex2_LASSO_GesamtVorEAVs)


#Bestes Lambda für Ex2_LASSO_GesamtVorEAVs bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex2_LASSO_GesamtVorEAVs<-cv.glmnet(Ex2_LASSO_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=1)

bestes_Lambda<-cv_Ex2_LASSO_GesamtVorEAVs$lambda.min

'Ausgabe:787757.4'


#Plot mit MSE und bestem Lambda
plot(cv_Ex2_LASSO_GesamtVorEAVs)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex2_LASSO_GesamtVorEAVs<-glmnet(Ex2_LASSO_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex2_LASSO_GesamtVorEAVs<-predict(Ex2_LASSO_GesamtVorEAVs,s=bestes_Lambda,newx=Ex2_LASSO_XMatrix)

#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Gesamt vor EAVs`-mean(dataFrame_Ertrag_Y$`Gesamt vor EAVs`))^2)
sse<-sum((y_predicted_Ex2_LASSO_GesamtVorEAVs - dataFrame_Ertrag_Y$`Gesamt vor EAVs`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.3855611
geringe Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex2_LASSO_GesamtVorEAVs-dataFrame_Ertrag_Y$`Gesamt vor EAVs`)^2)/length(y_predicted_Ex2_LASSO_GesamtVorEAVs))

'
MSE bestimmen für LASSO Regression
MSE = 2.717646e+13
'

mae=(sum(abs(y_predicted_Ex2_LASSO_GesamtVorEAVs-dataFrame_Ertrag_Y$`Gesamt vor EAVs`)/length(y_predicted_Ex2_LASSO_GesamtVorEAVs)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 4306515
----------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex2-LASSO-GesamtVorEAVs:

X-Variablen für Ex. 2-LASSO-GesamtVorEAVs:
 1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
 13)November,14)Dezember 

Y-Variablen für Ex.2-LASSO-GesamtVorEAVs:
1)GesamtVorEAVs


R-Quadrat bestimmen:
==>0.3855611
geringe Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für LASSO Regression
MSE = 2.717646e+13

MAE bestimmen für LASSO Regression:
MAE= 4306515
----------------------------------------------------------------------------------------------------
'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Regressionsmodelle Ex3-LASSO-Einflüsse vom Arbeitsmarkt (Erwerbstätige und Pendler)

'
X-Variablen für Ex. 3-LASSO:
 1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
  4)Einpendler insgesamt

Y-Variablen für Ex.3-LASSO:
1)Einzelfahrscheine Berlin ABC (450010)
2)Tageskarten Berlin ABC (450019)
3)Firmenticket (450015) [Ohne Arbeitstage als X-Variable]
4)Gesamt vor EAVs
'

#Regressionsmodell Ex3-LASSO-Einzelfahrscheine

'Dataframe als Vektoren jeweils speichern'

x_Arbeitslose_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Arbeitslose)
x_Ferientage_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Ferientage)
x_Arbeitstage_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Arbeitstage)
x_EinpendlerInsgesamt<-as.vector(dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.)



'Alle Vektoren in einer Variable speichern'
Ex3_LASSO_XVek<-c(x_Arbeitslose_Vek,x_Ferientage_Vek,x_Arbeitstage_Vek,x_EinpendlerInsgesamt)

Ex3_LASSO_XVek_OhneArbeitstage<-c(x_Arbeitslose_Vek,x_Ferientage_Vek,x_EinpendlerInsgesamt)

'Variable zu einer Matrix umwandeln'
Ex3_LASSO_XMatrix<-matrix(Ex3_LASSO_XVek,ncol=4)
Ex3_LASSO_XMatrix_OhneArbeitstage<-matrix(Ex3_LASSO_XVek_OhneArbeitstage,ncol=3)


#Regressionsmodell Ex3-LASSO-Einzelfahrscheine bilden und schätzen

'
Regressionsmodell Ex3-LASSO-Einzelfahrscheine:

X-Variablen für Ex. 3-LASSO:
 1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
  4)Einpendler insgesamt

Y-Variablen für Ex.3-LASSO:
1)Einzelfahrscheine Berlin ABC (450010)
'

library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex3_LASSO_Einzelfahrscheine<-glmnet(Ex3_LASSO_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=1)

summary(Ex3_LASSO_Einzelfahrscheine)


#Bestes Lambda für Ex3_LASSO_Einzelfahrscheine bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex3_LASSO_Einzelfahrscheine<-cv.glmnet(Ex3_LASSO_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=1)

bestes_Lambda<-cv_Ex3_LASSO_Einzelfahrscheine$lambda.min

'Ausgabe:4111.058'


#Plot mit MSE und bestem Lambda
plot(cv_Ex3_LASSO_Einzelfahrscheine)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex3_LASSO_Einzelfahrscheine<-glmnet(Ex3_LASSO_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex3_LASSO_Einzelfahrscheine<-predict(Ex3_LASSO_Einzelfahrscheine,s=bestes_Lambda,newx=Ex3_LASSO_XMatrix)

#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`-mean(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`))^2)
sse<-sum((y_predicted_Ex3_LASSO_Einzelfahrscheine - dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.642412
mittelmäßige Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex3_LASSO_Einzelfahrscheine-dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)^2)/length(y_predicted_Ex3_LASSO_Einzelfahrscheine))

'
MSE bestimmen für LASSO Regression
MSE = 2.075055e+12
'

mae=(sum(abs(y_predicted_Ex3_LASSO_Einzelfahrscheine-dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)/length(y_predicted_Ex3_LASSO_Einzelfahrscheine)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 904876.2
------------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex3-LASSO-Einzelfahrscheine:

X-Variablen für Ex. 3-LASSO:
 1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
  4)Einpendler insgesamt

Y-Variablen für Ex.3-LASSO:
1)Einzelfahrscheine Berlin ABC (450010)


R-Quadrat bestimmen:
==>0.642412
mittelmäßige Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für LASSO Regression
MSE = 2.075055e+12


MAE bestimmen für LASSO Regression:
MAE= 904876.2
-----------------------------------------------------------------------------------------------------
'
#----------------------------------------------------------------------------------------------------
#regressionsmodell Ex3-LASSO-Tageskarten

'
Regressionsmodell Ex3-LASSO-Tageskarten

X-Variablen für Ex. 3-LASSO:
 1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
  4)Einpendler insgesamt

Y-Variablen für Ex.3-LASSO:
1)Tageskarten Berlin ABC (450019)
'

'Variable zu einer Matrix umwandeln'
Ex3_LASSO_XMatrix<-matrix(Ex3_Ridge_XVek,ncol=4)
Ex3_LASSO_XMatrix_OhneArbeitstage<-matrix(Ex3_Ridge_XVek_OhneArbeitstage,ncol=3)


#Regressionsmodell EX3-LASSO-Tageskarten - bilden und schätzen
library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex3_LASSO_Tageskarten<-glmnet(Ex3_LASSO_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,alpha=1)

summary(Ex3_LASSO_Tageskarten)


#Bestes Lambda für Ex3_LASSO_Einzelfahrscheine bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex3_LASSO_Tageskarten<-cv.glmnet(Ex3_LASSO_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,alpha=1)

bestes_Lambda<-cv_Ex3_LASSO_Tageskarten$lambda.min

'Ausgabe:16730.34'


#Plot mit MSE und bestem Lambda
plot(cv_Ex3_LASSO_Tageskarten)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex3_LASSO_Tageskarten<-glmnet(Ex3_LASSO_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex3_LASSO_Tageskarten<-predict(Ex3_LASSO_Tageskarten,s=bestes_Lambda,newx=Ex3_LASSO_XMatrix)

#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Tageskarten ABC`-mean(dataFrame_Ertrag_Y$`Tageskarten ABC`))^2)
sse<-sum((y_predicted_Ex3_LASSO_Tageskarten - dataFrame_Ertrag_Y$`Tageskarten ABC`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.6844835
gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex3_LASSO_Tageskarten-dataFrame_Ertrag_Y$`Tageskarten ABC`)^2)/length(y_predicted_Ex3_LASSO_Tageskarten))

'
MSE bestimmen für LASSO Regression
MSE = 3.72113e+11
'

mae=(sum(abs(y_predicted_Ex3_LASSO_Tageskarten-dataFrame_Ertrag_Y$`Tageskarten ABC`)/length(y_predicted_Ex3_LASSO_Tageskarten)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 455666.9
--------------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex3-LASSO-Tageskarten

X-Variablen für Ex. 3-LASSO:
 1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
  4)Einpendler insgesamt

Y-Variablen für Ex.3-LASSO:
1)Tageskarten Berlin ABC (450019)


R-Quadrat bestimmen:
==>0.6844835
gute Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für LASSO Regression
MSE = 3.72113e+11

MAE bestimmen für LASSO Regression:
MAE= 455666.9

----------------------------------------------------------------------------------------------------
'
#---------------------------------------------------------------------------------------------------
#Regressionsmodell Ex3-LASSO-Firmenticket (Ohne Arbeitstage als X-Variable)

'
Regressionsmodell Ex3-LASSO-Firmenticket (Ohne Arbeitstage als X-Variable)

X-Variablen für Ex. 3-LASSO:
 1)Arbeitslose ,2)Ferientage,
3)Einpendler insgesamt

Y-Variablen für Ex.3-LASSO:
1)Firmenticket (450015)
'

'Variable zu einer Matrix umwandeln'
Ex3_LASSO_XMatrix<-matrix(Ex3_Ridge_XVek,ncol=4)
Ex3_LASSO_XMatrix_OhneArbeitstage<-matrix(Ex3_Ridge_XVek_OhneArbeitstage,ncol=3)


#Regressionsmodell EX3-LASSO-Firmenticket (Ohne Arbeitstage als X-Variable) - bilden und schätzen
library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex3_LASSO_Firmenticket<-glmnet(Ex3_LASSO_XMatrix_OhneArbeitstage,dataFrame_Ertrag_Y$Firmenticket,alpha=1)

summary(Ex3_LASSO_Firmenticket)


#Bestes Lambda für Ex3_LASSO_Firmenticket (Ohne Arbeitstage als X-Variable) bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex3_LASSO_Firmenticket<-cv.glmnet(Ex3_LASSO_XMatrix_OhneArbeitstage,dataFrame_Ertrag_Y$Firmenticket,alpha=1)

bestes_Lambda<-cv_Ex3_LASSO_Firmenticket$lambda.min

'Ausgabe:1372.195'


#Plot mit MSE und bestem Lambda
plot(cv_Ex3_LASSO_Firmenticket)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex3_LASSO_Firmenticket<-glmnet(Ex3_LASSO_XMatrix_OhneArbeitstage,dataFrame_Ertrag_Y$Firmenticket,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex3_LASSO_Firmenticket<-predict(Ex3_LASSO_Firmenticket,s=bestes_Lambda,newx=Ex3_LASSO_XMatrix_OhneArbeitstage)

#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$Firmenticket-mean(dataFrame_Ertrag_Y$Firmenticket))^2)
sse<-sum((y_predicted_Ex3_LASSO_Firmenticket - dataFrame_Ertrag_Y$Firmenticket)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.8174764
sehr gute Erklärung der Zielvariable durch die X-Variablen!
'


#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex3_LASSO_Firmenticket-dataFrame_Ertrag_Y$Firmenticket)^2)/length(y_predicted_Ex3_LASSO_Firmenticket))

'
MSE bestimmen für LASSO Regression
MSE = 79418690702
'

mae=(sum(abs(y_predicted_Ex3_LASSO_Firmenticket-dataFrame_Ertrag_Y$Firmenticket)/length(y_predicted_Ex3_LASSO_Firmenticket)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 214791.3
----------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex3-LASSO-Firmenticket (Ohne Arbeitstage als X-Variable)

X-Variablen für Ex. 3-LASSO:
 1)Arbeitslose ,2)Ferientage,
3)Einpendler insgesamt

Y-Variablen für Ex.3-LASSO:
1)Firmenticket (450015)


R-Quadrat bestimmen:
==>0.8174764
sehr gute Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für LASSO Regression
MSE = 79418690702


MAE bestimmen für LASSO Regression:
MAE= 214791.3
-----------------------------------------------------------------------------------------------------
'
#----------------------------------------------------------------------------------------------------
#Regressionsmodell Ex3-LASSO-GesamtVorEAVs

'
Regressionsmodell Ex3-LASSO-GesamtVorEAVs

X-Variablen für Ex. 3-LASSO:
 1)Arbeitslose ,2)Ferientage,3)Arbeitstage
3)Einpendler insgesamt

Y-Variablen für Ex.3-LASSO:
1)Gesamt Vor EAVs
'

'Variable zu einer Matrix umwandeln'
Ex3_LASSO_XMatrix<-matrix(Ex3_Ridge_XVek,ncol=4)


#Regressionsmodell EX3-LASSO-GesamtVorEAVs - bilden und schätzen
library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex3_LASSO_GesamtVorEAVs<-glmnet(Ex3_LASSO_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=1)

summary(Ex3_LASSO_GesamtVorEAVs)



#Bestes Lambda für EX3-LASSO-GesamtVorEAVs bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex3_LASSO_GesamtVorEAVs<-cv.glmnet(Ex3_LASSO_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=1)

bestes_Lambda<-cv_Ex3_LASSO_GesamtVorEAVs$lambda.min

'Ausgabe:188288'


#Plot mit MSE und bestem Lambda
plot(cv_Ex3_LASSO_GesamtVorEAVs)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex3_LASSO_GesamtVorEAVs<-glmnet(Ex3_LASSO_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex3_LASSO_GesamtVorEAVs<-predict(Ex3_LASSO_GesamtVorEAVs,s=bestes_Lambda,newx=Ex3_LASSO_XMatrix)

#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Gesamt vor EAVs`-mean(dataFrame_Ertrag_Y$`Gesamt vor EAVs`))^2)
sse<-sum((y_predicted_Ex3_LASSO_GesamtVorEAVs- dataFrame_Ertrag_Y$`Gesamt vor EAVs`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.7967226
sehr gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex3_LASSO_GesamtVorEAVs-dataFrame_Ertrag_Y$`Gesamt vor EAVs`)^2)/length(y_predicted_Ex3_LASSO_GesamtVorEAVs))

'
MSE bestimmen für LASSO Regression
MSE = 8.990903e+12
'

mae=(sum(abs(y_predicted_Ex3_LASSO_GesamtVorEAVs-dataFrame_Ertrag_Y$`Gesamt vor EAVs`)/length(y_predicted_Ex3_LASSO_GesamtVorEAVs)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 2157113
------------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex3-LASSO-GesamtVorEAVs

X-Variablen für Ex. 3-LASSO:
 1)Arbeitslose ,2)Ferientage,3)Arbeitstage
3)Einpendler insgesamt

Y-Variablen für Ex.3-LASSO:
1)Gesamt Vor EAVs


R-Quadrat bestimmen:
==>0.7967226
sehr gute Erklärung der Zielvariable durch die X-Variablen!

MSE bestimmen für LASSO Regression
MSE = 8.990903e+12


MAE bestimmen für LASSO Regression:
MAE= 2157113
------------------------------------------------------------------------------------------------------
'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

'Ex4-Wetter-und Witterungdaten wird weggelassen, da in der mutlivariaten Regression festgestellt wurde,
dass alle Regressionsmodelle NICHT stat. signifikant sind!'

#Regressionsmodelle Ex.5-LASSO-Preisentwicklung im ÖPNV (für einzelne Produktgruppen)

'
X-Variablen für Ex. 5-LASSO:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5-LASSO:
1)Einzelfahrscheine Berlin ABC (450010)
2)ABO (450014) Nutzerfin.
3)Firmenticket (450015)
4)Berlin Ticket S (450018)
'

#Regressionsmodell Ex.5-LASSO-Einzelfahrscheine

'
Regressionsmodell Ex.5-LASSO-Einzelfahrscheine
X-Variablen für Ex. 5-LASSO:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5-LASSO:
1)Einzelfahrscheine Berlin ABC (450010)
'

#Regressionsmodell Ex5-LASSO-Einzelfahrscheine

'Dataframe als Vektoren jeweils speichern'

x_Arbeitslose_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Arbeitslose)
x_Superbenzin_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Superbenzin)
x_Stau_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Stau)


'Alle Vektoren in einer Variable speichern'
Ex5_LASSO_XVek<-c(x_Arbeitslose_Vek,x_Superbenzin_Vek,x_Stau_Vek)


'Variable zu einer Matrix umwandeln'
Ex5_LASSO_XMatrix<-matrix(Ex5_LASSO_XVek,ncol=3)


#Regressionsmodell Ex5-LASSO-Einzelfahrscheine - bilden und schätzen
library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex5_LASSO_Einzelfahrscheine<-glmnet(Ex5_LASSO_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=1)

summary(Ex5_LASSO_Einzelfahrscheine)


#Bestes Lambda für EX5-LASSO-Einzelfahrscheine bestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex5_LASSO_Einzelfahrscheine<-cv.glmnet(Ex5_LASSO_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=1)

bestes_Lambda<-cv_Ex5_LASSO_Einzelfahrscheine$lambda.min

'Ausgabe:4951.784'


#Plot mit MSE und bestem Lambda
plot(cv_Ex5_LASSO_Einzelfahrscheine)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex5_LASSO_Einzelfahrscheine<-glmnet(Ex5_LASSO_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex5_LASSO_Einzelfahrscheine<-predict(Ex5_LASSO_Einzelfahrscheine,s=bestes_Lambda,newx=Ex5_LASSO_XMatrix)

#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`-mean(dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`))^2)
sse<-sum((y_predicted_Ex5_LASSO_Einzelfahrscheine- dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.4230087
mittlere Erklärung der Zielvariable durch die X-Variablen!
'


#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex5_LASSO_Einzelfahrscheine-dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)^2)/length(y_predicted_Ex5_LASSO_Einzelfahrscheine))

'
MSE bestimmen für LASSO Regression
MSE = 3.348235e+12
'

mae=(sum(abs(y_predicted_Ex5_LASSO_Einzelfahrscheine-dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`)/length(y_predicted_Ex5_LASSO_Einzelfahrscheine)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 1290937
------------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex.5-LASSO-Einzelfahrscheine
X-Variablen für Ex. 5-LASSO:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5-LASSO:
1)Einzelfahrscheine Berlin ABC (450010)


R-Quadrat bestimmen:
==>0.4230087
mittlere Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für LASSO Regression
MSE = 3.348235e+12


MAE bestimmen für LASSO Regression:
MAE= 1290937
------------------------------------------------------------------------------------------------------
'
#-----------------------------------------------------------------------------------------------------
#Regressionsmodell Ex5-LASSO-ABO (450014) Nutzerfin.

'
Regressionsmodell Ex.5-LASSO-ABO (450014) Nutzerfin.
X-Variablen für Ex. 5-LASSO:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5-LASSO:
1)ABO (450014) Nutzerfin.
'


'Variable zu einer Matrix umwandeln'
Ex5_LASSO_XMatrix<-matrix(Ex5_LASSO_XVek,ncol=3)


#Regressionsmodell Ex5-LASSO-Abo - bilden und schätzen
library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex5_LASSO_Abo<-glmnet(Ex5_LASSO_XMatrix,dataFrame_Ertrag_Y$ABO,alpha=1)

summary(Ex5_LASSO_Abo)


#Bestes Lambda für EX5-LASSO-Abobestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex5_LASSO_Abo<-cv.glmnet(Ex5_LASSO_XMatrix,dataFrame_Ertrag_Y$ABO,alpha=1)

bestes_Lambda<-cv_Ex5_LASSO_Abo$lambda.min

'Ausgabe:14263.67'


#Plot mit MSE und bestem Lambda
plot(cv_Ex5_LASSO_Abo)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex5_LASSO_Abo<-glmnet(Ex5_LASSO_XMatrix,dataFrame_Ertrag_Y$ABO,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex5_LASSO_Abo<-predict(Ex5_LASSO_Abo,s=bestes_Lambda,newx=Ex5_LASSO_XMatrix)

#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$ABO-mean(dataFrame_Ertrag_Y$ABO))^2)
sse<-sum((y_predicted_Ex5_LASSO_Abo- dataFrame_Ertrag_Y$ABO)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.7970937
sehr gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex5_LASSO_Abo-dataFrame_Ertrag_Y$ABO)^2)/length(y_predicted_Ex5_LASSO_Abo))

'
MSE bestimmen für LASSO Regression
MSE = 3.443002e+12
'

mae=(sum(abs(y_predicted_Ex5_LASSO_Abo-dataFrame_Ertrag_Y$ABO)/length(y_predicted_Ex5_LASSO_Abo)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 1428314
-------------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex.5-LASSO-ABO (450014) Nutzerfin.
X-Variablen für Ex. 5-LASSO:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5-LASSO:
1)ABO (450014) Nutzerfin.


R-Quadrat bestimmen:
==>0.7970937
sehr gute Erklärung der Zielvariable durch die X-Variablen!

MSE bestimmen für LASSO Regression
MSE = 3.443002e+12

MAE bestimmen für LASSO Regression:
MAE= 1428314
------------------------------------------------------------------------------------------------------
'
#-------------------------------------------------------------------------------------------------------
#Regressionsmodell Ex5-LASSO-Firmenticket (450015)

'
Regressionsmodell Ex.5-LASSO-Firmenticket (450015)
X-Variablen für Ex. 5-LASSO:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5-LASSO:
1)Firmenticket (450015)
'


'Variable zu einer Matrix umwandeln'
Ex5_LASSO_XMatrix<-matrix(Ex5_LASSO_XVek,ncol=3)


#Regressionsmodell Ex5-LASSO-Firmenticket - bilden und schätzen
library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex5_LASSO_Firmenticket<-glmnet(Ex5_LASSO_XMatrix,dataFrame_Ertrag_Y$Firmenticket,alpha=1)

summary(Ex5_LASSO_Firmenticket)


#Bestes Lambda für EX5-LASSO-Abobestimmen
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex5_LASSO_Firmenticket<-cv.glmnet(Ex5_LASSO_XMatrix,dataFrame_Ertrag_Y$Firmenticket,alpha=1)

bestes_Lambda<-cv_Ex5_LASSO_Firmenticket$lambda.min

'Ausgabe:1421.717'


#Plot mit MSE und bestem Lambda
plot(cv_Ex5_LASSO_Firmenticket)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex5_LASSO_Firmenticket<-glmnet(Ex5_LASSO_XMatrix,dataFrame_Ertrag_Y$Firmenticket,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex5_LASSO_Firmenticket<-predict(Ex5_LASSO_Firmenticket,s=bestes_Lambda,newx=Ex5_LASSO_XMatrix)

#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$Firmenticket-mean(dataFrame_Ertrag_Y$Firmenticket))^2)
sse<-sum((y_predicted_Ex5_LASSO_Firmenticket- dataFrame_Ertrag_Y$Firmenticket)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.4833129
sehr gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex5_LASSO_Firmenticket-dataFrame_Ertrag_Y$Firmenticket)^2)/length(y_predicted_Ex5_LASSO_Firmenticket))

'
MSE bestimmen für LASSO Regression
MSE = 224818155428
'

mae=(sum(abs(y_predicted_Ex5_LASSO_Firmenticket-dataFrame_Ertrag_Y$Firmenticket)/length(y_predicted_Ex5_LASSO_Firmenticket)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 361791.2
------------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex.5-LASSO-Firmenticket (450015)
X-Variablen für Ex. 5-LASSO:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5-LASSO:
1)Firmenticket (450015)


R-Quadrat bestimmen:
==>0.4833129
sehr gute Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für LASSO Regression
MSE = 224818155428


MAE bestimmen für LASSO Regression:
MAE= 361791.2
------------------------------------------------------------------------------------------------------
'
#-----------------------------------------------------------------------------------------------------
#Regressionsmodell Ex5-LASSO-Berlin Ticket S (450018)

'
Regressionsmodell Ex.5-LASSO-Berlin Ticket S (450018)
X-Variablen für Ex. 5-LASSO:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5-LASSO:
1)Berlin Ticket S (450018)
'

'Variable zu einer Matrix umwandeln'
Ex5_LASSO_XMatrix<-matrix(Ex5_LASSO_XVek,ncol=3)


#Regressionsmodell Ex5-LASSO-BerlinTicketS - bilden und schätzen
library(glmnet)
'alpha=1 ==> LASSO, alpha=0==> Ridge'

Ex5_LASSO_BerlinTicketS<-glmnet(Ex5_LASSO_XMatrix,dataFrame_Ertrag_Y$`Berlin Ticket S`,alpha=1)

summary(Ex5_LASSO_BerlinTicketS)


#Bestes Lambda für EX5-LASSO-BerlinTicketS
'das macht man damit man das bestmöglichste und sparsamste Model für den Datensatz
bestimmen kann!
'
cv_Ex5_LASSO_BerlinTicketS<-cv.glmnet(Ex5_LASSO_XMatrix,dataFrame_Ertrag_Y$`Berlin Ticket S`,alpha=1)

bestes_Lambda<-cv_Ex5_LASSO_BerlinTicketS$lambda.min

'Ausgabe:822.8926'


#Plot mit MSE und bestem Lambda
plot(cv_Ex5_LASSO_BerlinTicketS)

#Regressionsmodell mit bestem Lambda bilden
Best_Ex5_LASSO_BerlinTicketS<-glmnet(Ex5_LASSO_XMatrix,dataFrame_Ertrag_Y$`Berlin Ticket S`,alpha=1,lambda=bestes_Lambda)

#Bestimmtheitsmaß bestimmen
y_predicted_Ex5_LASSO_BerlinTicketS<-predict(Ex5_LASSO_BerlinTicketS,s=bestes_Lambda,newx=Ex5_LASSO_XMatrix)

#SST und SSE bestimmen
sst<-sum((dataFrame_Ertrag_Y$`Berlin Ticket S`-mean(dataFrame_Ertrag_Y$`Berlin Ticket S`))^2)
sse<-sum((y_predicted_Ex5_LASSO_BerlinTicketS- dataFrame_Ertrag_Y$`Berlin Ticket S`)^2)

rsq<-1- (sse/sst)
'
R-Quadrat bestimmen:
==>0.2314806
sehr gute Erklärung der Zielvariable durch die X-Variablen!
'

#MSE und MAE bestimmen für LASSO Regression
install.packages("ModelMetrics")
library(ModelMetrics)

mse=(sum((y_predicted_Ex5_LASSO_BerlinTicketS-dataFrame_Ertrag_Y$`Berlin Ticket S`)^2)/length(y_predicted_Ex5_LASSO_BerlinTicketS))

'
MSE bestimmen für LASSO Regression
MSE = 268510431289
'

mae=(sum(abs(y_predicted_Ex5_LASSO_BerlinTicketS-dataFrame_Ertrag_Y$`Berlin Ticket S`)/length(y_predicted_Ex5_LASSO_BerlinTicketS)  ))

'
MAE bestimmen für LASSO Regression:
MAE= 399461.5
------------------------------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex.5-LASSO-Berlin Ticket S (450018)
X-Variablen für Ex. 5-LASSO:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5-LASSO:
1)Berlin Ticket S (450018)


R-Quadrat bestimmen:
==>0.2314806
sehr gute Erklärung der Zielvariable durch die X-Variablen!


MSE bestimmen für LASSO Regression
MSE = 268510431289

MAE bestimmen für LASSO Regression:
MAE= 399461.5
------------------------------------------------------------------------------------------------------
'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Regressionsmodelle Ex1-LARS-Bevölkerungsentwicklung in Berlin 
#(insgesamt und in Altersgruppen, Schüler, Studierende)

install.packages("lars")
library(lars)

'
X-Variablen für Ex.1-LARS:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LARS:
1)Monatskarten Berlin ABC (450012) 
2)ABO (450014)  Nutzerfin.
3)Firmenticket (450015)  
4)Gesamt vor EAVs


Spezielles Regressionsmodell Ex1-LARS-Hochschulticket
x-Variablen: 1)Studierende
Y-Variablen: 1)Semester-/ Hochschulticket (450016)

'

#Regressionsmodell Ex1-LARS-Monatskarten

'
X-Variablen für Ex.1-LARS-Monatskarten:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LARS:
1)Monatskarten Berlin ABC (450012) 
'

'Dataframe als Vektoren jeweils speichern'

x_Arbeitslose_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Arbeitslose)
x_BevoelkAbZu_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber..Bevölkerungfsab..zunahme.)

'Alle Vektoren in einer Variable speichern'
Ex1_LARS_XVek<-c(x_Arbeitslose_Vek,x_BevoelkAbZu_Vek)

'Variable zu einer Matrix umwandeln'
Ex1_LARS_XMatrix<-matrix(Ex1_LARS_XVek,ncol=2)


library(lars)
Ex1_LARS_Monatskarten<-lars(Ex1_LARS_XMatrix,dataFrame_Ertrag_Y$Monatskarten_ABC,type="lar")

Ex1_LARS_Monatskarten
'
Einfach Ex1_LARS_Monatskarten ausgeben lassen
Ausgabe unter anderem: R-squared: 0.393 
'

summary(Ex1_LARS_Monatskarten)

#/////////////////////////////////////////////////////////////////////////////////
# Probiert MSE zu bestimmen aber hat nicht funktioniert
#Mit den üblichen Funktionen ist es nicht möglich ein predict() umzusetzen!

cv_Ex1_LARS_Monatskarten<-cv.lars(Ex1_LARS_XMatrix,dataFrame_Ertrag_Y$Monatskarten_ABC,type="lar")


y_predicted_Ex1_LARS_Monatskarten<-predict.lars(Ex1_LARS_Monatskarten,newx=Ex1_LARS_XMatrix,type="coefficients",Mode="step")

length(y_predicted_Ex1_LARS_Monatskarten)

'Umwandeln des Vorhersagemodells zu numeric'

vec_y_predicted_Ex1_LARS_Monatskarten<-as.numeric(unlist(vec_y_predicted_Ex1_LARS_Monatskarten))

'Durch numeric wurde NA produziert was man durch 0 ersetzt'
vec_y_predicted_Ex1_LARS_Monatskarten<-gsub("NA", "0",vec_y_predicted_Ex1_LARS_Monatskarten)

vec_y_predicted_Ex1_LARS_Monatskarten<-as.numeric(unlist(vec_y_predicted_Ex1_LARS_Monatskarten))


mse=(sum((vec_y_predicted_Ex1_LARS_Monatskarten-dataFrame_Ertrag_Y$Monatskarten_ABC)^2)/length(vec_y_predicted_Ex1_LARS_Monatskarten))



#r-quadrat bestimmen
sst<-sum((dataFrame_Ertrag_Y$Monatskarten_ABC-mean(dataFrame_Ertrag_Y$Monatskarten_ABC))^2)
sse<-sum((vec_y_predicted_Ex1_LARS_Monatskarten- dataFrame_Ertrag_Y$Monatskarten_ABC)^2)

rsq<-1- (sse/sst)

#--------

#Umwandeln des Vorhersagemodells zu numeric
vec_y_predicted_Ex1_LARS_Monatskarten<-predict.lars(Ex1_LARS_Monatskarten,newx=Ex1_LARS_XMatrix,type="fit")

vec_y_predicted_Ex1_LARS_Monatskarten<-as.numeric(unlist(vec_y_predicted_Ex1_LARS_Monatskarten))

'Durch numeric wurde NA produziert was man durch 0 ersetzt'
vec_y_Predicted_Ex1_LARS_Monatskarten<-gsub("NA", "0",vec_y_predicted_Ex1_LARS_Monatskarten)


class(dataFrame_Ertrag_Y$Monatskarten_ABC)
class(vec_y_predicted_Ex1_LARS_Monatskarten)

length(dataFrame_Ertrag_Y$Monatskarten_ABC)
length(vec_y_predicted_Ex1_LARS_Monatskarten)
length(Ex1_LARS_Monatskarten)
length(y_predicted_Ex1_LARS_Monatskarten)

library(ModelMetrics)
mse(SUM_Ex1_LARS_Monatskarten)

library(performance)
performance_mse(cv_Ex1_LARS_Monatskarten)
#/////////////////////////////////////////////////////////////////////////////////

'
----------------------------------------------------------------------------------
Auswertung:
Regressionsmodell Ex1-LARS-Monatskarten:
X-Variablen für Ex.1-LARS-Monatskarten:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LARS:
1)Monatskarten Berlin ABC (450012) 


Einfach Ex1_LARS_Monatskarten ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.393 
=> geringe Erklärung der Zielvariable durch die X-Variablen

----------------------------------------------------------------------------------
'
#---------------------------------------------------------------------------------
#Regressionsmodell Ex1-LARS-ABO (450014)  Nutzerfin.

'
Regressionsmodell Ex1-LARS-ABO (450014)  Nutzerfin.:

X-Variablen für Ex.1-LARS-ABO (450014)  Nutzerfin.:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LARS-ABO (450014)  Nutzerfin.:
1)ABO (450014)  Nutzerfin.
'

#Regressionsmodell Ex1-LARS-Abo bilden und schätzen

'Variable zu einer Matrix umwandeln'
Ex1_LARS_XMatrix<-matrix(Ex1_LARS_XVek,ncol=2)


library(lars)
Ex1_LARS_Abo<-lars(Ex1_LARS_XMatrix,dataFrame_Ertrag_Y$ABO,type="lar")

Ex1_LARS_Abo
'
Einfach Ex1_LARS_Abo ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.702 
==> sehr gute Erklärung der Zielvariable durch die X-Variablen
'

summary(Ex1_LARS_Abo)

'
----------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex1-LARS-ABO (450014)  Nutzerfin.:

X-Variablen für Ex.1-LARS-ABO (450014)  Nutzerfin.:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LARS-ABO (450014)  Nutzerfin.:
1)ABO (450014)  Nutzerfin.


Einfach Ex1_LARS_Abo ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.702 
==> sehr gute Erklärung der Zielvariable durch die X-Variablen

----------------------------------------------------------------------------------
'
#---------------------------------------------------------------------------------
#Regressionsmodell Ex1-LARS-Firmenticket (450015)  

'
Regressionsmodell Ex1-LARS-Firmenticket (450015) :

X-Variablen für Ex.1-LARS:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LARS:
1)Firmenticket (450015) 
'

#Regressionsmodell Ex1-LARS-Firmenticket - bilden und schätzen

'Variable zu einer Matrix umwandeln'
Ex1_LARS_XMatrix<-matrix(Ex1_LARS_XVek,ncol=2)


library(lars)
Ex1_LARS_Firmenticket<-lars(Ex1_LARS_XMatrix,dataFrame_Ertrag_Y$Firmenticket,type="lar")

Ex1_LARS_Firmenticket
'
Einfach Ex1_LARS_Abo ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.288  
==> schlechte Erklärung der Zielvariable durch die X-Variablen
'
summary(Ex1_LARS_Firmenticket)

'
----------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex1-LARS-Firmenticket (450015) :

X-Variablen für Ex.1-LARS:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LARS:
1)Firmenticket (450015) 


Einfach Ex1_LARS_Abo ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.288  
==> schlechte Erklärung der Zielvariable durch die X-Variablen

----------------------------------------------------------------------------------
'
#---------------------------------------------------------------------------------
#Regressionsmodell Ex1-LARS-GesamtVorEAVs
'
Regressionsmodell Ex1-LARS-Gesamt Vor EAVs :

X-Variablen für Ex.1-LARS:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LARS:
1)Gesamt Vor EAVs
'

#Regressionsmodell Ex1-LARS-GesamtVorEAVs - bilden und schätzen

'Variable zu einer Matrix umwandeln'
Ex1_LARS_XMatrix<-matrix(Ex1_LARS_XVek,ncol=2)


library(lars)
Ex1_LARS_GesamtVorEAVs<-lars(Ex1_LARS_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,type="lar")

Ex1_LARS_GesamtVorEAVs
'
Einfach Ex1_LARS_GesamtVorEAVs ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.812 
==> sehr sehr gute Erklärung der Zielvariable durch die X-Variablen
'
summary(Ex1_LARS_GesamtVorEAVs)
'
---------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex1-LARS-Gesamt Vor EAVs :

X-Variablen für Ex.1-LARS:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1-LARS:
1)Gesamt Vor EAVs


Einfach Ex1_LARS_GesamtVorEAVs ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.812 
==> sehr sehr gute Erklärung der Zielvariable durch die X-Variablen
-----------------------------------------------------------------------------------
'
#---------------------------------------------------------------------------------
#Regressionsmodell Ex1-LARS-Hochschulticket

'
Regressionsmodell Ex1-LARS-Hochschulticket

X-Variablen: 1)Studierende
Y-Variablen: 1)Semester-/ Hochschulticket (450016)
'

#Regressionsmodell Ex1-LARS-Hochschulticket - bilden und schätzen

'Dataframe als Vektoren jeweils speichern'

x_Studierende_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Studierende)

'Alle Vektoren in einer Variable speichern'
Ex1_LARS_XVek_Studierende<-c(x_Studierende_Vek)

'Variable zu einer Matrix umwandeln'
Ex1_LARS_XMatrix_Studierende<-matrix(Ex1_LARS_XVek_Studierende,ncol=1)


library(lars)
Ex1_LARS_Hochschulticket<-lars(Ex1_LARS_XMatrix_Studierende,dataFrame_Ertrag_Y$Hochschulticket,type="lar")

Ex1_LARS_Hochschulticket
'
Einfach Ex1_LARS_Hochschulticket ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.545 
==> mittelmäßige Erklärung der Zielvariable durch die X-Variable
'

summary(Ex1_LARS_Hochschulticket)
'
----------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex1-LARS-Hochschulticket

X-Variablen: 1)Studierende
Y-Variablen: 1)Semester-/ Hochschulticket (450016)


Einfach Ex1_LARS_Hochschulticket ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.545 
==> mittelmäßige Erklärung der Zielvariable durch die X-Variable
----------------------------------------------------------------------------------
'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Regressionsmodelle Ex2-LARS-Tourismuszahlen

'
X-Variablen für Ex. 2:

 1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
 13)November,14)Dezember 

Y-Variablen für Ex.2:
1)Einzelfahrscheine Berlin ABC (450010)
2)Tageskarten Berlin ABC (450019)
3)Gesamt vor EAVs

'

#X-Matrix erstellen

'Dataframe als Vektoren jeweils speichern'

x_Uebernachtungen_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Übernachtungen)
x_Samstage_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Samstage)
x_SonnFeiertage_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber..Sonn..Feiertage.)
x_Januar_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.January)
x_Februar_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.February)
x_Maerz_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.March)
x_April_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.April)
x_Mai_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.May)
x_Juni_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.June)
x_Juli_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.July)
x_September_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.September)
x_Oktober_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.October)
x_November_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.November)
x_Dezember_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.December)


'Alle Vektoren in einer Variable speichern'
Ex2_LARS_XVek<-c(x_Uebernachtungen_Vek,x_Samstage_Vek,x_SonnFeiertage_Vek,x_Januar_Vek,x_Februar_Vek,x_Maerz_Vek,x_April_Vek,x_Mai_Vek,x_Juni_Vek,x_Juli_Vek,x_September_Vek,x_Oktober_Vek,x_November_Vek,x_Dezember_Vek)

'Variable zu einer Matrix umwandeln'
Ex2_LARS_XMatrix<-matrix(Ex2_LARS_XVek,ncol=14)


#Regressionsmodell Ex2-LARS-Einzelfahrscheine - bilden und schätzen

'
Regressionsmodell Ex2-LARS-Einzelfahrscheine
X-Variablen für Ex. 2:

1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
13)November,14)Dezember 

Y-Variablen für Ex.2:
1)Einzelfahrscheine Berlin ABC (450010)
'

library(lars)
Ex2_LARS_Einzelfahrscheine<-lars(Ex2_LARS_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,type="lar")

Ex2_LARS_Einzelfahrscheine
'
Einfach Ex1_LARS_Hochschulticket ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.908 
==> Sehr sehr gute Erklärung der Zielvariable durch die X-Variable
'

summary(Ex2_LARS_Einzelfahrscheine)
'
----------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex2-LARS-Einzelfahrscheine
X-Variablen für Ex. 2:

1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
13)November,14)Dezember 

Y-Variablen für Ex.2:
1)Einzelfahrscheine Berlin ABC (450010)



Einfach Ex1_LARS_Hochschulticket ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.908 
==> Sehr sehr gute Erklärung der Zielvariable durch die X-Variable
----------------------------------------------------------------------------------
'
#---------------------------------------------------------------------------------
#Regressionsmodell Ex2-LARS-Tageskarten

'
Regressionsmodell Ex2-LARS-Tageskarten
X-Variablen für Ex. 2:

1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
13)November,14)Dezember 

Y-Variablen für Ex.2:
1)Tageskarten Berlin ABC (450019)
'

#Regressionsmodell Ex2-LARS-Tageskarten - bilden und schätzen

library(lars)
Ex2_LARS_Tageskarten<-lars(Ex2_LARS_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,type="lar")

Ex2_LARS_Tageskarten
'
Einfach Ex1_LARS_Tageskarten ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.979 
==> Fast perfekte Erklärung der Zielvariable durch die X-Variable
'
summary(Ex2_LARS_Tageskarten)


'
--------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex2-LARS-Tageskarten
X-Variablen für Ex. 2:

1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
13)November,14)Dezember 

Y-Variablen für Ex.2:
1)Tageskarten Berlin ABC (450019)



Einfach Ex1_LARS_Tageskarten ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.979 
==> Fast perfekte Erklärung der Zielvariable durch die X-Variable
--------------------------------------------------------------------------------
'
#--------------------------------------------------------------------------------
#Regressionsmodell Ex2-LARS-Gesamt vor EAVs

'
Regressionsmodell Ex2-LARS-GesamtVorEAVs
X-Variablen für Ex. 2:

1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
13)November,14)Dezember 

Y-Variablen für Ex.2:
1)GesamtVorEAVs
'

#Regressionsmodell Ex2-LARS-GesamtVorEAVs - bilden und schätzen

library(lars)
Ex2_LARS_GesamtVorEAVs<-lars(Ex2_LARS_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,type="lar")

Ex2_LARS_GesamtVorEAVs
'
Einfach Ex1_LARS_Tageskarten ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.497 
==> mittlere Erklärung der Zielvariable durch die X-Variable
'
summary(Ex2_LARS_GesamtVorEAVs)

'
----------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex2-LARS-GesamtVorEAVs
X-Variablen für Ex. 2:

1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
13)November,14)Dezember 

Y-Variablen für Ex.2:
1)GesamtVorEAVs

Einfach Ex1_LARS_Tageskarten ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.497 
==> mittlere Erklärung der Zielvariable durch die X-Variable
----------------------------------------------------------------------------------
'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Regressionsmodelle Ex3-LARS-Einflüsse vom Arbeitsmarkt (Erwerbstätige und Pendler)

'
Regressionsmodelle Ex3-LARS-Einflüsse vom Arbeitsmarkt

X-Variablen für Ex. 3:
 1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
  4)Einpendler insgesamt


Y-Variablen für Ex.3:
1)Einzelfahrscheine Berlin ABC (450010)
2)Tageskarten Berlin ABC (450019)
3)Firmenticket (450015) [OHNE Arbeitstage als X-Variable]
4)Gesamt vor EAVs
'

'Dataframe als Vektoren jeweils speichern'

x_Arbeitslose_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Arbeitslose)
x_Ferientage_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Ferientage)
x_Arbeitstage_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Arbeitstage)
x_EinpendlerInsgesamt<-as.vector(dataFrame_Treiber_X$data_Treiber..Einpendler.insgesamt.)

'Alle Vektoren in einer Variable speichern'
Ex3_LARS_XVek<-c(x_Arbeitslose_Vek,x_Ferientage_Vek,x_Arbeitstage_Vek,x_EinpendlerInsgesamt)

Ex3_LARS_XVek_OhneArbeitstage<-c(x_Arbeitslose_Vek,x_Ferientage_Vek,x_EinpendlerInsgesamt)

'Variable zu einer Matrix umwandeln'
Ex3_LARS_XMatrix<-matrix(Ex3_LARS_XVek,ncol=4)
Ex3_LARS_XMatrix_OhneArbeitstage<-matrix(Ex3_LARS_XVek_OhneArbeitstage,ncol=3)


#Regressionsmodell Ex3-LARS-Einzelfahrscheine - bilden und schätzen

'
Regressionsmodell Ex3-LARS-Einzelfahrscheine

X-Variablen für Ex. 3:
 1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
  4)Einpendler insgesamt

Y-Variablen für Ex.3:
1)Einzelfahrscheine Berlin ABC (450010)
'

library(lars)
Ex3_LARS_Einzelfahrscheine<-lars(Ex3_LARS_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,type="lar")

Ex3_LARS_Einzelfahrscheine
'
Einfach Ex3_LARS_Einzelfahrscheine ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.642 
==> gute Erklärung der Zielvariable durch die X-Variable
'
summary(Ex3_LARS_Einzelfahrscheine)

'
----------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex3-LARS-Einzelfahrscheine

X-Variablen für Ex. 3:
 1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
  4)Einpendler insgesamt

Y-Variablen für Ex.3:
1)Einzelfahrscheine Berlin ABC (450010)


Einfach Ex3_LARS_Einzelfahrscheine ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.642 
==> gute Erklärung der Zielvariable durch die X-Variable
----------------------------------------------------------------------------------
'
#---------------------------------------------------------------------------------
#Regressionsmodell Ex3-LARS-Tageskarten Berlin ABC (450019)

'
Regressionsmodell Ex3-LARS-Tageskarten Berlin ABC (450019)

X-Variablen für Ex. 3:
 1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
  4)Einpendler insgesamt

Y-Variablen für Ex.3:
1)Tageskarten Berlin ABC (450019)
'

library(lars)
Ex3_LARS_Tageskarten<-lars(Ex3_LARS_XMatrix,dataFrame_Ertrag_Y$`Tageskarten ABC`,type="lar")

Ex3_LARS_Tageskarten
'
Einfach Ex3_LARS_Tageskarten ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.687 
==> gute Erklärung der Zielvariable durch die X-Variable
'
summary(Ex3_LARS_Tageskarten)

'
----------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex3-LARS-Tageskarten Berlin ABC (450019)

X-Variablen für Ex. 3:
 1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
  4)Einpendler insgesamt

Y-Variablen für Ex.3:
1)Tageskarten Berlin ABC (450019)


Einfach Ex3_LARS_Tageskarten ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.687 
==> gute Erklärung der Zielvariable durch die X-Variable
----------------------------------------------------------------------------------
'
#---------------------------------------------------------------------------------
#Regressionsmodell Ex3-LARS-Firmenticket (450015) [OHNE der X-Variable Arbeitstage]

'
Regressionsmodell Ex3-LARS-Firmenticket (450015)
[OHNE die X-Variable Arbeitstage!]

X-Variablen für Ex. 3:
 1)Arbeitslose ,2)Ferientage
3)Einpendler insgesamt

Y-Variablen für Ex.3:
1)Firmenticket (450015)
'
Ex3_LARS_XMatrix_OhneArbeitstage<-matrix(Ex3_LARS_XVek_OhneArbeitstage,ncol=3)

library(lars)
Ex3_LARS_Firmenticket<-lars(Ex3_LARS_XMatrix_OhneArbeitstage,dataFrame_Ertrag_Y$Firmenticket,type="lar")

Ex3_LARS_Firmenticket
'
Einfach Ex3_LARS_Firmenticket [Ohne Arbeitstage] ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.818 
==> sehr gute Erklärung der Zielvariable durch die X-Variable
'
summary(Ex3_LARS_Firmenticket)

'
----------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex3-LARS-Firmenticket (450015)
[OHNE die X-Variable Arbeitstage!]

X-Variablen für Ex. 3:
 1)Arbeitslose ,2)Ferientage
3)Einpendler insgesamt

Y-Variablen für Ex.3:
1)Firmenticket (450015)


Einfach Ex3_LARS_Firmenticket [Ohne Arbeitstage] ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.818 
==> sehr gute Erklärung der Zielvariable durch die X-Variable
----------------------------------------------------------------------------------
'
#---------------------------------------------------------------------------------
#Regressionsmodell Ex3-LARS-Gesamt vor EAVs

'
Regressionsmodell Ex3-LARS-GesamtVorEAVs

X-Variablen für Ex. 3:
 1)Arbeitslose ,2)Ferientage
3)Einpendler insgesamt

Y-Variablen für Ex.3:
1)GesamtVorEAVs
'

library(lars)
Ex3_LARS_GesamtVorEAVs<-lars(Ex3_LARS_XMatrix,dataFrame_Ertrag_Y$`Gesamt vor EAVs`,type="lar")

Ex3_LARS_GesamtVorEAVs
'
Einfach Ex3_LARS_GesamtVorEAVs ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.804 
==> sehr gute Erklärung der Zielvariable durch die X-Variable
'
summary(Ex3_LARS_GesamtVorEAVs)

'
----------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex3-LARS-GesamtVorEAVs

X-Variablen für Ex. 3:
 1)Arbeitslose ,2)Ferientage
3)Einpendler insgesamt

Y-Variablen für Ex.3:
1)GesamtVorEAVs


Einfach Ex3_LARS_GesamtVorEAVs ausgeben lassen
Ausgabe unter anderem: 
R-squared: 0.804 
==> sehr gute Erklärung der Zielvariable durch die X-Variable
----------------------------------------------------------------------------------
'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
'Regressionsmodelle Ex4-Wetter-und Witterungsdaten werden nicht erstellt, da sich
bei der multivariaten Regression herausgestellt hat, dass ALLE Regressionsmodelle
zu Ex4 stat. NICHT signifikant ist! Das heißt, der p-Wert der F-Statistik ist
>0.05.
'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Regressionsmodelle Ex5-LARS-Preisentwicklung im ÖPNV (für einzelne Produktgruppen)

'
Regressionsmodelle Ex5-LARS-Preisentwicklung im ÖPNV (für einzelne Produktgruppen)

X-Variablen für Ex. 5:
1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)Einzelfahrscheine Berlin ABC (450010)
2)ABO (450014) Nutzerfin.
3)Firmenticket (450015)
4)Berlin Ticket S (450018)
'

'Dataframe als Vektoren jeweils speichern'

x_Arbeitslose_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Arbeitslose)
x_Superbenzin_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Superbenzin)
x_Stau_Vek<-as.vector(dataFrame_Treiber_X$data_Treiber.Stau)


'Alle Vektoren in einer Variable speichern'
Ex5_LARS_XVek<-c(x_Arbeitslose_Vek,x_Superbenzin_Vek,x_Stau_Vek)


'Variable zu einer Matrix umwandeln'
Ex5_LARS_XMatrix<-matrix(Ex5_LARS_XVek,ncol=3)

#Regressionsmodell Ex5-LARS-Einzelfahrscheine -  bilden und schätzen

'
Regressionsmodell Ex5-LARS-Einzelfahrscheine

X-Variablen für Ex. 5:
1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)Einzelfahrscheine Berlin ABC (450010)
'


library(lars)
Ex5_LARS_Einzelfahrscheine<-lars(Ex5_LARS_XMatrix,dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,type="lar")

Ex5_LARS_Einzelfahrscheine

'
Einfach Ex5_LARS_Einzelfahrscheine ausgeben lassen
Ausgabe unter anderem: 
R-squared:0.423
==> geringe Erklärung der Zielvariable durch die X-Variable
'
summary(Ex5_LARS_Einzelfahrscheine)

'
----------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex5-LARS-Einzelfahrscheine

X-Variablen für Ex. 5:
1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)Einzelfahrscheine Berlin ABC (450010)


Einfach Ex5_LARS_Einzelfahrscheine ausgeben lassen
Ausgabe unter anderem: 
R-squared:0.423
==> geringe Erklärung der Zielvariable durch die X-Variable
----------------------------------------------------------------------------------
'
#---------------------------------------------------------------------------------
#Regressionsmodell Ex5-LARS-ABO (450014) Nutzerfin.

'
Regressionsmodell Ex5-LARS-Abo

X-Variablen für Ex. 5:
1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)ABO (450014) Nutzerfin.
'

library(lars)
Ex5_LARS_Abo<-lars(Ex5_LARS_XMatrix,dataFrame_Ertrag_Y$ABO,type="lar")

Ex5_LARS_Abo

'
Einfach Ex5_LARS_Abo ausgeben lassen
Ausgabe unter anderem: 
R-squared:0.797 
==> sehr gute Erklärung der Zielvariable durch die X-Variable
'
summary(Ex5_LARS_Abo)

'
----------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex5-LARS-Abo

X-Variablen für Ex. 5:
1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)ABO (450014) Nutzerfin.


Einfach Ex5_LARS_Abo ausgeben lassen
Ausgabe unter anderem: 
R-squared:0.797 
==> sehr gute Erklärung der Zielvariable durch die X-Variable
----------------------------------------------------------------------------------
'
#---------------------------------------------------------------------------------
#Regressionsmodell Ex5-LARS-Firmenticket (450015)

'
Regressionsmodell Ex5-LARS-Firmenticket (450015)

X-Variablen für Ex. 5:
1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)Firmenticket (450015)
'

library(lars)
Ex5_LARS_Firmenticket<-lars(Ex5_LARS_XMatrix,dataFrame_Ertrag_Y$Firmenticket,type="lar")

Ex5_LARS_Firmenticket

'
Einfach Ex5_LARS_Firmenticket ausgeben lassen
Ausgabe unter anderem: 
R-squared:0.483 
==> mittlere Erklärung der Zielvariable durch die X-Variable
'
summary(Ex5_LARS_Firmenticket)


'
----------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex5-LARS-Firmenticket (450015)

X-Variablen für Ex. 5:
1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)Firmenticket (450015)


Einfach Ex5_LARS_Firmenticket ausgeben lassen
Ausgabe unter anderem: 
R-squared:0.483 
==> mittlere Erklärung der Zielvariable durch die X-Variable
----------------------------------------------------------------------------------
'
#---------------------------------------------------------------------------------
#Regressionsmodell Ex5-LARS-Berlin Ticket S (450018)

'
Regressionsmodell Ex5-LARS-Berlin Ticket S (450018)

X-Variablen für Ex. 5:
1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)Berlin Ticket S (450018)
'

library(lars)
Ex5_LARS_BerlinTicketS<-lars(Ex5_LARS_XMatrix,dataFrame_Ertrag_Y$`Berlin Ticket S`,type="lar")

Ex5_LARS_BerlinTicketS

'
Einfach Ex5_LARS_Firmenticket ausgeben lassen
Ausgabe unter anderem: 
R-squared:0.231 
==> geringe Erklärung der Zielvariable durch die X-Variable
'
summary(Ex5_LARS_BerlinTicketS)

'
----------------------------------------------------------------------------------
Auswertung:

Regressionsmodell Ex5-LARS-Berlin Ticket S (450018)

X-Variablen für Ex. 5:
1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)Berlin Ticket S (450018)


Einfach Ex5_LARS_Firmenticket ausgeben lassen
Ausgabe unter anderem: 
R-squared:0.231 
==> geringe Erklärung der Zielvariable durch die X-Variable
----------------------------------------------------------------------------------
'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Prognose durch Zeitreihenanalyse erstellen


'Data Cleaning also Daten vorbereiten+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

'Es muss die Spalte mit den Zeitdaten in die Datensätze eingefügt werden um Prognosen zu erstellen!'

# dataframe für Y-Variablen erstellen

Zeit_dataFrame_Ertrag_Y<-data.frame(data_Ertrag$...1,data_Ertrag$`Einzelfahrscheine Berlin ABC (450010)`,data_Ertrag$`Tageskarten Berlin ABC (450019)`,data_Ertrag$`Monatskarten  Berlin ABC (450012)`,data_Ertrag$`ABO (450014) Nutzerfin.`,data_Ertrag$`Firmenticket (450015)`,data_Ertrag$`Semester-/ Hochschulticket (450016)`,data_Ertrag$`Berlin Ticket S (450018)`,data_Ertrag$`Gesamt vor EAVs`)

'
Das Datum muss später vlcht hinzugefügt werden um zu schauen wann was passiert! DAs passiert wohl
eher bei der Zeitreihenbetrachtung. Man würde dann folgendes in den DataFrame noch einfügen:

data_Ertrag$...1

'


'
Y-Variablen:
Einzelfahrscheine.Berlin.ABC, Tageskarten.Berlin.ABC, Monatskarten..Berlin.ABC,
ABO..450014..Nutzerfin, Firmenticket..450015, Semester...Hochschulticket,
Berlin.Ticket.S, Gesamt.vor.EAVs


'

is.na(Zeit_dataFrame_Ertrag_Y)

#löschen der zeilen mit missing values
#dataFrame_Ertrag_Y<-na.omit(dataFrame_Ertrag_Y)

'
das bruache ich warscheinlich nicht mehr! 

# ab zeile 86 (01.01.2012) bis zeile 192 (01.12.2020)
dataFrame_Ertrag_Y<-dataFrame_Ertrag_Y[-c(1:84),]

'
# ab zeile 86 (01.01.2012) bis zeile 192 (01.12.2020)
Zeit_dataFrame_Ertrag_Y<-Zeit_dataFrame_Ertrag_Y[-c(1:84),]

names(Zeit_dataFrame_Ertrag_Y)
is.na(Zeit_dataFrame_Ertrag_Y)

str(Zeit_dataFrame_Ertrag_Y)
unique(Zeit_dataFrame_Ertrag_Y)
class(Zeit_dataFrame_Ertrag_Y)
names(Zeit_dataFrame_Ertrag_Y)
sapply(Zeit_dataFrame_Ertrag_Y,class)


#Spaltennamen ändern

# Datum vlcht hinzufügen später
#names(dataFrame_Ertrag_Y)[names(dataFrame_Ertrag_Y)=="data_Ertrag....1"]="Datum"

names(Zeit_dataFrame_Ertrag_Y)[names(Zeit_dataFrame_Ertrag_Y)=="data_Ertrag....1"]="Zeitdaten"

names(Zeit_dataFrame_Ertrag_Y)[names(Zeit_dataFrame_Ertrag_Y)=="data_Ertrag..Einzelfahrscheine.Berlin.ABC..450010.."]="Einzelfahrscheine ABC"

names(Zeit_dataFrame_Ertrag_Y)[names(Zeit_dataFrame_Ertrag_Y)=="data_Ertrag..Tageskarten.Berlin.ABC..450019.."]="Tageskarten ABC"

names(Zeit_dataFrame_Ertrag_Y)[names(Zeit_dataFrame_Ertrag_Y)=="Monatskarten_ABC"]="Monatskarten ABC"

names(Zeit_dataFrame_Ertrag_Y)[names(Zeit_dataFrame_Ertrag_Y)=="ABO"]="Abo"

names(Zeit_dataFrame_Ertrag_Y)[names(Zeit_dataFrame_Ertrag_Y)=="data_Ertrag..Firmenticket..450015.."]="Firmenticket"

names(Zeit_dataFrame_Ertrag_Y)[names(Zeit_dataFrame_Ertrag_Y)=="data_Ertrag..Semester...Hochschulticket..450016.."]="Hochschulticket"

names(Zeit_dataFrame_Ertrag_Y)[names(Zeit_dataFrame_Ertrag_Y)=="data_Ertrag..Berlin.Ticket.S..450018.."]="Berlin Ticket S"

names(Zeit_dataFrame_Ertrag_Y)[names(Zeit_dataFrame_Ertrag_Y)=="data_Ertrag..Gesamt.vor.EAVs."]="Gesamt vor EAVs"



'
==> Das heißt:

Zeit_dataFrame_Ertrag_Y== enthält alle relevanten Variablen aus dem Datensatz Ertrag_ohne_Schüler'


#-----------------------------------------------------------------------------------------------------------------#
#data frame für X-variablen erstellen 

Zeit_dataFrame_Treiber_X<- data.frame(data_Treiber$...1,data_Treiber$Arbeitslose,data_Treiber$Superbenzin,data_Treiber$Dieselkraftstoff,data_Treiber$`Bevölkerungfsab/-zunahme`,data_Treiber$Arbeitstage,data_Treiber$Samstage,data_Treiber$`Sonn-/Feiertage`,data_Treiber$Ferientage,data_Treiber$April,data_Treiber$December,data_Treiber$February,data_Treiber$January,data_Treiber$July,data_Treiber$June,data_Treiber$March,data_Treiber$May,data_Treiber$November,data_Treiber$October,data_Treiber$September,data_Treiber$`Einpendler insgesamt`,data_Treiber$Studierende,data_Treiber$Übernachtungen,data_Treiber$Stau)

'
X-Variablen:
...1,Arbeitslose, Superbenzin, Dieselkraftstoff, 
, Bevölkerungfsab..zunahme.,Arbeitstage, Samstage, Sonn..Feiertage., Ferientage, April,
December, February, January, July, June, March, May, November, October,
September,Einpendler.insgesamt, Studierende, Übernachtungen, Stau

'

names(Zeit_dataFrame_Treiber_X)

'
das brauche ich nicht mehr da es auch mit der na.omit fkt geht!

# ab zeile 86 (01.01.2012) bis zeile 192 (01.12.2020)
dataFrame_Treiber_X<-dataFrame_Treiber_X[-c(1:84),]

'

#zeilen nach 192 löschen

#suche nach missing values und anzeigen im data frame durch TRUE ausgabe
is.na(Zeit_dataFrame_Treiber_X)

#löschen der zeilen mit missing values
Zeit_dataFrame_Treiber_X<-na.omit(Zeit_dataFrame_Treiber_X)

Zeit_dataFrame_Treiber_X

#prüfen ob alle Variablen datentyp numeric haben
sapply(dataFrame_Treiber_X,class)

#Variable Arbeitslose gilt als factor daher müssen wir das in numeric umwandeln

#wenn eine Variable den datentyp factor hat, muss man diesen zuerst zum character umwandeln!
#dataFrame_Treiber_X$data_Treiber.Arbeitslose<-as.numeric(as.character(dataFrame_Treiber_X$Arbeitslose))

Zeit_dataFrame_Treiber_X$data_Treiber.Arbeitslose<-as.character(Zeit_dataFrame_Treiber_X$data_Treiber.Arbeitslose)

Zeit_dataFrame_Treiber_X$data_Treiber.Arbeitslose<-as.numeric(Zeit_dataFrame_Treiber_X$data_Treiber.Arbeitslose)


#prüfen ob alle Variablen datentyp numeric haben
sapply(Zeit_dataFrame_Treiber_X,class)

names(Zeit_dataFrame_Treiber_X)

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber....1"]="Zeitdaten"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.Arbeitslose"]="Arbeitslose"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber..Auszubildende.Insgesamt"]="Azubis insgesamt"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.Superbenzin"]="Superbenzin"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.Dieselkraftstoff"]="Dieselkraftstoff"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber..Bevölkerungfsab..zunahme."]="Bevoelkerungsab-/zunahme"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.Arbeitstage"]="Arbeitstage"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.Samstage"]="Samstage"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber..Sonn..Feiertage."]="Sonn-/Feiertage"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.Ferientage"]="Ferientage"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.April"]="April"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.December"]="Dezember"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.February"]="Februar"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.January"]="Januar"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.July"]="Juli"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.June"]="Juni"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.March"]="Maerz"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.May"]="Mai"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.November"]="November"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.October"]="Oktober"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.September"]="September"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber..Einpendler.insgesamt."]="Einpendler Insgesamt"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.Studierende"]="Studierende"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.Übernachtungen"]="Uebernachtungen"

names(Zeit_dataFrame_Treiber_X)[names(Zeit_dataFrame_Treiber_X)=="data_Treiber.Stau"]="Stau"


names(Zeit_dataFrame_Treiber_X)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Prognose erstellen - Erster Versuche

'
Folgende Datensätze werden nun genutzt:

Zeit_dataFrame_Treiber_X == Treiber-Daten

Zeit_dataFrame_Ertrag_Y == Zielvariablen

'

#erster Versuch

install.packages("tidyverse")
install.packages("tsibble")
install.packages("fable")
install.packages("feasts")
install.packages("tsibbledata")

library(dplyr)
library(tidyverse)
library(tsibble)
library(tsibbledata)
library(fable)
library(feasts)


#model(auto=ARIMA(Log(Zeit_dataFrame_Ertrag_Y$`Monatskarten ABC`)) ~ log(Zeit_dataFrame_Treiber_X$Arbeitslose)+log(Zeit_dataFrame_Treiber_X$`Bevoelkerungsab-/zunahme`) )

Zeit_Komplett<- merge(Zeit_dataFrame_Treiber_X, Zeit_dataFrame_Ertrag_Y, by="Zeitdaten")
tail(Zeit_Komplett)

Zeit_Komplett <- mutate(Zeit_Komplett, Zeitdaten = yearmonth(Zeitdaten))
names(Zeit_Komplett) <- make.names(names(Zeit_Komplett), unique=TRUE)
head(Zeit_Komplett)
tail(Zeit_Komplett)

Zeit_Komplett <- Zeit_Komplett %>% filter(Zeitdaten < yearmonth("2021-12"))

Zeit_Komplett <- Zeit_Komplett %>%
  as_tsibble(index=Zeitdaten)
tail(Zeit_Komplett)


Zeit_Komplett %>%
  autoplot(Monatskarten.ABC)


split_month <- "2021 Dec"

Zeit_Kleiner <- Zeit_Komplett %>%
  filter(Zeitdaten < yearmonth(split_month))
Zeit_Groesser <- Zeit_Komplett %>%
  filter(Zeitdaten >= yearmonth(split_month))



Zeit_Ausgabe1 <- Zeit_Komplett %>%
  model(
  
    auto = ARIMA(Monatskarten.ABC ~ Arbeitslose+Bevoelkerungsab..zunahme) 
  )

report(Zeit_Ausgabe1)
'
Series: Monatskarten.ABC 
Model: LM w/ ARIMA(0,1,1)(2,0,0)[12] errors 
Transformation: log(Monatskarten.ABC) 

Coefficients:
          ma1    sar1    sar2  log(Arbeitslose)  log(Bevoelkerungsab..zunahme)
      -0.5310  0.6801  0.1791           -1.3959                        -0.0248
s.e.   0.1015  0.1162  0.1215            0.2034                         0.0128

sigma^2 estimated as 0.002434:  log likelihood=143.4
AIC=-274.8   AICc=-273.96   BIC=-258.76
'

'eher unwichtig'


'MAPE-Wert bestimmen '
accuracy(Zeit_Ausgabe1)
'
MAPE = 5.15

'



'UNWICHTIG/////////////////////////////////////////////////////////////////////////////////////////////'

Zeit_Ausgabe1 %>%
  gg_tsresiduals(lag=12)


Zeit_Vorhersage <- Zeit_Ausgabe1 %>%
  forecast(new_data = Zeit_Groesser)


'Ausgabe MAPE'
accuracy(Zeit_Ausgabe1, Zeit_Komplett)


Zeit_Vorhersage %>%
  autoplot(Zeit_Komplett)

forecast(auto, new_data = Zeit_Groesser)

auto.arima(Zeit_Komplett)

augment(Zeit_Ausgabe1) %>%
  filter(.model == "auto") %>%
  features(.innov, ljung_box, lag=24, dof=4)


forecast(Zeit_Ausgabe1, h=24) %>%
  filter(.model=='auto') %>%
  autoplot(Zeit_Komplett) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

'klappt am ende nicht! 
==> Am besten mit forecast library umsetzen!
'
'UNWICHTIG/////////////////////////////////////////////////////////////////////////////////////////////'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Prognose Exogene Variable 1 : Bevölkerungsentwicklung in Berlin 
#(insgesamt und in Altersgruppen, Schüler, Studierende)

'
Prognose für Ex1 zu den Regressionsmodellen:

X-Variablen für Ex. 1:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme


Y-Variablen für Ex.1:
1)Monatskarten Berlin ABC 
2)Abo 
3)Firmenticket
4)Gesamt vor EAVs

und das spezielle Regressionsmodell Hochschulticket

X-Variable Ex1-Hochschulticket:
1)Studierende

Y-Variable Ex1-Hochschulticket:
1)Hochschulticket

'

#Daten vorbereiten
Zeit_Komplett<- merge(Zeit_dataFrame_Treiber_X, Zeit_dataFrame_Ertrag_Y, by="Zeitdaten")
tail(Zeit_Komplett)

Zeit_Komplett <- mutate(Zeit_Komplett, Zeitdaten = yearmonth(Zeitdaten))
names(Zeit_Komplett) <- make.names(names(Zeit_Komplett), unique=TRUE)
head(Zeit_Komplett)
tail(Zeit_Komplett)

Zeit_Komplett <- Zeit_Komplett %>% filter(Zeitdaten < yearmonth("2021-12"))

Zeit_Komplett <- Zeit_Komplett %>%
  as_tsibble(index=Zeitdaten)
tail(Zeit_Komplett)


Zeit_Komplett %>%
  autoplot(Monatskarten.ABC)


split_month <- "2021 Dec"

Zeit_Kleiner <- Zeit_Komplett %>%
  filter(Zeitdaten < yearmonth(split_month))
Zeit_Groesser <- Zeit_Komplett %>%
  filter(Zeitdaten >= yearmonth(split_month))


#-------------------------------------------------------------------------------------------------------
#Prognose Ex1_Monatskarten

'
Prognose für Ex1_Monatskarten

X-Variablen für Ex. 1:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1:
1)Monatskarten Berlin ABC 
'



library(dplyr)
library(tidyverse)
library(tsibble)
#library(tsibbledata)
library(fable)
#library(feasts)



Prognose_Ex1_Monatskarten <- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Monatskarten.ABC ~ Arbeitslose+Bevoelkerungsab..zunahme) 
  )

report(Prognose_Ex1_Monatskarten)
'
Series: Monatskarten.ABC 
Model: LM w/ ARIMA(0,1,1)(2,0,0)[12] errors 
Transformation: log(Monatskarten.ABC) 

Coefficients:
          ma1    sar1    sar2  log(Arbeitslose)  log(Bevoelkerungsab..zunahme)
      -0.5310  0.6801  0.1791           -1.3959                        -0.0248
s.e.   0.1015  0.1162  0.1215            0.2034                         0.0128

sigma^2 estimated as 0.002434:  log likelihood=143.4
AIC=-274.8   AICc=-273.96   BIC=-258.76
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex1_Monatskarten)
'
Prognose für Ex1_Monatskarten
MAPE = 5.15

'
#-------------------------------------------------------------------------------------------------------
#Prognose Ex1_abo

'
Prognose für Ex1_Abo

X-Variablen für Ex. 1:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1:
1)Abo
'



library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex1_Abo <- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Zeit_Komplett$Abo ~ Zeit_Komplett$Arbeitslose+Zeit_Komplett$Bevoelkerungsab..zunahme) 
  )

report(Prognose_Ex1_Abo)
'
Series: Abo 
Model: LM w/ ARIMA(3,1,0)(1,0,0)[12] errors 

Coefficients:
         ar1     ar2     ar3    sar1  Arbeitslose  Bevoelkerungsab..zunahme
      0.4246  0.0186  0.3771  0.7406      -5.4099                   -8.3487
s.e.  0.0930  0.1002  0.0974  0.0728       2.3081                    4.4136

sigma^2 estimated as 8.141e+09:  log likelihood=-1375.16
AIC=2764.33   AICc=2765.46   BIC=2783.04
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex1_Abo)
'
Prognose für Ex1_Abo
MAPE = 0.307

'
#-------------------------------------------------------------------------------------------------------
#Prognose Ex1_Firmenticket

'
Prognose für Ex1_Firmenticket

X-Variablen für Ex. 1:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1:
1)Firmenticket
'



library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex1_Firmenticket<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Firmenticket ~ Zeit_Komplett$Arbeitslose+Zeit_Komplett$Bevoelkerungsab..zunahme) 
  )

report(Prognose_Ex1_Firmenticket)
'
Series: Firmenticket 
Model: LM w/ ARIMA(1,1,2)(0,1,1)[12] errors 

Coefficients:
         ar1      ma1     ma2     sma1  Zeit_Komplett$Arbeitslose  Zeit_Komplett$Bevoelkerungsab..zunahme
      0.7333  -0.5217  0.2139  -0.6488                     1.4484                                -16.0799
s.e.  0.1274   0.1570  0.1192   0.1039                     1.8675                                  4.6430

sigma^2 estimated as 5.174e+09:  log likelihood=-1197.61
AIC=2409.22   AICc=2410.51   BIC=2427.1
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex1_Firmenticket)
'
Prognose für Ex1_Firmenticket
MAPE = 1.19

'
#-------------------------------------------------------------------------------------------------------
#Prognose Ex1_Gesamt Vor EAVs

'
Prognose für Ex1_Gesamt Vor EAVs

X-Variablen für Ex. 1:
 1)Arbeitslose,  2)Bevölkerungsab-und zunahme

Y-Variablen für Ex.1:
1)Gesamt Vor EAVs
'
library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex1_GesamtVorEAVs<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Gesamt.vor.EAVs ~ Zeit_Komplett$Arbeitslose+Zeit_Komplett$Bevoelkerungsab..zunahme) 
  )

report(Prognose_Ex1_GesamtVorEAVs)
'
Series: Gesamt.vor.EAVs 
Model: LM w/ ARIMA(2,0,0)(1,0,0)[12] errors 

Coefficients:
         ar1      ar2    sar1  Zeit_Komplett$Arbeitslose  Zeit_Komplett$Bevoelkerungsab..zunahme  intercept
      0.6957  -0.1710  0.5511                  -263.3981                                189.1515  103450976
s.e.  0.1077   0.1045  0.1011                    24.1177                                158.4905    4715670

sigma^2 estimated as 4.836e+12:  log likelihood=-1729.76
AIC=3473.52   AICc=3474.64   BIC=3492.29
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex1_GesamtVorEAVs)
'
Prognose für Ex1_Firmenticket
MAPE = 2.79

'
#-------------------------------------------------------------------------------------------------------
#Prognose Ex1_Hochschulticket

'
Prognose für Ex1_Hochschulticket

X-Variablen für Ex. 1:
 1)Studierende

Y-Variablen für Ex.1:
1)Hochschulticket
'

library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex1_Hochschulticket<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Hochschulticket ~ Zeit_Komplett$Studierende) 
  )

report(Prognose_Ex1_Hochschulticket)
'
Series: Hochschulticket 
Model: LM w/ ARIMA(1,0,1)(0,1,0)[12] errors 

Coefficients:
         ar1      ma1  Zeit_Komplett$Studierende  intercept
      0.9051  -0.5010                  -108.4208   704053.6
s.e.  0.0597   0.1022                    44.3702   252230.1

sigma^2 estimated as 4.73e+10:  log likelihood=-1314.36
AIC=2638.72   AICc=2639.39   BIC=2651.54
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex1_Hochschulticket)
'
Prognose für Ex1_Firmenticket
MAPE = 3.79

'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Prognose Ex2-Exogene Variable: Tourismuszahlen

'
Prognose für die Regressionsmodelle von Ex2-Exogene Variable: Tourismuszahlen

X-Variablen für Ex.2:
 1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
 13)November,14)Dezember 


Y-Variablen für Ex.2:
1)Einzelfahrscheine Berlin ABC 
2)Tageskarten Berlin ABC
3)Gesamt vor EAVs
'
#-------------------------------------------------------------------------------------------------------
#Prognose Ex2_Einzelfahrscheine

'
Prognose für Ex2 Einzelfahrscheine

X-Variablen für Ex.2:
 1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
 13)November,14)Dezember 

Y-Variablen für Ex.2:
1)Einzelfahrscheine Berlin ABC 

'

library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex2_Einzelfahrscheine<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Einzelfahrscheine.ABC ~ Uebernachtungen+Samstage+Sonn..Feiertage+Januar+Februar+Maerz+April+Mai+Juni+Juli+September+Oktober+November+Dezember) 
  )

report(Prognose_Ex2_Einzelfahrscheine)
'
Series: Einzelfahrscheine.ABC 
Model: LM w/ ARIMA(3,1,0)(1,0,0)[12] errors 

Coefficients:
          ar1      ar2      ar3    sar1  Uebernachtungen  Samstage  Sonn..Feiertage     Januar    Februar
      -0.8017  -0.5712  -0.3264  0.6939           4.0324  89033.31        -237408.7  5847454.2  3032577.9
s.e.   0.0965   0.1136   0.0992  0.0827           0.1577  96614.02          68560.2   701284.4   690612.5
        Maerz      April        Mai       Juni       Juli  September    Oktober   November   Dezember
      2239369  1038204.3  1538249.9  1209209.2   53742.66  1131859.4  1657859.1  2929162.2  3487832.7
s.e.   701134   622491.8   685320.4   666510.9  676494.51   676357.2   665917.5   666443.2   615871.3

sigma^2 estimated as 4.035e+11:  log likelihood=-1576.08
AIC=3190.16   AICc=3198.89   BIC=3240.94
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex2_Einzelfahrscheine)
'
Prognose für Ex2_Einzelfahrscheine
MAPE = 3.74

'
#--------------------------------------------------------------------------------------------------------
#Prognose Ex2-Tageskarten
'
Prognose für Ex2 Tageskarten

X-Variablen für Ex.2:
 1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
 13)November,14)Dezember 

Y-Variablen für Ex.2:
1)Tageskarten

'

library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex2_Tageskarten<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Tageskarten.ABC ~ Uebernachtungen+Samstage+Sonn..Feiertage+Januar+Februar+Maerz+April+Mai+Juni+Juli+September+Oktober+November+Dezember) 
  )

report(Prognose_Ex2_Tageskarten)
'
Series: Tageskarten.ABC 
Model: LM w/ ARIMA(1,0,2) errors 

Coefficients:
         ar1      ma1     ma2  Uebernachtungen  Samstage  Sonn..Feiertage     Januar    Februar      Maerz
      0.9386  -0.9507  0.2651           1.5560  23870.34         92029.93  116345.43  -97727.66  -158491.2
s.e.  0.0488   0.1110  0.1051           0.0376  28752.23         22856.89   73842.39   72170.76    67245.8
           April         Mai        Juni        Juli   September    Oktober   November   Dezember  intercept
      -118896.41  -202854.40  -118788.06  -201386.02  -209242.96  -93699.71  -37227.28  267595.68  -553677.0
s.e.    73161.98    81588.41    60026.11    66285.05    66418.81   63670.07   64722.93   79812.28   186354.5

sigma^2 estimated as 2.215e+10:  log likelihood=-1430.21
AIC=2898.42   AICc=2907.05   BIC=2949.38
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex2_Tageskarten)
'
Prognose für Ex2_Tageskarten
MAPE = 4.65

'
#-------------------------------------------------------------------------------------------------------
#Prognose Ex2-GesamtVorEAVs

'
Prognose für Ex2 GesamtVorEAVs

X-Variablen für Ex.2:
 1)Übernachtungen ,2)Samstage,3)Sonn-und Feiertage,4)Januar,
5)Februar,6)März,7)April,8)Mai,9)Juni,10)Juli,11)September,12)Oktober, 
 13)November,14)Dezember 

Y-Variablen für Ex.2:
1)GesamtVorEAVs

'

library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex2_GesamtVorEAVs<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Gesamt.vor.EAVs ~ Uebernachtungen+Samstage+Sonn..Feiertage+Januar+Februar+Maerz+April+Mai+Juni+Juli+September+Oktober+November+Dezember) 
  )

report(Prognose_Ex2_GesamtVorEAVs)
'
Series: Gesamt.vor.EAVs 
Model: LM w/ ARIMA(0,1,1)(0,0,1)[12] errors 

Coefficients:
          ma1    sma1  Uebernachtungen    Samstage  Sonn..Feiertage     Januar    Februar    Maerz
      -0.8751  0.4263           8.3614    8255.591        -425261.4  9720106.6  6782227.2  6609545
s.e.   0.0599  0.1079           0.2747  246101.366         169204.3   824723.3   805000.8   793848
          April        Mai       Juni      Juli  September    Oktober   November   Dezember  intercept
      4340562.4  3979976.9  3248581.9  843809.8  3490307.4  4908324.2  5923073.8  7575849.4   147774.5
s.e.   834727.6   872952.5   765481.7  754084.1   756767.4   777450.3   781706.9   862420.1    22364.4

sigma^2 estimated as 1.782e+12:  log likelihood=-1653.59
AIC=3343.17   AICc=3350.94   BIC=3391.28
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex2_GesamtVorEAVs)
'
Prognose für Ex2_GesamtVorEAVs
MAPE = 1.69

'
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Prognose für Ex.3-Einflüsse vom Arbeitsmarkt (Erwerbstätige und Pendler)

'
Prognose für die Regressionsmodelle von Ex3:

X-Variablen für Ex. 3:
1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
4)Einpendler insgesamt


Y-Variablen für Ex3:
1)Einzelfahrscheine Berlin ABC
2)Tageskarten Berlin ABC
3)(Ohne Arbeitstage)Firmenticket
4)Gesamt vor EAVs
'
#-------------------------------------------------------------------------------------------------------
#Prognose für Ex3-Einzelfahrscheine


'
Prognose für Ex3-Einzelfahrscheine

X-Variablen für Ex. 3:
1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
4)Einpendler insgesamt

Y-Variablen für Ex3:
1)Einzelfahrscheine Berlin ABC
'
library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex3_Einzelfahrscheine<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Einzelfahrscheine.ABC ~ Arbeitslose+Ferientage+Arbeitstage+Einpendler.Insgesamt) 
  )

report(Prognose_Ex3_Einzelfahrscheine)
'
Series: Einzelfahrscheine.ABC 
Model: LM w/ ARIMA(2,0,0)(1,0,0)[12] errors 

Coefficients:
         ar1      ar2    sar1  Arbeitslose  Ferientage  Arbeitstage  Einpendler.Insgesamt  intercept
      0.6050  -0.2766  0.7296    -117.5277   -25246.98     65714.78              -90.6734   60716598
s.e.  0.0946   0.0937  0.0834      10.2882    15930.58     79504.25               12.4098    5096166

sigma^2 estimated as 1.305e+12:  log likelihood=-1660.29
AIC=3338.57   AICc=3340.41   BIC=3362.71
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex3_Einzelfahrscheine)
'
Prognose für Ex3_Einzelfahrscheine
MAPE = 7.08

'
#-------------------------------------------------------------------------------------------------------
#Prognose Ex3-Tageskarten

'
Prognose für Ex3-Tageskarten

X-Variablen für Ex. 3:
1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
4)Einpendler insgesamt

Y-Variablen für Ex3:
1)Tageskarten
'

library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex3_Tageskarten<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Tageskarten.ABC ~ Arbeitslose+Ferientage+Arbeitstage+Einpendler.Insgesamt) 
  )

report(Prognose_Ex3_Tageskarten)
'
Series: Tageskarten.ABC 
Model: LM w/ ARIMA(2,0,2)(2,0,0)[12] errors 

Coefficients:
         ar1      ar2      ma1     ma2    sar1    sar2  Arbeitslose  Ferientage  Arbeitstage
      1.1177  -0.6595  -0.5457  0.3875  0.4976  0.3084     -55.0888   -586.2817    -85811.54
s.e.  0.1960   0.1412   0.2217  0.1426  0.1246  0.1392       3.6362   5885.1565     27240.25
      Einpendler.Insgesamt  intercept
                  -38.2470   27031720
s.e.                4.1883    1810467

sigma^2 estimated as 1.625e+11:  log likelihood=-1547.26
AIC=3118.52   AICc=3121.8   BIC=3150.7
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex3_Tageskarten)
'
Prognose für Ex3_Tageskarten
MAPE = 13.2

'
#-------------------------------------------------------------------------------------------------------
#Prognose Ex3-Firmenticket (Ohne Arbeitstage)

'
Prognose für Ex3-Firmenticket (Ohne Arbeitstage)

X-Variablen für Ex. 3:
1)Arbeitslose ,2)Ferientage
3)Einpendler insgesamt

Y-Variablen für Ex3:
1)Firmenticket
'

library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex3_Firmenticket<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Firmenticket ~ Arbeitslose+Ferientage+Einpendler.Insgesamt) 
  )

report(Prognose_Ex3_Firmenticket)
'
Series: Firmenticket 
Model: LM w/ ARIMA(0,0,5)(0,0,1)[12] errors 

Coefficients:
         ma1     ma2     ma3     ma4     ma5    sma1  Arbeitslose  Ferientage  Einpendler.Insgesamt
      1.1856  1.2794  1.1791  1.0524  0.7006  0.3510       5.1873  -2007.2412               23.9834
s.e.  0.1347  0.1888  0.1158  0.1288  0.1429  0.1863       2.1286    743.9038                3.0877
      intercept
       -5367652
s.e.    1151762

sigma^2 estimated as 1.008e+10:  log likelihood=-1394.78
AIC=2811.56   AICc=2814.31   BIC=2841.07
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex3_Firmenticket)
'
Prognose für Ex3_Firmenticket
MAPE = 2.42

'
#-------------------------------------------------------------------------------------------------------
#Prognose Ex3-GesamtVorEAVs
'
Prognose für Ex3-GesamtVorEAVs

X-Variablen für Ex. 3:
1)Arbeitslose ,2)Ferientage,3)Arbeitstage,
4)Einpendler insgesamt

Y-Variablen für Ex3:
1)GesamtVorEAVs
'

library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex3_GesamtVorEAVs<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Gesamt.vor.EAVs ~ Arbeitslose+Ferientage+Arbeitstage+Einpendler.Insgesamt) 
  )

report(Prognose_Ex3_GesamtVorEAVs)
'
Series: Gesamt.vor.EAVs 
Model: LM w/ ARIMA(1,0,0)(1,0,0)[12] errors 

Coefficients:
         ar1    sar1  Arbeitslose  Ferientage  Arbeitstage  Einpendler.Insgesamt
      0.9061  0.5617    -143.5335   -12513.17     165973.8              250.7118
s.e.  0.0426  0.1096      48.3912    27337.24     148600.3               35.5365

sigma^2 estimated as 5.725e+12:  log likelihood=-1739.77
AIC=3493.55   AICc=3494.67   BIC=3512.32
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex3_GesamtVorEAVs)
'
Prognose für Ex3_GesamtVorEAVs
MAPE = 2.86

'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Prognose Ex5-Preisentwicklung im ÖPNV (für einzelne Produktgruppen)

'
Prognose für die Regressionsmodelle von Ex5:

X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)Einzelfahrscheine Berlin ABC
2)Abo
3)Firmenticket
4)Berlin Ticket S
'
#------------------------------------------------------------------------------------------------------
#Prognose Ex5-Einzelfahrscheine

'
Prognose Ex5-Einzelfahrscheine

X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)Einzelfahrscheine Berlin ABC

'

library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex5_Einzelfahrscheine<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Einzelfahrscheine.ABC ~ Arbeitslose+Superbenzin+Stau) 
  )

report(Prognose_Ex5_Einzelfahrscheine)
'
Series: Einzelfahrscheine.ABC 
Model: LM w/ ARIMA(0,1,3)(1,0,0)[12] errors 

Coefficients:
          ma1      ma2      ma3    sar1  Arbeitslose  Superbenzin      Stau
      -0.3989  -0.2771  -0.2085  0.7416     -91.9207     34100.91  14286.88
s.e.   0.1000   0.1159   0.1045  0.0813      16.2148     23001.94  10973.48

sigma^2 estimated as 1.42e+12:  log likelihood=-1650.27
AIC=3316.53   AICc=3318   BIC=3337.92
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex5_Einzelfahrscheine)
'
Prognose für Ex5_Einzelfahrscheine
MAPE = 7.59

'
#------------------------------------------------------------------------------------------------------
#Prognose für Ex5-Abo
'
Prognose Ex5-Abo

X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)Abo

'

library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex5_Abo<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Abo ~ Arbeitslose+Superbenzin+Stau) 
  )

report(Prognose_Ex5_Abo)
'
Series: Abo 
Model: LM w/ ARIMA(1,1,0)(0,1,2)[12] errors 

Coefficients:
         ar1     sma1     sma2  Arbeitslose  Superbenzin        Stau
      0.5746  -0.2748  -0.2845      -6.7697     179.3674  -1086.3148
s.e.  0.0953   0.1600   0.1696       2.9133    2165.0422    601.8607

sigma^2 estimated as 1.017e+10:  log likelihood=-1228.25
AIC=2470.51   AICc=2471.79   BIC=2488.38
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex5_Abo)
'
Prognose für Ex5_Abo
MAPE = 0.282

'
#------------------------------------------------------------------------------------------------------
#Prognose Ex5-firmenticket

'
Prognose Ex5-Firmenticket

X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)Firmenticket

'

library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex5_Firmenticket<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Firmenticket ~ Arbeitslose+Superbenzin+Stau) 
  )

report(Prognose_Ex5_Firmenticket)
'
Series: Firmenticket 
Model: LM w/ ARIMA(2,1,2) errors 

Coefficients:
         ar1      ar2      ma1     ma2  Arbeitslose  Superbenzin      Stau  intercept
      1.5225  -0.7612  -1.3712  0.8333      -0.7692     851.2373   93.1665   27806.57
s.e.  0.1111   0.1201   0.1136  0.1180       1.1759    1439.1582  578.9457   12507.47

sigma^2 estimated as 4.848e+09:  log likelihood=-1341.5
AIC=2700.99   AICc=2702.85   BIC=2725.05
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex5_Firmenticket)
'
Prognose für Ex5_Firmenticket
MAPE = 1.15

'
#-------------------------------------------------------------------------------------------------------
#Prognose Ex5-BerlinTicketS

'
Prognose Ex5-Berlin Ticket S

X-Variablen für Ex. 5:
 1)Arbeitslose,2)Superbenzin,3)Stau

Y-Variablen für Ex.5:
1)Berlin Ticket S

'

library(dplyr)
library(tidyverse)
library(tsibble)
library(fable)


Prognose_Ex5_BerlinTicketS<- Zeit_Komplett %>%
  model(
    
    auto = ARIMA(Berlin.Ticket.S ~ Arbeitslose+Superbenzin+Stau) 
  )

report(Prognose_Ex5_BerlinTicketS)
'
Series: Berlin.Ticket.S 
Model: LM w/ ARIMA(0,1,0)(0,1,0)[12] errors 

Coefficients:
      Arbeitslose  Superbenzin      Stau
         -26.4871    10007.525  2798.365
s.e.       5.8289     4171.508  1523.824

sigma^2 estimated as 4.795e+10:  log likelihood=-1301.45
AIC=2610.9   AICc=2611.34   BIC=2621.12
'


'MAPE-Wert bestimmen '
accuracy(Prognose_Ex5_BerlinTicketS)
'
Prognose für Ex5_BerlinTicketS
MAPE =  4.07

'

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Prognose mit ts-funktion welches aber nur mit der Zielvariable arbeitet! Daher bringt es nicht viel
#Prognose für Einzelfahrscheine ABC

install.packages("forecast")
library(forecast)
#install.packages("fpp2")

tsdata<-ts(Zeit_dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,frequency = 12,start=c(2012,1))
plot(tsdata)

AutoArima_Einzelfahrscheine<-auto.arima(tsdata)

forecast_Einzelfahrscheine<-forecast(AutoArima_Einzelfahrscheine,h=12)

plot(forecast_Einzelfahrscheine)

'Plot zoomen von 2020 bis 2022 um Werte besser zu sehen'
plot(forecast_Einzelfahrscheine,xlim=c(2020,2022))

summary(forecast_Einzelfahrscheine)
accuracy(forecast_Einzelfahrscheine)

'
RMSE = 1261899

'

#-------------------------------------------------------------------------------------------------
#Prognose für Tageskarten

library(forecast)
#install.packages("fpp2")

tsdata<-ts(Zeit_dataFrame_Ertrag_Y$`Tageskarten ABC`,frequency = 12,start=c(2012,1))
plot(tsdata)

AutoArima_Tageskarten<-auto.arima(tsdata)

forecast_Tageskarten<-forecast(AutoArima_Tageskarten,h=12)

plot(forecast_Tageskarten)

'Plot zoomen von 2020 bis 2022 um Werte besser zu sehen'
plot(forecast_Tageskarten,xlim=c(2020,2022))

summary(forecast_Tageskarten)
accuracy(forecast_Tageskarten)

'
RMSE = 457018.8

'
#-------------------------------------------------------------------------------------------------
#Prognose für Monatskarten

library(forecast)
#install.packages("fpp2")

tsdata<-ts(Zeit_dataFrame_Ertrag_Y$`Monatskarten ABC`,frequency = 12,start=c(2012,1))
plot(tsdata)

AutoArima_Monatskarten<-auto.arima(tsdata)

forecast_Monatskarten<-forecast(AutoArima_Monatskarten,h=12)

plot(forecast_Monatskarten)

'Plot zoomen von 2020 bis 2022 um Werte besser zu sehen'
plot(forecast_Monatskarten,xlim=c(2020,2022))

summary(forecast_Monatskarten)
accuracy(forecast_Monatskarten)

'
RMSE = 543115.5

'

#-------------------------------------------------------------------------------------------------
#Prognose für Abo

library(forecast)
#install.packages("fpp2")

tsdata<-ts(Zeit_dataFrame_Ertrag_Y$Abo,frequency = 12,start=c(2012,1))
plot(tsdata)

AutoArima_Abo<-auto.arima(tsdata)

forecast_Abo<-forecast(AutoArima_Abo,h=12)

plot(forecast_Abo)

'Plot zoomen von 2020 bis 2022 um Werte besser zu sehen'
plot(forecast_Abo,xlim=c(2020,2022))

summary(forecast_Abo)
accuracy(forecast_Abo)

'
RMSE = 91179.15 

'

#-------------------------------------------------------------------------------------------------
#Prognose für Firmenticket

library(forecast)
#install.packages("fpp2")

tsdata<-ts(Zeit_dataFrame_Ertrag_Y$Firmenticket,frequency = 12,start=c(2012,1))
plot(tsdata)

AutoArima_Firmenticket<-auto.arima(tsdata)

forecast_Firmenticket<-forecast(AutoArima_Firmenticket,h=12)

plot(forecast_Firmenticket)

'Plot zoomen von 2020 bis 2022 um Werte besser zu sehen'
plot(forecast_Firmenticket,xlim=c(2020,2022))

summary(forecast_Firmenticket)
accuracy(forecast_Firmenticket)

'
RMSE = 72415.64

'
#-------------------------------------------------------------------------------------------------

#Prognose für Hochschulticket

library(forecast)
#install.packages("fpp2")

tsdata<-ts(Zeit_dataFrame_Ertrag_Y$Hochschulticket,frequency = 12,start=c(2012,1))
plot(tsdata)

AutoArima_Hochschulticket<-auto.arima(tsdata)

forecast_Hochschulticket<-forecast(AutoArima_Hochschulticket,h=12)

plot(forecast_Hochschulticket)

'Plot zoomen von 2020 bis 2022 um Werte besser zu sehen'
plot(forecast_Hochschulticket,xlim=c(2020,2022))

summary(forecast_Hochschulticket)
accuracy(forecast_Hochschulticket)

'
RMSE = 207657.2

'
#-------------------------------------------------------------------------------------------------
#Prognose für Berlin Ticket S

library(forecast)
#install.packages("fpp2")

tsdata<-ts(Zeit_dataFrame_Ertrag_Y$`Berlin Ticket S`,frequency = 12,start=c(2012,1))
plot(tsdata)

AutoArima_BerlinTicketS<-auto.arima(tsdata)

forecast_BerlinTicketS<-forecast(AutoArima_BerlinTicketS,h=12)

plot(forecast_BerlinTicketS)

'Plot zoomen von 2020 bis 2022 um Werte besser zu sehen'
plot(forecast_BerlinTicketS,xlim=c(2020,2022))

summary(forecast_BerlinTicketS)
accuracy(forecast_BerlinTicketS)

'
RMSE = 237829

'
#-------------------------------------------------------------------------------------------------------
#Prognose Gesamt vor EAVs

library(forecast)
#install.packages("fpp2")

tsdata<-ts(Zeit_dataFrame_Ertrag_Y$`Gesamt vor EAVs`,frequency = 12,start=c(2012,1))
plot(tsdata)

AutoArima_GesamtVorEAVs<-auto.arima(tsdata)

forecast_GesamtVorEAVs<-forecast(AutoArima_GesamtVorEAVs,h=12)

plot(forecast_GesamtVorEAVs)

'Plot zoomen von 2020 bis 2022 um Werte besser zu sehen'
plot(forecast_GesamtVorEAVs,xlim=c(2020,2022))

summary(forecast_GesamtVorEAVs)
accuracy(forecast_GesamtVorEAVs)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



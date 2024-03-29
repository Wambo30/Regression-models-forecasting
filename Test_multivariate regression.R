# Test code bezgl mutlivariater Regressionsanalyse

'#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  

regel- points(), abline()

syntax abline() zur Bestimmung der Regressionsgeraden:

abline(<NameLinearesModell>, col="<NameFarbeOderZahlFarbe>")
ODER
abline(c(<NameRegressionsYabschnitt>,<NameRegressionsSteigung>))


syntax points() zu bestimmung/einzeichnen von punkten im diagramm:

points(<Xwert>, <YwertOderTerm>, col="<NameFarbeOderZahlFarbe>")


bsp obda:

model<-lm(Verbrauch~Uebernacht)
plot(Uebernacht,Verbrauch)
abline(model,col="blue")

==>ausgabe: streudiagramm mit regressionsfkt in blau. Mit linearen
model lm(y~x) ist es ein tick einfacher umzusetzen!

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #'

#importieren des datensatzes
install.packages("readxl")
library(readxl)

# festlegen von dataframe also datensatz zur Variable zuweisen
data_Treiber<-read_excel("Treiber_ab_2005.xlsx", na="NA")


'Bsp1 f�r einfache lineare Regression++++++++++++++++++++++++++++++++'
#betrachte Merkmale ploten bzw. im Streudiagramm anschauen um zusammehang
#zu erkennen
plot(data_Treiber$�bernachtungen,data_Treiber$`Bev�lkerungsstand Monatsende`)
# Bei Plot: X-Variable kommt ZUERST dann erst Y-Variable!
# X= �bernachtungen, Y= Bev�lkerungsstand Monatsende
# je mehr �bernachtungen erfolgen desto gr��er wird die Bev�lkerung in diesem Zeitraum


'==> da ein direkter linearer zusammenhang zwischen X --> Y existiert ist es
nun sinnvoll ein lineares modell zu erstellen! Dh die Daten k�nnen durch ein
solches Modell erkl�rt werden, aufgrund der Linearit�t.
'

model1<-lm(data_Treiber$`Bev�lkerungsstand Monatsende`~ data_Treiber$�bernachtungen, data =data_TreiberAb2005 )
#Y-Variable kommt hier ZUERST und dann erst X-Variable
#<ModelName> <- lm(<NameY_Var> ~ <NameX_Var>, data = <NameDataFrameAlsoDatensatz>)
# X= �bernachtungen, Y= Bev�lkerungsstand Monatsende


#regressionsgerade durch modell zeichnen lassen
abline(model1, col="red")

# summary erstellen um die abst�nde/residuen zwischen den tats�chlichen daten
# und der gesch�tzen regressionsfkt zu berechnen

summary(model1)
'==> Ausgabe:
p-value: 0.8988 > 0.05 ==> H0 gilt immernoch
dh model1 leistet KEINEN Erkl�rungsbeitrag und kann komplett verworfen werden
'
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

'Bsp2 f�r einfache lineare Regression++++++++++++++++++++++++++++++++'


plot(data_Treiber$Ank�nfte,data_Treiber$`Bev�lkerungsstand Monatsende`)
# Bei Plot: X-Variable kommt ZUERST dann erst Y-Variable!
# X= Ank�nfte, Y= Bev�lkerungsstand Monatsende
# je mehr �bernachtungen erfolgen desto gr��er wird die Bev�lkerung in diesem Zeitraum


'==> da ein direkter linearer zusammenhang zwischen X --> Y existiert ist es
nun sinnvoll ein lineares modell zu erstellen! Dh die Daten k�nnen durch ein
solches Modell erkl�rt werden, aufgrund der Linearit�t.
'

model2<-lm(data_Treiber$`Bev�lkerungsstand Monatsende`~ data_Treiber$Ank�nfte, data =data_TreiberAb2005 )
#Y-Variable kommt hier ZUERST und dann erst X-Variable
# X= Ank�nfte, Y= Bev�lkerungsstand Monatsende

#regressionsgerade durch modell zeichnen lassen
abline(model2, col="red")


# summary erstellen um die abst�nde/residuen zwischen den tats�chlichen daten
# und der gesch�tzen regressionsfkt zu berechnen
summary(model2)
'==> Ausgabe:
p-value: 0.7079 > 0.05 ==> H0 gilt immernoch
dh model2 leistet KEINEN Erkl�rungsbeitrag und kann komplett verworfen werden

'
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
'Bsp3 f�r einfache lineare Regression++++++++++++++++++++++++++++++++'


plot(data_Treiber$Sommer,data_Treiber$Fahrrad)
# Bei Plot: X-Variable kommt ZUERST dann erst Y-Variable!
#X = Sommer , Y= Fahrrad

model3<-lm(data_Treiber$Sommer~ data_Treiber$Fahrrad, data =data_Treiber )
#Y-Variable kommt hier ZUERST und dann erst X-Variable
# X= Sommer, Y= Fahrrad

#regressionsgerade durch modell zeichnen lassen
abline(model3, col="red")


# summary erstellen um die abst�nde/residuen zwischen den tats�chlichen daten
# und der gesch�tzen regressionsfkt zu berechnen
summary(model3)

'==> Ausgabe:
p-value: 6.616e-11 < 0.05 ==> H0 wird verworfen
Somit gilt H1 dass das model3 einen Erkl�rungsbeitrag leistet
dass der Sommer auf die Fahrrad-Nutzung Einfluss nimmt!
'


'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
#L�SUNG!!!
'Bsp4 f�r einfache lineare Regression++++++++++++++++++++++++++++++++'

plot(data_Treiber$Ank�nfte,data_Treiber$�bernachtungen)
# Bei Plot: X-Variable kommt ZUERST dann erst Y-Variable!
#X = Ank�nfte , Y= �bernachtungen


model4<-lm(data_Treiber$�bernachtungen ~ data_Treiber$Ank�nfte, data =data_Treiber )
#Y-Variable kommt hier ZUERST und dann erst X-Variable
#X = Ank�nfte , Y= �bernachtungen


#regressionsgerade durch modell zeichnen lassen
abline(lm(data_Treiber$�bernachtungen~ data_Treiber$Ank�nfte), col="red")
#Y-Variable kommt hier ZUERST und dann erst X-Variable
#X = Ank�nfte , Y= �bernachtungen

summary(model4)

'Ausgabe:
F-statistic:  6908 on 1 and 192 DF,  p-value: < 2.2e-16

Das hei�t==> p-wert == p-value: < 2.2e-16 <0.05
==> H0 wird verworfen
Somit gilt H1 dass das model3 einen Erkl�rungsbeitrag leistet
dass die Ank�nfte auf die �ernachtungen Einfluss nimmt!
Wir k�nnen also mit der Interpretation dieses Models fortfahren
'

'
Wir betrachten nun die vorletzte Zeile in der summary Ausgabe also:
  " Multiple R-squared:  0.973,	Adjusted R-squared:  0.9728  "

Multiple R-squared besagt== 
  Wieviel Prozent der Varianz der abh�ngigen Variable kann ich mit dem
Modell erkl�ren?
  0 == keine Erkl�rung
1 == perfekte Erkl�rung

==> Folgerung:
Wir sehen wir k�nnen mit der X-Variable (Ank�nfte) ganz gut die Y-Varible
(�bernachtungen) erkl�ren!Das hei�t, wir k�nnen 97,3% der Y-Variable durch 
die X-Variable erkl�ren!

Wir betrachten hier eher "Multiple R-squared" da wir nur EINE X-Variable haben! 
H�tten wir mehr X-Variablen, dann w�rden wir "Adjusted R-squared" betrachten!


Wir betrachten nun die Koeffizienten:

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
(Intercept)           -1.514e+05  2.677e+04  -5.653 5.64e-08 ***
data_Treiber$Ank�nfte  2.547e+00  3.065e-02  83.112  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


Zeile data_Treiber$Ank�nfte, Spalte Estimate:
Der Wert 2.547e+00 ist positiv somit ist die Steigung der Regressionsgerade
positiv also verl�uft von unten links nach oben rechts.

Zeile data_Treiber$Ank�nfte, Spalte Pr(>|t|) :
P-Wert liegt bei < 2e-16 also wird die H1 Hypothese angenommen, somit ist
die X-Variable (Ank�nfte) statistisch signifikant f�r die Y-Variable
(�bernachtungen)! 
Die *** unterst�tzen da die statitische Signifikanz zur Wahrscheinlichkeit
von 0 bis 0.001 fehlerhaft also es ist zu 99% stat. signifikant!

==> der bezug erfolgt auf das lineare regressionsmodell

y = alpha + Beta1*x1
alpha == Y-Achsenabschnitt == Schnittpunkt zur y-achse
      == ersteZeileErsteSpalte in Summary Ausgabe
      == Zeile "Intercept" + Spalte "Estimate"

Beta1 == ZweiteZeileErsteSpalte 
      == Zeile "data_Treiber$Ank�nfte" + Spalte "Estimate"

x1 == Y-Variable also "�bernachtungen"
'

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
#L�SUNG!!!!!
'
X-Variablen == Aus Treiber Datensatz
Y-Variablen == Aus Ertrag Ohne Sch�ler Datensatz

'

'bsp 5 - lineare Regression '

plot(data_Treiber$Ank�nfte,data_Treiber$`Bev�lkerungfsab/-zunahme`)
# Bei Plot: X-Variable kommt ZUERST dann erst Y-Variable!
#X = Ank�nfte , Y= Bev�lkerungsab/-zunahme


model5<-lm(data_Treiber$`Bev�lkerungfsab/-zunahme` ~ data_Treiber$Ank�nfte, data =data_Treiber )
#Y-Variable kommt hier ZUERST und dann erst X-Variable
#X = Ank�nfte , Y= Bev�lkerungsab/-zunahme

#regressionsgerade durch modell zeichnen lassen
abline(lm(data_Treiber$`Bev�lkerungfsab/-zunahme`~ data_Treiber$Ank�nfte), col="red")
#Y-Variable kommt hier ZUERST und dann erst X-Variable
#X = Ank�nfte , Y= Bev�lkerungsab/-zunahme


summary(model5)

'Ausgabe ==>
Call:
lm(formula = data_Treiber$`Bev�lkerungfsab/-zunahme` ~ data_Treiber$Ank�nfte, 
    data = data_Treiber)

Residuals:
    Min      1Q  Median      3Q     Max 
-6080.8 -1477.4   -24.3  1360.9  7031.8 

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
(Intercept)           -3.754e+02  8.323e+02  -0.451    0.653    
data_Treiber$Ank�nfte  3.622e-03  8.291e-04   4.369 2.92e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2295 on 106 degrees of freedom
  (120 observations deleted due to missingness)
Multiple R-squared:  0.1526,	Adjusted R-squared:  0.1446 
F-statistic: 19.09 on 1 and 106 DF,  p-value: 2.92e-05


Erkl�rung:
Wir betrachten die letzte Zeile

p-Wert == p-value: 2.92e-05 < 0.05  => statitische Signifikanz gegeben!
==> H1 wird angenommen!
Somit gilt H1  also dass das model5 einen Erkl�rungsbeitrag leistet
dass die Ank�nfte auf die Bev�lkerungsab/-zunahme Einfluss nimmt!
Wir k�nnen also mit der Interpretation dieses Models fortfahren,da die 
stat. Signifikanz aufgrund des p-Werts gilt!


Wir betrachten nun die vorletzte Zeile in der summary Ausgabe also:
  " Multiple R-squared:  0.1526,	Adjusted R-squared:  0.1446 "

Multiple R-squared besagt== 
  Wieviel Prozent der Varianz der abh�ngigen Variable kann ich mit dem
Modell erkl�ren?
  0 == keine Erkl�rung
1 == perfekte Erkl�rung

==> Folgerung:
Wir k�nnen nicht besonders gut mit der X-Varible (Ank�nfte) die Y-Variable
(Bev�lkerungsab/-zunahme) erkl�ren! Das hei�t, wir k�nnen nur 15,26%
der Y-Variable durch die X-Variable erkl�ren!

Wir betrachten hier "Multiple R-squared" da wir nur EINE X-Variable haben. 
Wenn wir mehrere X-Variable h�tten, w�rden wir "Adjusted R-squared" betrachten!


Zeile data_Treiber$Ank�nfte, Spalte Estimate:
Der Wert 3.622e-03 ist positiv somit ist die Steigung der Regressionsgerade
positiv also verl�uft von unten links nach oben rechts.

Zeile data_Treiber$Ank�nfte, Spalte Pr(>|t|) :
P-Wert liegt bei 2.92e-05 *** also wird die H1 Hypothese angenommen, somit ist
die X-Variable (Ank�nfte) statistisch signifikant f�r die Y-Variable
(Bev�lkerungsab/-zunahme)! 
Die *** unterst�tzen da die statitische Signifikanz zur Wahrscheinlichkeit
von 0 bis 0.001 fehlerhaft also es ist zu 99% stat. signifikant!

==> der bezug erfolgt auf das lineare regressionsmodell

y = alpha + Beta1*x1
alpha == Y-Achsenabschnitt == Schnittpunkt zur y-achse
      == ersteZeileErsteSpalte in Summary Ausgabe
      == Zeile "Intercept" + Spalte "Estimate"

Beta1 == ZweiteZeileErsteSpalte 
      == Zeile "data_Treiber$Ank�nfte" + Spalte "Estimate"

x1 == Y-Variable also "Bev�lkerungsab/-zunahme"


'

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'










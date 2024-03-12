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


'Bsp1 für einfache lineare Regression++++++++++++++++++++++++++++++++'
#betrachte Merkmale ploten bzw. im Streudiagramm anschauen um zusammehang
#zu erkennen
plot(data_Treiber$Übernachtungen,data_Treiber$`Bevölkerungsstand Monatsende`)
# Bei Plot: X-Variable kommt ZUERST dann erst Y-Variable!
# X= Übernachtungen, Y= Bevölkerungsstand Monatsende
# je mehr Übernachtungen erfolgen desto größer wird die Bevölkerung in diesem Zeitraum


'==> da ein direkter linearer zusammenhang zwischen X --> Y existiert ist es
nun sinnvoll ein lineares modell zu erstellen! Dh die Daten können durch ein
solches Modell erklärt werden, aufgrund der Linearität.
'

model1<-lm(data_Treiber$`Bevölkerungsstand Monatsende`~ data_Treiber$Übernachtungen, data =data_TreiberAb2005 )
#Y-Variable kommt hier ZUERST und dann erst X-Variable
#<ModelName> <- lm(<NameY_Var> ~ <NameX_Var>, data = <NameDataFrameAlsoDatensatz>)
# X= Übernachtungen, Y= Bevölkerungsstand Monatsende


#regressionsgerade durch modell zeichnen lassen
abline(model1, col="red")

# summary erstellen um die abstände/residuen zwischen den tatsächlichen daten
# und der geschätzen regressionsfkt zu berechnen

summary(model1)
'==> Ausgabe:
p-value: 0.8988 > 0.05 ==> H0 gilt immernoch
dh model1 leistet KEINEN Erklärungsbeitrag und kann komplett verworfen werden
'
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

'Bsp2 für einfache lineare Regression++++++++++++++++++++++++++++++++'


plot(data_Treiber$Ankünfte,data_Treiber$`Bevölkerungsstand Monatsende`)
# Bei Plot: X-Variable kommt ZUERST dann erst Y-Variable!
# X= Ankünfte, Y= Bevölkerungsstand Monatsende
# je mehr Übernachtungen erfolgen desto größer wird die Bevölkerung in diesem Zeitraum


'==> da ein direkter linearer zusammenhang zwischen X --> Y existiert ist es
nun sinnvoll ein lineares modell zu erstellen! Dh die Daten können durch ein
solches Modell erklärt werden, aufgrund der Linearität.
'

model2<-lm(data_Treiber$`Bevölkerungsstand Monatsende`~ data_Treiber$Ankünfte, data =data_TreiberAb2005 )
#Y-Variable kommt hier ZUERST und dann erst X-Variable
# X= Ankünfte, Y= Bevölkerungsstand Monatsende

#regressionsgerade durch modell zeichnen lassen
abline(model2, col="red")


# summary erstellen um die abstände/residuen zwischen den tatsächlichen daten
# und der geschätzen regressionsfkt zu berechnen
summary(model2)
'==> Ausgabe:
p-value: 0.7079 > 0.05 ==> H0 gilt immernoch
dh model2 leistet KEINEN Erklärungsbeitrag und kann komplett verworfen werden

'
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
'Bsp3 für einfache lineare Regression++++++++++++++++++++++++++++++++'


plot(data_Treiber$Sommer,data_Treiber$Fahrrad)
# Bei Plot: X-Variable kommt ZUERST dann erst Y-Variable!
#X = Sommer , Y= Fahrrad

model3<-lm(data_Treiber$Sommer~ data_Treiber$Fahrrad, data =data_Treiber )
#Y-Variable kommt hier ZUERST und dann erst X-Variable
# X= Sommer, Y= Fahrrad

#regressionsgerade durch modell zeichnen lassen
abline(model3, col="red")


# summary erstellen um die abstände/residuen zwischen den tatsächlichen daten
# und der geschätzen regressionsfkt zu berechnen
summary(model3)

'==> Ausgabe:
p-value: 6.616e-11 < 0.05 ==> H0 wird verworfen
Somit gilt H1 dass das model3 einen Erklärungsbeitrag leistet
dass der Sommer auf die Fahrrad-Nutzung Einfluss nimmt!
'


'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
#LÖSUNG!!!
'Bsp4 für einfache lineare Regression++++++++++++++++++++++++++++++++'

plot(data_Treiber$Ankünfte,data_Treiber$Übernachtungen)
# Bei Plot: X-Variable kommt ZUERST dann erst Y-Variable!
#X = Ankünfte , Y= Übernachtungen


model4<-lm(data_Treiber$Übernachtungen ~ data_Treiber$Ankünfte, data =data_Treiber )
#Y-Variable kommt hier ZUERST und dann erst X-Variable
#X = Ankünfte , Y= Übernachtungen


#regressionsgerade durch modell zeichnen lassen
abline(lm(data_Treiber$Übernachtungen~ data_Treiber$Ankünfte), col="red")
#Y-Variable kommt hier ZUERST und dann erst X-Variable
#X = Ankünfte , Y= Übernachtungen

summary(model4)

'Ausgabe:
F-statistic:  6908 on 1 and 192 DF,  p-value: < 2.2e-16

Das heißt==> p-wert == p-value: < 2.2e-16 <0.05
==> H0 wird verworfen
Somit gilt H1 dass das model3 einen Erklärungsbeitrag leistet
dass die Ankünfte auf die Üernachtungen Einfluss nimmt!
Wir können also mit der Interpretation dieses Models fortfahren
'

'
Wir betrachten nun die vorletzte Zeile in der summary Ausgabe also:
  " Multiple R-squared:  0.973,	Adjusted R-squared:  0.9728  "

Multiple R-squared besagt== 
  Wieviel Prozent der Varianz der abhängigen Variable kann ich mit dem
Modell erklären?
  0 == keine Erklärung
1 == perfekte Erklärung

==> Folgerung:
Wir sehen wir können mit der X-Variable (Ankünfte) ganz gut die Y-Varible
(Übernachtungen) erklären!Das heißt, wir können 97,3% der Y-Variable durch 
die X-Variable erklären!

Wir betrachten hier eher "Multiple R-squared" da wir nur EINE X-Variable haben! 
Hätten wir mehr X-Variablen, dann würden wir "Adjusted R-squared" betrachten!


Wir betrachten nun die Koeffizienten:

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
(Intercept)           -1.514e+05  2.677e+04  -5.653 5.64e-08 ***
data_Treiber$Ankünfte  2.547e+00  3.065e-02  83.112  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


Zeile data_Treiber$Ankünfte, Spalte Estimate:
Der Wert 2.547e+00 ist positiv somit ist die Steigung der Regressionsgerade
positiv also verläuft von unten links nach oben rechts.

Zeile data_Treiber$Ankünfte, Spalte Pr(>|t|) :
P-Wert liegt bei < 2e-16 also wird die H1 Hypothese angenommen, somit ist
die X-Variable (Ankünfte) statistisch signifikant für die Y-Variable
(Übernachtungen)! 
Die *** unterstützen da die statitische Signifikanz zur Wahrscheinlichkeit
von 0 bis 0.001 fehlerhaft also es ist zu 99% stat. signifikant!

==> der bezug erfolgt auf das lineare regressionsmodell

y = alpha + Beta1*x1
alpha == Y-Achsenabschnitt == Schnittpunkt zur y-achse
      == ersteZeileErsteSpalte in Summary Ausgabe
      == Zeile "Intercept" + Spalte "Estimate"

Beta1 == ZweiteZeileErsteSpalte 
      == Zeile "data_Treiber$Ankünfte" + Spalte "Estimate"

x1 == Y-Variable also "Übernachtungen"
'

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
#LÖSUNG!!!!!
'
X-Variablen == Aus Treiber Datensatz
Y-Variablen == Aus Ertrag Ohne Schüler Datensatz

'

'bsp 5 - lineare Regression '

plot(data_Treiber$Ankünfte,data_Treiber$`Bevölkerungfsab/-zunahme`)
# Bei Plot: X-Variable kommt ZUERST dann erst Y-Variable!
#X = Ankünfte , Y= Bevölkerungsab/-zunahme


model5<-lm(data_Treiber$`Bevölkerungfsab/-zunahme` ~ data_Treiber$Ankünfte, data =data_Treiber )
#Y-Variable kommt hier ZUERST und dann erst X-Variable
#X = Ankünfte , Y= Bevölkerungsab/-zunahme

#regressionsgerade durch modell zeichnen lassen
abline(lm(data_Treiber$`Bevölkerungfsab/-zunahme`~ data_Treiber$Ankünfte), col="red")
#Y-Variable kommt hier ZUERST und dann erst X-Variable
#X = Ankünfte , Y= Bevölkerungsab/-zunahme


summary(model5)

'Ausgabe ==>
Call:
lm(formula = data_Treiber$`Bevölkerungfsab/-zunahme` ~ data_Treiber$Ankünfte, 
    data = data_Treiber)

Residuals:
    Min      1Q  Median      3Q     Max 
-6080.8 -1477.4   -24.3  1360.9  7031.8 

Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
(Intercept)           -3.754e+02  8.323e+02  -0.451    0.653    
data_Treiber$Ankünfte  3.622e-03  8.291e-04   4.369 2.92e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2295 on 106 degrees of freedom
  (120 observations deleted due to missingness)
Multiple R-squared:  0.1526,	Adjusted R-squared:  0.1446 
F-statistic: 19.09 on 1 and 106 DF,  p-value: 2.92e-05


Erklärung:
Wir betrachten die letzte Zeile

p-Wert == p-value: 2.92e-05 < 0.05  => statitische Signifikanz gegeben!
==> H1 wird angenommen!
Somit gilt H1  also dass das model5 einen Erklärungsbeitrag leistet
dass die Ankünfte auf die Bevölkerungsab/-zunahme Einfluss nimmt!
Wir können also mit der Interpretation dieses Models fortfahren,da die 
stat. Signifikanz aufgrund des p-Werts gilt!


Wir betrachten nun die vorletzte Zeile in der summary Ausgabe also:
  " Multiple R-squared:  0.1526,	Adjusted R-squared:  0.1446 "

Multiple R-squared besagt== 
  Wieviel Prozent der Varianz der abhängigen Variable kann ich mit dem
Modell erklären?
  0 == keine Erklärung
1 == perfekte Erklärung

==> Folgerung:
Wir können nicht besonders gut mit der X-Varible (Ankünfte) die Y-Variable
(Bevölkerungsab/-zunahme) erklären! Das heißt, wir können nur 15,26%
der Y-Variable durch die X-Variable erklären!

Wir betrachten hier "Multiple R-squared" da wir nur EINE X-Variable haben. 
Wenn wir mehrere X-Variable hätten, würden wir "Adjusted R-squared" betrachten!


Zeile data_Treiber$Ankünfte, Spalte Estimate:
Der Wert 3.622e-03 ist positiv somit ist die Steigung der Regressionsgerade
positiv also verläuft von unten links nach oben rechts.

Zeile data_Treiber$Ankünfte, Spalte Pr(>|t|) :
P-Wert liegt bei 2.92e-05 *** also wird die H1 Hypothese angenommen, somit ist
die X-Variable (Ankünfte) statistisch signifikant für die Y-Variable
(Bevölkerungsab/-zunahme)! 
Die *** unterstützen da die statitische Signifikanz zur Wahrscheinlichkeit
von 0 bis 0.001 fehlerhaft also es ist zu 99% stat. signifikant!

==> der bezug erfolgt auf das lineare regressionsmodell

y = alpha + Beta1*x1
alpha == Y-Achsenabschnitt == Schnittpunkt zur y-achse
      == ersteZeileErsteSpalte in Summary Ausgabe
      == Zeile "Intercept" + Spalte "Estimate"

Beta1 == ZweiteZeileErsteSpalte 
      == Zeile "data_Treiber$Ankünfte" + Spalte "Estimate"

x1 == Y-Variable also "Bevölkerungsab/-zunahme"


'

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'










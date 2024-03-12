#datensatz Ertrag ohne Schüler-Test in R multivariate Regresssionsnanalyse

#installation der library dazu
install.packages("readxl")
library(readxl)

#import von datensatz
data_Ertrag<-read_excel(file.choose(), na="NA")

'Bsp 1 - lineare regression+++++++++++++++++++++++++++++++++++++++++++++'

plot(data_Ertrag$`Einzelfahrscheine Berlin ABC (450010)`, data_Ertrag$`Gesamt nach EAVs`)
# Bei Plot: X-Variable kommt ZUERST dann erst Y-Variable! 
# X== Einzelfahrscheine, Y == Gesamt nach EAVs

model1<-lm(data_Ertrag$`Gesamt nach EAVs`~ data_Ertrag$`Einzelfahrscheine Berlin ABC (450010)`, data = data_Ertrag )
#Y-Variable kommt hier ZUERST und dann erst X-Variable

abline(lm (data_Ertrag$`Gesamt nach EAVs`~ data_Ertrag$`Einzelfahrscheine Berlin ABC (450010)`), col="red")
#Y-Variable kommt hier ZUERST und dann erst X-Variable


summary(model1)
'==> Ausgabe:
Call:
lm(formula = data_Ertrag$`Gesamt nach EAVs` ~ data_Ertrag$`Einzelfahrscheine Berlin ABC (450010)`, 
    data = data_Ertrag)

Residuals:
      Min        1Q    Median        3Q       Max 
-26576547  -4770365   -994976   4804074  20112673 

Coefficients:
                                                     Estimate Std. Error
(Intercept)                                         1.361e+07  2.773e+06
data_Ertrag$`Einzelfahrscheine Berlin ABC (450010)` 2.672e+00  2.196e-01
                                                    t value Pr(>|t|)    
(Intercept)                                           4.909 1.97e-06 ***
data_Ertrag$`Einzelfahrscheine Berlin ABC (450010)`  12.169  < 2e-16 ***
---
Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1 ' ' 1

Residual standard error: 6248000 on 190 degrees of freedom
Multiple R-squared:  0.438,	Adjusted R-squared:  0.435 
F-statistic: 148.1 on 1 and 190 DF,  p-value: < 2.2e-16


Erklärung:

Wir betrachten die letzte Zeile in der Ausgabe:

p-Wert ==  p-value: < 2.2e-16 < 0.05 ==> statistische Signifikanz gegeben!
==> H1 wird angenommen!
Somit gilt H1 dass das model1 einen Erklärungsbeitrag leistet
dass die "Einzelfahrscheine Berlin ABC" auf "Gesamt nach EAVs" Einfluss nimmt!
Wir können also mit der Interpretation dieses Models fortfahren!


Wir betrachten nun die vorletzte Zeile in der summary Ausgabe also:
  " Multiple R-squared:  0.438,	Adjusted R-squared:  0.435   "

Multiple R-squared besagt== 
  Wieviel Prozent der Varianz der abhängigen Variable kann ich mit dem
Modell erklären?
  0 == keine Erklärung
1 == perfekte Erklärung

==> Folgerung:
Wir sehen wir können mit der X-Variable (Einzelfahrscheine Berlin ABC) nicht 
gut die Y-Varible (Gesamt nach EAVs) erklären! Das heißt, wir können nur 43,5%
der Y-Variable durch die X-Variable erklären!

Wir betrachten hier eher "Multiple R-squared" da wir nur EINE X-Variable haben! 
Hätten wir mehr X-Variablen, dann würden wir "Adjusted R-squared" betrachten!


Wir betrachten nun die Koeffizienten:

Zeile data_Ertrag$Einzelfahrscheine Berlin ABC (450010), Spalte Estimate:
Der Wert 12.169 ist positiv somit ist die Steigung der Regressionsgerade
positiv also verläuft von unten links nach oben rechts.

Zeile data_Ertrag$Einzelfahrscheine Berlin ABC (450010), Spalte Pr(>|t|) :
P-Wert liegt bei < 2e-16 *** also wird die H1 Hypothese angenommen, somit ist
die X-Variable (Ankünfte) statistisch signifikant für die Y-Variable
(Übernachtungen)! 
Die *** unterstützen da die statitische Signifikanz zur Wahrscheinlichkeit
von 0 bis 0.001 fehlerhaft also es ist zu 99% stat. signifikant!

==> der bezug erfolgt auf das lineare regressionsmodell

y = alpha + Beta1*x1
<==>
y = alpha + Beta1*EinzelfahrscheineBerlinABC

alpha == Y-Achsenabschnitt == Schnittpunkt zur y-achse
      == ersteZeileErsteSpalte in Summary Ausgabe
      == Zeile "Intercept" + Spalte "Estimate"

Beta1 == ZweiteZeileErsteSpalte 
      == Zeile "data_Ertrag$Einzelfahrscheine Berlin ABC (450010)" + Spalte "Estimate"
      == Steigung
      
x1 == X-Variable also "Einzelfahrscheine Berlin ABC "



'
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'




#regressionsmodelle blatt 5: regressionsmodell, schätzung, lineares modell

##Aufgabe 1 
d <- data.frame(
  Geschwindigkeit = rep(c(56.3, 64.4, 72.4, 80.4, 88.5, 96.5), each = 2),
  Verbrauch = c(10.7, 11.8, 8.4, 7.6, 6.4, 6.2, 5.7, 6, 6.9, 6.4, 8.7, 7.8)
)

# Datenpunkte 
plot(Verbrauch~Geschwindigkeit, data=d, col="blue",
     xlab = "Geschwindigkeit", ylab = "Verbrauch")
# Modellanpassung
abc<-coef(fit<-lm(Verbrauch~Geschwindigkeit+I(Geschwindigkeit^2),data=d))
abc
# Modellfunktion
curve(abc[1]+abc[2]*x+abc[3]*x^2,col=gray(0.5), lwd=1.5,add=T)

summary(fit)
#Prognosewert bei 70 kmh
abc[1]+abc[2]*70+abc[3]*70^2
#Ableiten und nullstellen
#abc[2]+2*abc[3]*x=0
#L???sung ist
abc[2]/(-2*abc[3])

#Teil b)
tkoeff<-qt(0.975,9)
0.0094573-tkoeff*0.0007702 
0.0094573+tkoeff*0.0007702 

#Teil C) Warum ist der p-Wert der quadratische Parameter klein?

#Teil d)
fit3<-lm(Verbrauch~Geschwindigkeit+I(Geschwindigkeit^2)+I(Geschwindigkeit^3),data=d)
summary(fit3)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Aufgabe 2
#a)
temp<-c(7,  9,  15,  13,  11,  16,  12,  8,  13,  16, 4,
        5, 6, 3, 8, 10, 3, 9, 8, 12, 7, 10, 11, 12)
Atomkraft<-data.frame(Einstellung=temp,Gruppe=rep(c("A","B","C"),
                                                  c(10,8,6)))

'#
Ausgabe:

> Atomkraft
   Einstellung Gruppe
1            7      A
2            9      A
3           15      A
4           13      A
5           11      A
6           16      A
7           12      A
8            8      A
9           13      A
10          16      A
11           4      B
12           5      B
13           6      B
14           3      B
15           8      B
16          10      B
17           3      B
18           9      B
19           8      C
20          12      C
21           7      C
22          10      C
23          11      C
24          12      C

==> Dh es gilt

links== werte auf y-achse == Meinung Proband über Atomkraft

rechts == Buchstabe der Faktorgruppe der Filmart auf x-achse == Filme die die
Meinung der Probanden beeinflussen sollen

Jeder Proband schaut sich genau NUR einen film an:
Es schauen 
10 Probanden --> Film Gruppe A
8 Probanden --> Film Gruppe B
6 Probanden --> Film Gruppe C

Jeder Wert repräsentiert einen Probanden.
--> Je höher der wert ist desto mehr befürwortet der proband die atomkraft.
Das wird in Zusammenhang gesetzt welchen film der Proband geguckt hat!


#'

#Atomkraft == Name der Datentabelle wo jeder Wert einer Faktorgruppe zugewiesen




'#
-------------------------------------------------------------------------------
Regel- Sinn und Nutzung von data.frame

-sind dazu da um verschiedene arten von daten in einer tabelle zusammenzufassen
==> man nutzt das vorallem bei anova 


data.frame(., row.names = NULL, check.rows = FALSE,
           check.names = TRUE, fix.empty.names = TRUE,
           stringsAsFactors = default.stringsAsFactors())


-------------------------------------------------------------------------------
Regel- nutzung vektor c(<Datensatz>) 

temp2<-c(7,  9,  15,  13,  11,  16,  12,  8,  13,  16, 4,
        5, 6, 3, 8, 10, 3, 9, 8, 12, 7, 10, 11, 12)

ausgabe:
[1]  7  9 15 13 11 16 12  8 13 16  4  5  6  3  8 10  3  9  8 12  7 10 11 12

==> wir haben also einen vektor mit den daten/werten erstellt 



-------------------------------------------------------------------------------

regel- rep(...)-fkt mit dataframe und werten aus Cvektor um tabelle mit 
faktorgruppen zu erstellen:

Fall-1):

Buchstaben werden in der selben Reihenfolge immer den Zahlen zugewiesen, bis 
alle werte einen buchstaben haben. dabei muss die anzahl der buchstaben und
werte nicht gleich sein!

rep(<SpaltennameZahlen> = <CVektorMitZahlen>, <SpaltennameBuchstaben> = rep(c("A","B","C")) )


Bsp obda:

temp2<-c(7,  9,  15,  13,  11,  16,  12,  8,  13,  16, 4,
         5, 6, 3, 8, 10, 3, 9, 8, 12, 7, 10, 11, 12)

Test<-data.frame(Einstellung=temp2,Buchstaben = rep(c("A","B","C")))
ODER
Test<-data.frame(Einstellung=temp2,Buchstaben = c("A","B","C"))


==> Ausgabe:

> Test
   Einstellung Buchstaben
1            7          A
2            9          B
3           15          C
4           13          A
5           11          B
6           16          C
7           12          A
8            8          B
9           13          C
10          16          A
11           4          B
12           5          C
13           6          A
14           3          B
15           8          C
16          10          A
17           3          B
18           9          C
19           8          A
20          12          B
21           7          C
22          10          A
23          11          B
24          12          C


Fall-2):

Den werten werden Buchstaben in einer gewissen anzahl zugewiesen. Dh zb 10 mal 
ein A wird den ersten 10 Werten zugewiesen. 5 mal ein B den nächsten 5 Werten
zugewiesen usw


syntax:
rep(<SpaltennameZahlen> = <CVektorMitZahlen>, 
     <SpaltennameBuchstaben> = rep(c("A","B","C",....,"Z"), c(10,8,6)) )

Dh es kommt die Buchstaben folgendermaßen oft vor:
A == 10 mal, B == 8 mal , C == 6 mal

-->dieser effekt kommt durch rep(...) Funktion zustande 

bsp obda:

temp2<-c(7,  9,  15,  13,  11,  16,  12,  8,  13,  16, 4,
         5, 6, 3, 8, 10, 3, 9, 8, 12, 7, 10, 11, 12)
         
Test<-data.frame(Einstellung=temp2,Buchstaben = rep(c("A","B","C"),c(10,8,6)))

==> Ausgabe:

   Einstellung Buchstaben
1            7          A
2            9          A
3           15          A
4           13          A
5           11          A
6           16          A
7           12          A
8            8          A
9           13          A
10          16          A
11           4          B
12           5          B
13           6          B
14           3          B
15           8          B
16          10          B
17           3          B
18           9          B
19           8          C
20          12          C
21           7          C
22          10          C
23          11          C
24          12          C



-------------------------------------------------------------------------------


regel- bestimmen von linearen modell

syntax: 1) lm(y~x) oder 2)lm(y~x,dataframe)

zu 2): wird benutzt wenn man vorher eine datenmatrix dataframe 
erzeugt hatte

==> allgemein gilt: y == werte die abhängig von x sind
x == unabhängige werte dh x hat einfluss auf y. es ist gerichtet also
x -> y wie bei funktion

-------------------------------------------------------------------------------
syntax - lm bzgl anova tabelle 

lm.<NameObjekt> <-lm(<VarYwerteWasAbhängigSind> ~ <VarXwerteDieEinflussAufYnehmen>, data =<NameAnovaTabelleÜberXundY>)

bsp:
lm.obj1<-lm(Einstellung~Gruppe,data=Atomkraft)

Gruppe also Art der Filme nehmen einfluss auf die Meinung/Einstellung der Probanden
Hierzu wurde eine anova tabelle mit den probanden erstellt

i)wieviele probanden welchen film geguckt haben.denn jeder proband darf nur genau
einen film gucken
ii)jeder wert repräsentiert einen probanden--> je höher der wert ist desto
mehr befürwortet er die atomkraft
-------------------------------------------------------------------------------

#'

###############################################################################

#b)
lm.obj1<-lm(Einstellung~Gruppe,data=Atomkraft)

'#
Es schauen 
10 Probanden --> Film Gruppe A
8 Probanden --> Film Gruppe B
6 Probanden --> Film Gruppe C


x-achse == Gruppe also Buchstaben die bestimmte arten von filme sind und eine
Meinung repräsentieren
Film Gruppe A : für atomkraft
Film Gruppe B : gegen atomkraft
Film Gruppe C : pro und contra bzgl atomkraft dargelegt


y-achse == Einstellung also werte der Zuschauer die eine bestimmte Meinung
vertreten. Jeder Wert repräsentiert einen Probanden.
--> Je höher der wert ist desto mehr befürwortet der proband die atomkraft.
Das wird unter dem Zusammenhang gesetzt welchen film er geguckt hat!



x hat einfluss auf y dh es ist gerichtet
==> dh da die Filme Einfluss auf die Meinungen der Probanden nehmen , ist
die nutzung von lm sinnvoll! 

data = <Name Anova Tabelle wo jedem Wert einer Faktorgruppe zugewiesen ist>

#'


###############################################################################

#c&d)
summary(lm.obj1)

'#
==>Ausgabe:

Call:
lm(formula = Einstellung ~ Gruppe, data = Atomkraft)

Residuals:
   Min     1Q Median     3Q    Max 
 -5.00  -2.25   0.00   2.00   4.00 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  12.0000     0.8944  13.416 9.06e-12 ***
GruppeB      -6.0000     1.3416  -4.472  0.00021 ***
GruppeC      -2.0000     1.4606  -1.369  0.18537    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.828 on 21 degrees of freedom
Multiple R-squared:  0.4909,	Adjusted R-squared:  0.4424 
F-statistic: 10.12 on 2 and 21 DF,  p-value: 0.0008344


==>
Dh referenzgruppe == Gruppe A da sie als erstes in der Tabelle coefficients
erwähnt wird

p-wert Gruppe C = 0.18537 ==> dh da 0.18537 > 0.05 gilt, ist die meinung in 
                  gruppe C nicht bedeutend anders als von referenzgruppe. 
                  dh der unterschied zwischen gruppe A(=referenzgruppe) und
                  gruppe C ist nicht signifikant.
#'


'#
-------------------------------------------------------------------------------
regel-nutzen summary bei lm objekt

bsp obda:

lm.obj1<-lm(Einstellung~Gruppe,data=Atomkraft)
summary(lm.obj1)

==> durch nutzung von summary erkennt man alle nötigen infos bzgl der gruppen usw

-------------------------------------------------------------------------------
#'

################################################################################

#e)
anova(lm.obj1)

'#
==> Ausgabe:

Analysis of Variance Table

Response: Einstellung
          Df Sum Sq Mean Sq F value    Pr(>F)    
Gruppe     2    162      81  10.125 0.0008344 ***
Residuals 21    168       8                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



==> von downie:
Die  Faktorvariable Gruppe hat einen sehr kleinen p-Wert. Signifikant sowohl 
am 5% als auch 1% Signifikanznivaeau.Die Schlussfolgerung ist, dass die Art 
von Film einen signifikanten Einfluss zum Niveau 1% auf Einstellung zur Nutzung 
der Atomkraft wirkt.


--> dh zu 99% hat die art des films einen einfluss auf die meinung der probanden!
#'



qf(0.95,2,21)
qf(0.99,2,21)

pf(10.125,2,21)
1-pf(10.125,2,21)
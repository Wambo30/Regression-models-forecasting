#Regressionsmoddele - Blatt2 aufg 3), residuen

#a)Berechnen Sie für die Daten aus Aufgabe 1 die Residuen sowie die Quadratsumme der Residuen 
# ???^e2 (Summe von epsilon index i zum Quadrat)


x<-c(0,2,4,6,10) # x -komponenten der daten
y<-c(5.2,3.0,0.8,0.7,0.1) # y- komponenten
b1<- cov(x,y)/ var(x) # formel für b1 also kovarianz von x und ydurch varianz von x == Steigung
b0<- mean(y) - b1*mean(x) # formel für b0 also durschnitt y minus b1 mal durchschnitt x == y-Abschnitt


fitted<- b0+b1*x 
#funktion angepasster wert

resid<- y-fitted
#residuum == beobachteter wert in y minus angepasste werte
#==> ergebnis residuuen : [1]  1.1054054 -0.1243243 -1.3540541 -0.4837838  0.8567568


sum(resid) # ergebnis ==> 3.330669e-16




#b) Berechnen Sie die Quadratsumme der Zielgrößen ???y2 [Summe von y index i zum quadrat]

SummeBeobachteteWerte<-sum(y^2) # ergebnis ==> 37.18
sum(resid^2) #ergebnis==> 4.038919




#c)Berechnen Sie die Quadratsumme der angepassten Werte ???^y2i=???(??+??*xi)2
#[Summe aus y index i zum quadrat = Summe aus (alpha plus beta mal x index i ) zum quadrat]

SummeYdachQuadriert<-sum(fitted^2) 
# ergebnis ==> 33.14108 dh diese variable steht orthogonal auf den residuuen!

SummeResiduuenQuadriert<-sum(resid^2)




#d)Was ist der Zusammenhang dieser drei Quadratsummen?


SummeBeobachteteWerte2<-SummeYdachQuadriert+SummeResiduuenQuadriert
#ergebnis==> 37.18 also ist demnach gleich der summe der beobachteten werte zum quadrat
# wie in b)

#-----------------------------------------------------------------------------------------#
'#
Aufgabe 4) Matrixregressionsmodelle mit R. Benutzen Sie R um die Kleinstquadrat-Gleichungen 
des folgenden Beispiels zu anwenden. Ihre Lösung sollte die R-Funktionen %*%, t() und solve()
verwenden.
Die x und y Variablen sind: 
x<-1:10 
y<-c(1.4,12.0,16.8,20.9,23.1,25.5,25.1,24.2,16.8, 8.6)

#'

#statt das wir lm-funktion nutzen, nutzen wir die matrixmultiplikation allgemein

x<-1:10 
y<-c(1.4,12.0,16.8,20.9,23.1,25.5,25.1,24.2,16.8, 8.6)
n<-10
plot(x,y)
#der entstehende graf passt nicht so gut zu einer linearen regression --> wir
#versuchen aber das numerisch zu bestimmen

X<-cbind(rep(1,n),x)
#definition matrix x wobei rep == repeat dh rep(1,n)== 1 wird n-mal wiederholt

'#
==> ausgabe:

        x
 [1,] 1  1
 [2,] 1  2
 [3,] 1  3
 [4,] 1  4
 [5,] 1  5
 [6,] 1  6
 [7,] 1  7
 [8,] 1  8
 [9,] 1  9
[10,] 1 10

anderes bsp:

cbind(1:3,1:10)

==> ausgabe:

      [,1] [,2]
 [1,]    1    1
 [2,]    2    2
 [3,]    3    3
 [4,]    1    4
 [5,]    2    5
 [6,]    3    6
 [7,]    1    7
 [8,]    2    8
 [9,]    3    9
[10,]    1   10

==> dh erste spalte von matrix wird so oft wiederholt, so dass man die anzahl 
der werte der zweiten spalte!

#'

t(X) #X matrix transponiert
'#
==> ausgabe:

 [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
     1    1    1    1    1    1    1    1    1     1
     1    1    1    1    1    1    1    1    1     1
x    1    2    3    4    5    6    7    8    9    10


#'


t(X)%*%X
# matrixmultiplikation-zeichen==> %*%
'#
==>ausgabe:

         x
  10 10  55
  10 10  55
x 55 55 385

#'

solve(t(X)%*%X)

'#
regel- invertierung matrix

solve(<NameMatrix>) == Invertierung matrix

bsp:
solve(X) ==> ausgabe: invertierte matrix von X

#'

betahat<-solve(t(X)%*%X)%*%(t(X)%*%y)
# betahat == beta dach
'#
==>ausgabe:

        [,1]
  12.4266667
x  0.9115152


#'

Hut<-X %*%solve(t(X)%*%X)%*%t(X)
'#
==>ausgabe: eine 10x10 Matrix

             [,1]         [,2]         [,3]       [,4]       [,5]       [,6]
 [1,]  0.34545455  0.290909091  0.236363636 0.18181818 0.12727273 0.07272727
 [2,]  0.29090909  0.248484848  0.206060606 0.16363636 0.12121212 0.07878788
 [3,]  0.23636364  0.206060606  0.175757576 0.14545455 0.11515152 0.08484848
 [4,]  0.18181818  0.163636364  0.145454545 0.12727273 0.10909091 0.09090909
 [5,]  0.12727273  0.121212121  0.115151515 0.10909091 0.10303030 0.09696970
 [6,]  0.07272727  0.078787879  0.084848485 0.09090909 0.09696970 0.10303030
 [7,]  0.01818182  0.036363636  0.054545455 0.07272727 0.09090909 0.10909091
 [8,] -0.03636364 -0.006060606  0.024242424 0.05454545 0.08484848 0.11515152
 [9,] -0.09090909 -0.048484848 -0.006060606 0.03636364 0.07878788 0.12121212
[10,] -0.14545455 -0.090909091 -0.036363636 0.01818182 0.07272727 0.12727273
            [,7]         [,8]         [,9]       [,10]
 [1,] 0.01818182 -0.036363636 -0.090909091 -0.14545455
 [2,] 0.03636364 -0.006060606 -0.048484848 -0.09090909
 [3,] 0.05454545  0.024242424 -0.006060606 -0.03636364
 [4,] 0.07272727  0.054545455  0.036363636  0.01818182
 [5,] 0.09090909  0.084848485  0.078787879  0.07272727
 [6,] 0.10909091  0.115151515  0.121212121  0.12727273
 [7,] 0.12727273  0.145454545  0.163636364  0.18181818
 [8,] 0.14545455  0.175757576  0.206060606  0.23636364
 [9,] 0.16363636  0.206060606  0.248484848  0.29090909
[10,] 0.18181818  0.236363636  0.290909091  0.34545455

round(Hut,3) also auf 3 nachkommastellen abrunden

==> ausgabe: eine symmetrische matrix

        [,1]   [,2]   [,3]  [,4]  [,5]  [,6]  [,7]   [,8]   [,9]  [,10]
 [1,]  0.345  0.291  0.236 0.182 0.127 0.073 0.018 -0.036 -0.091 -0.145
 [2,]  0.291  0.248  0.206 0.164 0.121 0.079 0.036 -0.006 -0.048 -0.091
 [3,]  0.236  0.206  0.176 0.145 0.115 0.085 0.055  0.024 -0.006 -0.036
 [4,]  0.182  0.164  0.145 0.127 0.109 0.091 0.073  0.055  0.036  0.018
 [5,]  0.127  0.121  0.115 0.109 0.103 0.097 0.091  0.085  0.079  0.073
 [6,]  0.073  0.079  0.085 0.091 0.097 0.103 0.109  0.115  0.121  0.127
 [7,]  0.018  0.036  0.055 0.073 0.091 0.109 0.127  0.145  0.164  0.182
 [8,] -0.036 -0.006  0.024 0.055 0.085 0.115 0.145  0.176  0.206  0.236
 [9,] -0.091 -0.048 -0.006 0.036 0.079 0.121 0.164  0.206  0.248  0.291
[10,] -0.145 -0.091 -0.036 0.018 0.073 0.127 0.182  0.236  0.291  0.345

#'

ydach<-Hut%*%y
#angepasste werte
'#
==> ausgabe:
         [,1]
 [1,] 13.33818
 [2,] 14.24970
 [3,] 15.16121
 [4,] 16.07273
 [5,] 16.98424
 [6,] 17.89576
 [7,] 18.80727
 [8,] 19.71879
 [9,] 20.63030
[10,] 21.54182

#'

#Prüfe ob herkömmliche selbes ergebnis hat wie diese

ydach-(betahat[1]+betahat[2]*x)
'#
==> ausgabe:

             [,1]
 [1,]  8.881784e-15
 [2,]  1.421085e-14
 [3,]  1.776357e-15
 [4,]  7.105427e-15
 [5,]  7.105427e-15
 [6,] -7.105427e-15
 [7,]  7.105427e-15
 [8,]  3.552714e-15
 [9,] -3.552714e-15
[10,]  1.065814e-14


#'

#eintragen von ydach in das diagramm
points(x,ydach,pch="+")
#man sieht hier dann eine gerade aus plus zeichen, dh pch== <zeichenNutzung>
# ==> ydach passt daher nicht so und man würde eine quadratische anpassung nehmen

resid<- y-ydach
sum(resid^2 - mean(resid)) # ausgabe=> 510.6381
var(resid) # ausgabe=> 56.73756

sum(resid^2)/9 # ausgabe=> 56.73756
# n-1 = 9



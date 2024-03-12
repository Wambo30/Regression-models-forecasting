'Master Paper mit dem Thema

Univariate Zeitreihenanalyse zur Prognose der Lebensumst¨ande
von Senioren in der Bundesrepublik Deutschland

Schritte zur Umsetzung
1) Daten hier hochladen
2) Die Spalten ordnen
1)Hierzu werde ich die Bibliothek ts nutzen


Fertiger Code zur Orientierung:

install.packages("forecast")
library(forecast)
#install.packages("fpp2")

tsdata<-ts(Zeit_dataFrame_Ertrag_Y$`Einzelfahrscheine ABC`,frequency = 12,start=c(2012,1))
plot(tsdata)

AutoArima_Einzelfahrscheine<-auto.arima(tsdata)

forecast_Einzelfahrscheine<-forecast(AutoArima_Einzelfahrscheine,h=12)

plot(forecast_Einzelfahrscheine)

#Plot zoomen von 2020 bis 2022 um Werte besser zu sehen
plot(forecast_Einzelfahrscheine,xlim=c(2020,2022))

summary(forecast_Einzelfahrscheine)
accuracy(forecast_Einzelfahrscheine)


'
# Einlesen der Datensätze


install.packages("readxl")
library(readxl)

'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
#Datensatz 22121-0001
data0001_mUeber65<-read_excel(file.choose(), na="NA")

names(data0001_mUeber65)

dataFrame_0001_mUeber65<-data.frame(data0001_mUeber65$`Insgesamt männlich - 65 Jahre und mehr`)

library(forecast)
#tsdata<-ts(dataFrame_Y_0001_mUeber65$data_221210001..Insgesamt.männlich...65.Jahre.und.mehr.)

' frequency ist nur 1 da man es jährlich betrachtet und nicht monatlich!'
#tsdata<-ts(data_221210001$`Insgesamt männlich - 65 Jahre und mehr`,frequency = 1,start=c(2005,1))
tsdata<- ts(dataFrame_0001_mUeber65, frequency = 1,start=c(2005,1))
plot (tsdata)

AutoArima_0001_mUeber65<-auto.arima(tsdata)

'Prüfung durch Ljung-Box Test, ob man Prognose erstellen kann '
Box.test(AutoArima_0001_mUeber65$residuals,type="Ljung-Box")

'
Ausgabe==>
Box-Ljung test

data:  AutoArima_0001_mUeber65$residuals
X-squared = 0.099896, df = 1, p-value = 0.752


==> Dh da der P-Wert >0.05 ist ==> White-Noise Prozess! Somit sind die Werte unabhängig voneinander
i.i.d. und die Forecast auf diesem ARIMA-Modell sind stichhaltig !

Erst jetzt kann man die Prognose erstellen

'

forecast_0001_mUeber65<-forecast(AutoArima_0001_mUeber65,h=12)

plot(forecast_0001_mUeber65)

plot(forecast_0001_mUeber65,xlim=c(2005,2024))

' hat nicht funktioniert!es kommt eine horizontale gerade raus!

'

summary(forecast_0001_mUeber65)

'
MAPE = 6.590725
==> sehr hohe Prognosegenauigkeit!

'

#------------------------------------------------------------------------------------------------------

data0001_wUeber65<-read_excel(file.choose(), na="NA")
names(data0001_wUeber65)
dataFrame_0001_wUeber65<-data.frame(data0001_wUeber65$`Ort der Leistungserbringung - Insgesamt weiblich`)
library(forecast)

tsdata<- ts(dataFrame_0001_wUeber65, frequency = 1,start=c(2005,1))
plot (tsdata)

AutoArima_0001_wUeber65<-auto.arima(tsdata)

'Prüfung durch Ljung-Box Test, ob man Prognose erstellen kann '
Box.test(AutoArima_0001_wUeber65$residuals,type="Ljung-Box")

'
Ausgabe==>
Box-Ljung test

data:  AutoArima_0001_wUeber65$residuals
X-squared = 1.641, df = 1, p-value = 0.2002


==> Dh da der P-Wert >0.05 ist ==> White-Noise Prozess! Somit sind die Werte unabhängig voneinander
i.i.d. und die Forecast auf diesem ARIMA-Modell sind stichhaltig !

Erst jetzt kann man die Prognose erstellen

'

forecast_0001_wUeber65<-forecast(AutoArima_0001_wUeber65,h=12)

plot(forecast_0001_wUeber65)
plot(forecast_0001_wUeber65,xlim=c(2005,2024))

' hat funktioniert! Werte steigen'

summary(forecast_0001_wUeber65)

#------------------------------------------------------------------------------------------------------

'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

#23211-0004 

install.packages("readxl")
library(readxl)

data0004<-read_excel(file.choose(), na="NA")

names(data0004)

#------------------------------------------------------------------------------------------------------
#0004_PsychVerhaltsstAlk_m_65BisUnter70

data0004_PsychVerhaltsstAlk_m_65BisUnter70<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsstAlk_m_65BisUnter70<-data.frame(data0004_PsychVerhaltsstAlk_m_65BisUnter70$`männlich 65 bis unter 70 Jahre`)
library(forecast)

tsdata<- ts(dataFrame_0004_PsychVerhaltsstAlk_m_65BisUnter70, frequency = 1,start=c(2005,1))
plot (tsdata)
#dataFrame_0004_PsychVerhaltsst_mUeber65BisUnter70<-data.frame(data0004$Zeitdaten , data0004$Todesursachen, data0004$`männlich 65 bis unter 70 Jahre`)

AutoArima_0004_PsychVerhaltsstAlk_m_65BisUnter70<-auto.arima(tsdata)

forecast_0004_PsychVerhaltsstAlk_m_65BisUnter70<-forecast(AutoArima_0004_PsychVerhaltsstAlk_m_65BisUnter70,h=12)

plot(forecast_0004_PsychVerhaltsstAlk_m_65BisUnter70)
plot(forecast_0004_PsychVerhaltsstAlk_m_65BisUnter70,xlim=c(2015,2030))

'
nicht geklappt- eine Gerade!

'
#------------------------------------------------------------------------------------------------------

#0004_PsychVerhaltsstAlk_m_70BisUnter75

data0004_PsychVerhaltsstAlk_m_70BisUnter75<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsstAlk_m_70BisUnter75<-data.frame(data0004_PsychVerhaltsstAlk_m_70BisUnter75$`männlich - 70 bis unter 75 Jahre`)
library(forecast)

tsdata<- ts(dataFrame_0004_PsychVerhaltsstAlk_m_70BisUnter75, frequency = 1,start=c(2005,1))
plot (tsdata)

AutoArima_0004_PsychVerhaltsstAlk_m_70BisUnter75<-auto.arima(tsdata)

forecast_0004_PsychVerhaltsstAlk_m_70BisUnter75<-forecast(AutoArima_0004_PsychVerhaltsstAlk_m_70BisUnter75,h=12)

plot(forecast_0004_PsychVerhaltsstAlk_m_70BisUnter75)
plot(forecast_0004_PsychVerhaltsstAlk_m_70BisUnter75,xlim=c(2015,2030))

'nicht geklappt- eine Gerade!'

#------------------------------------------------------------------------------------------------------

#0004_PsychVerhaltsstAlk_m_75BisUnter80

data0004_PsychVerhaltsstAlk_m_75BisUnter80<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsstAlk_m_75BisUnter80<-data.frame(data0004_PsychVerhaltsstAlk_m_75BisUnter80$`männlich - 75 bis unter 80 Jahre`)
library(forecast)

tsdata<- ts(dataFrame_0004_PsychVerhaltsstAlk_m_75BisUnter80, frequency = 1,start=c(2005,1))
plot (tsdata)

AutoArima_0004_PsychVerhaltsstAlk_m_75BisUnter80<-auto.arima(tsdata)

forecast_0004_PsychVerhaltsstAlk_m_75BisUnter80<-forecast(AutoArima_0004_PsychVerhaltsstAlk_m_75BisUnter80,h=12)

plot(forecast_0004_PsychVerhaltsstAlk_m_75BisUnter80)
plot(forecast_0004_PsychVerhaltsstAlk_m_75BisUnter80,xlim=c(2015,2030))

'nicht geklappt- eine Gerade!'

#------------------------------------------------------------------------------------------------------
#0004_PsychVerhaltsstAlk_m_80BisUnter85

data0004_PsychVerhaltsstAlk_m_80BisUnter85<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsstAlk_m_80BisUnter85<-data.frame(data0004_PsychVerhaltsstAlk_m_80BisUnter85$`männlich - 80 bis unter 85 Jahre`)
library(forecast)

tsdata<- ts(dataFrame_0004_PsychVerhaltsstAlk_m_80BisUnter85, frequency = 1,start=c(2005,1))
plot (tsdata)

AutoArima_0004_PsychVerhaltsstAlk_m_80BisUnter85<-auto.arima(tsdata)

forecast_0004_PsychVerhaltsstAlk_m_80BisUnter85<-forecast(AutoArima_0004_PsychVerhaltsstAlk_m_80BisUnter85,h=12)

plot(forecast_0004_PsychVerhaltsstAlk_m_80BisUnter85)
plot(forecast_0004_PsychVerhaltsstAlk_m_80BisUnter85,xlim=c(2015,2030))

'hat geklappt-  werte steigen'

#------------------------------------------------------------------------------------------------------
#0004_PsychVerhaltsstAlk_m_85UndMehr


data0004_PsychVerhaltsstAlk_m_85UndMehr<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsstAlk_m_85UndMehr<-data.frame(data0004_PsychVerhaltsstAlk_m_85UndMehr$`männlich - 85 Jahre und mehr`)
library(forecast)

tsdata<- ts(dataFrame_0004_PsychVerhaltsstAlk_m_85UndMehr, frequency = 1,start=c(2005,1))
plot (tsdata)


AutoArima_0004_PsychVerhaltsstAlk_m_85UndMehr<-auto.arima(tsdata)

forecast_0004_PsychVerhaltsstAlk_m_85UndMehr<-forecast(AutoArima_0004_PsychVerhaltsstAlk_m_85UndMehr,h=12)

plot(forecast_0004_PsychVerhaltsstAlk_m_85UndMehr)
plot(forecast_0004_PsychVerhaltsstAlk_m_85UndMehr,xlim=c(2015,2030))

'hat geklappt-  werte steigen'





#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
#0004_PsychVerhaltsst_m_65BisUnter70

data0004_PsychVerhaltsst_m_65BisUnter70<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsst_m_65BisUnter70<-data.frame(data0004_PsychVerhaltsst_m_65BisUnter70$`männlich 65 bis unter 70 Jahre`)
library(forecast)

tsdata<- ts(dataFrame_0004_PsychVerhaltsst_m_65BisUnter70, frequency = 1,start=c(2005,1))
plot (tsdata)

AutoArima_0004_PsychVerhaltsst_m_65BisUnter70<-auto.arima(tsdata)

'Prüfung durch Ljung-Box Test, ob man Prognose erstellen kann '
Box.test(AutoArima_0004_PsychVerhaltsst_m_65BisUnter70$residuals,type="Ljung-Box")

'
Ausgabe==>
Box-Ljung test

data:  AutoArima_0004_PsychVerhaltsst_m_65BisUnter70$residuals
X-squared = 0.69902, df = 1, p-value = 0.4031


==> Dh da der P-Wert >0.05 ist ==> White-Noise Prozess! Somit sind die Werte unabhängig voneinander
i.i.d. und die Forecast auf diesem ARIMA-Modell sind stichhaltig !

Erst jetzt kann man die Prognose erstellen

'


forecast_0004_PsychVerhaltsst_m_65BisUnter70<-forecast(AutoArima_0004_PsychVerhaltsst_m_65BisUnter70,h=12)

plot(forecast_0004_PsychVerhaltsst_m_65BisUnter70)
plot(forecast_0004_PsychVerhaltsst_m_65BisUnter70,xlim=c(2005,2024))

'hat geklappt-  werte steigen'


summary(forecast_0004_PsychVerhaltsst_m_65BisUnter70)

#------------------------------------------------------------------------------------------------------
#0004_PsychVerhaltsst_m_70BisUnter75
data0004_PsychVerhaltsst_m_70BisUnter75<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsst_m_70BisUnter75<-data.frame(data0004_PsychVerhaltsst_m_70BisUnter75$`männlich - 70 bis unter 75 Jahre`)
library(forecast)

tsdata<- ts(dataFrame_0004_PsychVerhaltsst_m_70BisUnter75, frequency = 1,start=c(2005,1))
plot (tsdata)

AutoArima_0004_PsychVerhaltsst_m_70BisUnter75<-auto.arima(tsdata)

'Prüfung durch Ljung-Box Test, ob man Prognose erstellen kann '
Box.test(AutoArima_0004_PsychVerhaltsst_m_70BisUnter75$residuals,type="Ljung-Box")

'
Ausgabe==>
Box-Ljung test

data:  AutoArima_0004_PsychVerhaltsst_m_70BisUnter75$residuals
X-squared = 1.21, df = 1, p-value = 0.2713


==> Dh da der P-Wert >0.05 ist ==> White-Noise Prozess! Somit sind die Werte unabhängig voneinander
i.i.d. und die Forecast auf diesem ARIMA-Modell sind stichhaltig !

Erst jetzt kann man die Prognose erstellen

'

forecast_0004_PsychVerhaltsst_m_70BisUnter75<-forecast(AutoArima_0004_PsychVerhaltsst_m_70BisUnter75,h=12)

plot(forecast_0004_PsychVerhaltsst_m_70BisUnter75)
plot(forecast_0004_PsychVerhaltsst_m_70BisUnter75,xlim=c(2015,2024))

'hat geklappt-  werte steigen'

summary(forecast_0004_PsychVerhaltsst_m_70BisUnter75)

#------------------------------------------------------------------------------------------------------
#0004_PsychVerhaltsst_m_75BisUnter80

library(readxl)
data0004_PsychVerhaltsst_m_75BisUnter80<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsst_m_75BisUnter80<-data.frame(data0004_PsychVerhaltsst_m_75BisUnter80$`männlich - 75 bis unter 80 Jahre`)
library(forecast)

tsdata<- ts(dataFrame_0004_PsychVerhaltsst_m_75BisUnter80, frequency = 1,start=c(2005,1))
plot (tsdata)


AutoArima_0004_PsychVerhaltsst_m_75BisUnter80<-auto.arima(tsdata)

'Prüfung durch Ljung-Box Test, ob man Prognose erstellen kann '
Box.test(AutoArima_0004_PsychVerhaltsst_m_75BisUnter80$residuals,type="Ljung-Box")

'
Ausgabe==>
Box-Ljung test

data:  AutoArima_0004_PsychVerhaltsst_m_75BisUnter80$residuals
X-squared = 1.2144, df = 1, p-value = 0.2705


==> Dh da der P-Wert >0.05 ist ==> White-Noise Prozess! Somit sind die Werte unabhängig voneinander
i.i.d. und die Forecast auf diesem ARIMA-Modell sind stichhaltig !

Erst jetzt kann man die Prognose erstellen

'


forecast_0004_PsychVerhaltsst_m_75BisUnter80<-forecast(AutoArima_0004_PsychVerhaltsst_m_75BisUnter80,h=12)

plot(forecast_0004_PsychVerhaltsst_m_75BisUnter80)
plot(forecast_0004_PsychVerhaltsst_m_75BisUnter80,xlim=c(2005,2024))


'hat geklappt-  werte steigen'

summary(forecast_0004_PsychVerhaltsst_m_75BisUnter80)

#------------------------------------------------------------------------------------------------------
#0004_PsychVerhaltsst_m_80BisUnter85

library(readxl)
data0004_PsychVerhaltsst_m_80BisUnter85<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsst_m_80BisUnter85<-data.frame(data0004_PsychVerhaltsst_m_80BisUnter85$`männlich - 80 bis unter 85 Jahre`)
library(forecast)


tsdata<- ts(dataFrame_0004_PsychVerhaltsst_m_80BisUnter85, frequency = 1,start=c(2005,1))
plot (tsdata)


AutoArima_0004_PsychVerhaltsst_m_80BisUnter85<-auto.arima(tsdata)


'Prüfung durch Ljung-Box Test, ob man Prognose erstellen kann '
Box.test(AutoArima_0004_PsychVerhaltsst_m_80BisUnter85$residuals,type="Ljung-Box")

'
Ausgabe==>
Box-Ljung test

data:  AutoArima_0004_PsychVerhaltsst_m_80BisUnter85$residuals
X-squared = 0.056992, df = 1, p-value = 0.8113

==> Dh da der P-Wert >0.05 ist ==> White-Noise Prozess! Somit sind die Werte unabhängig voneinander
i.i.d. und die Forecast auf diesem ARIMA-Modell sind stichhaltig !

Erst jetzt kann man die Prognose erstellen

'


forecast_0004_PsychVerhaltsst_m_80BisUnter85<-forecast(AutoArima_0004_PsychVerhaltsst_m_80BisUnter85,h=12)

plot(forecast_0004_PsychVerhaltsst_m_80BisUnter85)
plot(forecast_0004_PsychVerhaltsst_m_80BisUnter85,xlim=c(2015,2024))


'hat geklappt-  werte steigen'

summary(forecast_0004_PsychVerhaltsst_m_80BisUnter85)

#------------------------------------------------------------------------------------------------------
#0004_PsychVerhaltsst_m_85UndMehr

library(readxl)
data0004_PsychVerhaltsst_m_85UndMehr<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsst_m_85UndMehr<-data.frame(data0004_PsychVerhaltsst_m_85UndMehr$`männlich - 85 Jahre und mehr`)
library(forecast)

tsdata<- ts(dataFrame_0004_PsychVerhaltsst_m_85UndMehr, frequency = 1,start=c(2005,1))
plot (tsdata)


AutoArima_0004_PsychVerhaltsst_m_85UndMehr<-auto.arima(tsdata)

'Prüfung durch Ljung-Box Test, ob man Prognose erstellen kann '
Box.test(AutoArima_0004_PsychVerhaltsst_m_85UndMehr$residuals,type="Ljung-Box")
'
Ausgabe==>
Box-Ljung test

data:  AutoArima_0004_PsychVerhaltsst_m_85UndMehr$residuals
X-squared = 1.4698, df = 1, p-value = 0.2254

==> Dh da der P-Wert >0.05 ist ==> White-Noise Prozess! Somit sind die Werte unabhängig voneinander
i.i.d. und die Forecast auf diesem ARIMA-Modell sind stichhaltig !

Erst jetzt kann man die Prognose erstellen

'

forecast_0004_PsychVerhaltsst_m_85UndMehr<-forecast(AutoArima_0004_PsychVerhaltsst_m_85UndMehr,h=12)

plot(forecast_0004_PsychVerhaltsst_m_85UndMehr)
plot(forecast_0004_PsychVerhaltsst_m_85UndMehr,xlim=c(2015,2024))

'hat geklappt-  werte steigen'

summary(forecast_0004_PsychVerhaltsst_m_85UndMehr)

#------------------------------------------------------------------------------------------------------
#0004_PsychVerhaltsst_w_65BisUnter70

library(readxl)
data0004_PsychVerhaltsst_w_65BisUnter70<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsst_w_65BisUnter70<-data.frame(data0004_PsychVerhaltsst_w_65BisUnter70$`weiblich - 65 bis unter 70 Jahre`)
library(forecast)

tsdata<- ts(dataFrame_0004_PsychVerhaltsst_w_65BisUnter70, frequency = 1,start=c(2005,1))
plot (tsdata)


AutoArima_0004_PsychVerhaltsst_w_65BisUnter70<-auto.arima(tsdata)

'Prüfung durch Ljung-Box Test, ob man Prognose erstellen kann '
Box.test(AutoArima_0004_PsychVerhaltsst_w_65BisUnter70$residuals,type="Ljung-Box")

'
Ausgabe==>
Box-Ljung test

data:  AutoArima_0004_PsychVerhaltsst_w_65BisUnter70$residuals
X-squared = 0.88622, df = 1, p-value = 0.3465

==> Dh da der P-Wert >0.05 ist ==> White-Noise Prozess! Somit sind die Werte unabhängig voneinander
i.i.d. und die Forecast auf diesem ARIMA-Modell sind stichhaltig !

Erst jetzt kann man die Prognose erstellen

'

forecast_0004_PsychVerhaltsst_w_65BisUnter70<-forecast(AutoArima_0004_PsychVerhaltsst_w_65BisUnter70,h=12)

plot(forecast_0004_PsychVerhaltsst_w_65BisUnter70)
plot(forecast_0004_PsychVerhaltsst_w_65BisUnter70,xlim=c(2015,2024))

'hat geklappt-  werte steigen'

summary(forecast_0004_PsychVerhaltsst_w_65BisUnter70)

'
MAPE = 8.218079

'

#------------------------------------------------------------------------------------------------------
#0004_PsychVerhaltsst_w_70BisUnter75

library(readxl)
data0004_PsychVerhaltsst_w_70BisUnter75<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsst_w_70BisUnter75<-data.frame(data0004_PsychVerhaltsst_w_70BisUnter75$`weiblich - 70 bis unter 75 Jahre`)
library(forecast)

tsdata<- ts(dataFrame_0004_PsychVerhaltsst_w_70BisUnter75, frequency = 1,start=c(2005,1))
plot (tsdata)


AutoArima_0004_PsychVerhaltsst_w_70BisUnter75<-auto.arima(tsdata)

'Prüfung durch Ljung-Box Test, ob man Prognose erstellen kann '
Box.test(AutoArima_0004_PsychVerhaltsst_w_70BisUnter75$residuals,type="Ljung-Box")
'
Ausgabe==>
Box-Ljung test

data:  AutoArima_0004_PsychVerhaltsst_w_70BisUnter75$residuals
X-squared = 1.9528, df = 1, p-value = 0.1623

==> Dh da der P-Wert >0.05 ist ==> White-Noise Prozess! Somit sind die Werte unabhängig voneinander
i.i.d. und die Forecast auf diesem ARIMA-Modell sind stichhaltig !

Erst jetzt kann man die Prognose erstellen

'

forecast_0004_PsychVerhaltsst_w_70BisUnter75<-forecast(AutoArima_0004_PsychVerhaltsst_w_70BisUnter75,h=12)

plot(forecast_0004_PsychVerhaltsst_w_70BisUnter75)
plot(forecast_0004_PsychVerhaltsst_w_70BisUnter75,xlim=c(2015,2024))

'hat geklappt-  werte steigen'

summary(forecast_0004_PsychVerhaltsst_w_70BisUnter75)

'
MAPE = 6.916871

'

#------------------------------------------------------------------------------------------------------
#0004_PsychVerhaltsst_w_75BisUnter80

library(readxl)
data0004_PsychVerhaltsst_w_75BisUnter80<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsst_w_75BisUnter80<-data.frame(data0004_PsychVerhaltsst_w_75BisUnter80$`weiblich - 75 bis unter 80 Jahre`)
library(forecast)

tsdata<- ts(dataFrame_0004_PsychVerhaltsst_w_75BisUnter80, frequency = 1,start=c(2005,1))
plot (tsdata)


AutoArima_0004_PsychVerhaltsst_w_75BisUnter80<-auto.arima(tsdata)

'Prüfung durch Ljung-Box Test, ob man Prognose erstellen kann '
Box.test(AutoArima_0004_PsychVerhaltsst_w_75BisUnter80$residuals,type="Ljung-Box")

'
Ausgabe==>
Box-Ljung test

data:  AutoArima_0004_PsychVerhaltsst_w_75BisUnter80$residuals
X-squared = 0.062696, df = 1, p-value = 0.8023

==> Dh da der P-Wert >0.05 ist ==> White-Noise Prozess! Somit sind die Werte unabhängig voneinander
i.i.d. und die Forecast auf diesem ARIMA-Modell sind stichhaltig !

Erst jetzt kann man die Prognose erstellen

'

forecast_0004_PsychVerhaltsst_w_75BisUnter80<-forecast(AutoArima_0004_PsychVerhaltsst_w_75BisUnter80,h=12)

plot(forecast_0004_PsychVerhaltsst_w_75BisUnter80)
plot(forecast_0004_PsychVerhaltsst_w_75BisUnter80,xlim=c(2005,2024))

'hat geklappt-  werte steigen'

summary(forecast_0004_PsychVerhaltsst_w_75BisUnter80)

'
MAPE = 7.798129

'

#------------------------------------------------------------------------------------------------------
#0004_PsychVerhaltsst_w_80BisUnter85

library(readxl)
data0004_PsychVerhaltsst_w_80BisUnter85<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsst_w_80BisUnter85<-data.frame(data0004_PsychVerhaltsst_w_80BisUnter85$`weiblich - 80 bis unter 85 Jahre`)
library(forecast)

tsdata<- ts(dataFrame_0004_PsychVerhaltsst_w_80BisUnter85, frequency = 1,start=c(2005,1))
plot (tsdata)


AutoArima_0004_PsychVerhaltsst_w_80BisUnter85<-auto.arima(tsdata)

'Prüfung durch Ljung-Box Test, ob man Prognose erstellen kann '
Box.test(AutoArima_0004_PsychVerhaltsst_w_80BisUnter85$residuals,type="Ljung-Box")

'
Ausgabe==>
Box-Ljung test

data:  AutoArima_0004_PsychVerhaltsst_w_80BisUnter85$residuals
X-squared = 0.0037256, df = 1, p-value = 0.9513

==> Dh da der P-Wert >0.05 ist ==> White-Noise Prozess! Somit sind die Werte unabhängig voneinander
i.i.d. und die Forecast auf diesem ARIMA-Modell sind stichhaltig !

Erst jetzt kann man die Prognose erstellen

'


forecast_0004_PsychVerhaltsst_w_80BisUnter85<-forecast(AutoArima_0004_PsychVerhaltsst_w_80BisUnter85,h=12)

plot(forecast_0004_PsychVerhaltsst_w_80BisUnter85)
plot(forecast_0004_PsychVerhaltsst_w_80BisUnter85,xlim=c(2015,2024))

'hat geklappt-  werte steigen'

summary(forecast_0004_PsychVerhaltsst_w_80BisUnter85)

'
MAPE = 5.951762

'

#------------------------------------------------------------------------------------------------------
#0004_PsychVerhaltsst_w_85UndMehr


library(readxl)
data0004_PsychVerhaltsst_w_85UndMehr<-read_excel(file.choose(), na="NA")

dataFrame_0004_PsychVerhaltsst_w_85UndMehr<-data.frame(data0004_PsychVerhaltsst_w_85UndMehr$`weiblich - 85 Jahre und mehr`)
library(forecast)



tsdata<- ts(dataFrame_0004_PsychVerhaltsst_w_85UndMehr, frequency = 1,start=c(2005,1))

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++TEST CODE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

'anderer code'
tsdata<- ts(dataFrame_0004_PsychVerhaltsst_w_85UndMehr, frequency = 2,start=2005)
plot (tsdata)

'
==> ich muss die frequence=2 also auf mindestens 2 setzen, da ich sonst die nachfolgende 
Dekomposition nicht nutzen kann! 

'
help(stl)
stl(tsdata)

'ausgabe um welches objekt es sich handelt! '
class(tsdata)

'gibt durchschnitt und andere standardwerte vom datensatz wieder    '
summary(tsdata)

str(tsdata)

'Zeitreihendekomposition  '
decompose(tsdata,"additive")

'alle Zeitreihenkomponenten werden ausgegeben  '
plot(decompose(tsdata,"additive"))


'anderen decompose befehl nutzen der für jährliche daten nutzbar ist'
install.packages("mFilter")
library(mFilter)
filtered<-mFilter(tsdata,filter="HP")

print(filtered)
plot(filtered)
summary(filtered)

'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++ ENDE TEST CODE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'


AutoArima_0004_PsychVerhaltsst_w_85UndMehr<-auto.arima(tsdata)

'test'
acf(residuals(AutoArima_0004_PsychVerhaltsst_w_85UndMehr))

Box.test(AutoArima_0004_PsychVerhaltsst_w_85UndMehr$residuals,type="Ljung-Box")
'
==> Ausgabe:
Box-Ljung test

data:  AutoArima_0004_PsychVerhaltsst_w_85UndMehr$residuals
X-squared = 1.2162, df = 1, p-value = 0.2701

==> Dh da der P-Wert >0.05 ist ==> White-Noise Prozess! Somit sind die Werte unabhängig voneinander
i.i.d. und die Forecast auf diesem ARIMA-Modell sind stichhaltig !

Erst jetzt kann man die Prognose erstellen
'


forecast_0004_PsychVerhaltsst_w_85UndMehr<-forecast(AutoArima_0004_PsychVerhaltsst_w_85UndMehr,h=12)

'test

test<-auto.arima(tsdata, seasonal=TRUE)
test2<-forecast(test,h=12)
plot(test2)
'

forecast_0004_PsychVerhaltsst_w_85UndMehr<-forecast(AutoArima_0004_PsychVerhaltsst_w_85UndMehr,h=12)


plot(forecast_0004_PsychVerhaltsst_w_85UndMehr)
plot(forecast_0004_PsychVerhaltsst_w_85UndMehr,xlim=c(2015,2024))


'hat geklappt-  werte steigen'

summary(forecast_0004_PsychVerhaltsst_w_85UndMehr)

'
MAPE =

'

#------------------------------------------------------------------------------------------------------


















######################################################################
#Aufgabe 2- Analyse Challenger Katastrophe

Temp <- c(66, 70, 69, 68, 67, 72, 73, 70, 57, 63, 70, 78, 67, 
          53, 67, 75, 70, 81, 76, 79, 75, 76, 58)
Failure <- c(0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 
             0, 0, 0, 0, 1, 0, 1)
#a)
plot(Temp,Failure)
stripchart(Temp~Failure,method="stack")
#Anmerkung: Mit niedrigen Temperatur sind alle Beobchtungen  der Ausfallen 

#b)
table(Failure)

prop.table(table(Failure))

#c)
Gruppen<-cut(Temp,c(52,57,62,67,72,77,82),right=F)
table(Failure,Gruppen)

#d)
prop.table(table(Failure,Gruppen),2)
round(prop.table(table(Failure,Gruppen),2),2)
pihat<-round(prop.table(table(Failure,Gruppen),2),2)[2,]
pihat

#e)
Gruppenmittel<-c(54.5,59.5,64.5,69.5,74.5,79.5)

#f)
plot(Gruppenmittel,pihat)

#Aufgabe 3
#a)
pihat2<-pihat
pihat2[1:2]<-0.99
pihat2[6]<-0.01
pihat2
points(Gruppenmittel,pihat2,pch=3)

#b)
logits<-log(pihat2/(1-pihat2))
logits
#c)
plot(Gruppenmittel,logits)

#d)
lm.obj<-lm(logits~Gruppenmittel)
abline(lm.obj)
summary(lm.obj)
#e)
Prognose<-function(t) exp(25.4670-0.3758*t)/(1+exp(25.4670-0.3758*t))
#f)
Prognose(61)
#g)
Prognose(36)

#h)
plot(Gruppenmittel,pihat2)
curve(Prognose,add=T)

##zwei neben einander
par(mfrow=c(1,2))
plot(Gruppenmittel,logits)
abline(lm.obj)
plot(Gruppenmittel,pihat2)
curve(Prognose,add=T)
par(mfrow=c(1,1))


#############################
#Aufgabe 4
glm.obj<-glm(Failure~Temp,family=binomial)
summary(glm.obj)
Prognose2<-function(t) exp(15.0429-0.2322*t)/(1+exp(15.0429-0.2322*t))
plot(jitter(Temp),Failure)
curve(Prognose,add=T)
curve(Prognose2,add=T,col=2)


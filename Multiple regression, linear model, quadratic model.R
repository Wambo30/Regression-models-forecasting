#####
#lineares Modell, quadratisches Modell


# Daten laden
###Sie müssen den DatenPfad aendern
load("Daten/multReg.RData")


#########
#Erstes Diagramm
pairs(Eingangstest,lower.panel=panel.smooth) ##Geht auch mit plot, weil Eingangstest data.frame ist



#######
# Erster Blick: lineare mehrfache Regression 
fit<-lm(Abschlussnote~Mathetest+Sprachtest, data=Eingangstest)
fit


summary(fit)


######################################
## Abschluss gegen Mathe fuer linear Modell
plot(Abschlussnote~Mathetest,data=Eingangstest)
abline(lm(Abschlussnote~Mathetest,data=Eingangstest)$coefficient)


######################################
## Abschluss gegen Mathe & Sprachtest fuer linear Modell
opar<-par(mfrow=c(1,2))
plot(Abschlussnote~Mathetest,data=Eingangstest)
abline(lm(Abschlussnote~Mathetest,data=Eingangstest)$coefficient)
plot(Abschlussnote~Sprachtest,data=Eingangstest,xlim=c(40,100))
abline(lm(Abschlussnote~Sprachtest,data=Eingangstest)$coefficient)
par(opar)


#######Erstes Loess-plot
grid<-40:100
loess.m<-loess(Abschlussnote~Mathetest,data=Eingangstest)
loess.s<-loess(Abschlussnote~Sprachtest,data=Eingangstest)
p.m<-predict(loess.m, newdata=data.frame(Mathetest=grid))
p.s<-predict(loess.s, newdata=data.frame(Sprachtest=grid))

opar<-par(mfrow=c(1,2))
plot(Abschlussnote~Mathetest,data=Eingangstest)
lines(grid,p.m,col=4) #loess

plot(Abschlussnote~Sprachtest,data=Eingangstest)
lines(grid,p.s,col=4)
par(opar)


###################################################
#quadratisches Modell
fit.quad<-update(fit,.~.+I(Mathetest^2)+I(Sprachtest^2)+I(Mathetest*Sprachtest))


summary(fit.quad)


model.compare<-anova(fit,fit.quad)
model.compare


###################################################
#Max fitted value (Tafel)
###################################################
beta<-coef(fit.quad)
beta
ms<-solve(matrix(c(2*beta[4],beta[6],beta[6],2*beta[5]),2,2,byrow=T),
          -beta[2:3])
round(ms,1)
fmax<-predict(fit.quad,
              newdata=data.frame(Mathetest=ms[1],Sprachtest=ms[2]))
fmax


##### Contour plot
new<-expand.grid(Mathetest=40:100,Sprachtest=40:100)
pred.quad<-predict(fit.quad,newdata=new)
par(opar)
plot(Sprachtest~Mathetest,data=Eingangstest,type="n")
m<-40:100
contour(m,m,matrix(pred.quad,nrow=61),col=gray(.3),add=T) 
points(Sprachtest~Mathetest,data=Eingangstest,
       col = c(gray(.7),"black")[1.5+.5*sign(fit.quad$residuals)],pch=19)
#Schwarz bezeichnet positive residuen und grau negative Residuen

text(Eingangstest[,1],Eingangstest[,2],Eingangstest[,3],pos=4,cex=.75,
     col = 4)
points(ms[1],ms[2],col=2,cex=2)
legend(37,64,pch=19,col=c(gray(.7),"black"),legend=c("-ve Resid","+ve Resid"))

###################################################
### Perspektiv plot
###################################################
persp(m,m,matrix(pred.quad,nrow=61),xlab="Mathetest",ylab="Sprachtest",zlab="Abschlussnote",
      theta = -30, phi = 15,col=gray(.9),lty=3,zlim=c(1.2,4))->res
points(trans3d(Eingangstest[,1],Eingangstest[,2],Eingangstest[,3],pmat=res), 
       col = c(gray(.7),"black")[1.5+.5*sign(fit.quad$residuals)], pch =16)
#par(opar) 


###################################################
### Diagnostik Diagramme 
opar<-par(mfrow=c(2,2))
plot(fit.quad,col=4)
par(opar)

###################################################



#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2019/2020
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
### dott. Claudio  Zandonella Callegher (claudio.zandonellacallegher@phd.unipd.it)
#######################################################

## CONTENUTO DEL CODICE ################################
# (A) Esplorazione dei dati
# (B) Stima di eta e della precisione
# (C) Misurazione indiretta e propagazione dell'errore
########################################################

# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2019_2020/testing_psicologico/")


# Caricamento dei dati ----------------------------------------------------
load("laboratorio/datalab2.rda")
str(datax)
summary(datax)
n = dim(datax)[1]; t = dim(datax)[2]


# (A) Esplorazione dei dati ---------------------------------------------------

## istogramma delle misurazioni
par(mfrow=c(2,4))
for(j in 1:t){
  hist(datax[,j],xlab = "measure",main=paste("time: ",j,sep=""))
  abline(v = mean(apply(datax,2,mean)),col=2,lty=3,lwd=2) #media complessiva
}

## boxplot appaiato delle misurazioni
datax.long = reshape2::melt(datax) #per trasformare il dataset fa "wide" a "long"
datax.long$variable = as.factor(datax.long$variable)
str(datax.long)

par(mfrow=c(1,1))
boxplot(datax.long$value~datax.long$variable,xlab="time",ylab="measure",main="misure nel tempo")
abline(h=mean(datax.long$value),col=2,lty=3,lwd=2) #media complessiva
abline(h=mean(datax.long$value[datax.long$variable %in% c("X4","X5","X6","X7","X8")]),col=3,lty=4,lwd=2) #media escludendo X1-X3
legend("topleft", legend=c("overall mean", "X4-X8 mean"),col=c(2,3), lty=c(3,4), cex=0.8)

## istogrammi delle misurazioni confrontandole con la media delle ultime cinque misurazioni
datax.mean = mean(datax.long$value[datax.long$variable %in% c("X4","X5","X6","X7","X8")])
par(mfrow=c(2,4))
for(j in 1:t){
  hist(datax[,j],xlab = "measure",main=paste("time: ",j,sep=""),xlim=c(0,30))
  abline(v = datax.mean,col=2,lty=3,lwd=2)
}

## istogramma è un modello parametrico ed il parametro è il numero di bin/breaks: al variare del parametro cambia il risultato ottenuto
par(mfrow=c(2,2))
hist(datax[,1],breaks = 10)
hist(datax[,1],breaks = 25)
hist(datax[,1],breaks = 75)
hist(datax[,1],breaks = 150)

## Tre modi per calcolare il numero di bin/breaks
par(mfrow=c(1,3))
hist(datax[,1],breaks = 1+log2(n),main="Bins con Sturges")
hist(datax[,1],breaks = 2*n^(1/3),main="Bins con Rice")
hist(datax[,1],breaks = sqrt(n),main="Bins con Radice quadrata")


# (B) Stima di eta e della precisione -----------------------------------------

## usiamo la funzione apply() per fare i calcoli colonna per colonna
datax.means = apply(datax,2,mean) #media
datax.sds = apply(datax,2,sd) #deviazione standard
datax.deltas = apply(datax,2,function(x)max(x)-min(x)) #campo di variazione
datax.cvs = apply(datax,2,function(x)sd(x)/abs(mean(x))) #coeff di variazione
datax.snrs = apply(datax,2,function(x)mean(x)/sd(x)) #rapporto segnale-rumore
datax.means.err = apply(datax,2,function(x)sd(x)/length(x)) #errore standard della media
datax.mean = mean(datax.means)

## aggreghiamo i risultati in un dataframe unico
datax.summary = cbind(datax.means,datax.sds,datax.deltas,datax.cvs,datax.snrs,datax.means.err)
colnames(datax.summary) = c("eta","sd.x","delta.x","cv.x","snr.x","sd.eta")
print(datax.summary)

## rappresentiamo medie, intervalli di incertezza, e rapporto segnale rumore
par(mfrow=c(1,1))
plot(datax.means,col=1,lwd=1,pch=16,ylim=c(2.5,22.5),xlab="time",ylab="measures") #medie
abline(h = datax.mean,col=2,lty=3,lwd=1) #media delle medie (vero valore del misurando?)
points(datax.means,col=8,lwd=sqrt(datax.snrs),pch=1) #rapporto segnale-rumore 
arrows(1:8, datax.means-datax.sds, 1:8, datax.means+datax.sds, length=0.05, angle=90, code=3) #intervalli di incertezza



# (C) Misurazione indiretta e propagazione dell'errore ------------------------

## caso1: Y = X1+X2+X3 (assumendo no correlazione tra le misurazioni)
cor(datax[,c(1,2,3)]) # no correlazione, procediamo con il calcolo per misure indipendenti 
eta.y1 = mean(apply(datax[,c(1,2,3)],2,mean)) # stima di eta (misurando)
sd.eta1 = sqrt(sum(datax.sds[c(1,2,3)]^2))  # incertezza di eta

## caso 2: Y = X4+X5+X6 
cor(datax[,c(4,5,6)]) # no correlazione, procediamo con il calcolo per misure indipendenti 
eta.y2 = mean(apply(datax[,c(4,5,6)],2,mean))
sd.eta2 = sqrt(sum(datax.sds[c(4,5,6)]^2)) 

## rappresentiamo graficamente il caso 1 e il caso 2
plot(1:2,c(eta.y1,eta.y2),col=3,pch=16,ylim=c(5,20),xlim=c(0.5,2.5),ylab="Y",xlab="",frame.plot = FALSE)
abline(h = datax.mean,col=2,lty=3,lwd=1)
arrows(1, eta.y1-sd.eta1, 1, eta.y1+sd.eta1, length=0.05, angle=90, code=3)
arrows(2, eta.y2-sd.eta2, 2, eta.y2+sd.eta2, length=0.05, angle=90, code=3)

## utilizziamo ora la libreria propagate (..occorre averla installata prima)
library(propagate)
## caso 1: Y=X1+X2+X3
Y1 = propagate(expr = expression((X1+X2+X3)/3), data = datax[,1:3], do.sim = FALSE,cov = FALSE,second.order = FALSE)
summary(Y1) 
# do.sim = FALSE fa propagare l'incertezza per via numerica e non con il metodo Monte-Carlo
# cov = FALSE assume che le misurazioni siano a correlazione nulla
# ulteriori info tramite ?propagate
# propagate() restituisce in output diversi elementi, tra cui il coverage factor che è la quantità che permette di costruire l'intervallo [y-k*sd.y, y+k*sd.y], 
# il termine k*sd.y è chiamato "expanded uncertainty" e serve per costruire l'intervallo di confidenza del misurando y

## caso 2: Y=X4+X5+X6
Y2 = propagate(expr = expression((X4+X5+X6)/3), data = datax[,4:6], do.sim = FALSE,cov = FALSE,second.order = FALSE)
summary(Y2)

## caso 2: Y=X4+X5+X6 (includendo la covarianza tra le misurazioni)
Y2.cor = propagate(expr = expression((X4+X5+X6)/3), data = datax[,4:6], do.sim = FALSE,cov = TRUE,second.order = TRUE)
summary(Y2.cor) 
# cambia leggermente la stima di eta.sd quando si utilizza il termine di covarianza tra le misurazioni

## caso 3: Y = (X4+X5)/log(X6) con X6>0
Y3 = propagate(expr = expression((X4+X5)/log(X6)),data = datax[,4:6],second.order = FALSE,do.sim = FALSE,cov = FALSE)
summary(Y3)

## caso 4: misurazioni in input altamente correlate
X = cbind(datax[,1],datax[,1]*0.45+datax[,2]); colnames(X) = c("X1","X2") # creiamo due input altamente correlati
plot(X[,1],X[,2]); cor(X) # grafico a dispersione e calcolo della correlazione

# calcoliamo "a mano" (usando le formule per assenza di correlazione)
eta.x = mean(apply(X,1,sum))
sd.eta.x = sqrt(sum(apply(X,2,sd)^2)) 

# calcoliamo usando propagate() ed includendo la covarianza tra le misurazioni (come è giusto fare in questo caso)
Y4.cor = propagate(expr = expression(X1+X2),data = X,second.order = FALSE,do.sim = FALSE,cov = TRUE)
summary(Y4.cor) 
Y4.cor$prop

# ripetiamo il calcolo "a mano" questa volta con propagate (cov=FALSE)
Y4 = propagate(expr = expression(X1+X2),data = X,second.order = FALSE,do.sim = FALSE,cov = FALSE)
summary(Y4)
Y4$prop
# quali differenze emergono tra i risultati della propagazione includendo/escludendo il parametro di covarianza tra le misurazioni?
# come sono gli intervalli di confidenza (expanded uncertainty) tra i due metodi?

## caso 5: media delle medie (usando dati aggregati in input)
Y5 = propagate(expr = expression((X1+X2+X3+X4+X5+X6+X7+X8)/8),data = rbind(datax.means,datax.sds),do.sim = FALSE,cov = FALSE,second.order = FALSE)
summary(Y5)
Y5$prop
mean(apply(datax,2,mean))



     
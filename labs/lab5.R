#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2019/2020
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
### dott. Claudio  Zandonella Callegher (claudio.zandonellacallegher@phd.unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) Stima delle quantità della TCT
# (B) Stime della TCT e coerenza interna degli items
# (C) Aggiungere items ad una scala
# (D) Utilizzo delle quantità stimate E[T]
##########################################################


# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2019_2020/testing_psicologico/")
library(psych)
source(file = "laboratorio/utilities_TCT.R")


#  (A) Stima delle quantità della TCT -------------------------------------

## ESEMPIO DI LORD & NOVICK (1968) #####################################################################
# Esempio tratto da Lord & Novick (1968), pp.156-157, cap.7
# stesso esempio nelle slide 47-53 del modulo C

# Dati: 10 persone, 1 solo item (o misura), 2 misurazioni parallele (repliche) per l'item
N=10 #numero di persone
r=2 #repliche per ogni item 

datax = matrix(cbind(c(125,119,109,104,101,98,97,94,90,81), #dati dell'esempio
                     c(120,122,107,108,98,106,96,99,93,87)),
               nrow = r,ncol = N,byrow = TRUE)

# Stima della varianza di errore VAR(E) 
VAR.E = compute_VAR.E(datax)

## Stima della varianza del punteggio vero VAR(T)
VAR.T = compute_VAR.T(datax)

## stima dell'attendibilità
rho2_yt = VAR.T/(VAR.T+VAR.E)

## stima della varianza del punteggio osservato VAR(X)
VAR.X = apply(datax,1,var)

## stima del punteggio vero E(T) 
x = apply(datax,2,mean)
mu.x = mean(x)
E.T = rho2_yt*x + (1-rho2_yt)*mu.x

########################################################################################################


## ESEMPIO CON DATI SIMULATI ###########################################################################
# Costruiamo un insieme di misure parallele (aventi media e varianza uguale) che definiscono un certo
# misurando (o costrutto latente). Per costruzione, ogni item (misura parallela) sarà campionata dalla
# medesima legge di probabilità x ~ N(mu=mu0, sigma=sqrt(sigma0)).

set.seed(12341)
mu0=100; sigma0=12.25
n=25 #numero di individui (unità statistiche)
p=4 #numero di misure parallele
X = matrix(data=NA,nrow = p,ncol = n) #matrice iniziale da popolare con le misurazioni
for(j in 1:p){
  X[j,] = rnorm(n = n,mean = mu0,sd = sqrt(sigma0))
}
X = data.frame(X)
head(X)

summary(X)


# Gli items sono generati secondo la stessa legge di probabilità e con i medesimi parametri. Per costruzione, dunque, gli items sono da considerarsi paralleli.
# Per stimare le quantità di interesse E[T], VAR[T], VAR[E], utilizziamo le formule viste per le misure con repliche parallele.

# Stima di VAR[E]
VAR.E = compute_VAR.E(X)

# Stima di VAR[T]
VAR.T = compute_VAR.T(X)

## stima dell'attendibilità
rho2_yt = VAR.T/(VAR.T+VAR.E)

## stima del punteggio vero E(T) 
x = apply(X,2,mean)
mu.x = mean(x)
E.T = rho2_yt*x + (1-rho2_yt)*mu.x

x11()
plot(x,bty="n",ylab="punteggi medi osservati",xlab="soggetti",cex=3,ylim=c(min(x)-1,max(x)+1))
points(E.T,col=2,pch=2,cex=3); abline(h = mu.x,lty=2,col=1)
legend("topright",legend = c("osservati","veri"),col = c(1,2),pch=c(1,2),bty = "n")

# Notiamo come i punteggi veri per gli n soggetti sono molto simili tra loro e presentano poca fluttuazione rispetto ai valori osservati. Ciò è dovuto alla bassa
# precisione (rho2_yt) del test nel misurare la quantità di interesse. Inoltre notiamo anche come VAR.E > VAR.T
# Difatti, la correlazione tra le misure parallele è (per come abbiamo costruito gli indicatori x) piuttosto bassa:
cor(t(X)) #t(.) traspone la matrice dei dati da AxB in BxA

# Proviamo ad aumentare la correlazione tra gli item, cioè tra le variabili osservate. Per far ciò, generiamo le osservate usando un unico modello 
# X ~ N(mu=mu0,sigma=Sigma0) dove Sigma0 è la matrice di covarianza tra le variabili mentre mu0 è il vettore di medie per ciascuna osservabile.
# Usiamo dunque un modello normale multivariato (prima avevamo usato un modello normale univariato) che tenga conto della covarianza tra le osservabili.

n=25 #numero di individui (unità statistiche)
p=4 #numero di misure parallele

# Per generare Sigma0 generiamo una matrice di correlazione in modo che presenti una correlazione tra le osservabili medio/alta.
# Usiamo la funzione rcorrmatrix() del pacchetto clusterGeneration fissando il parametro della funzione ad un valore vicino allo zero.
# Maggiori info: ?rcorrmatrix
set.seed(123211111)

# Usiamo la funzione compute_Sigma0(.) per generare una matrice di correlazione
# p: numero di variabili (items)
# sigmap: varianze delle singole variabili
# alphad: grado di correlazione in [0,Inf] (valori prossimi a zero indicano assenza di correlazione)
Sigma0 = generate_Sigma0(p = p,sigmap = 3,alphad = 1e-09)

# Fissiamo successivamente le medie delle singole osservazioni
mu0=rep(0,p)

# Generiamo un insieme di dati secondo il modello multivariato normale
X = mvtnorm::rmvnorm(n = n,mean = mu0,sigma = Sigma0)
X = t(X) #trasponiamo il nostro dataset: da nxp a pxn

# Procediamo ora a stimare le quantità di interesse della TCT

# Stima di VAR[E]
VAR.E = compute_VAR.E(X)

# Stima di VAR[T]
VAR.T = compute_VAR.T(X)

## stima dell'attendibilità
rho2_yt = VAR.T/(VAR.T+VAR.E)

## stima del punteggio vero E(T) 
x = apply(X,2,mean)
mu.x = mean(x)
E.T = rho2_yt*x + (1-rho2_yt)*mu.x

x11()
plot(x,bty="n",ylab="punteggi medi osservati",xlab="soggetti",cex=3,ylim=c(min(x)-1,max(x)+1))
points(E.T,col=2,pch=2,cex=3); abline(h = mu.x,lty=2,col=1)
legend("topright",legend = c("osservati","veri"),col = c(1,2),pch=c(1,2),bty = "n")

# A differenza dei dati precedenti, possiamo notare come in questo caso VAR[T] > VAR[E] unitamente al fatto che la precisione del test rho2_yt è abbastanza alta.
# Conseguentemente, i punteggi stimati tau presentano una maggiore fluttuazione intorno alla loro media. 


#  (B) Stime della TCT e coerenza interna degli items ---------------------
# Abbiamo dunque compreso come la stima delle quantità della TCT sono in stretta relazione alla 'coerenza interna' delle osservabili,
# ossia alla struttura di correlazione che intercorre tra le variabili (items).
# Effettuiamo ora un piccolo studio di simulazione per comprendere al meglio tale relazione dove facciamo variare la coerenza interna 
# degli items (da alta coerenza a coerenza nulla).

n=25 #numero di individui (unità statistiche)
p=4 #numero di misure parallele

# Generiamo una matrice di covarianza da usare poi per generare le variabili X~N(mu0,Sigma0)
set.seed(123211111) #fissiamo il seme di generazione dei numeri casuali
Sigma0 = generate_Sigma0(p = p,sigmap = 3,alphad = 1e-09) #cov matrix

R = cov2cor(Sigma0) #convertiamo la matrice di covarianza in matrice di correlazione
R = R + 0.10 #usiamo la matrice di correlazione precedente aumentando ciascun elemento di 0.20
diag(R) = 1 #imponiamo che la diagonale della matrice abbia 1

# Definiamo una sequenza di valori delta per diminuire la magnitudo della correlazione fino ad azzerarla
delta = seq(from=1,to = 0.1,by = -0.1)
delta = c(delta,1e-9) #aggiungiamo un valore finale per la coerenza nulla
K = length(delta)

# Fissiamo le medie delle singole variabili
mu0=rep(100,p)

# Creiamo la matrice dei risultati che verranno popolate durante la simulazione
Y = matrix(NA,K,3)

n=1000 #numero di individui (unità statistiche)
set.seed(122)
for(k in 1:K){
  R_delta = R*delta[k] #degradiamo la correlazione
  diag(R_delta) = 1
  Sigma0 = convert_R_to_Sigma(R_delta,sigma0) #calcoliamo la matrice di covarianza
  X = mvtnorm::rmvnorm(n = n,mean = mu0,sigma = Sigma0) #generiamo i dati/misurazioni
  
  Y[k,1] = compute_VAR.E(t(X)) #calcoliamo VAR[E]
  Y[k,2] = compute_VAR.T(t(X)) #calcoliamo VAR[T]
  Y[k,3] = Y[k,2]/(Y[k,1]+Y[k,2]) #calcoliamo rho2_yt
}
colnames(Y) = c("VarE","VarT","rho2")
head(Y)
h = min(which(Y[,1]>=Y[,2]))

x11(); par(mfrow=c(1,2))
plot(delta,Y[,1],type="l",ylim=c(0,6),bty="n",col=2,xlab="delta",ylab="varianze")
lines(delta,Y[,2],col=4); legend("bottomright",legend = c("Var[E]","Var[T]"),col = c(2,4),bty = "n",lty=1)
abline(v = delta[h],lty=2)
plot(delta,Y[,3],type="l",bty="n",col=1,xlab="delta",ylab="attendibilità",ylim=c(0.1,0.9))
abline(v = delta[h],lty=2)

# Quando la correlazione tra le osservabili è alta (alta coerenza interna tra gli items), la precisione del test (attendibilità) è alta
# e la componente di varianza d'errore VAR[E] è inferiore a VAR[T] (ad indicare che la variabilità delle misurazioni sono da attribuire al costrutto indagato).
# Al contrario, all'aumentare del fattore di degradazione delta (e dunque al diminuire della correlazione tra gli items, al diminuire della loro coerenza interna),
# la precisione diminuisce progressivamente mentre la VAR[E] aumenta. Quando delta < 0.2 si verifica che VAR[E] > VAR[T] e ciò indica che la variabilità dei dati non
# è dovuta al costrutto indagato ma all'errore di misurazione. In questo contesto specifico, ossia quello dell'analisi della coerenza interna degli items, VAR[E] > VAR[T]
# indica che gli items utilizzati (le variabili osservate) non sono idonee a quantificare il misurando latente. L'idoneità non è da intendersi in maniera assoluta ma
# sempre rispetto al criterio della coerenza interna. Quando VAR[E] > VAR[T] diremo quindi che la scala (ossia l'insieme degli items, delle variabili osservate) usata per
# quantificare/misurare il misurando latente non è idonea rispetto al criterio della coerenza interna. 



# (C) Aggiungere items ad una scala --------------------------------------
# Prendiamo una scala contenente un certo numero di items e studiamo il problema di determinare il numero di items da aggiungere per avere una certa precisione,
# un certo livello di attendibilità. 

# Prendiamo una delle matrici di correlazione utilizzate in precedenza e generiamo delle misurazioni
R = R*delta[5]
diag(R) = 1
Sigma0 = convert_R_to_Sigma(R,sigma0) #calcoliamo la matrice di covarianza
X = mvtnorm::rmvnorm(n = n,mean = mu0,sigma = Sigma0) #generiamo i dati/misurazioni

# Calcoliamo le quantità della TCT
VAR.E = compute_VAR.E(t(X))
VAR.T = compute_VAR.T(t(X))
rho2_yt = VAR.T/(VAR.T+VAR.E)

# Di quanto cambierebbero le varianze della misurazione se aggiungessimo m indicatori/items paralleli?
m=4 #aggiungiamo 4 items per un totale di m+p=8
VAR.T.m = VAR.T*m^2
VAR.E.m = VAR.E*m
rho2_yt.m = (m*rho2_yt)/(1+(m-1)*rho2_yt)
# L'aggiunta di m misure parallele alla scala precedente permetterebbe di aumentare considerevolmente VAR[T] così come, di conseguenza, la precisione del test rho2_yt

# Quante misure parallele m dovremmo aggiungere per portare il nostro test ad una precisione di 0.89?
# vedi BN(2.6), p.59
k = (0.89*(1-rho2_yt))/(rho2_yt*(1-0.89))
m = round(k*p) #p è il numero di item che compongono il test di base
# Per avere un test finale con attendibilità pari a 0.89 dovremmo avere un test complessivamente composto da m=15 items e, rispetto al test iniziale con p elementi, dovremmo
# aggiungerne altri (m - p) = 11.


# (D) Utilizzo delle quantità stimate E[T] -------------------------------
# vedi BN(2.8.0)

# Abbiamo visto nella sezione (A) come ottenere i valori veri tau = E[T] per ciascun soggetto a cui è stato somministrato un test. Ora possiamo determinare l'intervallo
# di confidenza ad un livello (1-alpha) fissato per il punteggio stesso, determinando così gli estremi di un intervallo tau_CI = [tau_lb, tau_ub] che conterranno con una probabilità
# (1-alpha) il punteggio vero del soggetto. Per le assunzioni della TCT (i)-(iii), possiamo ragionevolmente assumere che E ~ N(0,VAR[E]) e dunque utilizzare la legge normale
# per determinare gli estremi dell'intervallo tau_CI.
# Definizione: Prob[tau_i-z*sigma.e < tau_i < tau_i+z*sigma.e] = (1-alpha)
# nota: z è il quantile di riferimento (valore critico) della normale standardizzata che corrisponde al livello di probabilità alpha

alpha=0.05 #alpha è 5% 
z = qnorm(p = 1-alpha/2,mean = 0,sd = 1) #poiché la Normale è simmetrica prendiamo il quantile corrispondente a (1-alpha/2) 

# Consideriamo le misurazioni generate nella sezione precedente
iid = sample(1:NROW(X),25,replace=FALSE) #prendiamo solo 25 unità statistiche
X = X[iid,]
x = apply(X,1,mean) #punteggi medi osservati
tau = rho2_yt*x + (1-rho2_yt)*mean(x)

# Calcoliamo gli intervalli di confidenza
sigma.e = sqrt((VAR.T+VAR.E)*1-rho2_yt) #vedi risultato (viii) della TCT, slide 15, dove COR[Xj,Xj'] è stata sostituita da rho2_yt 
tau_CI = cbind(tau-z*sigma.e, tau+z*sigma.e)

# Visualizziamo tau e tau_CI
plot(tau,1:length(tau),bty="n",xlim=c(90,110),cex=1.25,pch=20,xlab="tau",ylab="soggetti")
points(tau_CI[,1],1:length(tau),col=1,pch=4)
points(tau_CI[,2],1:length(tau),col=1,pch=4)
segments(x0 = tau,x1 = tau_CI[,1],y0 = 1:length(tau),lty=2)
segments(x0 = tau,x1 = tau_CI[,2],y0 = 1:length(tau),lty=2)
abline(v = mean(x),lty=2)






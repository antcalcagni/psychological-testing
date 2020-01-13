#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2019/2020
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
### dott. Claudio  Zandonella Callegher (claudio.zandonellacallegher@phd.unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) Intro generale ai metodi Monte Carlo
# (B) Statistiche e metodi Monte Carlo
# (C) Propagazione dell'incertezza via Monte Carlo
##########################################################


# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2019_2020/testing_psicologico/")
set.seed(1234) #fissiamo il seme per il generatore di numeri casuali


# (A) Intro generale ai metodi Monte Carlo -----------------------------------------
# MC methods are a broad class of computational algorithms that rely on repeated random sampling to obtain numerical results
# ulteriori dettagli su: https://en.wikipedia.org/wiki/Monte_Carlo_method

# Supponiamo X ~ N(mu=100,sigma=25) e vogliamo calcolare P(X > 75)
x = seq(from=20,to=180,length.out = 100)
plot(x,dnorm(x = x,mean=100,sd = 25),type="l")
abline(v=100,lty=2); abline(v=75,lty=2,col=4) # X=75
# P(X > 75) è l'area a sinistra della linea blu tratteggiata

# Calcoliamo P(X>75) usando la funzione di ripartizione F(X)
p_75 = 1-pnorm(q = 75,mean = 100,sd = 25)
print(p_75)

# Calcoliamo P(X>75) come integrale
fx = function(x)dnorm(x,100,25) # funzione di densità di X, ossia f(X)
p_75_int = integrate(f = fx,lower = 75,upper = Inf)
print(p_75_int$value)

# Un metodo alternativo per calcolare P(X>75) è quello di ricorrere ai metodi Monte Carlo dove l'area di f(x) 
# corrispondente all'intervallo [75,+Inf) è ottenuta via simulazione
y = rnorm(n = 50,mean = 100,sd = 25) # generiamo numeri casuali secondo il modello X~N(mu=100,sigma=25) poiché conosciamo F(X) in questo caso
p_75_mc = sum(y > 75)/length(y) # sfruttiamo il fatto che l'integrale è approssimabile da somme discrete
print(p_75_mc) #risultato leggermente diverso da quello atteso 

# affiniamo il calcolo dell'integrale via somma aumentando il numero di osservazioni simulate
M=c(seq(from=10,to=100,by=25),1000,10000,100000)
px = rep(NA,length(M))
for(j in 1:length(M)){
  y = rnorm(n=M[j],mean = 100,sd = 25)
  px[j] = sum(y>75)/M[j]
}
print(round(cbind(M,px),3))
plot(log(M),px,ylim=c(0.60,1),xlab="ripetizioni",ylab="P(X>75)",bty="n",type="l"); 
abline(h=p_75,lty=2,col=2) # valore vero di P(X>75)
# quando M -> +Inf l'approssimazione Monte Carlo eguaglia il valore vero P(X>75)

# Facciamo un altro esempio dell'uso dei metodi MC per la risoluzione di integrali definiti
fx = function(x){exp(x)*sin(pi)+(1/sqrt(abs(sin(x))))} # ..una funzione a salti
x = seq(from=0.1,to=10,length.out = 100) # dominio reale dove valutare fx
plot(x,fx(x),type="l") # grafico di fx

# Calcoliamo l'integrale (per via numerica) di fx nell'intervallo [2,4]
fx_int = integrate(f = fx,lower = 2,upper = 4)
print(fx_int$value)

# Approssimiamo ora l'integrale via MC
a=2; b=4
y = runif(n = 10000,min = a,max = b) # generiamo tante osservazioni dalla legge X~Unif(a=0.1,b=10) nell'intervallo di integrazione di fx
fx_mc = (b-a)*mean(fx(y)) # usiamo la formula generale dell'integrazione MC
print(c(fx_mc,fx_int$value))

# Qualche dettaglio in più sull'integrazione via MC: https://www.datacamp.com/community/tutorials/tutorial-monte-carlo



# (B) Statistiche e metodi Monte Carlo ------------------------------------

## ESEMPIO 1 #############################################################################
# Altezza massima delle piante - fonte: P(12.7)
# Un agricoltore deve comprare n=50 paletti per sostenere le sue viti. Deve valutare quale altezza di paletti scegliere, in relazione questa 
# all'altezza futura X delle piante (altezza in cm che egli ritiene le piante raggiungeranno). Sulla base di dati storici, formalizza la sua esperienza secondo la
# legge X ~ N(mu=120,sigma=10). L'agricoltore fa uno studio di simulazione in cui valuta M scenari possibili di crescita delle viti.
# Formalmente, ciò che l'agricoltore deve sapere per comprare i paletti è la v.a. X_max = max(X_1,...,X_50). 

M=5000
x_max = rep(NA,M)
for(j in 1:M){
  print(paste("Simulazione no.:",j,sep=" "))
  x = rnorm(n = 50,mean = 120,sd = 10)
  x_max[j] = max(x)  
}
summary(x_max) # statistiche descrittive dei campioni x_max
par(mfrow=c(1,2))
hist(x_max,breaks = 30,xlab="",ylab="",main="max(X1,...,Xn)")
abline(v=mean(x_max),lty=2,col=2);abline(v=median(x_max),lty=2,col=3);abline(v=quantile(x_max,probs = c(0.05,0.95)),lty=2,col=4);
plot(density(x_max),bty="n",main="stima di f(X_max)",xlab="")

############################################################################################



# (C) Propagazione dell'incertezza via Monte Carlo ------------------------
# Riprendiamo quanto fatto nel modulo [A]. Nella misurazione indiretta Y = f(X1,..,Xn) il misurando Y è stimato secondo f(..) mentre
# l'incertezza è valutata secondo le regole di propagazione. Questo metodo tuttavia non ci fornisce informazioni su f(Y), ossia
# sulla sua funzione di densità (o ripartizione). 
library(propagate)
# Ritorniamo ad usare propagate(), questa volta usando l'opzione 'do.sim=TRUE'.
# Generiamo dei campioni di n misurazioni secondo X ~ Exp(x;lambda)

n=1000
# Campioniamo delle misure secondo il loro modello distributivo
x1 = rexp(n = 1000,rate = 1.2)
x2 = rexp(n = 1000,rate = 1.9)
x3 = rexp(n = 1000,rate = 2.1)
X = cbind(x1,x2,x3)
colnames(X) = c("X1","X2","X3")

fy = expression(X1+X2+X3) # semplice funzione di integrazione

# Avviamo propagate()
Y = propagate::propagate(expr = fy,data = X,do.sim = TRUE,nsim = 20000,cov = TRUE)
print(Y)
# propagate() ci restituisce i risultati sia via MC che calcolo analitico:
# notiamo delle differenze non nella stima della media di Y ma della stima dell'intervallo di confidenza del misurando. 
# Avere l'informazione di f(Y) ci permette di conoscere meglio il comportamento di Y come variabile aleatoria.

plot(Y) # otteniamo f(Y)

# Proviamo a modificare la funzione di legame f(..) ed avviamo propagate()
fy = expression((X1*log(X2))-X3)
Y = propagate::propagate(expr = fy,data = X,do.sim = TRUE,nsim = 20000,cov = TRUE)
print(Y)
plot(Y) # otteniamo f(Y)

# Immaginiamo di non avere lambda ma di sapere che questi parametri sono nell'intervallo [1.2,4.1]
lambda = runif(n=3,min = 1.2,max = 4.1) # ragionevole supporre che lambda ~ Unif(a,b) poiché conosciamo solo gli estremi dell'intervallo di appartenenza

x1 = rexp(n = 1000,rate = lambda[1])
x2 = rexp(n = 1000,rate = lambda[2])
x3 = rexp(n = 1000,rate = lambda[3])
X = cbind(x1,x2,x3); colnames(X) = c("X1","X2","X3")

fy = expression(X1+X2+X3)
Y = propagate::propagate(expr = fy,data = X,do.sim = TRUE,nsim = 20000,cov = TRUE)
print(Y)
plot(Y)
# Con un solo campione di lambda non possiamo valutare l'impatto che questi hanno su Y: facciamo uno studio di scenario

M=100 # numero di scenari da valutare
fy = expression(X1+X2+X3)
Y_out = matrix(NA,M,6)
lambda = matrix(NA,M,3)
for(j in 1:M){
  lambda[j,] = runif(n=3,min=1.2,max=4.1)
  x1 = rexp(n = 1000,rate = lambda[j,1])
  x2 = rexp(n = 1000,rate = lambda[j,2])
  x3 = rexp(n = 1000,rate = lambda[j,3])
  X = cbind(x1,x2,x3); colnames(X) = c("X1","X2","X3")
  Y = propagate::propagate(expr = fy,data = X,do.sim = TRUE,nsim = 20000,cov = TRUE)
  Y_out[j,] = Y$sim
}
colnames(Y_out) = names(Y$sim)
Y_out = data.frame(Y_out)
head(Y_out)

par(mfrow=c(3,3))
plot(lambda[,1],Y_out$Mean,bty="n",ylab="mean(Y)"); 
plot(lambda[,2],Y_out$Mean,bty="n",ylab="mean(Y)")
plot(lambda[,3],Y_out$Mean,bty="n",ylab="mean(Y)")

plot(lambda[,1],Y_out$sd,bty="n",ylab="sd(Y)")
plot(lambda[,2],Y_out$sd,bty="n",ylab="sd(Y)")
plot(lambda[,3],Y_out$sd,bty="n",ylab="sd(Y)")

plot(lambda[,1],Y_out$X97.5.-Y_out$X2.5.,bty="n",ylab="cover(Y)")
plot(lambda[,2],Y_out$X97.5.-Y_out$X2.5.,bty="n",ylab="cover(Y)")
plot(lambda[,3],Y_out$X97.5.-Y_out$X2.5.,bty="n",ylab="cover(Y)")

par(mfrow=c(1,3))
hist(lambda[,1]); hist(lambda[,2]); hist(lambda[,3])

# Facciamo ora variare lambda secondo la legge lambda ~ Chisq(x;10)
M=100 # numero di scenari da valutare
fy = expression(X1+X2+X3)
Y_out = matrix(NA,M,6)
lambda = matrix(NA,M,3)
for(j in 1:M){
  lambda[j,] = rchisq(n=3,df = 10)
  x1 = rexp(n = 1000,rate = lambda[j,1])
  x2 = rexp(n = 1000,rate = lambda[j,2])
  x3 = rexp(n = 1000,rate = lambda[j,3])
  X = cbind(x1,x2,x3); colnames(X) = c("X1","X2","X3")
  Y = propagate::propagate(expr = fy,data = X,do.sim = TRUE,nsim = 20000,cov = TRUE)
  Y_out[j,] = Y$sim
}
colnames(Y_out) = names(Y$sim)
Y_out = data.frame(Y_out)
head(Y_out)
w
x11();
par(mfrow=c(3,3))
plot(lambda[,1],Y_out$Mean,bty="n",ylab="mean(Y)"); 
plot(lambda[,2],Y_out$Mean,bty="n",ylab="mean(Y)")
plot(lambda[,3],Y_out$Mean,bty="n",ylab="mean(Y)")

plot(lambda[,1],Y_out$sd,bty="n",ylab="sd(Y)")
plot(lambda[,2],Y_out$sd,bty="n",ylab="sd(Y)")
plot(lambda[,3],Y_out$sd,bty="n",ylab="sd(Y)")

plot(lambda[,1],Y_out$X97.5.-Y_out$X2.5.,bty="n",ylab="cover(Y)")
plot(lambda[,2],Y_out$X97.5.-Y_out$X2.5.,bty="n",ylab="cover(Y)")
plot(lambda[,3],Y_out$X97.5.-Y_out$X2.5.,bty="n",ylab="cover(Y)")

par(mfrow=c(1,3))
hist(lambda[,1]); hist(lambda[,2]); hist(lambda[,3])


# Cambiamo ora il legame f(..) lasciando invariato lo schema di simulazione
M=100 # numero di scenari da valutare
fy = expression((X1*X2)-sqrt(X3))
Y_out = matrix(NA,M,6)
lambda = matrix(NA,M,3)
for(j in 1:M){
  lambda[j,] = rchisq(n=3,df = 10)
  x1 = rexp(n = 1000,rate = lambda[j,1])
  x2 = rexp(n = 1000,rate = lambda[j,2])
  x3 = rexp(n = 1000,rate = lambda[j,3])
  X = cbind(x1,x2,x3); colnames(X) = c("X1","X2","X3")
  Y = propagate::propagate(expr = fy,data = X,do.sim = TRUE,nsim = 20000,cov = TRUE)
  Y_out[j,] = Y$sim
}
colnames(Y_out) = names(Y$sim)
Y_out = data.frame(Y_out)
head(Y_out)

x11();
par(mfrow=c(3,3))
plot(lambda[,1],Y_out$Mean,bty="n",ylab="mean(Y)"); 
plot(lambda[,2],Y_out$Mean,bty="n",ylab="mean(Y)")
plot(lambda[,3],Y_out$Mean,bty="n",ylab="mean(Y)")

plot(lambda[,1],Y_out$sd,bty="n",ylab="sd(Y)")
plot(lambda[,2],Y_out$sd,bty="n",ylab="sd(Y)")
plot(lambda[,3],Y_out$sd,bty="n",ylab="sd(Y)")

plot(lambda[,1],Y_out$X97.5.-Y_out$X2.5.,bty="n",ylab="cover(Y)")
plot(lambda[,2],Y_out$X97.5.-Y_out$X2.5.,bty="n",ylab="cover(Y)")
plot(lambda[,3],Y_out$X97.5.-Y_out$X2.5.,bty="n",ylab="cover(Y)")

par(mfrow=c(1,3))
hist(lambda[,1]); hist(lambda[,2]); hist(lambda[,3])




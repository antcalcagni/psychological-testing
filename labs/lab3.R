#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2019/2020
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
### dott. Claudio  Zandonella Callegher (claudio.zandonellacallegher@phd.unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) Calcolo delle probabilità: approccio classico
# (B) Calcolo delle probabilità: approccio frequentista
# (C) Calcolo delle probabilità: uso di modelli notevoli
##########################################################


# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2019_2020/testing_psicologico/")
set.seed(1234) #fissiamo il seme per il generatore di numeri casuali


# (A) Calcolo delle probabilità - approccio classico --------------------------
# Il calcolo delle probabilità usando l'assegnazione classica di P sugli eventi A1,..,Ak di Omega viene fatto prima dell'esperimento aleatorio e
# l'assegnazione delle probabilità avviene sfruttando la conoscenza di Omega e di come gli eventi A1,..,Ak sono definiti. Tale calcolo avviene
# mediante principi e tecniche della matematica combinatoria (es.: permutazioni, combinazioni).
# Permutazioni: the act of arranging the members of a set into a sequence/order; if the set is already ordered, rearranging (reordering) its elements
# Combinazioni: selection of items from a collection, such that - unlike permutations - the order of selection does not matter


## ESEMPIO 1 #############################################################################
# Una password è composta da tre lettere passwd = {p1,p2,p3} (es.: "afd", "deo"); qual è la probabilità che una password scelta a caso 
# non abbia lettere ripetute al suo interno (es.: "aae", "bhb")?

# Costruzione di Omega (spazio campionario)
Omega = expand.grid(letters,letters,letters) #{a,b,...,z}^3 
colnames(Omega) = c("p1","p2","p3")
head(Omega)
dim(Omega)

# Definizione evento di interesse: A = {(p1,p2,p3) in Omega: p1 != p2 != p3}
# una volta scelta la prima lettera p1 dall'insieme composto da 26 lettere, la lettera p2 può essere scelta tra le 25 lettere rimanenti mentre 
# p3 può essere scelta tra le 24 lettere rimanenti. Il numero di risultati a favore di A, ossia #A, è quindi:
n_A = prod(26:24) # 26 x 25 x 24

# Calcolo di P(A)
p_A = n_A/dim(Omega)[1]
# la probabilità che una passwd di tre lettere conterrà tre lettere distinte è pari a 0.88
############################################################################################



## ESEMPIO 2 #############################################################################
# Una scatola contiene 75 oggetti tipo A e 25 tipo B per un totale di 100 oggetti. 
# Se ne estraiamo 12 a caso qual è la probabilità che siano di tipo A?

# Costruzione di Omega (spazio campionario)
# tutti i possibili campioni di 12 oggetti estraibili dalla scatola
n_Omega = choose(n = 100,k = 12) #choose() => oefficiente binomiale

# Definizione evento di interesse A = {"oggetti tipo A"}
n_A = choose(n = 75,k = 12) 
#ci sono n_A modi di estrarre 12 oggetti tipo A su un totale di 75

# Calcolo di P(A)
p_A = n_A/n_Omega
# la prob di estrarre 12 oggetti tipo A è 0.024
############################################################################################


## ESEMPIO 3 #############################################################################
# Data una classe di n studenti, qual è la probabilità che almeno due studenti festeggino 
# il compleanno lo stesso giorno? NB: (i) il compleanno indica che gli studenti siano nati lo stesso giorno
# non lo stesso anno; (ii) escludiamo l'anno bisestile, assumento che tutti gli anni abbiano 365 giorni
# (iii) 'n' è una variabile nel problema

# Costruzione di Omega (spazio campionario)
n_Omega = function(n){365^n}
# uno studente può avere il compleanno in uno dei 365 giorni, n studenti possono avere il compleanno un uno dei 365^n giorni (permutazioni)

# Definizione evento di interesse A = {"almeno due studenti festeggiano il compleanno lo stesso giorno"}
# per praticità lavoriamo con l'evento complementare Ac = {"tutti gli studenti festeggiano il compleanno in giorni diversi"}
# Ac è costruito negando A (si vedano le regole di negazione degli enunciati logici)
n_Ac = function(n){prod(365:(365-n+1))}
# n_A è costruito con la formula delle permutazioni senza ripetizione (vedi esercizio 1)

# Calcolo di P(A)
p_A = function(n){1-(n_Ac(n)/n_Omega(n))} # 1-P(Ac) è la prob dell'evento di interesse P(A), poiché (Ac) è evento complementare ad (A)

p_A(2) # P(A) quando la classe ha numerosità pari a n=2
p_A(10) # P(A) quando la classe ha numerosità pari a n=10
p_A(100) # P(A) quando la classe ha numerosità pari a n=100

# Calcoliamo P(A) per n diversi
N = seq(from=1,to=50,by = 1) #sequenza di n
pA = rep(NA,length(N)) #array vuoto dove immagazzineremo i risultati
for(i in 1:length(N)){
  pA[i] = p_A(N[i])
}

plot(N,pA,type="b",xlab="numero di studenti",ylab="prob compleanno stesso giorno")
n_05 = which.max(pA[pA<=0.5]) #numero di studenti n per i quali P(A) approx 0.5
abline(h = pA[n_05],v = n_05,lty=2)
############################################################################################



# (B) Calcolo delle probabilità - approccio frequentista --------------------------
# Il calcolo delle probabilità usando l'assegnazione frequentista di P sugli eventi A1,..,Ak di Omega viene fatto dopo l'esperimento aleatorio e
# l'assegnazione delle probabilità avviene osservando le frequenze (empiriche) di occorrenza degli eventi un numero molto elevato di volte.
#
# There is an "experimental" approach to calculating probabilities, which can, to a 
# certain extent, validate or otherwise the "classical" approach above. The idea is to
# repeat over and over again the relevant experiment, and then count how many times
# an event occurs. Now the "relative frequency" of an event E will in theory get closer
# and closer to P(E), the "probability of E."

## ESEMPIO 1 #############################################################################
# Lancio di una moneta non truccata
x = sample(x = c("T","C"),size = 10,replace = TRUE)
# la funzione sample() permette di campionare 'size' elementi dal vettore 'x=c(..)' che indica lo spazio campionario
# replace=TRUE indica che il campionamento deve avvenire con reinserimento

print(x) #visualizziamo 'x'
p_x = table(x)/10 #calcoliamo le probs degli eventi simulati calcolando le frequenze relative

# Simuliamo l'esperimento aleatorio facendo variare il numero N di lanci della moneta
N = seq(from=10,to=9000,by=5); 
p_x = matrix(NA,length(N),2); colnames(p_x) = c("T","C")
for(i in 1:length(N)){
  x = sample(x = c("T","C"),size = N[i],replace = TRUE)
  p_x[i,] = table(x)/N[i]
}
head(p_x)

x11();par(mfrow=c(2,2)) #dividiamo in due la finestra grafica

# grafico successione di eventi 'T' su N lanci
plot(x=N,y=p_x[,1],xlab="numero di lanci",ylab="P(X='T')",type="l",col="gray"); abline(h = 0.5,lty=2,col=2,lwd=2); abline(h = c(0.49,0.51),lty=2,col=1,lwd=1.5)

# grafico successione di eventi 'C' su N lanci
plot(x=N,y=p_x[,2],xlab="numero di lanci",ylab="P(X='C')",type="l",col="gray"); abline(h = 0.5,lty=2,col=2,lwd=2); abline(h = c(0.49,0.51),lty=2,col=1,lwd=1.5)

# grafico successione di eventi 'T' sui primi M=500 lanci
plot(x=N[1:99],y=p_x[1:99,1],xlab="numero di lanci",ylab="P(X='T')",type="l",col="gray"); abline(h = 0.5,lty=2,col=2,lwd=2); abline(h = c(0.49,0.51),lty=2,col=1,lwd=1.5)

# grafico successione di eventi 'C' sui primi M=500 lanci
plot(x=N[1:99],y=p_x[1:99,2],xlab="numero di lanci",ylab="P(X='C')",type="l",col="gray"); abline(h = 0.5,lty=2,col=2,lwd=2); abline(h = c(0.49,0.51),lty=2,col=1,lwd=1.5)

# Come possiamo commentare i quattro grafici? 
# Quale teorema ci è utile per spiegare la convergenza dei primi due grafici?
############################################################################################

## ESEMPIO 2 #############################################################################
# Riprendiamo l'esempio 1 della sezione A e mostriamo che la probabilità calcolata secondo l'approccio frequentista
# approssima quella calcolata secondo l'approccio classico.

# Riprendiamo l'esempio:
# Una password è composta da tre lettere passwd = {p1,p2,p3} (es.: "afd", "deo"); qual è la probabilità che una password scelta a caso 
# non abbia lettere ripetute al suo interno (es.: "aae", "bhb")?

# Costruzione di Omega (spazio campionario)
Omega = expand.grid(letters,letters,letters) #{a,b,...,z}^3 
colnames(Omega) = c("p1","p2","p3")

n_A = prod(26:24) # 26 x 25 x 24
p_A = n_A/dim(Omega)[1] # P(A) secondo l'approccio classico

# Ora utilizziamo l'approccio frequentista. Per semplicità utilizziamo una funzione già creata per contare il numero di occorrenze delle lettere all'interno 
# delle password
source("laboratorio/count_rep.R") #modificare il percorso del file count_rep.R rispetto a dove si trova il vostro file sulla vostra macchina
# la funzione "count_rep.R" restituisce 0 se l'evento non ha avuto occorrenza, 1 se invece l'evento si è verificato.

N=1000 #fissiamo il numero di ripetizioni dell'esperimento
res=rep(NA,N) #array vuoto per immagazzinare i risultati

# ciclo per simulare l'esperimento aleatorio
for(i in 1:N){ 
  iid = sample(x = 1:NROW(Omega),size = 1) #indice per campionare scelto a caso 
  x=Omega[iid,] #campioniamo un'osservazione dallo spazio campionario
  res[i] = count_rep(x) #verifichiamo l'evento di interesse
}
probs = table(res)/N
print(probs)
# osserviamo che P(X=0) = 0.89 molto prossima a quella calcolata mediante l'approccio classico p_A=0.887.

## ESEMPIO 3 #############################################################################
# Flavia e Giulia giocano scommettendo sugli esiti di un lancio di una moneta non truccata: ogni qualvolta l'esito della moneta è Testa
# Flavia vince 1Euro mentre quando l'esito è Croce Flavia perde 1Euro. Giulia lancia la moneta ed è responsabile di contare la vincita di Flavia.

N = 10000 # numero di lanci
x = sample(x = c("T","C"),replace = TRUE,size = N)
table(x)/N # probs degli eventi dopo l'esperimento

x_euro = rep(NA,N) #array per immagazzinare le vincite sulla base dell'array degli esiti 'x'
x_euro[x=="T"] = 1; x_euro[x=="C"] = -1

X = data.frame(esito=x,euro=x_euro) # costruiamo un dataset complessivo
print(X)

x_vincita = cumsum(x_euro) # array che contiene la vincita 'corrente' al lancio n-esimo (somma cumulata delle vincite parziali)
par(mfrow=c(1,1))
plot(x_vincita,type="l",ylim=c(-1,1)*max(abs(x_vincita)),xlab="lancio",ylab="vincita"); abline(h = 0,lty=2,col=2)

# aggiungiamo una retta al grafico per il trend di vincita
mod_vincita =lm(x_vincita~rep(1:N)); abline(mod_vincita,lty=2,col="gray")
print(mod_vincita)
# segno positivo del coeff angolare: trend positivo
# segno negativo del coeff angolare: trend negativo

# incremento complessivo di vincita dall'inizio del gioco
x_vincita[N]-x_vincita[1]

# incremento complessivo di vincita dalla metà del gioco
x_vincita[N]-x_vincita[floor(N/2)]

# vincita finale
x_vincita[N] # analogo a sum(x_euro)



# (C) Calcolo delle probabilità - uso di modelli notevoli ---------------------

# Per una rassegna sui modelli notevoli in R si consulti il cap.7 del manuale:
# ftp://ftp.tuebingen.mpg.de/pub/kyb/bresciani/Crawley%20-%20The%20R%20Book.pdf

## ESEMPIO 1 #############################################################################
# Gioco della lotteria: da una popolazione di N=36 numeri, n=6 sono scelti per costituire la "sestina vincente". Ogni giocatore acquista una "schedina" che contiene
# k=6 numeri. Il giocatore vince il jackpot se k=n, ossia se i numeri della sua schedina sono gli stessi di quelli rappresentati dalla sestina vincente.

# Definiamo la variabile casuale X = "match your selection"
# X=0: "nessun numero beccato su n"
# X=1: "un numero beccato su n"
# X=2: "due numeri beccati su n"
# ecc...
# X ~ hyperGeometric(x; n,N-n,n)

M=36; n=6
x = seq(from=0,to=n,by=1)

# Calcoliamo le probabilità degli esiti di X
p_x = dhyper(x = x,m = n,n = M-n,k = n) 
p_x = round(p_x,3) # per questioni di visualizzazione

plot(x,p_x,type="h",xlab="esiti del gioco",ylab="probs",bty="n") # type="h" disegna delle linee verticali
points(x,p_x) # aggiungiamo dei punti
# the probabilities of matching more than 2 decreases very quickly to 0.04% for match 3; 0.003% for match 4; 0.00009% for match 5, 
# and a whopping 0.0000005% for the jackpot.
# There is only one way of winning the jackpot, matching all 6 out of a total of choose(36,6) ..and yet people believe they are going to win!

# ..e se aumentiamo la cardinalità della popolazione N?
par(mfrow=c(2,2))
plot(x,dhyper(x = x,m = n,n = 36-n,k = n),type="c",xlab="esiti del gioco",ylab="probs",bty="n",main="N=36")
plot(x,dhyper(x = x,m = n,n = 39-n,k = n),type="c",xlab="esiti del gioco",ylab="probs",bty="n",main="N=39")
plot(x,dhyper(x = x,m = n,n = 45-n,k = n),type="c",xlab="esiti del gioco",ylab="probs",bty="n",main="N=45")
plot(x,dhyper(x = x,m = n,n = 79-n,k = n),type="c",xlab="esiti del gioco",ylab="probs",bty="n",main="N=79")
# la probabilità di vincere il jackpot diminuisce al crescere di N
############################################################################################


# Modello di Poisson X~Pois(lambda)
# modello ad un solo parametro (lambda) con distribuzione asimmetrica usato spesso per fenomeni di conteggio o fenomeni rari

x = rpois(n = 100,lambda = 1.5) # generiamo/campioniamo dei dati secondo il modello di Poisson
fx = table(x)
par(mfrow=c(1,1))
barplot(fx)

x = seq(from=0,to=20,by=1)
par(mfrow=c(2,2))
plot(x,dpois(x = x,lambda = 1.5),type="h",bty="n",ylab="p(x)",main="lambda=1.5");points(x,dpois(x = x,lambda = 1.5))
plot(x,dpois(x = x,lambda = 3),type="h",bty="n",ylab="p(x)",main="lambda=3");points(x,dpois(x = x,lambda = 3))
plot(x,dpois(x = x,lambda = 6.5),type="h",bty="n",ylab="p(x)",main="lambda=6.5");points(x,dpois(x = x,lambda = 6.5))
plot(x,dpois(x = x,lambda = 10.5),type="h",bty="n",ylab="p(x)",main="lambda=10.5");points(x,dpois(x = x,lambda = 10.5))

# fissato lambda=3, calcoliamo P(X>5)
q=1
p_q = 1-ppois(q = q,lambda = 3) # ppois calcola la funzione di ripartizione
par(mfrow=c(1,1))
plot(x,dpois(x = x,lambda = 3),type="h",bty="n",ylab="p(x)",main="lambda=3");points(x,dpois(x = x,lambda = 3))
abline(v=q,col=2,lty=2)

# fissato lambda=3, calcoliamo P(X<2)
ppois(q = 1,lambda = 3) # ppois calcola la funzione di ripartizione


# Modello Normale X~N(mu,sigma)
x = rnorm(n = 250,mean =0.9,sd=2.9) # generiamo/campioniamo dei dati secondo il modello Normale
hist(x) # istogramma dei dati generati
plot(density(x)) # stima della funzione di densità

x = rnorm(n=250,mean=0,sd=1) # modello Normale standardizzata
hist(x)
plot(density(x))

x = seq(from=-3,to=3,length.out = 250)
plot(x,dnorm(x = x,mean = 0,sd = 1),type="l",ylab="f(x)",bty="n")

# Calcoliamo F(X < -2)
pnorm(q = -2,mean = 0,sd = 1)

plot(x,dnorm(x = x,mean = 0,sd = 1),type="l",ylab="f(x)",bty="n")
abline(v = -2,lty=2,col=2) 
# F(X < -2): a sx della linea rossa tratteggiata pnorm(-2,0,1)
# F(X > -2): a dx della linea rossa tratteggiata 1-pnorm(-2,0,1)




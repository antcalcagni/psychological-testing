# La vincita finale di Giulia è in relazione alle probabilità degli esiti del lancio della moneta?
# Studio di simulazione
N = seq(from=100,to=20000,by=10) # sequenza di lanci

Y = matrix(NA,length(N),3) # array che conterrà i risultati dell'esperimento di simulazione
colnames(Y) = c("pT","pC","vincita")
head(Y) # vediamo com'è fatta la matrice Y che conterrà i risultati

for(i in 1:length(N)){
  print(paste("Simulazione id:",i,sep=" ")) # visualizzare la progressione della simulazione (opzionale)
  
  x = sample(x = c("T","C"),replace = TRUE,size = N[i])
  Y[i,1:2] = table(x)/N[i]
  x_euro = rep(NA,N[i]); x_euro[x=="T"] = 1; x_euro[x=="C"] = -1
  Y[i,3] = sum(x_euro)
}

head(Y,20) # visualizziamo i primi 20 risultati
# ad una prima ispezione notiamo che per perdite/vincite sono associate a eventi {T,C} non equiprobabili
# vincita=0 si ha quando p(T)=p(C)=0.5

# trasformiamo la matrice Y in data.frame per velocizzarne l'utilizzo
Y = data.frame(Y)

# aggiungiamo una variabile 'col' a Y per identificare con un colore la probs degli eventi
Y$col[Y$pC==0.5] = 2 # rosso
Y$col[Y$pC>0.5] = 3 # verde
Y$col[Y$pT>0.5] = 4 # blu

# aggiungiamo una variabile 'pch' a Y per identificare con un marker la probs degli eventi
Y$pch[Y$pC==0.5] = 4 
Y$pch[Y$pC>0.5] = 2 
Y$pch[Y$pT>0.5] = 3

# grafico finale
plot(N,Y$vincita,col=Y$col,pch=Y$pch,cex=4,xlab="numero di lanci",ylab="vincita in euro",bty="n")
abline(h = 0)
text(x=2000,y=max(Y$vincita),labels = "p(C)>p(T)")
text(x=2000,y=min(Y$vincita),labels = "p(C)<p(T)")


# Il trend di vincita di Giulia è in relazione alle probabilità degli esiti del lancio della moneta?
# Studio di simulazione
N = seq(from=100,to=20000,by=10) 
Y = matrix(NA,length(N),3);colnames(Y) = c("pT","pC","trend")

for(i in 1:length(N)){
  print(paste("Simulazione id:",i,sep=" ")) # visualizzare la progressione della simulazione (opzionale)
  
  x = sample(x = c("T","C"),replace = TRUE,size = N[i])
  Y[i,1:2] = table(x)/N[i]
  x_euro = rep(NA,N[i]); x_euro[x=="T"] = 1; x_euro[x=="C"] = -1
  mod_vincita = lm(cumsum(x_euro)~rep(1:N[i])) # stimiamo il trend di vincita con un modello lineare
  Y[i,3] = mod_vincita$coefficients[2] # salviamo solo il coefficiente angolare
}

Y = data.frame(Y)

#..come per il caso precedente
Y$col[Y$pC==0.5] = 2; Y$col[Y$pC>0.5] = 3; Y$col[Y$pT>0.5] = 4 # blu
Y$pch[Y$pC==0.5] = 4; Y$pch[Y$pC>0.5] = 2; Y$pch[Y$pT>0.5] = 3

# grafico finale
par(mfrow=c(1,2))
plot(N,Y$trend,col=Y$col,pch=Y$pch,cex=4,xlab="numero di lanci",ylab="vincita in euro",bty="n");abline(h = 0);
plot(N[1:99],Y$trend[1:99],col=Y$col[1:99],pch=Y$pch[1:99],cex=4,xlab="numero di lanci",ylab="vincita in euro",bty="n");abline(h = 0);


# La differenza di vincita dall'inizio è in relazione alle probabilità degli esiti del lancio della moneta?
# Studio di simulazione
N = seq(from=100,to=20000,by=10) 
Y = matrix(NA,length(N),3);colnames(Y) = c("pT","pC","diffvincita")

for(i in 1:length(N)){
  print(paste("Simulazione id:",i,sep=" ")) # visualizzare la progressione della simulazione (opzionale)
  
  x = sample(x = c("T","C"),replace = TRUE,size = N[i])
  Y[i,1:2] = table(x)/N[i]
  x_euro = rep(NA,N[i]); x_euro[x=="T"] = 1; x_euro[x=="C"] = -1
  Y[i,3] = cumsum(x_euro)[N[i]]-cumsum(x_euro)[1]
}

Y = data.frame(Y)

#..come per il caso precedente
Y$col[Y$pC==0.5] = 2; Y$col[Y$pC>0.5] = 3; Y$col[Y$pT>0.5] = 4 # blu
Y$pch[Y$pC==0.5] = 4; Y$pch[Y$pC>0.5] = 2; Y$pch[Y$pT>0.5] = 3

# grafico finale
par(mfrow=c(1,2))
plot(N,Y$diffvincita,col=Y$col,pch=Y$pch,cex=4,xlab="numero di lanci",ylab="vincita in euro",bty="n");abline(h = 0);
plot(N[1:99],Y$diffvincita[1:99],col=Y$col[1:99],pch=Y$pch[1:99],cex=4,xlab="numero di lanci",ylab="vincita in euro",bty="n");abline(h = 0);


# Uno studio di simulazione un po più complesso
# Generiamo M processi di vincita indipendenti ognuno dei quali è stato osservato N volte: gli M processi sono giochi/scommesse indipendenti mentre N rappresenta ancora la
# lunghezza del gioco di lancio della moneta
M=50;N=10000
Y = matrix(NA,N,M)

for(j in 1:M){
  print(paste("Simulazione id:",j,sep=" ")) # visualizzare la progressione della simulazione (opzionale)
  
  x = sample(x = c("T","C"),replace = TRUE,size = N)
  x_euro = rep(NA,N[i]); x_euro[x=="T"] = 1; x_euro[x=="C"] = -1
  Y[,j] = cumsum(x_euro)
}

ylims = c(min(apply(Y,2,min)), max(apply(Y,2,max))) # per visualizzare bene tutti i grafici
plot(1:N,Y[,1],ylim=ylims,col="gray",type="l",xlab="numero di lanci",ylab="vincita",bty="n") # visualizziamo il primo processo
for(j in 2:M){ # visualizziamo i restanti processi
  points(1:N,Y[,j],type="l",col="gray")
}
points(apply(Y,1,mean),type="l") # visualizziamo anche il "processo medio" su M
abline(h=0,lty=2,col=2,lwd=2) # linea orizzontale sullo zero (come in precedenza)

# Cosa notiamo? Come possiamo commentare? 
############################################################################################

# (D) Uso delle disuguaglianze  -----------------------------------------------

## ESEMPIO 1 #############################################################################
# Da studi precedenti sappiamo che l'intelligenza dei bambini X (nella popolazione) ha media 10 e vogliamo conoscere la 
# proporzione di bambini con intelligenza superiore o uguale a 20.

# Non conoscendo né legge distributiva di X né varianza di Var[X], usiamo l'unica informazione a disposizione, ossia E[X]=10, con
# la disuguaglianza di Markov P(X >= q) <= E[X]/q
p_20_markov = 10/20 # upper bound di P(X>=20)

# Supponiamo ora di sapere che X~N(mu=10.8,sd=?) con media 10.8 e deviazione standard, ossia sqrt(Var[X]), ignota.
# Non conoscendo la varianza, simuliamo uno scenario in cui facciamo variare il parametro ignoto Var[X] e calcoliamo la quantità richiesta P(X>=20)
sdx = seq(from=0.01,to=100,length.out = 25)
p_20 = 1-pnorm(q = 20,mean = 10,sd=sdx)
plot(sdx,p_20,xlab="possibili valori di sigma",ylab="P(X>=20)",bty="n",ylim=c(0,0.6))
abline(h = p_20_markov,lty=2,col=2); text(x=50,y=0.55,labels="limite superiore di Markov")
# for the lower values of σ, Ρ(Χ > 20) is substantially less than the bound given by Markov's inequality but when σ is large, σ > 100, for
# example, the probability gets near the Markov bound of 0.5. As σ increases, the tail probability tends to be 0.5 but never exceeds it. 
# What this suggests is that the Markov's inequality will be accurate when applied to a random variable that deviates a lot from
# its expectation, and not so good when applied to data which have a small variance.
# This example illustrates the "conservativeness" of the Markov's inequality.

############################################################################################


## ESEMPIO 2 #############################################################################
# A manufacturer of minicomputer systems surveyed customers regarding downtime of a specific minicomputer system and found that the average downtime experienced was 
# 25 min a month with variance 144 min2.

# How confident can the manufacturer be that the downtime will be within 15 min of the mean?
# Calcoliamo P(|X-25|<15) mediante la disuguaglianza di Chebishev conoscendo E[X] e Var[X]:
p_15_cheb = 1- 144/(15^2) 
# We are at least 36% confident that the downtime is within 15 min of its mean.

# ...if we know that downtime is normally distributed that is X~N(mu=25,sigma=12), sigma=sqrt(Var[X])
# Calcoliamo P(|X-25|<15) = P(10 < X < 40)
p_15_norm = pnorm(q = 40,mean = 25,sd = 12) - pnorm(q = 10,mean = 25,sd = 12)
# When the distribution is normal, there is a 78.8% probability that the downtime will be between 10 and 40 min, which is much greater than the 36% obtained from
# Chebyshev's inequality. Chebyshev's inequality gives the best estimate of a probability interval when all that is known is the mean and variance of the distribution. 
# It can be viewed as giving a "crude estimate" of the probability. It is not surprising that it can be sharpened when the actual distribution becomes known.

############################################################################################





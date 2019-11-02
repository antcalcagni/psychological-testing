#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2019/2020
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
### dott. Claudio  Zandonella Callegher (claudio.zandonellacallegher@phd.unipd.it)
#######################################################

## CONTENUTO DEL CODICE ################################
# (A) Preparazione dataset
# (B) Punteggi questionari
# (C) Analisi descrittive del dataset
# (D) Costruzione algoritmi
########################################################

# Inizializzazione ambiente di lavoro -------------------------------------

rm(list=ls())  # Elimino oggetti presenti (importante all'inizio di ogni sessione)

install.packages(c("rstudioapi","readxl"))  # install.packages() installare i pacchetti  utilizzati 

setwd(dirname(rstudioapi::getSourceppEdppitorContext()$path))  # working directory

library("readxl")  # Pacchetto per caricare file excel


# Caricamento dei dati ----------------------------------------------------

data<-as.data.frame(readxl::read_excel("data.xls"))

# Alternativa usando file .csv
# data<-read.csv("data.csv", header = TRUE, sep=";", stringsAsFactors = F)
 

# Alternativa Rdata
# load("data.rda")


str(data)  # struttura del dataset

##### Descrizione dataset

# Nel presente dataset sono riportati parte dei dati riguardanti un vero proggetto di ricerca.
# In questo studio è stata valutata nei bambini del 3° e 4° anno di scuola primaria la relazione tra:
# 1) attaccamento verso la madre
# 2) status socio-economico 
# 3) problemi internalizzanti ed esternaizzanti

# Nel dataset abbiamo le seguenti variabili
# id : codice identificativo  di ogni bambino
# escludere : bambini che per i criteri dello studio non sono da includere nelle analisi finali (1=escludere; 0=includere)
# raccolta : i dati sono stati raccolti in due giornate diverse (1=raccolta A; 2=raccolta B )
# consenso : bambini che hanno ricevuto il consenso dei genitori per il trattamento dei dati (1=si; 0=no)
# presenza : bambini presenti il giorno della raccolta dati (1=si; 0=no)
# classe : classe frequentata dal bambino (3°anno o 4°anno scuole primarie)
# genere : genere del bambino (0=M; 1=F)
# mesi : età del bambio in mesi
# fas* (da 1 a 4): item del questionario sullo status socio-economico
# ssm* (da 1 a 15): item del questionario sicurezza materna
# sdq* (da 1 a 25): idem del questionario problemi internalizzanti e esternalizzanti




# (A) Preparazione dataset  ---------------------------

##### Rendo fattori le variabili categoriali

data$escludere<-factor(data$escludere)
data$consenso<-factor(data$consenso)
data$presenza<-factor(data$presenza)
data$classe<-factor(data$classe)

data$genere<-factor(data$genere)
levels(data$genere)<-c("M","F")  # definire il nome dei livelli del fattore

# data$genere<-factor(data$genere, labels=c("M","F"))


##### Selezione soggetti

# Seleziono solo i soggetti che hanno il consenso, da includere e che erano presenti 

data<-data[data$consenso=="1" & data$escludere=="0" & data$presenza=="1",]
summary(data)


##### rimuovo  i dati  mancnati

# Nel caso dei dati mancanti "NA" è corretto usare la funzione is.na(x)
# e non x==NA
# ?is.na()  

# data$ssp4==NA
# is.na(data$ssp4)

data<-data[complete.cases(data),]


##### Ricodifico il FAS

# Per quanto riguarda il FAS abbimao 4 item.
# Le possibili risposte sono
# item 1: 0-1
# item 2: 0-1-2
# item 3: 0-1-2-3
# item 4: 0-1-2-3

summary(data[,c("fas1","fas2","fas3","fas4")])

# Purtoppo nella seconda raccolata c'è stata una codifica scorretta
# item 1: 1-2
# item 2: 1-2-3
# item 3: 1-2-3-4
# item 4: 1-2-3-4

# Dobbiamo riportarli alla scala originale (sottrarre 1)

# Divido i due dataset

d1<-data[data$raccolta=="1",]
d2<-data[data$raccolta=="2",]

d2$fas1<-d2$fas1-1
d2$fas2<-d2$fas2-1
d2$fas3<-d2$fas3-1
d2$fas4<-d2$fas4-1

data<-rbind(d1,d2)  # Unire le i dataset per riga (cbind() è usato per le colonne)

summary(data[,c("fas1","fas2","fas3","fas4")])

# C'è ancora un problema con fas2. Il massimo dovrebbe essere 2

table(data$fas2)  # Controllo quanti sono i punteggi sbagliati

data[data$fas2==3,"id"] # Controllo quale soggetto ha una codifica sbagliata

data$fas2[data$fas2==3]<-2 # Correggo

##### Creo la variabile età in anni

data$age<-round(data$mesi/12,1)  


##### rimuovo le colonne che non utilizzo più

data<-data[,-c(2,3,4,5,8)]

# data<-data[,-c(2:5,8)]

summary(data)
str(data)

##### Salviamo il dataset che abbiamo sistemato
save(data,file = "data_pulito.rda")


# (B) Calcolare i punteggi degli item ------------------------

load("data_pulito.rda")

#### FAS (status socio economico)

# Il punteggio tatale  è dato dalla somma degli item singoli
# Punteggi alti indicano maggiore benessere

fas.tot<-apply(data[,c("fas1","fas2","fas3","fas4")], MARGIN = 1, sum)


#### SSM (Attaccamento verso la madre)

# Questionario con 15 item a risposte su scala likert a 4 punti
# Le risposte sono codificate come 1-2-3-4

# Tuttavia per costruire il punteggio finale è necessario
# invertire i punteggi degli item 1,3,4,9,10,13,15

# Ovvero: 1-->4; 2-->3; 3-->2; 4-->1

head(data[c("ssm1", "ssm3", "ssm4", "ssm9", "ssm10", "ssm13", "ssm15")])

data$ssm1<-abs(data$ssm1-5)
data$ssm3<-abs(data$ssm3-5)
data$ssm4<-abs(data$ssm4-5)
data$ssm9<-abs(data$ssm9-5)
data$ssm10<-abs(data$ssm10-5)
data$ssm13<-abs(data$ssm13-5)
data$ssm15<-abs(data$ssm15-5)

head(data[c("ssm1", "ssm3", "ssm4", "ssm9", "ssm10", "ssm13", "ssm15")])

# Ora il punteggio totale è dato dalla media degli item
# Punteggi alti indicano un attaccamento sicuro

ssm.tot<-apply(data[,paste0("ssm",1:15)],MARGIN = 1, mean)

# paste0("ssm",1:15)

#### SDQ

#  Questionario con 25 item a risposte su scala likert a 3 punti
# Le risposte sono codificate come 0-1-2

# Tuttavia per costruire il punteggio finale è necessario
# invertire i punteggi degli item 7,11,14,21,25

# Ovvero: 0-->2; 1-->1; 2-->0;

tail(data[c("sdq7", "sdq11", "sdq14", "sdq21", "sdq25")])

data$sdq7<-abs(data$sdq7-2)
data$sdq11<-abs(data$sdq11-2)
data$sdq14<-abs(data$sdq14-2)
data$sdq21<-abs(data$sdq21-2)
data$sdq25<-abs(data$sdq25-2)

tail(data[c("sdq7", "sdq11", "sdq14", "sdq21", "sdq25")])

#  I punteggi totali sono dati dalla somma degli item
#  Punteggi più alti indicano maggiori problemi

#  Abbiamo 5 sottoscale formate dagli item:
#  emotion	=	3 8 13 16 24
#  conduct	=	5 7 12 18 22
#  hyper	=	2 10 15 21 25
#  peer	=	6 11 14 19 23
#  prosoc	=	1 4 9 17 20

#  Due punteggi riassuntivi vengono creati unendo le sottoscale

#  internalizing = emotion + peer
#  externalizing = hyper + conduct

# 

#  internalizing = item (3,6,8,11,13,14,16,19,23,24)
#  externalizing = item (2,5,7,10,12,15,18,21,22,25)

int.tot<-apply(data[,paste0("sdq",c(3,6,8,11,13,14,16,19,23,24))],MARGIN = 1, sum)
ext.tot<-apply(data[,paste0("sdq",c(2,5,7,10,12,15,18,21,22,25))],MARGIN = 1, sum)


data<-cbind(data[,1:3],fas.tot,ssm.tot,int.tot,ext.tot)

save(data, file="data_finale.rda")

# (C) Statistiche descrittive  -------------------------------

load("data_finale.rda")

### ssm.tot ~ genere*classe

# Valutiamo a livello descrittivo i valori di attaccamento verso la madre
# a seconda del genere e della classe 

# genere e  classe
table(data$genere)
table(data$classe)
table(data$genere,data$classe)

# ssm.tot
summary(data$ssm.tot)
sd(data$ssm.tot)

hist(data$ssm.tot, col="lightblue", xlab="SSM", main = "Distribuzione SSM")
boxplot(data$ssm.tot,col="lightblue", ylab="SSM", main = "Distribuzione SSM")


# ssm.tot ~ genere*classe

# mean(data$ssm.tot[data$genere=="F" & data$classe=="3"])
# sd(data$ssm.tot[data$genere=="F" & data$classe=="3"])

tapply(data$ssm.tot, INDEX = list(data$genere, data$classe), FUN=mean)
tapply(data$ssm.tot, INDEX = list(data$genere, data$classe), FUN=sd)

# Grafici
par(mfrow=c(2,2)) # Determino il numero di grafici da rappresentare su una stessa pagina

hist(data$ssm.tot[data$genere=="M" & data$classe=="3"], 
     col="green", xlab="SSM", main = "M 3°", ylab=NULL, xlim = c(2,4))
hist(data$ssm.tot[data$genere=="F" & data$classe=="3"], 
     col="orange", xlab="SSM", main = "F 3°", ylab=NULL,xlim = c(2,4))
hist(data$ssm.tot[data$genere=="M" & data$classe=="4"], 
     col="lightblue", xlab="SSM", main = "M 4°", ylab=NULL,xlim = c(2,4))
hist(data$ssm.tot[data$genere=="F" & data$classe=="4"], 
     col="lightpink", xlab="SSM", main = "F 4°", ylab=NULL,xlim = c(2,4))


par(mfrow=c(1,1)) 
boxplot(data$ssm.tot~data$genere*data$classe,
        col=c("lightgreen", "orange","lightblue","lightpink"), ylab="SSM", 
        main="Relazione SSM con Genere e Classe")

# Per digitare il simbolo tilde (~)
# Windows: ALT + 126 (tastierino numerico)
#          ALT + Fn + 126 (senza tastierino)
# MacOS: option + 5

# Per salvare un grafico premere "Export" 


### ssm.tot ~ ext.tot

# Valutiamo a livello descrittivo la relazione tra
# attaccamento materno e problemi esternalizzanti

# ext.tot
summary(data$ext.tot)
sd(data$ext.tot)

hist(data$ext.tot, col="red", xlab="EXT", main = "Distribuzione EXT")
boxplot(data$ext.tot,col="red", ylab="EXT", main = "Distribuzione EXT")

# ssm.tot~ext.tot
plot(data$ssm.tot,data$ext.tot, ylab="EXT", xlab="SSM", main="SSM~EXT")
plot(jitter(data$ssm.tot,4),data$ext.tot,  ylab="EXT", xlab="SSM", main="SSM~EXT")

cor(data$ssm.tot,data$ext.tot)

### Esercizi:
# Valuta a livello descrittivo (media, deviazione standard  e rappresentazione grafica):
# 1) i punteggi dei problemi esternalizzanti a seconda del genere e della classe
# 2) i punteggi dei problemi internalizzanti a seconda del genere e della classe
# 3) la relazione tra attaccamento materno e problemi internalizzanti
# 4) la relazione tra status socio-economico e problemi internalizzanti
# 5) la relazione tra status socio-economico e problemi esternalizzanti


# (D) Costruzione algoritmi -----------------------------

### Ciclo for

for (i in 1:10){
  print(i)
}

#
for (i in c(6,3,5,8,8)){
  print(i)
}

#
for (i in c("Hello","World","!")){
  print(i)
}


### If else
k<-2019

if (k!=2019){
  print("1° condizione")
} else {
  print ("2° condizione")}

#
if (k==2018){
  print("1° condizione")
} else if(k==2020) {
  print ("2° condizione")
}else{
  print("3° condizione")
  }

#### Esercizio 1

# Costruire una funzione che dato un vettore di valori numerici ne calcoli la media

my_mean<-function(x){
  if(is.numeric(x)){k<-0
  for(i in 1:length(x)){
    k=k+x[i]
  }
  k=k/length(x)
  
  cat("La media è:\n")
  return(k)
  }else{
    warning("x deve essere un vettore di valori numerici")
  }
  
}

my_mean(c(4,6,3,87,5,4,45))
my_mean("ciao")

mean(c(4,6,3,87,5,4,45))


#### Esercizio 2

# Costruire una funzione che dato un vettore di valori ne determini il massimo


my_max<-function(x){
  if(is.numeric(x)){
    k<-x[1]
  for(i in 1:length(x)){
    if(k<x[i]){
      k<-x[i]
    }
  }
  
  cat("Il massimo è:\n")
  return(k)
  }else{
    warning("x deve essere un vettore di valori numerici")
  }
}
  
  
#### Esercizio 3
  
# Camminata aleatoria
  
# Partendo dal punto zero ad ogni turno si può compiere un passo a sinistra (+1) 
# oppure un passo a destra (-1) secondo la probabilità p
  
# Creare una funzione che riproduca la camminata aleatoria
# rappresentandola graficamente.
  
# Fare in modo che si possa definire il  numero di trials (passeggiate)
# il numero di passi e la probabilità p

  random_walk<-function(n_trials=10, n_steps=100, p=c(.5,.5)){
    
    res<-matrix(0, nrow = n_trials, ncol = n_steps+1)
    
    for(i in 1:n_trials){
      steps<-sample(c(-1,1),size = n_steps,replace = TRUE, prob = p)
      res[i,2:(n_steps+1)]<-cumsum(steps)
    }
    
    up<-max(res)
    low<-abs(min(res))
    bound<-max(up,low)
    
    plot(x=c(0,n_steps), y=c(0+bound,0-bound), type="n", xlab="Passi",ylab="Posizione")
    
    for(i in 1:n_trials){
      lines(x=0:n_steps, y=res[i,], type = "l")
    }
  }
  
  random_walk()
  
  random_walk(n_trials = 10,n_steps=1000, p=c(.49,.51))

  
###################  Fine  ########################
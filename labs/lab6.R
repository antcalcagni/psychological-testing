#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2019/2020
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
### dott. Claudio  Zandonella Callegher (claudio.zandonellacallegher@phd.unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) Analisi descrittiva degli items
# (B) Coerenza delle scale
# (C) Difficoltà degli items
# (D) Capacità discriminativa degli items
##########################################################


# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2019_2020/testing_psicologico/")
library(hemp); library(psych)



# (A) Analisi descrittiva degli items ------------------------------------
# Utilizziamo il dataset 'bfi' (pacchetto psych) che contiene 25 items presi dal test IPIP (International Personality Item Pool, http:\\ipip.ori.org) per la quantificazione
# dei 5 fattori di personalità Agreeableness (A), Conscientiousness (C), Extraversion (E), Neuroticism (N), and Opennness (O). Il dataset contiene anche 3 variabili ulteriori di classificazione
# (gender, education, age). Per maggiori info: bfi.dictionary

data("bfi") #importazione dei dati dalla libreria psych al workspace corrente di R

# Prime visualizzazioni dei dati
str(bfi) 
head(bfi)

# Analisi dei valori mancanti nel dataset
num.na = apply(bfi,2,function(x)sum(is.na(x))) #conta il numero di NAs per ciascuna colonna (variabile) del dataset
barplot(num.na[1:25],horiz = TRUE) #grafico a barre degli NA's per gli items
barplot(num.na[26:28],horiz = TRUE) #grafico a barre degli NA's per le variabili di classificazione

# Gli items non contengono molti valori mancanti rispetto al totale di unità statistiche. La sola variabile che contiene molti valori mancanti è 'education'. 
# Se usiamo l'esclusione row-wise (default di R) che elimina le righe che hanno almeno un NA su una variabile perderemmo NROW(bfi)-NROW(na.omit(bfi))=564 unità.
# Se al contrario escludiamo la variabile 'education' perderemmo NROW(bfi[,-27])-NROW(na.omit(bfi[,-27]))=364 unità. Sebbene la differenza non sia così elevata
# procediamo escludendo la variabile 'education'.
bfi = bfi[,-27]

# Statistiche descrittive e grafiche
describe(x = bfi) #complessive
describeBy(x = bfi,group = "gender") #per livelli della variabile di classificazione 'gender'

x11();par(mfrow=c(5,5))
for(j in 1:25){
  barplot(table(bfi[,j]),main = colnames(bfi)[j]) #distribuzioni di frequenza per ciascun item 
}

# Abbiamo usato barplot() e non hist() poiché le osservabili sono rilevate a livello ordinale con scale a 6 livelli. Possiamo notare come le distribuzioni tendono ad 
# essere poco simmetriche ed alcuni items (es.: C5,N4) tendono ad avere distribuzioni prossime a quella uniforme (tutte i livelli della scala sono scelti dai soggetti).

# Calcoliamo un indice di eterogeneità per variabili categoriali (mutabili), ad esempio l'indice di entropia normalizzata Hj = -sum(fi*log(fi))/log(6)
Y = matrix(NA,25,6+1) #25 items x 6 livelli della scala + 1 entropia
for(j in 1:25){
  fi = table(bfi[,j])/NROW(bfi[,j])  
  Hj = -sum(fi*log(fi))/log(6)
  Y[j,1:6] = fi #prime 6 colonne: frequenze per ciascun livello della scala
  Y[j,7] = Hj #ultima colonna: entropia normalizzata
}
Y=data.frame(Y); colnames(Y)[7] = "H"

plot(Y$H,type="b", bty="n",xlab="item",ylab="entropia normalizzata",ylim=c(0.7,1))
abline(h=mean(Y$H),lty=2)

iid=which(Y$H>mean(Y$H)) #items che hanno entropia norm. sopra la media (indice dell'intem)
names(bfi)[iid] #items che hanno entropia norm. sopra la media (nome dell'item)

# Visualizziamo la distribuzione degli items per gender
x11();par(mfrow=c(5,5))
for(j in 1:25){
  bi.bars(bfi,names(bfi)[j],"gender",ylab=names(bfi)[j])
}



# (B) Coerenza delle scale -----------------------------------------------
# Il test IPIP contiene cinque scale ognuna con cinque items. Per valutare la coerenza interna delle singole scale (attendibilità) dobbiamo dapprima
# verificare la presenza di "reversed items", ossia items che presentano inversione di scala rispetto agli altri (un item con scala invertita, ad esempio, è un item
# dove il primo livello corrisponde all'estremo superiore mentre l'ultimo livello all'estremo inferiore). Spesso questi tipi di items sono utilizzati per evitare
# l'effetto faking, di manipolazione della risposta, o come items di controllo. 
# Tale informazione è disponibile nell'oggetto bfi.dictionary alla colonna 'keying' (codifica)
bfi.dictionary
reversed.items = rownames(bfi.dictionary[1:25,])[bfi.dictionary$Keying[1:25]<0] #items con codifica rovesciata (reversed items)
print(reversed.items)


# Possiamo usare alcune funzioni della libreria psych per codificare gli items

bfi.omit = na.omit(bfi) #psych non elimina i dati mancanti row-wise ma gli sostituisce con la mediana delle osservazioni

keys.list = list(agree=c("-A1","A2","A3","A4","A5"), #lista contenente le scale con i relativi items (il segno "-" indica che l'item è rovesciato)
                  conscientious=c("C1","C2","C3","-C4","-C5"),
                  extraversion=c("-E1","-E2","E3","E4","E5"),
                  neuroticism=c("-N1","-N2","N3","-N4","-N5"), 
                  openness = c("O1","-O2","O3","O4","-O5")) 

bfi.keys = make.keys(bfi.omit,keys.list) #crea una matrice di indici con gli items rovesciati

scores = scoreItems(keys = bfi.keys,items = bfi.omit,min=1,max=6)  #per info vedi scoreItems() 
print(scores)

# La funzione scoreItems() calcola diverse quantità utili all'analisi della coerenza interna secondo la TCT:
# alpha di Cronbach per ciascuna scala (con errore standard dell'indice)
# correlazione media e mediana degli items con la scala
# indice di attendibilità di Guttman e rapporto segnale rumore
# Correlazioni tra scale corrette per attenuazione:
## Correlations between scales are attenuated by a lack of reliability. 
## Correcting correlations for reliability (by dividing by the square roots of the reliabilities of each scale) sometimes help show structure. 
## This is done in the scale intercorrelation matrix with raw correlations below the diagonal and unattenuated correlation above the diagonal.



# (C) Difficoltà degli items ----------------------------------------------

# Items dicotomici
# Il nostro dataset non contiene scale con items dicotomici. Proviamo, per ragioni didattiche, a rendere dicotomici gli items della scala A
# utilizzando la mediana teorica (m = 3)di ciascun item come soglia per la boleanizzazione.

bfi.A = bfi.omit[,paste0("A",1:5)]
bfi.A.bool = apply(bfi.A,2,function(x)ifelse(x<=3,0,1))

# Calcoliamo ora l'indice h per ciascun item e per la scala A complessivamente
h = apply(bfi.A.bool,2,mean)
h.tot = sum(h)/length(h)

# L'item A1 è quello con maggiore difficoltà. La scala A complessivamente presenta un grado di difficoltà bassso (h_tot = 0.713).

# Item politomici
# Usiamo questa volta la scala A così come è stata rilevata, ossia mediante una scala Likert a 6 punti.

h = abs(apply(bfi.A,2,median)-3) #3 è la mediana teorica della scala

# Per calcolare h_tot calcoliamo dapprima il punteggio totale della scala per mediana
bfi.A.tot = apply(bfi.A,1,median)
h.tot = abs(median(bfi.A.tot)-3)

# Risultano difficili gli items A2-A5 della scala (h>1.5). Complessivamente anche la scala risulta difficile (h.tot>1.5).
# Nota: questo risultato è influenzato dalla scelta della mediana teorica (m=3) nonché dal fatto che gli items non si distribuiscono simmetricamente. Ragione per cui
# gli indici di difficoltà proposti, basati sulla simmetria della distribuzione degli items, non sono idonei in questo particolare caso.



# (D) Capacità discriminativa degli items ---------------------------------

# Items dicotomici

bfi.A.tot = apply(bfi.A.bool,1,sum) #calcolo del punteggio totale via somma

# Un modo per calcolare la capacità discriminativa degli items è quello basato sulla correlazione 
# tra items e punteggio totale, BN(2.11.1)
d = cor(x = bfi.A.bool, y = bfi.A.tot,method = "spearman")
print(d)

# Gli items della scala presentano correlazioni medio-alte ad eccezione dell'item A1. Ciò segnala che A2-A5 sono concordi tra loro (coerenti)
# nel quantificare il costrutto A. L'item A1, come visto anche con l'analisi della difficoltà, può essere eliminato.

# Un secondo modo è l'utilizzo della divisione in due gruppi del campione, BN(2.11.1). A tal fine usiamo la funzione idi() della libreria 'hemp'.
d = idi(data = bfi.A.bool,item = bfi.A.bool[,1] ,perc_cut = 0.25) #calcolo sull'item 1
d = d[1]-d[2]

# Il 44% degli individui nel gruppo con alto punteggio (Upper 25%) ha risposto all'item correttamente mentre il 55% di essi ha risposto correttamente nel gruppo a basso punteggio.
# Questo suggerisce che l'item non è utile per discriminare tra i due gruppi: infatti d < 0.20 come suggeriscono le norme di Ebel (1965). 
# Il gruppo con alti punteggi è stato più difficile rispetto al gruppo con bassi punteggi (d è negativo). Questo risultato concorda con quelli ottenuti precedentemente sullo stesso item.

# Calcoliamo l'indice d per tutti gli items della scala
D = matrix(NA,2,5)
for(j in 1:5){
  D[,j] = idi(data=bfi.A.bool,item=bfi.A.bool[,j],perc_cut=0.25)
}
d = D[1,]-D[2,]

# Per i restanti items A2-A5 vediamo che l'indice d è positivo, ad indicare che gli items sono stati più facili per il gruppo ad alto punteggio rispetto a quello a basso punteggio.
# Inoltre, d>0.3 per A2-A5 e ciò suggerisce che gli items discriminano bene e possono essere tenuti per formare la scala A.

# Item politomici
# Possono essere usate le due procedure cor() e idi() viste per gli item dicotomici. I risultati possono variare rispetto a quelli ottenuti in precedenza e ciò è dovuto al fatto
# che si utilizza maggiore informazione (gli items hanno maggiore eterogeneità statistica). 










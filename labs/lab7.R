#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2019/2020
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
### dott. Claudio  Zandonella Callegher (claudio.zandonellacallegher@phd.unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) Covarianza e correlazione: breve intro
# (B) Correlazione parziale
# (C) Grafici per matrici di correlazione
# (D) Comparare matrici di correlazione
##########################################################


# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2019_2020/testing_psicologico/")
library(hemp); library(psych)


# [A] Covarianza e correlazione: breve intro ------------------------------

# Utilizziamo il dataset "interest" della libreria hemp che contiene risposte ad un questionario con scale sugli aspetti cognitivi e di personalità.
# Maggiori info sulle variabili del dataset: ?hemp::interest
interest = hemp::interest
str(interest)
interest$gender = as.factor(interest$gender)
head(interest)

# Il dataset presenta tre variabili iniziali (gender,educ, age) che non riguardano le scale del questionario. Inoltre ci sono dim(interest)[2] = 30 variabili in tutto, troppe.
# Prendiamo un sottoinsieme con alcune scale interessanti:
interest.scales = interest[,c("vocab","reading","sentcomp","geometry","analyrea","sociabty","worry")]
# Mettiamo le variabili concomitanti age,educ,gender in un secondo dataframe:
interest.cmts = interest[,c(1:3)]

# Le variabili riferite alle scale sono già standardizzate, infatti:
apply(X = interest.scales,MARGIN = 2,FUN = mean) #approx 0
apply(X = interest.scales,MARGIN = 2,FUN = var) #approx 1

# Correlazione e covarianza sono difatti simili
cor(interest.scales)
cov(interest.scales)

# Immaginiamo di conoscere le varianze delle variabili/scale, ad esempio:
set.seed(123)
vars.scales = runif(n = NCOL(interest.scales),min = 2.34,max = 7.78) #campioniamo NCOL(interest.scales) = 7 varianze da una distribuzione uniforme in [2.34,7.78]

# e otteniamo la matrice di covarianza da quella di correlazione:
cov.scales = lavaan::cor2cov(R = cor(interest.scales),sds = sqrt(vars.scales))
cor.scales = cov2cor(V = cov.scales)

# Prima rappresentazione grafica delle correlazioni a coppia:
psych::pairs.panels(x = interest.scales)

# Lungo la diagonale del grafico abbiamo l'istogramma di frequenze e quello perequato delle variabili/scale, nel triangolo inferiore del grafico abbiamo i grafici a dispersione bivariati 
# con la stima del modello di relazione, infine nel triangolo superiore abbiamo i coeff di correlazione bivariati (calcolati con la formula di Pearson)

# Per quantificare l'associazione quando le variabili non sono continue, possiamo usare le misure di correlazione per ranghi:
# Spearman's rank correlation coefficient and Kendall's rank correlation coefficient (τ) measure the extent to which, as one variable increases, the other variable tends to increase, 
# without requiring that increase to be represented by a linear relationship. If, as the one variable increases, the other decreases, the rank correlation coefficients will be negative.
# https://en.wikipedia.org/wiki/Correlation_and_dependence#Rank_correlation_coefficients
# Facciamo un esempio prendendo le variabili concomitanti del nostro dataset:
cor(x = interest.cmts$educ,interest.cmts$age,method = "spearman")
cor(x = interest.cmts$educ,interest.cmts$age,method = "kendall")



# [B] Correlazione parziale -----------------------------------------------
# Given three or more variables, an interesting question to ask is what is the relationship 
# between x and y when the effect of z has been removed? In the correlational case, it is likely that z and x are correlated. 
# A solution is to consider linear regression to predict x and y from z and to correlate the residuals.
# Partial correlations are used when arguing that the effect of x on y either does or does remain when other variables, z are statistically “controlled”.
# https://en.wikipedia.org/wiki/Partial_correlation

# Calcoliamo la correlazione tra vocab e reading controllando/parzializzando per educ (anni di scolarità)
vocab.educ.residual = residuals(lm(interest.scales$vocab~interest.cmts$educ))
reading.educ.residual = residuals(lm(interest.scales$reading~interest.cmts$educ))
cor(vocab.educ.residual,reading.educ.residual) #correlazione parziale calcolata sui residui di vocab e reading regresse sul confounder educ
cor(interest.scales$vocab,interest.scales$reading)

# Possiamo notare come la correlazione parziale risulti più bassa della correlazione semplice a ragione del datto che le due variabili covariano 
# con una terza variabile che agisce da confounder.
# Allo stesso modo possiamo calcolare correlazioni parziali rispetto ad altre variabili concomitanti, ad esempio age


# [C] Grafici per matrici di correlazione -------------------------------------

# Un modo alternativo e grafico per rappresentare matrici di correlazioni (soprattuto quando le dimensioni sono grandi)
library(corrplot)

corrplot(cor.scales, method = "circle")
# Grandezza dei cerchi e colore sono in relazione alla magnitudine della correlazione

corrplot(cor.scales, method = "square") #..usando i quadratini
# Grandezza dei quadrati e colore sono in relazione alla magnitudine della correlazione

corrplot(cor.scales, method = "color") #..usando solo il colore
# Il colore sono in relazione alla magnitudine della correlazione

# In generale notiamo che le variabili {vocab,reading,sentcomp} sono tra loro abbastanza correlate (colore blu scuro), analogamente {geometry,analyrea}.
# Le variabili sociabty e worry invece non correlano con il resto delle variabili (colore chiaro tendente al bianco).

corrplot.mixed(corr = cor.scales, upper = "circle") #..usando cerchi e numeri

# E' possibile anche ordinare le correlazioni graficamente in modo che quelle più alte siano più vicine tra loro:
corrplot(cor.scales, method = "circle",order="hclust") #..usando un metodo di clustering gerarchico per l'ordinamento
corrplot(cor.scales, method = "circle",order="FPC") #..usando un metodo di decomposizione in componenti principali (PCA) per l'ordinamento

# oppure semplicemente usando l'ordine alfabetico (senza tener conto della magnitudine delle correlazioni):
corrplot(cor.scales, method = "circle",order="alphabet") 

# Le visualizzazioni possono essere personalizzate in modi diversi. Si veda il tutorial della libreria:
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

# Un modo ulteriore è quello che combina il grafico heatmap insieme al clustering gerarchico:
heatmap(x = cor.scales,symm = TRUE)

# ..cambiando il gradiente del colore:
col = colorRampPalette(c("white", "red", "orange"))(NCOL(cor.scales)) #white: zero cor; red: negative cor; organe: positive cor
heatmap(x = cor.scales,symm = TRUE,col=col)

# Il dendrogramma posto sopra i margini del grafico indica i raggruppamenti (cluster) ottenuti sulle similarità delle correlazioni mediante clustering

# E' possibile anche visualizzare una matrice di correlazione mediante un grafico a rete
library(qgraph)
qgraph(input = cor.scales,minimum=0.05,vsize=10,legend=FALSE,borders=TRUE) 

# I nodi connessi indicano presenza di correlazione, colore e spessore dei nodi rappresentano la magnitudine della correlazione
# Possiamo anche raggruppare i nodi della rete rispetto al gruppo di appartenenza delle variabili, ad esempio rispetto alle aree di indagine delle scale.
# Poniamo: {vocab,reading,sentcomp} = area_A, {geometry,analyrea} = area_B, {worry,sociabty} = area_C

groups = list(A=c(1:3),B=c(4:5),C=c(6:7))
qgraph(input = cor.scales,minimum=0.05,vsize=10,legend=TRUE,borders=TRUE,groups=groups) #raggruppamenti per colore

# ..oppure possiamo usare un raggruppamento basato sul clustering gerarchico delle variabili con metodo di Ward
hclust.scales = hclust(d = dist(cor.scales),method = "ward.D2")
plot(hclust.scales) #dendrogramma del clustering effettuato sulla matrice delle distanze tra le variabili (trasformazione della matrice di correlazione)
groups2 = cutree(hclust.scales,k = 2) #decidiamo 2 gruppi

groups2 = list(A=which(groups2==1),B=which(groups2==2)) #creiamo la lista dei gruppi per la funzione qgraph
qgraph(input = cor.scales,minimum=0.05,vsize=10,legend=TRUE,borders=FALSE,groups=groups2,theme="classic") #raggruppamenti per colore

# Grafico finale per le due reti
par(mfrow=c(1,2))
qgraph(input = cor.scales,minimum=0.0,vsize=10,legend=TRUE,borders=TRUE,groups=groups,layout="spring")
qgraph(input = cor.scales,minimum=0.0,vsize=10,legend=TRUE,borders=FALSE,groups=groups2,layout="spring") # le due reti differiscono per raggruppamento



# [D] Comparare matrici di correlazione -----------------------------------
library(dendextend)
# https://cran.r-project.org/web/packages/dendextend/vignettes/introduction.html
# La comparazione grafica tra matrici di correlazione può avvenire in diversi modi. Qui utilizzeremo la comparazione tra dendrogrammi ottenuti
# sulle matrici di correlazione.
# Compariamo le matrici di correlazione sulle alcune variabili (p=22) del nostro dataset interest, classificate per maschi e femmine 

matA = cor(interest[interest$gender==1,c(2:22)]) #matrice di correlazione per femmine
matB = cor(interest[interest$gender==2,c(2:22)]) #matrice di correlazione per maschi

# Clustering gerarchico con il metodo di Ward
hclust.matA = hclust(d = dist(matA),method = "ward.D2") 
hclust.matB = hclust(d = dist(matB),method = "ward.D2")

# Creazione dei dendrogrammi
dendA = as.dendrogram(hclust.matA)
dendB = as.dendrogram(hclust.matB)

# Visualizzazione del tanglegram
# A tanglegram plot gives two dendrogram (with the same set of labels), one facing the other, 
# and having their labels connected by lines. Tanglegram can be used for visually comparing two methods of Hierarchical clustering.
tanglegram(dendA,dendB,main_left = "femmine",main_right = "maschi")

# ..quanto sono allineati/simili i nostri dendrogrammi?
entanglement(dendA,dendB) # approx 1: scarso allineamento (approx 0: ottimo allineamento)

# Le strutture che sottendono le nostre variabili tra femmine e maschi sono poco simili, come evidenziato dal basso indice di entanglement e dal
# grafico tipo tanglegram (gli archi che connettono le variabili del dendrogramma si incrociano).
# Le matrici di correlazione tra femmine e maschi sulle variabili cognitive e di personalità sono differenti.

# Ulteriore confronto con qgraph()
# E' possibile visualizzare matrici di correlazione di maschi e femmine a confronto usando la visualizzazione a rete, come visto nella sezione [C]:
par(mfrow=c(1,2))
qgraph(input = matA,minimum=0.0,vsize=10,legend=FALSE,borders=TRUE,layout="spring",title="Femmine")
qgraph(input = matB,minimum=0.0,vsize=10,legend=FALSE,borders=TRUE,layout="spring",title="Maschi") 








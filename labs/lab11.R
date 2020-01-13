#######################################################
### Testing psicologico (PSP6075525)
### A.A. 2019/2020
### prof. Antonio Calcagnì (antonio.calcagni@unipd.it)
### dott. Claudio  Zandonella Callegher (claudio.zandonellacallegher@phd.unipd.it)
#######################################################

## CONTENUTO DEL CODICE ##################################
# (A) BFI: preparazione dati
# (B) BFI: modelli e stime
# (C) BFI: analisi dei profili
##########################################################


# Inizializzazione ambiente di lavoro -------------------------------------
rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2019_2020/testing_psicologico/")
library(lavaan); library(semPlot); library(psych)



# [A] BFI: preparazione dati ----------------------------------------------

source("laboratorio/lab10.R") # Recuperiamo quanto fatto nel laboratorio precedente, soprattutto i dati sottoposti a pre-processing (es.: reversing items negativi)

# I dati sono misurati su scale likert e sono espressi da variabili categoriali ordinali. Per poter utilizzare la CFA confermativa occorre:
# 1) trasformare i dati in quantitativi (procedura che si chiama "optimal scaling")
# 2) utilizzare un algoritmo di stima per i parametri della CFA adatto per modellare le associazioni tra variabili categoriali
# In questo corso utilizzeremo il caso (2) senza approfondirne le parti teniche. Per informazioni più dettagliate si consulti BN(4.3).
# La libreria lavaan permette di utilizzare dati categoriali ordinali mediante 
#
#                                             cfa(...,ordered=c("item_1",...,"item_p"),estimator="DWLS")
#
# dove il parametro ordered=c(..) permette di specificare quali items sono categoriali ordinati (nel nostro esempio, tutti) mentre estimator="DWLS" specifica il tipo
# di algoritmo utilizzato per la stima dei parametri del modello CFA quando le variabili di input sono categoriali ordinate.

# Lavoriamo sul 50% restante del dataset: bfi_B

# Prima di definire il modello ed adattarlo ai dati, nel caso di variabili categoriali, occorre che queste siano definite come "ordered factors"
str(bfi_B,1)
# Attualmente le variabili nel dataframe bfi sono definite come numeriche e/o intere. Per ciascuna delle 25 variabili dobbiamo applicare una trasformazione per renderle categoriali ordinate in modo che lavaan possa riconoscerle come tali.

# Facciamo un ciclo evitando di riscrivere a mano 25 volte la trasformazione:
bfi.ord = bfi_B # d'ora innanzi lavoriamo su bfi.ord che è lo stesso di bfi_B e contiene variabili dichiarate come ordinali
for(j in 1:25){
  bfi.ord[,j] = factor(bfi.ord[,j],ordered = TRUE)
}
str(bfi.ord,1) #visualizziamo la nuova struttura
# Ora le variabili osservate sono tutte definite come categoriali ordinate (ordered factors). Trasformiamo anche le variabili "gender" e "education" in categoriali:
bfi.ord$gender = as.factor(bfi.ord$gender)
bfi.ord$education = as.factor(bfi.ord$education)



# [B] BFI: modelli e stime ------------------------------------------------

# modello teorico del bfi a 5 fattori 
bfi.model0 = "piacevolez=~A1+A2+A3+A4+A5 \n coscienzios=~C1+C2+C3+C4+C5 \n estrovers=~E1+E2+E3+E4+E5 \n emozion=~N1+N2+N3+N4+N5 \n apertur=~O1+O2+O3+O4+O5"

# modello ottenuto via hclust tipo Ward
bfi.model1 = "f1=~N1+N2+N3+N4+N5 \n f2=~C1+C2+C3+C4+C5+O1+O2+O3+O4+O5 \n f3=~E1+E2+E3+E4+E5+A1+A2+A3+A4+A5"

# modello ottenuto via hclust tipo Complete linkage (prima soluzione)
bfi.model2a = "f1=~C1+C2+C3+C4+C5 \n f2=~N1+N2+N3+N4+N5 \n f2=~A1+A2+A3+A4+A5+E1+E2+E3+E4+E5 \n f3=~O1+O2+O3+O4+O5"

# modello ottenuto via hclust tipo Complete linkage (seconda soluzione)
bfi.model2b = "f1=~O1+O2+O3+O4+O5 \n f2=~N1+N2+N3+N4+N5 \n f2=~C1+C2+C3+C4+C5+A1+A2+A3+A4+A5+E1+E2+E3+E4+E5"

# stima dei modelli
bfi.cfa0 = cfa(model = bfi.model0,data = bfi.ord[,1:25],ordered = names(bfi.ord)[1:25],estimator="DWLS")
bfi.cfa1 = cfa(model = bfi.model1,data = bfi.ord[,1:25],ordered = names(bfi.ord)[1:25],estimator="DWLS")
bfi.cfa2a = cfa(model = bfi.model2a,data = bfi.ord[,1:25],ordered = names(bfi.ord)[1:25],estimator="DWLS")
bfi.cfa2b = cfa(model = bfi.model2b,data = bfi.ord[,1:25],ordered = names(bfi.ord)[1:25],estimator="DWLS")

# valutazione dell'adattamento ai dati (fit dei modelli) -- nota: AIC non disponibile quando estimator="DWLS"
bfi.fits = matrix(NA,4,5) #matrice per i risultati dei fit dei modelli
bfi.fits[1,] = fitmeasures(object = bfi.cfa0,fit.measures = c("RMSEA","CFI","chisq","df","npar"))
bfi.fits[2,] = fitmeasures(object = bfi.cfa1,fit.measures = c("RMSEA","CFI","chisq","df","npar"))
bfi.fits[3,] = fitmeasures(object = bfi.cfa2a,fit.measures = c("RMSEA","CFI","chisq","df","npar"))
bfi.fits[4,] = fitmeasures(object = bfi.cfa2b,fit.measures = c("RMSEA","CFI","chisq","df","npar"))
colnames(bfi.fits) = c("RMSEA","CFI","chisq","df","npar")
rownames(bfi.fits) = c("model0","model1","model2a","model2b")

print(bfi.fits)
# Il modello con il peggior RMSEA è model2b ottenuto dalla seconda soluzione di hclust con metodo Complete Linkage mentre il modello che sembra meglio adattarsi 
# ai dati in termini di RMSEA è model0, quello teorico del bfi a 5 fattori lantenti. 

summary(bfi.cfa0,standardized=TRUE)
# Il summary del modello ora contiene ulteriori campi (es.: "Thresholds") che sono prodotti dall'algoritmo di stima per variabili categoriali ordinate. Per semplicità posono essere per ora tralasciati.

# Grafico di model0
semPaths(bfi.cfa0,nCharNodes = 3,what = "model", whatLabels = "std.all",edge.label.cex = 1.5,edge.color = "black",sizeMan = 7,sizeLat=8,style = "lisrel",nDigits = 1,intercepts = FALSE,thresholds = FALSE)

# Estrazione delle matrici del modello 
A = inspect(object = bfi.cfa0,what = "std.all")

A$lambda #Lambda
A$theta #Theta_delta
A$psi #Phi

# Calcolo dei punteggi fattoriali eta_hat
bfi.eta = lavPredict(object = bfi.cfa0,type = "lv",method = "regression")




# [C] BFI: analisi dei profili --------------------------------------------

# Calcoliamo i profili medi dei 5 fattori misurati rispetto al genere. Per facilitare il calcolo possiamo usare comodamente la funzione aggregate():
# medie:
bfi.aggreg.gender = aggregate(bfi.eta,list(bfi.ord$gender),mean)
bfi.aggreg.educ = aggregate(bfi.eta,list(bfi.ord$education),mean)
# varianze:
bfi.aggreg.gender_var = aggregate(bfi.eta,list(bfi.ord$gender),var)
bfi.aggreg.educ_var = aggregate(bfi.eta,list(bfi.ord$education),var)

# Grafico 4x4 per i profili: in riga le variabili categoriali {gender, educ}, in colonna medie e varianze dei profili
x11(); par(mfrow=c(2,2))

plot(1:5,bfi.aggreg.gender[1,2:6],type="b",bty="n",ylim=c(-0.15,0.15),xlab="fattori latenti",ylab="medie",main="profili per genere")
points(1:5,bfi.aggreg.gender[2,2:6],type="b",col=4,lty=2)
legend("topleft", legend=c("maschi", "femmine"),col=c(1,4), lty=c(1,2))

plot(1:5,bfi.aggreg.gender_var[1,2:6],type="b",bty="n",ylim=c(-0.1,0.8),xlab="fattori latenti",ylab="varianze",main="profili per genere")
points(1:5,bfi.aggreg.gender_var[2,2:6],type="b",col=4,lty=2)
legend("topleft", legend=c("maschi", "femmine"),col=c(1,4), lty=c(1,2))

plot(1:5,bfi.aggreg.educ[1,2:6],type="b",bty="n",ylim=c(-0.15,0.3),xlab="fattori latenti",ylab="medie",main="profili per educ")
for(i in 2:5){
  points(1:5,bfi.aggreg.educ[i,2:6],type="b",col=i,lty=i)  
}
legend("topleft", legend=rownames(bfi.aggreg.educ),col=c(1:5),lty=c(1:5))

plot(1:5,bfi.aggreg.educ_var[1,2:6],type="b",bty="n",ylim=c(-0.1,0.95),xlab="fattori latenti",ylab="varianze",main="profili per educ")
for(i in 2:5){
  points(1:5,bfi.aggreg.educ_var[i,2:6],type="b",col=i,lty=i)  
}
legend("topleft", legend=rownames(bfi.aggreg.educ),col=c(1:5),lty=c(1:5))



bfi.age = rep(0,NROW(bfi.eta)) #0: ragazzi
bfi.age[bfi.ord$age>=21 & bfi.ord$age<=30] = 1 #1: giovani
bfi.age[bfi.ord$age>30] = 2 #2: adulti
table(bfi.age)

x11()
bfi.aggreg.age = aggregate(bfi.eta,list(bfi.age),mean)
plot(1:5,bfi.aggreg.age[1,2:6],type="b",bty="n",ylim=c(-0.15,0.2),xlab="fattori latenti",ylab="medie",main="profili per age")
points(1:5,bfi.aggreg.age[2,2:6],type="b",col=2,lty=2)  
points(1:5,bfi.aggreg.age[3,2:6],type="b",col=3,lty=3)  
legend("topleft", legend=c("ragazzi","giovani","adulti"),col=c(1:5),lty=c(1:5))








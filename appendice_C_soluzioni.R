###########################################################################
#### Soluzioni agli esercizi presenti in Appendice C della dispensa ######
###########################################################################

rm(list=ls())

#--------   Scrittura espressioni ------

# 1

((45+21)^3+3/4)/sqrt(32-12/17)

# 2

(sqrt(7-pi)/(3*(45-34)))

# 3

(12-exp(1)^2)^(1/3)+log(10*pi)

# 4

(sin(3/4*pi)^2+cos(3/2*pi))/log(exp(1)^(3/2),base = 7)

# 5
sum(1:10)/10



#---------  Creazione di vettori  -----

# 1
x<-c(4,6,12,34,8)

# 2
y<-seq(from = 2, to = 24, by=2)

#3
z<-seq(from = 14, by=7,  length.out = 10)

#4
s<-rep(c("A","B","C"), times=4)

#5
t<-rep(c("A","B","C"), each=4)


#---------- Selezione di vettori  ------

# 1
x[c(2,3,5)]

# 2
y[y<13 | y>19]

#3
z[z>24 & z<50]

#4
z[-c(3,5)]

#5
s[s=="A"]

#6
t[t!="B"]

#---------  Operazioni tra vettori ------

# 1
j<-c(x,z)

# 2
j<-j[-(13:15)]
length(j)==length(y)

#3
y+j

#4
z*3

#5
y[1:10]*z


#---------  Creazione di matrici  -----

# 1
A<-matrix(data=c(2,34,12,7,46,93,27,99,23,38,7,04),
          ncol=4, nrow=3, byrow=TRUE)

# 2
B<-matrix(data=seq(from=1, by=2,length.out = 12),
          ncol=3, nrow=4, byrow=TRUE)

#3
C<-matrix(data=seq(from=9, by=9,length.out = 12),
          ncol=4, nrow=3, byrow=TRUE)

#4
D<-matrix(data=rep(c("A","B","C"), 4),
          ncol=3, nrow=4, byrow=TRUE)

#5
E<-matrix(data=rep(c("A","B","C"), 4),
          ncol=4, nrow=3)


#---------- Selezione di matrici  ------

# 1
A[2,3]

#2
B[2:4,2:3]

# 3
A[A%%2==0]

#4
C[-3,-3]

#5
B[2:3,]

#6
D[D!="B"]

#---------  Operazioni tra matrici ------

# 1
G<-cbind(A,C[,1:2])

# 2
H<-rbind(C, t(B)[1:2,])

#3
A<-A[,-2]
B<-B[-1,]
dim(A)==dim(B)

#4
A*B
B*A
A%*%B
B%*%A

# 2*7+12*13+7*19

#5
colnames(C)<-paste0("col_",1:4)
rownames(C)<-paste0("row_",1:3)


#---------  Creazione di DataFrames  -----

# 1
data_wide<-data.frame(Id=c("subj_1","subj_2","subj_3"),
                      age=c(21,23,19),
                      sex=c("F","M","F"),
                      item_1=c(2,1,1),
                      item_2=c(0,2,1),
                      item_3=c(2,0,1))

# 2
data_long<-data.frame(Id=rep(c("subj_1","subj_2","subj_3"),each=3),
                      age=rep(c(21,23,19),each=3),
                      sex=rep(c("F","M","F"),each=3),
                      item=rep(1:3,3),
                      response=c(2,1,1,0,2,1,2,0,1))


#---------- Selezione di DataFrames  ------

# 1
data_long[4:6,c(4,5)]

#2
data_long[data_long$Id=="subj_2", c("item", "response")]

# 3
data_wide[data_wide$item_1==1, c("Id","sex")]

#4
data_long[data_long$sex=="F" & data_long$age>20, ]

#5
data_long[-(4:6),-3]


#---------  Funzioni DataFrame ------

# 1
data_wide$memory_pre<-c(3,2,1)
data_long$memeory_pre<-rep(c(3,2,1),each=3)

# 2
data_wide$gruppo<-c("trattamento","trattemento","controllo")
data_long$gruppo<-rep(c("trattamento","trattemento","controllo"),each=3)

#3
data_new<-data.frame(Id=c("subj_4","subj_5"),
                     age=c(25,22),
                     sex=c("F","M"),
                     item_1=c(1,1),
                     item_2=c(0,1),
                     item_3=c(2,0),
                     memory_pre=c(1,3),
                     gruppo=c("trattemento","controllo"))

data_wide<-rbind(data_wide, data_new)

#4
data_wide$memory_post<-with(data_wide, item_1+item_2+item_3)

#5
colnames(data_wide)[4:6]<-paste0("problem_", 1:3) 
data_wide

#---------  Creazione di Liste  -----

# 1
esperimento_1<-list(data_wide=data_wide,
                    A=A,x=x,
                    info="Hello world!")

# 2
esperimento_2<-list(data_long,
                    C,y,
                    info="Prima racolta dati")


#---------- Selezione di Liste  ------

# 1
esperimento_1[[1]][c(1,4),c("age","sex","gruppo")]

#2
esperimento_1$data_wide[c(1,4),c("age","sex","gruppo")]

# 3
esperimento_2[[c(1,3)]]

# r select multiple object from a list
# https://stackoverflow.com/questions/12119019/select-multiple-elements-from-a-list
esperimento_2[c(1,3)]

#4
names(esperimento_2)<-c("dati_esperimento", "matrice_VCV", "codici_Id", "note")
esperimento_2

#---------- Fattori ---------

#1
sex<-factor(c(rep(c("M","F"),3),"F","F","M"))
sex

#2
levels(sex)<-c("donne","uomini")
sex

#3
intervento<-factor(c(rep(c("CBT","Psicanalisi"),3),"Controllo","Controllo","CBT"))
intervento

#4
intervento[7:8]<-"Farmaci"

levels(intervento)<-c(levels(intervento), "Farmaci")
intervento[7:8]<-"Farmaci"

#5
trattamento<-as.factor(c(as.character(intervento),c("Farmaci","Controllo","Farmaci")))

#-------

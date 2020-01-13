rm(list=ls()); graphics.off()
setwd("~/MEGA/Lavoro_sync/Didattica/2019_2020/testing_psicologico/")
library(ggplot2); library(latex2exp); library(gridExtra);
library(lavaan); library(lavaanPlot); library(xtable)
gg_opts = theme_bw() + theme(panel.border = element_blank(),axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),
                             plot.title = element_text(size=18))
set.seed(140719)

# [A] Misurazione ---------------------------------------------------------

#Figura 1
x = rnorm(n = 50,mean = 20.1)
data_plot = data.frame(x=x,iid=factor(1:length(x)))
g1 = ggplot(data=data_plot,aes(x=1:length(x),y=x,col=iid)) + geom_point(size=4,show.legend = FALSE) + xlab("sfere") + ylab("distanza") + gg_opts
g2 = ggplot(data=data_plot,aes(x=x)) + geom_histogram(color="black", fill="lightgray",position="stack",bins = 7) + xlab("distanza") + ylab("frequenze") + gg_opts
cowplot::plot_grid(g1,g2,ncol = 2)
ggsave(filename = "lezioni/Figura/fig1.pdf",device = "pdf",width = 8.5,height = 4)

#Figura 2
n=100
data_plot = data.frame(x=c(rnorm(n,10,0.5),rnorm(n,10,1.6),rnorm(n,2.5,0.8),rnorm(n,2.5,2.1)),var=factor(rep(LETTERS[1:4],each=n)))
ggplot(data=data_plot,aes(x=x)) + geom_histogram(color="black", fill="lightgray",position="stack",bins = 25) + xlab("misure") + ylab("frequenze") + geom_vline(xintercept = 10,col="red",linetype=2) + gg_opts +
  facet_grid(~var,scales = "free_y")
ggsave(filename = "lezioni/Figura/fig2.pdf",device = "pdf",width = 8.5,height = 4)

#Figura 3
n=100; sdx=2
data_plot = data.frame(x=c(rnorm(n,10,sdx),rnorm(n,12,sdx),rnorm(n,15,sdx),rnorm(n,13,sdx)),var=factor(rep(paste0("t",1:4),each=n)))
ggplot(data=data_plot,aes(y=x,x=var)) + geom_boxplot(color="black", fill="lightgray") + xlab("tempo") + ylab("misure") + geom_hline(yintercept = 10,col="red",linetype=2) + gg_opts
ggsave(filename = "lezioni/Figura/fig3.pdf",device = "pdf",width = 8.5,height = 4)

#Figura 4
n=100; mux=10
data_plot = data.frame(x=c(rnorm(n,mux,1),rnorm(n,mux,3),rnorm(n,mux,6),rnorm(n,mux,2)),var=factor(rep(paste0("t",1:4),each=n)))
ggplot(data=data_plot,aes(y=x,x=var)) + geom_boxplot(color="black", fill="lightgray") + xlab("tempo") + ylab("misure") + geom_hline(yintercept = 10,col="red",linetype=2) + gg_opts
ggsave(filename = "lezioni/Figura/fig4.pdf",device = "pdf",width = 8.5,height = 4)

#Figura 5
n=100; 
data_plot = data.frame(x=c(rnorm(n,8,1),rnorm(20,1.2,0.2)))
ggplot(data=data_plot,aes(x=x)) + geom_histogram(color="black", fill="lightgray",position="stack",bins = 25) + xlab("misure") + ylab("frequenze") + geom_vline(xintercept = 8,col="red",linetype=2) + gg_opts
ggsave(filename = "lezioni/Figura/fig5.pdf",device = "pdf",width = 8.5,height = 4)

#Figura 6
n=100; 
data_plot = data.frame(x=c(rnorm(n,8,1),rnorm(n/2,-1.2,0.1)))
g1 = ggplot(data=data_plot,aes(x=x)) + geom_histogram(color="black", fill="lightgray",position="stack",bins = 25) + xlab("misure") + ylab("frequenze") + 
  geom_vline(xintercept = mean(data_plot$x),col="blue",linetype=2) + geom_vline(xintercept = 8,col="red",linetype=2) + gg_opts
data_plot = data.frame(x=c(rnorm(n,8,0.2),rnorm(n,6.5,0.4)),var=rep(LETTERS[1:2],each=n))
g2 = ggplot(data=data_plot,aes(x=x)) + geom_histogram(color="black", fill="lightgray",position="stack",bins = 25) + xlab("misure") + ylab("frequenze") + 
  geom_vline(xintercept = mean(data_plot$x),col="blue",linetype=2) + gg_opts
cowplot::plot_grid(g1,g2,ncol = 2,labels = "AUTO")
ggsave(filename = "lezioni/Figura/fig6.pdf",device = "pdf",width = 8.5,height = 4)

#Figura 7
n=100; 
data_plot = data.frame(x=c(rnorm(n,2.99,0.35),rnorm(n,3.15,0.25)),var=rep(LETTERS[1:2],each=n))
cvs = with(data_plot,aggregate(x,list(var),function(x)sd(x)/abs(mean(x)))$x)
snrs = with(data_plot,aggregate(x,list(var),function(x)mean(x)/sd(x))$x)
tlt = paste0("CV(A)=",round(cvs[1],3),", CV(B)=",round(cvs[2],3),", SNR(A)=",round(snrs[1],3),", SNR(B)=",round(snrs[2],3))
ggplot(data=data_plot,aes(y=x,x=var)) + geom_boxplot(color="black", fill="lightgray") + xlab("strumenti") + ylab("misure") +
  geom_hline(yintercept = 3,col=2,linetype=2) + ggtitle(tlt) + gg_opts
ggsave(filename = "lezioni/Figura/fig7.pdf",device = "pdf",width = 8.5,height = 4)

#Figura 8
ns=c(500,100,25,10); x=var=c()
for(i in 1:length(ns)){
    x=c(x,rnorm(ns[i]))
    var=c(var,rep(LETTERS[i],each=ns[i]))
}
with(data_plot,aggregate(x,list(var),function(x)sd(x)/sqrt(length(x))))
with(data_plot,aggregate(x,list(var),mean))
data_plot = data.frame(x=x,var=var)
g1 = ggplot(data=data_plot,aes(x=x,col=var,fill=var)) + geom_histogram(aes(y=..count../sum(..count..)),alpha=0.5,position="stack",bins = 15) + xlab("misure") + ylab("frequenze") + gg_opts + theme(legend.title = element_blank())
g2 = ggplot(data=data_plot,aes(x=var,y=x,col=var)) + geom_point(show.legend = FALSE,alpha=0.8) + xlab("campioni") + ylab("misure") + gg_opts + 
    stat_summary(geom = "point",fun.y = "mean",col = "black",size = 3,shape = 21,fill = "darkgrey") + ylim(c(-0.5,0.5)) + geom_hline(yintercept = 0,col=2,linetype=2)
cowplot::plot_grid(g1,g2,ncol = 2,labels = "AUTO")
ggsave(filename = "lezioni/Figura/fig8.pdf",device = "pdf",width = 8.5,height = 4)

#Figura 9
library(EnvStats)
data_plot = data.frame(x=c(x,x),y=c(curve(dunif,-1,2)$y,curve(dtri,-1,2)$y),var=rep(c("rectangular","triangular"),each=101))
ggplot(data=data_plot,aes(x=x,y=y)) + geom_line() + xlab("") + ylab("") + gg_opts + theme(legend.title = element_blank()) +
  facet_wrap(~var)
ggsave(filename = "lezioni/Figura/fig9.pdf",device = "pdf",width = 8.5,height = 4)




# [B] Fondamenti statistici -----------------------------------------------


# Figura 1
x = c(runif(n = 10,min = 1,max = 5),rexp(10,1),rchisq(10,5))
data_plot = data.frame(x = rep(1:10,3),y = c(x[1:10]/sum(x[1:10]),x[11:20]/sum(x[11:20]),x[21:30]/sum(x[21:30])), var = factor(rep(c("PMF1","PMF2","PMF3"),each=10)))
ggplot(data = data_plot,aes(x=x,y=y)) + geom_point() + geom_segment(aes(xend=x),yend=0,size=1.05) + facet_wrap(~var) + scale_x_continuous(breaks = 1:10) +
  gg_opts + xlab("") + ylab("p(X=x)") + theme(panel.background = element_rect(colour = "black", size=0.5, fill=0),
                                             strip.text.x = element_text(size = 14, colour = "black", angle = 0))
ggsave(filename = "lezioni/Figura/fig10.pdf",device = "pdf",width = 8.5,height = 4)

# Figura 2
x1 = rnorm(100); x2 = rbeta(100,shape1 = 5,shape2 = 1); x3 = rchisq(100,1)
x = c(density(x1,n=100)$x,density(x2,n=100)$x,density(x3,n=100)$x); y = c(density(x1,n=100)$y,density(x2,n=100)$y,density(x3,n=100)$y)
data_plot = data.frame(x=x,y=y,var=factor(rep(c("PDF1","PDF2","PDF3"),each=100)))
ggplot(data=data_plot,aes(x=x,y=y)) + geom_line() + facet_wrap(~var,scale="free") +
  xlab("") + ylab(TeX("$f(X)$")) + geom_area(data = data_plot[data_plot$x>1,], aes(x=x,y=y), fill="darkgrey") + 
  gg_opts + theme(panel.background = element_rect(colour = "black", size=0.5, fill=0),strip.text.x = element_text(size = 14, colour = "black", angle = 0))
ggsave(filename = "lezioni/Figura/fig11.pdf",device = "pdf",width = 8.5,height = 4)

# Figura 3
data_plot = data.frame(x=c(x1,x2,x3),var=factor(rep(c("CDF1","CDF2","CDF3"),each=100)))
g1=ggplot(data=data_plot[data_plot$var=="CDF1",],aes(x=x)) + stat_ecdf(pad = TRUE,col="gray") + stat_function(fun=pnorm,color="black",args=list(0,1)) +
  xlab("") + ylab(TeX("$F(X)$")) +gg_opts + geom_area(data = data.frame(x=x1[x1<1],y=pnorm(x1)[x1<1]),aes(x=x,y=y), fill="darkgrey") + ggtitle("CDF1")
g2=ggplot(data=data_plot[data_plot$var=="CDF2",],aes(x=x)) + stat_ecdf(pad = TRUE,col="gray") + stat_function(fun=pbeta,color="black",args=list(5,1)) +
  xlab("") + ylab(TeX("$F(X)$")) +gg_opts + geom_area(data = data.frame(x=x2[x2<0.9],y=pbeta(x2,5,1)[x2<0.9]),aes(x=x,y=y), fill="darkgrey")+ ggtitle("CDF2")
g3=ggplot(data=data_plot[data_plot$var=="CDF3",],aes(x=x)) + stat_ecdf(pad = TRUE,col="gray") + stat_function(fun=pchisq,color="black",args=list(1)) +
  xlab("") + ylab(TeX("$F(X)$")) +gg_opts + geom_area(data = data.frame(x=x3[x3<1],y=pchisq(x3,1)[x3<1]),aes(x=x,y=y), fill="darkgrey")+ ggtitle("CDF3")
cowplot::plot_grid(g1,g2,g3,ncol=3)
ggsave(filename = "lezioni/Figura/fig12.pdf",device = "pdf",width = 8.5,height = 4)

# Figura 4
library(rmutil);library(gridExtra)
N=500
data_plot = data.frame(x=c(runif(N),rnorm(N),rexp(N),rchisq(N,df=5),rmutil::rlaplace(N)),dist=rep(c("uniforme","normale","esponenziale","chi-quadrato","laplace"),each=N),
                       dist2=rep(c("dunif","dnorm","dexp","dchisq","dlaplace"),each=N))
data_plot$dist2 = as.character(data_plot$dist2)


gglist=list()
datax = data_plot[data_plot$dist==levels(data_plot$dist)[1],]
gglist[[1]] = ggplot(data=datax,aes(x=x)) + stat_function(fun=datax$dist2[1],color="black",args=list(df=5)) + xlab("") + ylab(TeX("$f(X)$")) +gg_opts + ggtitle(datax$dist[1])
for(i in 2:5){
  datax = data_plot[data_plot$dist==levels(data_plot$dist)[i],]
  gglist[[i]] = ggplot(data=datax,aes(x=x)) + stat_function(fun=datax$dist2[1],color="black") +
    xlab("") + ylab(TeX("$f(X)$")) +gg_opts + ggtitle(datax$dist[1])
}
ggp=do.call("grid.arrange", c(gglist, ncol=3))
ggsave(filename = "lezioni/Figura/fig13.pdf",device = "pdf",width = 8.5,height = 4,plot = ggp)


# Figura 5
rchisq_n = function(n){sapply(1:500,function(x)mean(rchisq(n,k)))}
M=500;n=c(5,10,100,1000);k=6.5;eps=0.3
x=c(); for(j in n){x=c(x,rchisq_n(j))}
data_plot = data.frame(x=x,n=factor(rep(n,each=M)))
ggplot(data=data_plot,aes(x)) + geom_histogram(bins = 25,aes(y=..density..),position = "identity",fill="lightgrey",col="black") + 
  geom_density(alpha=0.3,fill="lightblue") + facet_wrap(~n,scales = "free") + geom_vline(xintercept = c(k-eps,k+eps),show.legend = FALSE,col=2,linetype=2) +
  gg_opts + xlab("") + ylab("")
ggsave(filename = "lezioni/figure/fig14.pdf",device = "pdf",width = 8.5,height = 4)
with(data_plot,aggregate(x,list(n),function(x)sum(x>=k-eps & x<=k+eps)/M))


# Figura 6
m=1000;lx = 1/1.75
n=c(2,5,50,1000)
zetam = function(n,lx){(lx*sum(rexp(n,lx))-n)/sqrt(n)}
DZ = mapply(function(i)replicate(m,zetam(n[i],lx)),1:length(n))
data_plot = data.frame(x=as.vector(DZ),n=rep(n,each=m))
ggplot(data=data_plot,aes(x)) + geom_histogram(bins = 25,aes(y=..density..),position = "identity",fill="lightgrey",col="black") + 
  geom_density(alpha=0.3,fill="lightblue") + facet_wrap(~n,scales = "free") +
  gg_opts + xlab("") + ylab("") + stat_function(fun=dnorm,color="red",linetype=2)
ggsave(filename = "lezioni/Figura/fig15.pdf",device = "pdf",width = 8.5,height = 4)

# Figura 7
ggplot(data=data_plot,aes(x)) + geom_histogram(bins = 25,aes(y=..density..),position = "identity",fill="lightgrey",col="black") + 
  stat_ecdf() + facet_wrap(~n,scales = "free") +
  gg_opts + xlab("") + ylab("") + stat_function(fun=pnorm,color="red",linetype=2)
ggsave(filename = "lezioni/Figura/fig16.pdf",device = "pdf",width = 8.5,height = 4)





# [C] Testing - parte I ---------------------------------------------------

# Figura 1
x = seq(1e-9,1,length.out = 100);y = (2*x)/(1+x)
data_plot = data.frame(x=x,y=y)
ggplot(data = data_plot,aes(x=x,y=y)) + geom_line() + gg_opts + xlab(TeX("$\\rho^2_{YY'}$")) + ylab(TeX("$\\rho^2_{XX'}$")) + geom_abline(linetype=2) + ylim(0,1) + xlim(0,1)
ggsave(filename = "lezioni/Figura/fig17.pdf",device = "pdf",width = 8.5,height = 4)


# Figura 2
m=30; x = seq(3,m); vary = 0.35; vart = 0.45; vare = 0.23; rhoyy = 0.62
var.m = x*vary*(1+(x-1)*rhoyy)
var.t = x^2*vart
var.e = x*vare
data_plot=data.frame(x=rep(x,3),y=c(var.m,var.t,var.e),variance=as.factor(rep(c("VarX","VarT","VarE"),each=length(x))))
ggplot(data=data_plot,aes(x=x,y=log(y),col=variance)) + geom_line() + gg_opts + xlab("m")+ ylab(TeX("$\\log(variance)$"))
ggsave(filename = "lezioni/Figura/fig18.pdf",device = "pdf",width = 8.5,height = 4)

# Figura 3
m=20; x=seq(2,m); rhoxx=seq(0.05,0.95,length.out=10)
rhoxx.m = sapply(rhoxx,function(rhoxx)(x*rhoxx)/(1+(x-1)*rhoxx))
data_plot = data.frame(x=rep(x,length(rhoxx)),y=as.vector(rhoxx.m),rhoxx=as.factor(rep(rhoxx,each=length(x))))
ggplot(data = data_plot,aes(x=x,y=y,col=rhoxx)) + geom_line(linetype=1) + gg_opts + xlab("m")+ ylab(TeX("$\\rho_{XX'm}^2$")) +
  scale_x_continuous(breaks = x) + scale_y_continuous(breaks = round(seq(0,1,length.out = 15),2))
ggsave(filename = "lezioni/Figura/fig19.pdf",device = "pdf",width = 8.5,height = 4)


# Figura 4
n=1000; K=2; c=0
p=seq(from=0.01,to=0.99,length.out = 5) 
datax = matrix(NA,length(p),K+3)
for(k in 1:length(p)){
  x = rbinom(n,K-1,p[k])
  g = (n-sum(x==c))/(K-1)
  h = sum(x==c)/n
  h_cor = max(0,(sum(x==c)-g)/n)
  datax[k,] = c(table(x)/n,h,h_cor,g)
}
data_plot = data.frame(datax,p)
names(data_plot) = c(as.character(seq(0,K-1)),"h","h_cor","g","pi")
data_plot = data.frame(x=rep(c(0,1),each=length(p)),y=as.vector(unlist(data_plot[,1:K])),h=rep(data_plot$h,K),h_cor=rep(data_plot$h_cor,K),
                       g=rep(data_plot$g,K),correct=c,p=as.factor(paste0("p=",rep(round(p,2),K))))

gglist=list()
for(j in 1:length(p)){
  ptxt=paste0("p=",round(p[j],2))
  g0 = ggplot(data=data_plot[data_plot$p==ptxt,],aes(x=x,y=y)) + geom_bar(stat = "identity",fill="lightgray",col="gray") + facet_wrap(~p) + xlab("") + ylab("") + 
    scale_x_continuous(breaks=seq(0,K)) + scale_y_continuous(breaks=c(0,0.5,1)) + gg_opts + theme(strip.text = element_text(size=15))
  tbl = tableGrob(data_plot[data_plot$p==ptxt,c(3:5)][1,], rows=NULL, theme=ttheme_default(colhead=list(fg_params = list(parse=TRUE))))
  gglist[[j]] = cowplot::plot_grid(g0,tbl,nrow = 2,rel_heights = c(4,1))
}
ggp=do.call("grid.arrange", c(gglist, ncol=5))
ggsave(filename = "lezioni/Figura/fig20.pdf",device = "pdf",width = 8.5,height = 3,plot = ggp)

# Figura 5
n=1000; K=3; c=0
p=seq(from=0.01,to=0.99,length.out = 5) 
datax = matrix(NA,length(p),K+3)
for(k in 1:length(p)){
  x = rbinom(n,K-1,p[k])
  g = (n-sum(x==c))/(K-1)
  h = sum(x==c)/n
  h_cor = max(0,(sum(x==c)-g)/n)
  tb = sapply(X = 0:(K-1),function(X){sum(x==X)/n})
  datax[k,] = c(tb,h,h_cor,g)
}
data_plot = data.frame(datax,p)
names(data_plot) = c(as.character(seq(0,K-1)),"h","h_cor","g","pi")
data_plot = data.frame(x=rep(c(0:(K-1)),each=length(p)),y=as.vector(unlist(data_plot[,1:K])),h=rep(data_plot$h,K),h_cor=rep(data_plot$h_cor,K),
                       g=rep(round(data_plot$g,1),K),correct=c,p=as.factor(paste0("p=",rep(round(p,2),K))))

gglist=list()
for(j in 1:length(p)){
  ptxt=paste0("p=",round(p[j],2))
  g0 = ggplot(data=data_plot[data_plot$p==ptxt,],aes(x=x,y=y)) + geom_bar(stat = "identity",fill="lightgray",col="gray") + facet_wrap(~p) + xlab("") + ylab("") + 
    scale_x_continuous(breaks=seq(0,K)) + scale_y_continuous(breaks=c(0,0.5,1)) + gg_opts + theme(strip.text = element_text(size=15))
  tbl = tableGrob(data_plot[data_plot$p==ptxt,c(3:5)][1,], rows=NULL, theme=ttheme_default(colhead=list(fg_params = list(parse=TRUE))))
  gglist[[j]] = cowplot::plot_grid(g0,tbl,nrow = 2,rel_heights = c(4,1))
}
ggp=do.call("grid.arrange", c(gglist, ncol=5))
ggsave(filename = "lezioni/Figura/fig21.pdf",device = "pdf",width = 8.5,height = 3,plot = ggp)


# Figura 6
set.seed(123)
n=75
x1=rnorm(n,4,0.7); x2=rnorm(n,8,1.7); x3=rchisq(n,2); x4=runif(n,1,9)
data_plot = data.frame(x=c(x1,x2,x3,x4),item=as.factor(paste0("item ",rep(1:4,each=n))))

gglist=list()
for(j in 1:4){
  txt=paste0("item ",j)
  gglist[[j]]=ggplot(data=data_plot[data_plot$item==txt,],aes(x=x)) + geom_histogram(bins = 15,col="darkgray",fill="gray") + facet_grid(cols = vars(item)) + gg_opts + xlab("") + ylab("") + 
    theme(strip.text = element_text(size=15)) + geom_vline(xintercept = median(data_plot[data_plot$item==txt,1]),linetype=2,lwd=1.15) + 
    geom_vline(xintercept = 5,col="red",linetype=1,lwd=0.5) + xlim(0,10)
} 
tbx = data.frame(item = 1:4,round(psych::describe(cbind(x1,x2,x3,x4))[c(8,4,5,9,11,12)],2))
tbl = tableGrob(tbx, rows=NULL, theme=ttheme_default(colhead=list(fg_params = list(parse=TRUE))))
ggp=do.call("grid.arrange", c(gglist, ncol=4))
ggp=cowplot::plot_grid(ggp,tbl,ncol = 1,rel_heights = c(4,1)); ggp
ggsave(filename = "lezioni/Figura/fig22.pdf",device = "pdf",width = 10.5,height = 6.5,plot = ggp)



# [D] Testing - parte II --------------------------------------------------

# Figura 1
set.seed(1232)
S = clusterGeneration::genPositiveDefMat(dim = 2,covMethod = "onion",eta = 1)$Sigma
S[lower.tri(S)] = S[lower.tri(S)]*-1; S[upper.tri(S)] = S[upper.tri(S)]*-1
X = mvtnorm::rmvnorm(n = 100,mean = c(0,0),sigma = S)

data_plot = data.frame(x=X[,1],y=X[,2])
g1 = ggplot(data = data_plot,aes(x=x,y=y)) + geom_point(col="orange") + gg_opts + xlab("X1") + ylab("X2") + xlim(c(-8,8)) + ylim(c(-8,8)) + 
  geom_hline(yintercept = 0,linetype=2,col="grey") + geom_vline(xintercept = 0,linetype=2,col="grey") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

S[lower.tri(S)] = S[lower.tri(S)]*-1; S[upper.tri(S)] = S[upper.tri(S)]*-1
X = mvtnorm::rmvnorm(n = 100,mean = c(0,0),sigma = S)

data_plot = data.frame(x=X[,1],y=X[,2])
g2 = ggplot(data = data_plot,aes(x=x,y=y)) + geom_point(col="orange") + gg_opts + xlab("X1") + ylab("X2") + xlim(c(-8,8)) + ylim(c(-8,8)) +
  geom_hline(yintercept = 0,linetype=2,col="grey") + geom_vline(xintercept = 0,linetype=2,col="grey") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

S[lower.tri(S)] = S[lower.tri(S)]*1e-4; S[upper.tri(S)] = S[upper.tri(S)]*1e-4
X = mvtnorm::rmvnorm(n = 100,mean = c(0,0),sigma = S)

data_plot = data.frame(x=X[,1],y=X[,2])
g3 = ggplot(data = data_plot,aes(x=x,y=y)) + geom_point(col="orange") + gg_opts + xlab("X1") + ylab("X2") + xlim(c(-8,8)) + ylim(c(-8,8)) +
  geom_hline(yintercept = 0,linetype=2,col="grey") + geom_vline(xintercept = 0,linetype=2,col="grey") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggp=cowplot::plot_grid(g1,g2,g3,ncol = 3,labels = "AUTO"); ggp
ggsave(filename = "lezioni/Figura/fig23.pdf",device = "pdf",width = 8.5,height = 3,plot = ggp)


# Figura 2
set.seed(1232)
data_plot = data.frame(x=X[,1],y=X[,1]^2+rnorm(100,0,1.5))
g1 = ggplot(data = data_plot,aes(x=x,y=y)) + geom_point(col="orange") + gg_opts + xlab("X1") + ylab("X2") + xlim(c(-8,8)) + ylim(c(-8,8)) + 
  geom_hline(yintercept = 0,linetype=2,col="grey") + geom_vline(xintercept = 0,linetype=2,col="grey") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

data_plot = data.frame(x=X[,1],y=3.5/X[,1]+rnorm(100,0,0.5))
g2 = ggplot(data = data_plot,aes(x=x,y=y)) + geom_point(col="orange") + gg_opts + xlab("X1") + ylab("X2") + xlim(c(-8,8)) + ylim(c(-8,8)) + 
  geom_hline(yintercept = 0,linetype=2,col="grey") + geom_vline(xintercept = 0,linetype=2,col="grey") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

data_plot = data.frame(x=X[,1],y=sin(X[,1])*2.1+rnorm(100,0,0.5))
g3 = ggplot(data = data_plot,aes(x=x,y=y)) + geom_point(col="orange") + gg_opts + xlab("X1") + ylab("X2") + xlim(c(-8,8)) + ylim(c(-8,8)) + 
  geom_hline(yintercept = 0,linetype=2,col="grey") + geom_vline(xintercept = 0,linetype=2,col="grey") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ggp=cowplot::plot_grid(g1,g2,g3,ncol = 3,labels = "AUTO"); ggp
ggsave(filename = "lezioni/Figura/fig24.pdf",device = "pdf",width = 8.5,height = 3,plot = ggp)


# Figura 3
set.seed(12341)

R = as.matrix(clusterGeneration::rcorrmatrix(d = 5,alphad = 1))
colnames(R) = paste0("X",1:5); rownames(R) = paste0("X",1:5)
data_plot = reshape2::melt(R)
g1 = ggplot(data = data_plot, aes(Var1, Var2, fill = value)) + gg_opts + coord_fixed() + xlab("") + ylab("") + 
  geom_tile(color = "white")+ scale_fill_gradient2(low = "darkgrey", high = "orange", mid = "lightgrey", midpoint = 0, limit = c(-1,1), space = "Lab", name=TeX("$P_X$"))

R = as.matrix(clusterGeneration::rcorrmatrix(d = 5,alphad = 1e-9))
colnames(R) = paste0("X",1:5); rownames(R) = paste0("X",1:5)
R[upper.tri(R)] = NA
data_plot = reshape2::melt(R,na.rm=TRUE)
g2 = ggplot(data = data_plot, aes(Var1, Var2, fill = value)) + gg_opts + coord_fixed() + xlab("") + ylab("") + 
  geom_tile(color = "white")+ scale_fill_gradient2(low = "darkgrey", high = "orange", mid = "lightgrey", midpoint = 0, limit = c(-1,1), space = "Lab", name=TeX("$P_X$"))

ggp=cowplot::plot_grid(g1,g2,ncol = 2,labels = "AUTO"); ggp
ggsave(filename = "lezioni/Figura/fig25.pdf",device = "pdf",width = 8.5,height = 4.5,plot = ggp)


# Figura 4

## matrice di correlazione dati di Lawley e Maxwell (1963): sei test scolastici su 220 studenti, 6 materie, 2 fattori ipotizzati
S = matrix(data = c(1,0.439,0.410,0.288,0.329,0.248,0,1,0.351,0.354,0.320,0.329,0,0,1,0.164,0.190,0.181,0,0,0,1,0.595,0.470,0,0,0,0,1,0.464,0,0,0,0,0,1),nrow = 6,ncol = 6,byrow = FALSE)
for(j in 1:6){S[j,] = S[,j]}
isSymmetric(S)
colnames(S) = rownames(S) = c("gaelico","inglese","storia","aritmetica","algebra","geometria")
##

model.maxwell = "eta1 =~ gaelico+inglese+storia"
cfa.maxwell = lavaan::cfa(sample.cov = S,model = model.maxwell,sample.nobs = 220)

pdf(file = "lezioni/Figura/fig26.pdf",width = 200.5,height = 120.5) 
g1=semPlot::semPaths(cfa.maxwell,nCharNodes = 0,what = "model", whatLabels = "std",edge.label.cex = 1.4,edge.color = "black",sizeMan = 10,sizeLat=12,style = "lisrel")
g1$graphAttributes$Nodes$labels = c("X1","X2","X3","eta")
plot(g1)
dev.off()
    

# Figura 5
model.maxwell = "eta1 =~ gaelico+inglese+storia\n eta2 =~ aritmetica+algebra+geometria"
cfa.maxwell = lavaan::cfa(sample.cov = S,model = model.maxwell,sample.nobs = 220)

g1=semPlot::semPaths(cfa.maxwell,nCharNodes = 0,what = "model", whatLabels = "std",edge.label.cex = 1.4,edge.color = "black",sizeMan = 10,sizeLat=12,style = "lisrel")
g1$graphAttributes$Nodes$labels = c("X1","X2","X3","X4","X5","X6","eta1","eta2")
pdf(file = "lezioni/Figura/fig27.pdf",width = 200.5,height = 120.5) 
plot(g1)
dev.off()

# Figura 6
model.maxwell = "eta1 =~ gaelico+inglese+storia\n eta2 =~ inglese+aritmetica+algebra+geometria"
cfa.maxwell = lavaan::cfa(sample.cov = S,model = model.maxwell,sample.nobs = 220)

g1=semPlot::semPaths(cfa.maxwell,nCharNodes = 0,what = "model", whatLabels = "std",edge.label.cex = 1.4,edge.color = "black",sizeMan = 10,sizeLat=12,style = "lisrel")
g1$graphAttributes$Nodes$labels = c("X1","X2","X3","X4","X5","X6","eta1","eta2")
pdf(file = "lezioni/Figura/fig28.pdf",width = 200.5,height = 120.5) 
plot(g1)
dev.off()

# Figura 6
model.maxwell = "eta1 =~ gaelico+inglese+storia+aritmetica+algebra+geometria\n eta2 =~ gaelico+inglese+storia+aritmetica+algebra+geometria"
cfa.maxwell = lavaan::cfa(sample.cov = S,model = model.maxwell,sample.nobs = 220)

g1=semPlot::semPaths(cfa.maxwell,nCharNodes = 0,what = "model",edge.label.cex = 1.4,edge.color = "black",sizeMan = 10,sizeLat=12,style = "lisrel")
g1$graphAttributes$Nodes$labels = c("X1","X2","X3","X4","X5","X6","eta1","eta2")
pdf(file = "lezioni/Figura/fig29.pdf",width = 200.5,height = 120.5) 
plot(g1)
dev.off()

# Figura 7
model.maxwell = "eta1 =~ gaelico+inglese+storia\n eta2 =~ aritmetica+algebra+geometria\n inglese~~storia\n algebra~~geometria"
cfa.maxwell = lavaan::cfa(sample.cov = S,model = model.maxwell,sample.nobs = 220)

g1=semPlot::semPaths(cfa.maxwell,nCharNodes = 0,what = "model", whatLabels = "std",edge.label.cex = 1.4,edge.color = "black",sizeMan = 10,sizeLat=12,style = "lisrel")
g1$graphAttributes$Nodes$labels = c("X1","X2","X3","X4","X5","X6","eta1","eta2")
pdf(file = "lezioni/Figura/fig30.pdf",width = 200.5,height = 120.5) 
plot(g1)
dev.off()

# WISC-IV
S = lavaan::lav_matrix_lower2full(c(
  + 1,
  + .38, 1,
  + .26, .35, 1,
  + .34, .43, .28, 1,
  + .25, .14, .15, .11, 1,
  + .33, .62, .33, .41, .13, 1,
  + .29, .35, .42, .35, .19, .38, 1,
  + .42, .41, .29, .43, .20, .40, .35, 1,
  + .27, .51, .24, .35, .15, .59, .30, .30, 1, 
  + .30, .27, .20, .24, .46, .24, .24, .26, .22, 1))
rownames(S) = colnames(S) = c("DC","SO","MC","CI","CR","VC","LN","RM","CO","RS")

##Primo modello: fattore g unico
model.vci1 = "g=~SO+VC+CO+DC+CI+RM+MC+LN+CR+RS"
cfa.vci1 = cfa(model = model.vci1,sample.cov = S,sample.nobs = 2200)
g1=semPlot::semPaths(cfa.vci1,nCharNodes = 2,what = "model", whatLabels = "std.all",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)
pdf(file = "lezioni/figure/fig31.pdf",width = 200.5,height = 120.5); plot(g1); dev.off()

Lambda = lavaan::inspect(object = cfa.vci1,what="est")$lambda; xtableMatharray(x = Lambda,digits = 2)
Phi = lavaan::inspect(object = cfa.vci1,what="est")$psi; xtableMatharray(x = Phi,digits = 2)
Theta = lavaan::inspect(object = cfa.vci1,what="est")$theta; Theta[upper.tri(Theta)] = NA; xtableMatharray(x = Theta,digits = 2,auto = TRUE)
fitmeasures(object = cfa.vci1,fit.measures = c("RMSEA","CFI","AIC"))

##Secondo modello
model.vci2 = "verbal=~SO+VC+CO \n percep=~DC+CI+RM \n elab=~MC+LN+CR+RS" 
cfa.vci2 = cfa(model = model.vci2,sample.cov = S,sample.nobs = 2200)
g1=semPlot::semPaths(cfa.vci2,nCharNodes = 2,what = "model", whatLabels = "std.all",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)
pdf(file = "lezioni/figure/fig32.pdf",width = 200.5,height = 120.5); plot(g1); dev.off()

Lambda = lavaan::inspect(object = cfa.vci2,what="est")$lambda; xtableMatharray(x = Lambda,digits = 2)
Phi = lavaan::inspect(object = cfa.vci2,what="est")$psi; Phi[upper.tri(Phi)] = NA; xtableMatharray(x = Phi,digits = 2)
Theta = lavaan::inspect(object = cfa.vci2,what="est")$theta; Theta[upper.tri(Theta)] = NA; xtableMatharray(x = Theta,digits = 2,auto = TRUE)
fitmeasures(object = cfa.vci2,fit.measures = c("RMSEA","CFI","AIC"))


##Terzo modello
model.vci3 = "VCI=~SO+VC+CO \n PRI=~DC+CI+RM \n WMI=~MC+LN \n PSI=~CR+RS" 
cfa.vci3 = cfa(model = model.vci3,sample.cov = S,sample.nobs = 2200)
g1=semPlot::semPaths(cfa.vci3,nCharNodes = 3,what = "model", whatLabels = "std.all",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)
pdf(file = "lezioni/figure/fig33.pdf",width = 200.5,height = 120.5); plot(g1); dev.off()

Lambda = lavaan::inspect(object = cfa.vci3,what="est")$lambda; xtableMatharray(x = Lambda,digits = 2)
Phi = lavaan::inspect(object = cfa.vci3,what="est")$psi; Phi[upper.tri(Phi)] = NA; xtableMatharray(x = Phi,digits = 2)
Theta = lavaan::inspect(object = cfa.vci3,what="est")$theta; Theta[upper.tri(Theta)] = NA; xtableMatharray(x = Theta,digits = 2,auto = TRUE)
fitmeasures(object = cfa.vci3,fit.measures = c("RMSEA","CFI","AIC"))
A = modificationindices(object = cfa.vci3,sort. = TRUE); xtable(x = A[1:10,1:4],digits = 2,auto = TRUE,include.rownames=FALSE)
A = lavResiduals(cfa.vci3,add.labels=TRUE,type = "cor.bollen",summary=TRUE)$cov.z; A[upper.tri(A)] = NA; xtableMatharray(x = A,digits = 2)
A = semTools::reliability(cfa.vci3); xtable(x = A[1:2,],digits = 2,auto = TRUE,include.rownames=FALSE)

set.seed(111232111)
Xdata = mvtnorm::rmvnorm(n = 100,sigma = S); colnames(Xdata) = colnames(S)
Ydata = lavPredict(object = cfa.vci3,newdata = Xdata,type = "lv",method = "regression")
b = c(0,runif(n = 4,min = -3,max = 3))
school.perf = cbind(1,Ydata)%*%b + rnorm(n = NROW(Xdata),mean = 0,sd = 1.5)
lm.vci3 = lm(school.perf~Ydata)

data_plot = data.frame(y=rep(school.perf,NROW(Ydata)),x=as.vector(Ydata),var=factor(rep(colnames(Ydata),each=NROW(Ydata))))
g1=ggplot(data=data_plot,aes(x=x,y=y)) + geom_point(col="grey") + geom_smooth(linetype=1,col="orange",method = "lm") + facet_wrap(~var) + gg_opts + ylab("school performance") + xlab(TeX("$\\hat{\\eta}$")) + xlim(c(-1,1))
ggsave(filename = "lezioni/figure/fig35.pdf",device = "pdf",width = 8.5,height = 4.5,plot = g1)

school.succ = ifelse(school.perf>median(school.perf),1,0)
A = reshape2::melt(aggregate(Ydata,list(school.succ),mean))[3:10,]
A = cbind(A,rep(c("0","1"),2))
data_plot = data.frame(x=A$value,var=factor(A$variable),success=A$`rep(c("0", "1"), 2)`)
g1=ggplot(data=data_plot,aes(x=var,y=x,col=success)) + geom_point(size=1.8,show.legend = FALSE) + geom_path(lwd=0.8,linetype=1,aes(group=success)) + gg_opts + xlab("") + ylab("school success")
ggsave(filename = "lezioni/figure/fig36.pdf",device = "pdf",width = 8.5,height = 4.5,plot = g1)

##Quarto modello
model.vci4 = "VCI=~SO+VC+CO \n PRI=~DC+CI+RM \n WMI=~MC+LN \n PSI=~CR+RS \n g=~VCI+PRI+WMI+PSI" 
cfa.vci4 = cfa(model = model.vci4,sample.cov = S,sample.nobs = 2200)
g1=semPlot::semPaths(cfa.vci4,nCharNodes = 3,what = "model", whatLabels = "std.all",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)
pdf(file = "lezioni/figure/fig34.pdf",width = 200.5,height = 120.5); plot(g1); dev.off()

Lambda = lavaan::inspect(object = cfa.vci4,what="est")$lambda; xtableMatharray(x = Lambda,digits = 2)
Phi = lavaan::inspect(object = cfa.vci4,what="est")$psi; Phi[upper.tri(Phi)] = NA; xtableMatharray(x = Phi,digits = 2)
Theta = lavaan::inspect(object = cfa.vci4,what="est")$theta; Theta[upper.tri(Theta)] = NA; xtableMatharray(x = Theta,digits = 2,auto = TRUE)
fitmeasures(object = cfa.vci4,fit.measures = c("RMSEA","CFI","AIC"))
A = modificationindices(object = cfa.vci4,sort. = TRUE); xtable(x = A[1:10,1:4],digits = 2,auto = TRUE,include.rownames=FALSE)
A = lavResiduals(cfa.vci4,add.labels=TRUE,type = "cor.bollen",summary=TRUE)$cov.z; A[upper.tri(A)] = NA; xtableMatharray(x = A,digits = 2)
A = semTools::reliability(cfa.vci4); xtable(x = A[1:2,],digits = 2,auto = TRUE,include.rownames=FALSE)


# Invarianza
set.seed(1241)
S0 = S[c("SO","VC","CO","DC","CI","RM"),c("SO","VC","CO","DC","CI","RM"),drop=FALSE]
Xdata = mvtnorm::rmvnorm(n = 100,sigma = S0); colnames(Xdata) = colnames(S0)
g = rbinom(n = 100,size = 1,prob = 0.5)
Xdata = as.data.frame(cbind(Xdata,factor(g))); colnames(Xdata)[7] = "g"

##configural
model.invariance = "VCI=~SO+VC+CO \n PRI=~DC+CI+RM" 
cfa.conf = cfa(model = model.invariance,data = Xdata,group = "g")
pdf(file = "lezioni/figure/fig37.pdf",width = 200.5,height = 120.5); 
semPlot::semPaths(title = FALSE,ask=FALSE,intercepts = FALSE,panelGroups = TRUE,combineGroups = FALSE,cfa.conf,nCharNodes = 3,what = "model", whatLabels = "est",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)
dev.off()

A = lavaan::inspect(object = cfa.conf,what="est")
Lambda = A$`1`$lambda; xtableMatharray(x = Lambda,digits = 2)
Lambda = A$`2`$lambda; xtableMatharray(x = Lambda,digits = 2)
Phi = A$`1`$psi; Phi[upper.tri(Phi)] = NA; xtableMatharray(x = Phi,digits = 2)
Phi = A$`2`$psi; Phi[upper.tri(Phi)] = NA; xtableMatharray(x = Phi,digits = 2)
Tau = A$`1`$nu; xtableMatharray(x = t(Tau),digits = 2,auto = TRUE)
Tau = A$`2`$nu; xtableMatharray(x = t(Tau),digits = 2,auto = TRUE)
Theta = as.matrix(diag(A$`1`$theta)); xtableMatharray(x = t(Theta),digits = 2,auto = TRUE)
Theta = as.matrix(diag(A$`2`$theta)); xtableMatharray(x = t(Theta),digits = 2,auto = TRUE)
#fitmeasures(object = cfa.conf,fit.measures = c("RMSEA","CFI","AIC"))
A = as.matrix(fitmeasures(object = cfa.conf,fit.measures = c("chisq","df","npar","AIC"))); xtableMatharray(x = t(A),digits = 2,auto = TRUE)

##weak
cfa.weak = cfa(model = model.invariance,data = Xdata,group = "g",group.equal="loadings")
pdf(file = "lezioni/figure/fig38.pdf",width = 200.5,height = 120.5); 
semPlot::semPaths(title = FALSE,ask=FALSE,intercepts = FALSE,panelGroups = TRUE,combineGroups = FALSE,cfa.weak,nCharNodes = 3,what = "model", whatLabels = "est",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)
dev.off()

A = lavaan::inspect(object = cfa.weak,what="est")
Lambda = A$`1`$lambda; xtableMatharray(x = Lambda,digits = 2)
Lambda = A$`2`$lambda; xtableMatharray(x = Lambda,digits = 2)
Phi = A$`1`$psi; Phi[upper.tri(Phi)] = NA; xtableMatharray(x = Phi,digits = 2)
Phi = A$`2`$psi; Phi[upper.tri(Phi)] = NA; xtableMatharray(x = Phi,digits = 2)
Tau = A$`1`$nu; xtableMatharray(x = t(Tau),digits = 2,auto = TRUE)
Tau = A$`2`$nu; xtableMatharray(x = t(Tau),digits = 2,auto = TRUE)
Theta = as.matrix(diag(A$`1`$theta)); xtableMatharray(x = t(Theta),digits = 2,auto = TRUE)
Theta = as.matrix(diag(A$`2`$theta)); xtableMatharray(x = t(Theta),digits = 2,auto = TRUE)
#A = as.matrix(fitmeasures(object = cfa.conf,fit.measures = c("chisq","df","npar","AIC"))); xtableMatharray(x = t(A),digits = 2,auto = TRUE)
A = anova(cfa.conf,cfa.weak)[,c(1:2,4:7)]; xtable(x = A,digits = 2,auto = TRUE)

##strong  
cfa.strong = cfa(model = model.invariance,data = Xdata,group = "g",group.equal=c("loadings","intercepts"))
pdf(file = "lezioni/figure/fig39.pdf",width = 200.5,height = 120.5); 
semPlot::semPaths(title = FALSE,ask=FALSE,intercepts = FALSE,panelGroups = TRUE,combineGroups = FALSE,cfa.strong,nCharNodes = 3,what = "model", whatLabels = "est",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)
dev.off()

A = lavaan::inspect(object = cfa.strong,what="est")
Lambda = A$`1`$lambda; xtableMatharray(x = Lambda,digits = 2)
Lambda = A$`2`$lambda; xtableMatharray(x = Lambda,digits = 2)
Phi = A$`1`$psi; Phi[upper.tri(Phi)] = NA; xtableMatharray(x = Phi,digits = 2)
Phi = A$`2`$psi; Phi[upper.tri(Phi)] = NA; xtableMatharray(x = Phi,digits = 2)
Tau = A$`1`$nu; xtableMatharray(x = t(Tau),digits = 2,auto = TRUE)
Tau = A$`2`$nu; xtableMatharray(x = t(Tau),digits = 2,auto = TRUE)
Theta = as.matrix(diag(A$`1`$theta)); xtableMatharray(x = t(Theta),digits = 2,auto = TRUE)
Theta = as.matrix(diag(A$`2`$theta)); xtableMatharray(x = t(Theta),digits = 2,auto = TRUE)
#A = as.matrix(fitmeasures(object = cfa.conf,fit.measures = c("chisq","df","npar","AIC"))); xtableMatharray(x = t(A),digits = 2,auto = TRUE)
A = anova(cfa.weak,cfa.strong)[,c(1:2,4:7)]; xtable(x = A,digits = 2,auto = TRUE)

##strict
cfa.strict = cfa(model = model.invariance,data = Xdata,group = "g",group.equal=c("loadings","intercepts","residuals"))
pdf(file = "lezioni/figure/fig40.pdf",width = 200.5,height = 120.5); 
semPlot::semPaths(title = FALSE,ask=FALSE,intercepts = FALSE,panelGroups = TRUE,combineGroups = FALSE,cfa.strict,nCharNodes = 3,what = "model", whatLabels = "est",edge.label.cex = 1.2,edge.color = "black",sizeMan = 7,sizeLat=12,style = "lisrel",nDigits = 1)
dev.off()

A = lavaan::inspect(object = cfa.strict,what="est")
Lambda = A$`1`$lambda; xtableMatharray(x = Lambda,digits = 2)
Lambda = A$`2`$lambda; xtableMatharray(x = Lambda,digits = 2)
Phi = A$`1`$psi; Phi[upper.tri(Phi)] = NA; xtableMatharray(x = Phi,digits = 2)
Phi = A$`2`$psi; Phi[upper.tri(Phi)] = NA; xtableMatharray(x = Phi,digits = 2)
Tau = A$`1`$nu; xtableMatharray(x = t(Tau),digits = 2,auto = TRUE)
Tau = A$`2`$nu; xtableMatharray(x = t(Tau),digits = 2,auto = TRUE)
Theta = as.matrix(diag(A$`1`$theta)); xtableMatharray(x = (Theta),digits = 2,auto = TRUE)
Theta = as.matrix(diag(A$`2`$theta)); xtableMatharray(x = t(Theta),digits = 2,auto = TRUE)
#A = as.matrix(fitmeasures(object = cfa.conf,fit.measures = c("chisq","df","npar","AIC"))); xtableMatharray(x = t(A),digits = 2,auto = TRUE)
A = anova(cfa.strong,cfa.strict)[,c(1:2,4:7)]; xtable(x = A,digits = 2,auto = TRUE)





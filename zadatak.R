podaci<-read.table("zad26c.txt",header=TRUE)

pcb<-podaci[,1]
debljina<-podaci[,2]
plot(pcb,debljina,pch=20,col="red",xlab="PCB",ylab="debljina ljuske",main="Kartezijev produkt")


###b dio
n<-length(pcb)
k<-8

min<-min(pcb)
max<-max(pcb)
raspon_pcb<-max-min
sirina<-raspon_pcb/8
sirina<-51
min<-45.5
granice<-numeric(9)
granice[1]<-min
for(i in 1:8)
{granice[i+1]=granice[i]+sirina
  
  
}
granice

p<-hist(pcb,col="red",breaks=granice,probability = TRUE,xlab="pcb",ylab="relativne frekvencije",main="Histogram PCB-a")$counts
mean_pcb<-mean(pcb)

curve(dnorm(x,mean_pcb,sd(pcb)),col="blue",add=TRUE,lwd=2)


teorijske<-numeric(8)
for(i in 2:7)
{
  teorijske[i]<-n*(pnorm(granice[i+1],mean_pcb,sd(pcb))-pnorm(granice[i],mean_pcb,sd(pcb)))

  }
n
teorijske[1]<-n*pnorm(granice[2],mean(pcb),sd(pcb))
teorijske
teorijske[8]<-n*(1-pnorm(granice[8],mean(pcb),sd(pcb)))

p[2]<-sum(p[1:2])
p[6]<-sum(p[6:8])
p<-p[2:6]

teorijske[2]<-sum(teorijske[1:2])
teorijske[6]<-sum(teorijske[6:8])
teorijske<-teorijske[2:6]
teorijske

H
H=sum((p-teorijske)^2/teorijske)
1-pchisq(H,2)
###hist debljina
n<-length(pcb)
k<-8

min<-min(debljina)
max<-max(debljina)
raspon_pcb<-max-min
sirina<-raspon_pcb/8
sirina<-0.05
min<-0.135
granice<-numeric(9)
granice[1]<-min
for(i in 1:8)
{granice[i+1]=granice[i]+sirina


}
granice

d<-hist(debljina,col="blue",breaks=granice,probability = TRUE,xlab="debljina ljuske",ylab="relativne frekvencije",main="Histogram debljine ljuske")$counts
mean_debljina<-mean(debljina)
d
curve(dnorm(x,mean_debljina,sd(debljina)),col="red",add=TRUE,lwd=2)

teorijske<-numeric(8)
for(i in 2:7)
{
  teorijske[i]<-n*(pnorm(granice[i+1],mean(debljina),sd(debljina))-pnorm(granice[i],mean(debljina),sd(debljina)))
  
}
n
teorijske[1]<-n*pnorm(granice[2],mean(debljina),sd(debljina))
sum(teorijske)
teorijske[8]<-n*(1-pnorm(granice[8],mean(debljina),sd(debljina)))

d[2]<-sum(d[1:2])
d[6]<-sum(d[6:8])
d<-d[2:6]
d
sum(d)

teorijske[2]<-sum(teorijske[1:2])
teorijske[6]<-sum(teorijske[6:8])
teorijske<-teorijske[2:6]
sum(teorijske)
H
H=sum((d-teorijske)^2/teorijske)
1-pchisq(H,2)
####normalni grafovi... lagano



#####lillie_test
library(nortest)
lillie.test(pcb)
lillie.test(debljina)

###hi_kvadrat





####procjene parametara
mi_pcb<-mean(pcb)
sigma_pcb<-sd(pcb)
ro<-cor(pcb,debljina,method="pearson")
t<-ro*sqrt(n-2)/sqrt(1-ro^2)
pt(t,n-2)






####izohipse
vjerojatnosti <- seq(6/65, 60/65, 6/65)
qchisq(vjerojatnosti, df=2)
r <- sqrt(qchisq(vjerojatnosti, df=2))
C = matrix(0,2,2)
C[1,1] <- var(pcb)
C[2,2] <- var(debljina)
C[1,2] <- cov(pcb,debljina)
C[2,1] <- cov(pcb,debljina)
eigen(C)
library(expm)
A <- sqrtm(C)
A %*% t(A)
mi <- c(mean(pcb), mean(debljina))
phi <-  seq(0, 2*pi, length.out = 100)
m =100 
elipse <- function(kut, radijus){
  w <- c(radijus * cos(kut), radijus* sin(kut))
  izlaz <- A %*% w + mi
  return(izlaz)
}

elipse(0,2)
plot(pcb,debljina,type="p",col="blue",xlim=c(-10,500),ylim=c(0.05,0.55),pch=20)
for(i in 1:10){
  tocke <- matrix(0, nrow=100, ncol=2)
  for(j in 1:100)
  {
    tocke[j, ] <- elipse(phi[j], r[i])
  }
  points(tocke[, 1], tocke[, 2], type="l",col="red")
}
B<-solve(A)
matrica<-matrix(c(podaci[,1],podaci[,2]),ncol=2,byrow=FALSE)
for(i in 1:length(pcb))
{matrica[i,]<-B%*%(as.numeric(podaci[i,]-mi))
}
podaci[1,]-mi
prvi
chi<-sqrt(debljina^2+)
matrica-mi
vektor<-numeric(length(debljina))
for(i in 1:65)
{vektor[i]<-sqrt(matrica[i,1]^2+matrica[i,2]^2)
  
  
}
###vektor je matrica translatiranih podataka izraèunata ranije
B<-solve(A)
vjerojatnosti <- seq(6/65, 60/65, 6/65)
r <- sqrt(qchisq(vjerojatnosti, df=2))
ind<-numeric(11)
for(i in 2:10)
ind[i]<-length(which(vektor<r[i] & vektor>r[i-1]))
ind[1]<-length(which(vektor<r[1]))
ind[11]<-length(which(vektor>r[10]))
ind
sum(ind)
teor<-c(6,6,6,6,6,6,6,6,6,6,5)
length(ind)
H<-(teor-ind)^2/teor
H=sum(H)
1-pchisq(H,5)

R<-numeric(10)
for (i in 1:10){
   R[i]<-sqrt(-2*log(1-vjerojatnosti[i]))}
R
r


sort_pcb=sort(pcb)
qi<-numeric(length(pcb))
for(i in 1:length(sort_pcb))
{qi[i]<-qnorm((i-3/8)/(length(pcb)+1/4))
  
  
}
qi
plot(qi,sort_pcb,pch=20,col="red",main="Normalni vjerojatnosni graf za PCB")
abline(mean(pcb),sd(pcb),col="blue",lwd=2)

sort_d=sort(debljina)
qi<-numeric(length(debljina))
for(i in 1:length(sort_d))
{qi[i]<-qnorm((i-3/8)/(length(pcb)+1/4))


}
qi
plot(qi,sort_d,pch=20,col="blue",main="Normalni vjerojatnosni graf za debljinu ljuske",ylab="debljina ljuske")
abline(mean(debljina),sd(debljina),col="red",lwd=2)
lillie.test(debljina)
mean(pcb)
sd(pcb)
mean(debljina)
sd(debljina)
n<-length(pcb)
mean(debljina)-qt(0.025,n-1)*sd(debljina)/sqrt(n)
(n-1)*sd(debljina)^2/qchisq(0.025,n-1)



Z=sqrt(n-3)*(1/2*log((1+pearson)/(1-pearson))-1/2*log((1-0.24)/(1+0.24)))

qnorm(0.05,0,1)                                  
pnorm(Z,0,1)

jakost<-function(ro)
{
  
  pnorm(qnorm(0.05,0,1)+sqrt(n-3)/2*(log((1-0.24)/(1+0.24))-log((1+ro)/(1-ro))),0,1)
  
}

curve(jakost,from=-0.95,to=0.95,col="red",main="jakost testa",ylab="jakost",xlab="ro")
abline(0.05,0,col="blue")

pobjeda<-0
for(i in 1:100000)
{x<-runif(3,0,1)
y<-numeric(3)  
if(x[1]<1/4)
{y[1]=x[1]
  if(x[2]>x[1] & x[2]-x[1]>1-x[2])
  {y[3]=x[2]
    y[2]=x[3]
  }
if(x[2]>x[1] & x[2]-x[1]<1-x[2])
{y[3]=x[3]
y[2]=x[2]
}



}
  
if(x[1]>=1/4 & x[1]<=3/4)
{y[2]=x[1]
  if(x[2]>x[1])
  {y[3]=x[2]
    y[1]=x[3]
  }
  if(x[2]<x[1])
  {y[1]=x[2]
  y[3]=x[3]  
  }
}


if(x[1]>3/4)
{y[3]=x[1]
if(x[2]<x[1] & x[1]-x[2]>x[2])
{y[1]=x[2]
y[2]=x[3]
}
if(x[1]>x[2] & x[1]-x[2]<x[2])
{y[1]=x[3]
y[2]=x[2]
}



}
if(sum(sort(x)-y==0)==3)
{pobjeda=pobjeda+1}

}

pobjeda
x
y
proba
proba<-runif(3,0,1)
sort<-sort(proba)
sum(proba-proba==0)
setequal(proba,sort)
y<-numeric(3)



rasporedi<-function(rezultat)
{
  
  
  
}
pobjeda
x
y
pobjeda<-0
for(i in 1:100000)
{x<-runif(3,0,1)
y<-numeric(3)  
if(x[1]<1/3)
{y[1]=x[1]
if(x[2]>x[1] & x[2]-x[1]>1-x[2])
{y[3]=x[2]
y[2]=x[3]
}
if(x[2]>x[1] & x[2]-x[1]<1-x[2])
{y[3]=x[3]
y[2]=x[2]
}



}

if(x[1]>=1/3 & x[1]<=2/3)
{y[2]=x[1]
if(x[2]>x[1])
{y[3]=x[2]
y[1]=x[3]
}
if(x[2]<x[1])
{y[1]=x[2]
y[3]=x[3]  
}
}


if(x[1]>2/3)
{y[3]=x[1]
if(x[2]<x[1] & x[1]-x[2]>x[2])
{y[1]=x[2]
y[2]=x[3]
}
if(x[1]>x[2] & x[1]-x[2]<x[2])
{y[1]=x[3]
y[2]=x[2]
}



}
if(sum(sort(x)-y==0)==3)
{pobjeda=pobjeda+1}

}
pobjeda


install.packages('nortest')
library(nortest)
lillie.test(5)
lillie.test(c(1,2,3,4,5))

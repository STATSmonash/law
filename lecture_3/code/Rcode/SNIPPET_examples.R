## poisson counts

nsim<-1000
n<-200
p<-.01
x<-rpois(nsim,n*p)
y<-rbinom(nsim,n,p)

par(mfrow=c(1,3))
hist(x)
hist(y)
qqplot(jitter(x,2),jitter(y,2))



## correlations
n<-30
x<-rnorm(n)
y<- -.5*x+rnorm(n,10,.5)

par(mfrow=c(2,2))
cor(x,y)
plot(x,y,main=paste("cor",cor(x,y)))
qqplot(x,y)

hist(x)
hist(y)





## law contingency table
x<-matrix(c(10,7,46,53,17,17,40,9,2,13,8,11,4,4,0,3,2,3,4,0),ncol=2, byrow =TRUE)
x

chisq.test(x)
fisher.test(x[6:10,],simulate.p.value = TRUE,B=150000)
fisher.test(x[1:4,],simulate.p.value = TRUE,B=150000)

fisher.test(x[-c(4,5),],simulate.p.value = TRUE,B=150000)
chisq.test(x[-c(4,5),])

fisher.test(x[c(4,5),],simulate.p.value = TRUE,B=150000)



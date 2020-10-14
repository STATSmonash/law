cat("\014")  # clean console window
rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv) # remove all variables and start again :)

# get file from Spiegalhalter's book website
fname<-"https://raw.githubusercontent.com/dspiegel29/ArtofStatistics/master/07-4-mother-daughter-bootstrap/07-4-galton-x.csv"
new_csv_name<-"galton.csv"
download.file(fname, destfile = new_csv_name) #, method = "curl")
temp<-read.csv(file=new_csv_name,check.names = FALSE)  # upload from clean CSV file

x<-temp
isgirl<-x$Gender=="F"
str(temp)

# make a separate matrix for the regression ...
nboot<-250
xreg<-data.frame(y=x$Height[isgirl],x=x$Mother[isgirl])
dim(xreg)

actual_reg<-lm(xreg$y~xreg$x)

boot_picks<-matrix(sample(1:nrow(xreg),nrow(xreg)*nboot,replace=TRUE),ncol=nboot)
boot_samples<-lapply(1:ncol(boot_picks),function(i){xreg[boot_picks[,i],]})
boot_regs<-lapply(boot_samples,function(i){lm(i$y~i$x)})
boot_predictions<-lapply(boot_regs,function(i){data.frame(y=xreg$y,yhat=cbind(1,xreg$x)%*%as.matrix(i$coe))})

colour_pink<-c("#E768AD")
jit<-10 # jitter to expose double-ups .... but use base data for regression
plot(jitter(x$Mother[isgirl],jit),jitter(x$Height[isgirl],jit),cex=.75,xlab="mother's height",ylab="daughter's height",pch=21,col=adjustcolor(colour_pink,alpha.f=.75),bg=adjustcolor(colour_pink,alpha.f=.75))
for(i in 1:length(boot_predictions)){
  bdata<-boot_predictions[[i]]
  lines(xreg$x,bdata$yhat,col=adjustcolor("grey", alpha.f = 0.1),lwd=2.5)
}
lines(xreg$x,predict(actual_reg),lty=1,col=1) # actual regression line for original sample




##### further exploration
colour_blue<-c("#89CFF0")
jit<-20 # jitter to expose double-ups .... but use base data for regression
plot(c(min(x$Mother),max(x$Mother)),c(min(x$Height),max(x$Height)),ylab="kid's height",xlab="mother's height",col=0)
points(jitter(x$Mother[isgirl],jit),jitter(x$Height[isgirl],jit),pch=21,col=adjustcolor(colour_pink,alpha.f=.75),bg=adjustcolor(colour_pink,alpha.f=.75))
points(jitter(x$Mother[!isgirl],jit),jitter(x$Height[!isgirl],jit),pch=21,col=adjustcolor(colour_blue,alpha.f=.75),bg=adjustcolor(colour_blue,alpha.f=.75))

# now do for boys
xreg2<-data.frame(y=x$Height[!isgirl],x=x$Mother[!isgirl])
actual_reg<-lm(xreg2$y~xreg2$x)

boot_picks<-matrix(sample(1:nrow(xreg2),nrow(xreg2)*nboot,replace=TRUE),ncol=nboot)
boot_samples<-lapply(1:ncol(boot_picks),function(i){xreg2[boot_picks[,i],]})
boot_regs2<-lapply(boot_samples,function(i){lm(i$y~i$x)})
boot_predictions2<-lapply(boot_regs2,function(i){data.frame(y=xreg2$y,yhat=cbind(1,xreg2$x)%*%as.matrix(i$coe))})

dum<-lapply(boot_predictions,function(i){lines(xreg$x,i$yhat,col=adjustcolor("grey", alpha.f = 0.05))})
dum2<-lapply(boot_predictions2,function(i){lines(xreg2$x,i$yhat,col=adjustcolor("grey", alpha.f = 0.05))})


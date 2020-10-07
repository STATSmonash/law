cat("\014")  # clean console window
rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv) # remove all variables and start again :)

function_files<-paste0("source('functions//",list.files(path="functions//",pattern="^f.*.R$"),"', encoding='utf-8')")
for(i in function_files){ eval(parse(text=i)) }

# read in raw file, formated with columns set by a tab
temp<-read.csv(file="law_data_input_TAB_update.txt",check.names = FALSE,sep="\t")  # upload from clean file tab delimited (\t)

# set some particular values to NA
temp[temp=="."]<-NA
temp$time_to_med[temp$time_to_med==999]<-NA
temp[temp==""]<-NA
temp[temp=="Unknown"]<-NA

#View(temp)
names(temp)

# restrict main variable to a subset of columns (can always expand or work with main table)
columns_of_interest<-c("year","dur_mths","reg_binary","gender_pl","family_context","dh_5_bin","dirs_bin","rel_s461"
                       ,"regdate_to_commencement_mths","dh_no","no_pl_affis","no_dirs_case_start"
                        ,"sh_change_precase", "time_to_med","disposition","buscat","majsh")

x<-temp[colnames(temp)%in%columns_of_interest]
#View(x)
str(x)

# convert into numbers if possible (2 Oct: rely on source)
#for(i in 1:ncol(x)){if(is.numeric(as.numeric(x[,i]))) (x[,i]<-as.numeric(x[,i])) } ; str(x)

# make some labels and set factors for specified columns
x$family_context<-factor(x$family_context,levels=c("no","yes"),labels=c("not-family","family"))
x$reg_binary<-factor(x$reg_binary,levels=c("deregistered","registered"),labels=c("deregistered","registered"))
x$gender_pl<-factor(x$gender_pl,levels=c(0,1),labels=c("male","female"))
x$rel_s461<-factor(x$rel_s461,levels=c("yes","no"),labels=c("parallel order","no parallel order"))
x$dirs_bin<-factor(x$dirs_bin,levels=c(1,0),labels=c(">=3 directors","<3 directors"))
x$dh_5_bin<-factor(x$dh_5_bin,levels=c(1,0),labels=c(">=5 hearings","<5 hearings"))
x$sh_change_precase<-factor(x$sh_change_precase,levels=c("yes","no"),labels=c("recent share trade","no recent share trade"))
x$disposition<-factor(x$disposition)
x$buscat<-factor(x$buscat)
x$majsh<-factor(x$majsh,levels=c(0,1),labels=c("minority holder","majority holder"))

# make any new variables or transformations
x$period<- x$year<=2012
x$period<-factor(x$period,levels=c(TRUE,FALSE),labels=c("period1","period2"))
#np<-3 ; x$period<- cut(x$year,np)

View(x)

x$LOG_regdate_to_commencement_mths<-log(x$regdate_to_commencement_mths)
x$LOG_dur_mths<-log(x$dur_mths)
x$LOG_dur_mths[is.infinite(x$LOG_dur_mths)]<-0
x$LOG_time_to_med<-log(x$time_to_med)
x$LOG_time_to_med[is.infinite(x$LOG_time_to_med)]<-0
x$LOG_time_to_med[is.na(x$LOG_time_to_med)]<-0


# create logical vectors of the numeric and factor columns
x_nums<-sapply(x, is.numeric)
x_facts<-sapply(x, is.factor)
x_facts_two<-sapply(x, function(i){is.factor(i)&length(unique(i[!is.na(i)]))==2})

cbind(x_nums,x_facts,x_facts_two)
# now we are ready to analyse the data!!!

############################################################################################
##  DATA SUMMARIES - numeric variables
############################################################################################

x_sum<-fsum_grid(x[,x_nums])
write.table(x_sum$table,"data_summary.CSV",sep=",",col.names = NA)
#write(rbind("",x_sum$blurb),file="data_summary.CSV",append=TRUE)
write.table(c("",x_sum$blur),file=paste0("data_summary.CSV") , append = TRUE,  sep=',', row.names=F, col.names=F )

x_cor<-cor(x[,x_nums],use="pairwise.complete.obs")
x_cor_spearman<-cor(x[,x_nums],use="pairwise.complete.obs",method="spearman")

# p-values: need to do a loop over all columns vs all columns!
xdo<-x[,x_nums]
x_cor_test<-matrix(NA,nrow=ncol(xdo),ncol=ncol(xdo)) 
dimnames(x_cor_test)<-list(colnames(xdo),colnames(xdo))
x_cor_test_spearman<-x_cor_test
for(i in 1:ncol(xdo)){
  for(ii in 1:ncol(xdo)){
    x_cor_test[ii,i]<-cor.test(xdo[,ii],xdo[,i],use="pairwise.complete.obs",method="pearson")$p.value
    x_cor_test_spearman[ii,i]<-cor.test(xdo[,ii],xdo[,i],use="pairwise.complete.obs",exact=FALSE,method="spearman")$p.value
    }
}

write(cbind("Pearson Correlations"),file="data_correlations.CSV")
write.table(x_cor,file="data_correlations.CSV",append=TRUE,sep=",",col.names = NA)
write(cbind("","p-values"),file="data_correlations.CSV",append=TRUE)
write.table(x_cor_test,file="data_correlations.CSV",append=TRUE,sep=",",col.names = NA)

write(rbind("","Spearman Correlations"),file="data_correlations.CSV",append=TRUE)
write.table(x_cor_spearman,file="data_correlations.CSV",append=TRUE,sep=",",col.names = NA)
write(cbind("","p-values"),file="data_correlations.CSV",append=TRUE)
write.table(x_cor_test_spearman,file="data_correlations.CSV",append=TRUE,sep=",",col.names = NA)



############################################################################################
##  DATA SUMMARIES - factors
############################################################################################
fodds_ratio<-function(x,alpha=.05){
# function to calculate the odds ratio for 2by2 contingency tables
  if(dim(x)[1]==2&dim(x)[2]==2){
  n11<-x[1,1] ; n21<-x[2,1] ; n12<-x[1,2] ; n22<-x[2,2]
  p1<-n11/(n11+n21)
  p2<-n12/(n12+n22)
  OR_hat<-(p1/(1-p1))/(p2/(1-p2))
  OR_hat
  (n11/n21)/(n12/n22)
  
  sigma_hat<-sqrt(1/n11+1/n21+1/n12+1/n22)
  CI<-exp(log(OR_hat)+qnorm(1-alpha/2)*sigma_hat*c(-1,1))
  out<-c(OR_hat,CI)
  names(out)<-c("OR",paste0((1-alpha),"CI_low"),paste0((1-alpha),"CI_up"))
  t(as.matrix(out,nrow=1))
  } else {matrix(c(NA,NA,NA),nrow=1)}
  
}


x_temp<-x[,x_facts]
#which(x_facts)
#t(combn(which(x_facts), 2))
#ct_pairs<-list(c(1,2),c(1,3),c(2,3))
ct_pairs<-list(c(1,2),c(1,3),c(2,3))
ct_pairs_mat<-t(combn(1:ncol(x_temp),2))
ct_pairs<-lapply(1:nrow(ct_pairs_mat),function(i){ct_pairs_mat[i,]})

ct_list<-lapply(ct_pairs,function(i){ftable(xtabs(~.,data=x_temp[,i]))})
names(ct_list)<-unlist(lapply(ct_list,function(i){paste( names(attr(i,"row.vars")), "vs", names(attr(i,"col.vars")))}))
ct_chisq<-lapply(ct_list,function(i){chisq.test(i,correct=FALSE)})
ct_odds_ratio<-lapply(ct_list,function(i){fodds_ratio(i)})

out_tab_sum<-data.frame(t(sapply(ct_pairs,function(i){colnames(x_temp)[i]})),as.matrix(sapply(ct_chisq,function(i){i$p.value})))
colnames(out_tab_sum)<-c("row","col","chisq p-value")
out_or_sum<-t(sapply(ct_odds_ratio,function(i){i})) ; colnames(out_or_sum)<-colnames(ct_odds_ratio[[1]])
if( all(rownames(out_tab_sum)==rownames(out_or_sum)) ) { out_sum<-cbind(out_tab_sum,out_or_sum)} else {out_sum<-NULL}

ct_out_name<-"contingency_tables_SUMMARY.CSV"
write.table(out_sum,file=ct_out_name, append=FALSE,col.names = NA,sep=",",row.names=TRUE)

ct_out_name<-"contingency_tables.CSV"
for(i in 1:length(ct_list)){
  ct_do<-ct_list[[i]]
  tab_name<-names(ct_list)[i]
  write.table(rbind("",tab_name),file=ct_out_name, append=i>1,col.names = FALSE,row.names=FALSE,sep=",")
  write.table(as.matrix(ct_do),file=ct_out_name, append=TRUE,col.names = NA,sep=",",row.names=TRUE)
  write.table(data.frame("Chi-sq p-value",ct_chisq[[i]]$p.value),col.names = FALSE,file=ct_out_name, append=TRUE,sep=",",row.names=FALSE)
  write.table(ct_odds_ratio[[i]],file=ct_out_name, append=TRUE,col.names = TRUE,sep=",",row.names=FALSE)
  #write.table(test_results,file=ct_out_name, append=TRUE,col.names = NA,sep=",",row.names=TRUE)
  #write.table(ct_chisq[[i]]$expected,col.names = FALSE,file=ct_out_name, append=TRUE,sep=",",row.names=FALSE)
}





############################################################################################
##  Charts
############################################################################################
x_plot<-x[,x_nums]
chart_folder<-"CHART_"


cname<-"scatter_plots"
csize<-c(10,10)*1.25
pdf(paste0(chart_folder,cname,".pdf"),height=csize[1],width=csize[2])
  pairs(x_plot)
dev.off()


csize<-c(5,5)
cname<-"histograms"
pdf(paste0(chart_folder,cname,".pdf"),height=csize[1],width=csize[2])
  par(mfrow=c(1,1))
  for(i in 1:ncol(x_plot)){
    hist(x_plot[,i],main=colnames(x_plot)[i],xlab=NA)
  }
dev.off()

cname<-"qqplots"
pdf(paste0(chart_folder,cname,".pdf"),height=csize[1],width=csize[2])
par(mfrow=c(1,1))
for(i in 1:ncol(x_plot)){
  qqnorm(x_plot[,i],main=paste0(colnames(x_plot)[i]))
  qqline(x_plot[,i])
  title(main="qqplot with N(0,1): the straighter the line the more normal the data",cex.main=.65,line=.75,font.main=1)
}
dev.off()

############################################################################################
##  Misc aggregations
############################################################################################
fsum_mini<-function(x){
  nmiss<-is.na(x)
  x<-x[!nmiss]
  out<-c(length(x),sum(nmiss),mean(x),median(x),min(x),max(x))
  names(out)<-c("n","nmiss","mean","median","min","max")
  out
}
aggregate(dur_mths~year,data=x,function(i){fsum_mini(i)})
fsum_mini(x$dur_mths)

aggregate(dh_no~year ,data=x,function(i){fsum_mini(i)})
fsum_mini(x$dh_no)

############################################################################################
##  t tests for binary factors
############################################################################################

ft_test_sum<-function(xt){
# function used to summarise a t-test
  est<-as.matrix(xt$estimate)
  df<-xt$parameter
  meth<-xt$parameter
  ci<-as.matrix(xt$conf.int);rownames(ci)<-c("CI.95 low","CI.95 low")
  pval<-xt$p.value  
  out<-rbind(est,df,ci,pval)
  out
}

ft_test_quick<-function(xx,xfact){
# function takes a binary factor and a matrix - does a ttest for each column based on the factor  
  if(!length(unique(xfact[!is.na(xfact)]))==2){warning("t-test cannot be run because factor is not binary")}
  xx_t<-lapply(xx,function(i){t.test(i~xfact)})
  lapply(xx_t,ft_test_sum)
}

out_t<-sapply(x[,which(x_facts_two)],function(i){ft_test_quick(x[,x_nums],i)})
out_t_format<-lapply(1:ncol(out_t),function(i){lapply(out_t[,i],function(ii){temp<-ii;rownames(temp)<-rownames(out_t[,i][[1]]);temp}) })
out_t_format<-lapply(1:ncol(out_t),function(i){sapply(out_t[,i],function(ii){ii})})
out_t_format<-lapply(1:length(out_t_format),function(i){temp<-out_t_format[[i]];rownames(temp)<-rownames(out_t[,i][[1]]);temp})
names(out_t_format)<-colnames(out_t)


ct_out_name<-"t_tests.CSV"
for(i in 1:length(out_t_format)){
  ct_do<-out_t_format[[i]]
  tab_name<-names(out_t_format)[i]
  suppressWarnings( write.table(rbind("",tab_name),file=ct_out_name, append=i>1,col.names = FALSE,row.names=FALSE,sep=",") )
  suppressWarnings( write.table(as.matrix(ct_do),file=ct_out_name, append=TRUE,col.names = NA,sep=",",row.names=TRUE) )
  
}



############################################################################################
##  MODELS - continuous y-variable
############################################################################################
use_cols_reg<-c("LOG_dur_mths","dur_mths","dh_no","no_pl_affis","regdate_to_commencement_mths","LOG_regdate_to_commencement_mths"
                ,"family_context","dh_5_bin","dirs_bin","no_dirs_case_start"
                ,"LOG_time_to_med","majsh","year")
x_missing<-apply(x[,colnames(x)%in%use_cols_reg],1,function(i){any(is.na(i))})
x_no_na<-x[!x_missing,colnames(x)%in%use_cols_reg]
x_no_na<-cbind(x_no_na, RANK_dh_no=rank(x_no_na$dh_no),RANK_no_pl_affis=rank(x_no_na$no_pl_affis))
#x_no_na<-x_no_na[!x_no_na$dur_mths==0,]

#mod0<-lm(LOG_dur_mths ~ poly(dh_no,1), data = x_no_na)
mod1<-lm(LOG_dur_mths ~ RANK_dh_no, data = x_no_na)
#mod2<-lm(LOG_dur_mths ~ log(dh_no+.01)+log(no_pl_affis+.01), data = x_no_na)
mod2<-update(mod1,~.+RANK_no_pl_affis+LOG_time_to_med)
mod3<-update(mod2,~.+LOG_regdate_to_commencement_mths)
mod4<-update(mod3,~.+dh_5_bin*family_context) 
anova(mod1,mod2,mod3,mod4)

summary(mod3)
anova(mod4)

mod_choose<-mod3
res<-mod_choose$residuals
qqnorm(res);qqline(res)

# mod_check<-lm(dur_mths ~ dirs_bin, data = x_no_na)
# res<-mod_check$residuals
# qqnorm(res);qqline(res)

############################################################################################
##  MODELS - binary y-variable
############################################################################################
colour_p<-c("#E768AD")

use_cols_reg<-c("reg_binary","no_dirs_case_start","LOG_regdate_to_commencement_mths","year","gender_pl","rel_s461","majsh","time_to_med")
x_missing<-apply(x[,colnames(x)%in%use_cols_reg],1,function(i){any(is.na(i))})
x_no_na<-x[!x_missing,colnames(x)%in%use_cols_reg]
      
mod1<-glm(reg_binary ~ no_dirs_case_start, family = binomial(link = "logit"), data = x_no_na)
mod2<-glm(reg_binary ~ no_dirs_case_start+LOG_regdate_to_commencement_mths, family = binomial(link = "logit"), data = x_no_na)
mod3<-glm(reg_binary ~ no_dirs_case_start+LOG_regdate_to_commencement_mths+gender_pl, family = binomial(link = "logit"), data = x_no_na)
mod4<-glm(reg_binary ~ no_dirs_case_start+LOG_regdate_to_commencement_mths+gender_pl+year, family = binomial(link = "logit"), data = x_no_na)
mod5<-glm(reg_binary ~ no_dirs_case_start+LOG_regdate_to_commencement_mths+year+gender_pl+rel_s461, family = binomial(link = "logit"), data = x_no_na)
mod6<-update(mod5,~.+rank(time_to_med))
  
anova(mod1,mod2,mod3,mod4,mod5,mod6,test="LRT")
summary.glm(mod4)
#exp(-1.38450+c(-1,1)*qnorm(.975)*0.45419)

y_fit<-predict(mod4,type='response')

data_box<-split(y_fit,x_no_na$reg_binary)
boxplot(data_box,outline=FALSE,names=NA,axes=FALSE)
stripchart(data_box, jitter = .1 , method = "jitter",vertical = TRUE,col=0,xlab=NULL,cex.axis=1,axes=TRUE)
boxplot(data_box,outline=FALSE,add=TRUE,names=NA,axes=FALSE,boxwex=0.25)
stripchart(data_box,jitter = .1 , method = "jitter",pch=16,cex=1.5, col=adjustcolor(colour_p, alpha.f = 0.3),vertical = TRUE,add=TRUE,axes=FALSE)
out_roc<-froc(x_no_na$reg_binary,y_fit)

xx<-table(data.frame(x_no_na$reg_binary,y_fit>.25))
1-xx[2,2]/(xx[2,1]+xx[2,2]) # 1- sensitivity
xx[1,1]/(xx[1,1]+xx[1,2]) # specificity

#page 177 in hosmer 2013 chapter 6 in Spegielhalter.
y_fit1<-predict(mod2,type='response')
out_roc1<-froc(x_no_na$reg_binary,y_fit1)
out_roc1<-froc(x_no_na$reg_binary,y_fit1)

plot(out_roc$roc)
points(out_roc1$roc,col=2)



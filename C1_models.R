source("/home/bd/Dropbox/projects/hrs/cognition_attrition/src/00_bigfun.R")
load(file="/home/bd/Dropbox/projects/hrs/cognition_attrition/wd/df.Rdata")

#df<-df[df$wave %in% 5:6,]

########################################################################################
##baseline
maketab<-function(df) {
    L<-list()
    for (st in c("proxy","attrit","dead")) {
        tmp<-df[df$status %in% c(st,"eligible"),]
        tmp$status<-ifelse(tmp$status==st,1,0)
        L[[st]]<-bigfun(tmp,allmodels=FALSE)
    }
    tmp<-data.frame(do.call("rbind",L))
    tab<-proc(tmp) #zz<-lapply(L,proc) #no extenion: apm quantites, .1=p^m, .2=win
    tab$van.1-tab$base.1 -> tab$delta
    tab2<-tab[,c("base.1","van","delta","van.2")]
    tab2
}
tab<-maketab(df)

tabL<-list()
for (i in 1:10) {
    print(i)
    index<-sample(1:nrow(df),nrow(df),replace=TRUE)
    tabL[[i]]<-maketab(df[index,])
}
nm<-grep("van.2",names(tab),fixed=TRUE)
L<-lapply(tabL,function(x) x[,nm])
est<-do.call("rbind",L)
est<-apply(est,2,quantile,c(.025,.975))
tab2<-cbind(tab,t(est))

library(xtable)
print(xtable(tab2,digits=4),include.rownames=TRUE)


########################################################################################
##all models
source("/home/bd/Dropbox/projects/hrs/cognition_attrition/src/00_bigfun.R")
load(file="/home/bd/Dropbox/projects/hrs/cognition_attrition/wd/df.Rdata")
maketab<-function(df) {
    L<-list()
    for (st in c("proxy","attrit","dead")) {
        tmp<-df[df$status %in% c(st,"eligible"),]
        tmp$status<-ifelse(tmp$status==st,1,0)
        L[[st]]<-bigfun(tmp,allmodels=TRUE)
    }
    tmp<-data.frame(do.call("rbind",L))
    tab<-proc(tmp) #zz<-lapply(L,proc) #no extenion: apm quantites, .1=p^m, .2=win
    tab$van.1-tab$base.1 -> tab$delta
    tab2<-tab[,c("base.1","van","delta","van.2","std.2","spl.2")]
    tab2
}
tab<-maketab(df)
library(xtable)
ii<-grep(".2",names(tab),fixed=TRUE)
print(xtable(tab[,ii],digits=4),include.rownames=TRUE)


########################################################################################
##stratified by age and sex

load(file="/home/bd/Dropbox/projects/hrs/cognition_attrition/wd/gender.Rdata")
df<-df[!is.na(df$ragender) & !is.na(df$age),]
gr<-cut(df$age,c(-Inf,60,70,80,Inf))
gr<-paste(df$ragender,gr)
L<-split(df,gr)

out<-list()
for (st in c("proxy","dead")) {
    for (i in 1:length(L)) {
        z<-L[[i]]
        tmp<-z[z$status %in% c(st,"eligible"),]
        tmp$status<-ifelse(tmp$status==st,1,0)
        out[[paste(st,names(L)[i]) ]]<-bigfun(tmp)
    }
}

tmp<-data.frame(do.call("rbind",out))
tab<-proc(tmp) #zz<-lapply(L,proc) #no extenion: apm quantites, .1=p^m, .2=win
ii<-grep(".2",names(tab),fixed=TRUE)
tab[,ii]->tab
rownames(tab)->rns
gr<-ifelse(grepl("proxy",rns),"proxy","dead")
sex<-ifelse(grepl("1.male",rns),"male","female")
age<-strsplit(rns,"e ")
age<-sapply(age,"[[",2)
tab<-cbind(sex,age,tab)
print(xtable(tab[gr=='proxy',],digits=4),include.rownames=FALSE)
print(xtable(tab[gr=='dead',],digits=4),include.rownames=FALSE)





source("/home/bd/Dropbox/projects/hrs/cognition_attrition/src/00_bigfun.R")
load(file="/home/bd/Dropbox/projects/hrs/cognition_attrition/wd/df.Rdata")

########################################################################################
##baseline
df$hs<-ifelse(df$raedyrs>=12,1,0)
df$college<-ifelse(df$raedyrs>=16,1,0)
df$iad<-ifelse(df$iadl>0,1,0)
df$dep<-ifelse(df$cesd>=4,1,0)

#get iad and depression from next wave
tmp<-df[,c("hhidpn","wave","iad","dep")]
NULL->df$iad -> df$dep
tmp$wave<-tmp$wave -1 
df<-merge(df,tmp,all.x=TRUE)

maketab<-function(df) {
    L<-list()
    for (st in c("hs","college","iad","dep")) {
        tmp<-df[!is.na(df[[st]]),]
        tmp$status<-tmp[[st]]
        print(st)
        print(mean(tmp$st,na.rm=TRUE))
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

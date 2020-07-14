source("/home/bd/Dropbox/projects/hrs/cognition_attrition/src/00_bigfun.R")
load(file="/home/bd/Dropbox/projects/hrs/cognition_attrition/wd/df.Rdata")
df$cog -> df$cog.l0
df<-df[!is.na(df$age),]

dim(df)
for (i in 1:2) {
    tmp<-df[,c("hhidpn","cog","wave")]
    names(tmp)[2]<-paste("cog.l",i,sep="")
    tmp$wave<-tmp$wave+i
    df<-merge(df,tmp)
}
dim(df)

test<-df$age<=80
test<-ifelse(test,"<=80",">80")
L<-split(df,test)

f<-function(df) {
    L<-list()
    for (st in c("proxy","dead")) {
        tmp<-df[df$status %in% c(st,"eligible"),]
        tmp$status<-ifelse(tmp$status==st,1,0)
        for (i in 0:2) {
            tmp2<-tmp
            tmp2$cog<-NULL
            tmp2$cog<-tmp2[[paste("cog.l",i,sep="")]]
            L[[paste(st,i)]]<-bigfun(tmp2,allmodels=TRUE)
        }
    }
    tab<-proc(do.call("rbind",L))#lapply(L,proc) #no extenion: apm quantites, .1=p^m, .2=win
    ii<-grep(".2",names(tab),fixed=TRUE)
    tab[,ii]->tab
}
tab<-lapply(L,f)

tab2<-do.call("rbind",tab)
rownames(tab2)->rns
txt<-strsplit(rns,".",fixed=TRUE)
age<-sapply(txt,"[[",1)
txt<-sapply(txt,"[[",2)
txt<-strsplit(txt," ",fixed=TRUE)
status<-sapply(txt,"[[",1)
w<-sapply(txt,"[[",2)
tab2<-cbind(age,status,w,tab2)
library(xtable)
print(xtable(tab2,digits=4),include.rownames=FALSE)

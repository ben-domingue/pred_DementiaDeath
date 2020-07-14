load(file="/home/bd/Dropbox/projects/hrs/cognition_attrition/wd/df.Rdata")

L<-split(df,df$w)
L$all<-df

f<-function(x) {
    age<-c(mean(x$age,na.rm=TRUE),sd(x$age,na.rm=TRUE))
    cog<-c(mean(x$cog,na.rm=TRUE),sd(x$cog,na.rm=TRUE))
    dead<-ifelse(x$status=="dead",1,0)
    alive<-ifelse(x$status=="attrit",1,0)
    prox<-ifelse(x$status=="proxy",1,0)
    c(length(unique(x$hhidpn)),nrow(x),age,cog,mean(dead,na.rm=TRUE),mean(alive,na.rm=TRUE),mean(prox,na.rm=TRUE))
}
tab<-lapply(L,f)
tab<-do.call("rbind",tab)

library(xtable)
print(xtable(tab,digits=2),include.rownames=TRUE)





par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,4),oma=rep(.5,4))
tmp<-df[df$age<90,]
cols<-c(eligible='black',dead='blue',proxy='red',attrit='gray')
plot(NULL,xlim=c(50,90),xlab='age',ylab="Proportion at next wave",lwd=2,ylim=c(0,1),bty='n')
for (st in unique(df$status)) {
    nex<-ifelse(tmp$status==st,1,0)
    mm<-aggregate(nex,list(tmp$age),mean)
    col<-cols[st]
    lines(mm,lwd=2,col=col)
    mtext(side=4,at=mm[nrow(mm),2],st,col=col,las=2)
}
mtext(side=3,at=85,'A',line=0)
##
plot(NULL,xlim=c(50,90),ylim=c(10,25),xlab='age',ylab='cognition',bty='n')
L<-split(tmp,tmp$status)
for (i in 1:length(L)) {
    z<-L[[i]]
    z<-aggregate(z$cog,list(z$age),mean,na.rm=TRUE)
    col<-cols[names(L)[i]]
    lines(z,col=col,lwd=2)
    mtext(side=4,las=2,line=.1,at=z[nrow(z),2],names(L)[i],col=col)
}
mtext(side=3,at=85,'B',line=0)

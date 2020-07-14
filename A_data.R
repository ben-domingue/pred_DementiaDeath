library(foreign)
read.dta("/home/bd/Dropbox/projects/hrs/spousal_death_cesd/data/randhrs1992_2016v1.dta")->x

##hhidpn,sex, race, education
df<-x[,c("hhidpn","ragender","raracem","raedyrs","raddate")]
#year, cog, 
L<-list()
for (w in 3:12) {
    age<-x[[paste("r",w,"agey_m",sep='')]]
    cog<-x[[paste("r",w,"cogtot",sep="")]]
    prox<-x[[paste("r",w,"status",sep="")]]
    cog<-ifelse(prox=="1.cog meas",cog,NA)
    int.date<-x[[paste("r",w,"iwend",sep="")]]
    cesd<-x[[paste("r",w,"cesd",sep="")]]
    iadl<-x[[paste("r",w,"iadlza",sep="")]]
    ##
    tmp<-data.frame(hhidpn=x$hhidpn,cog=cog,int.date=int.date,wave=w,age=age,cesd=cesd,iadl=iadl)
    L[[as.character(w)]]<-merge(df,tmp,all.x=TRUE)
}
df<-data.frame(do.call("rbind",L))
df<-df[!is.na(df$cog),]
   
#status
#proxy respondent
L<-list()
for (w in 1:13) {
    prox<-x[[paste("r",w,"proxy",sep="")]]
    prox<-ifelse(prox=="1.proxy",1,0)
    L[[as.character(w)]]<-data.frame(hhidpn=x$hhidpn,prox=prox,w=w)
}
prox<-data.frame(do.call("rbind",L))
prox<-prox[!is.na(prox$prox) & prox$prox==1,]
id<-paste(prox$hhidpn,prox$w-1)
df$proxy<-ifelse(paste(df$hhidpn,df$wave) %in% id,1,0)
##death
as.POSIXct(df$raddate*24*60^2,origin = "1960-01-01")->df$ddate.raw
as.POSIXct(df$int.date*24*60^2,origin = "1960-01-01")->df$iwend.raw
delta<-df$ddate.raw - df$iwend.raw
df$dead<-ifelse(delta<365*2,1,0)
##next test
L<-list()
for (w in 3:12) {
    prox<-x[[paste("r",w,"proxy",sep="")]]
    int.date<-x[[paste("r",w,"iwend",sep="")]]
    tmp<-data.frame(hhidpn=x$hhidpn,prox=prox,w=w,int.date=int.date)
    L[[as.character(w)]]<-tmp
}
tmp<-data.frame(do.call("rbind",L))
tmp<-tmp[!is.na(tmp$int.date),]
tmp<-tmp[tmp$prox=='0.not proxy',]
id<-paste(tmp$hhidpn,tmp$w-1)
df$inhrs<-ifelse(paste(df$hhidpn,df$wave) %in% id,1,0)

##status
st<-ifelse(df$inhrs==1 & df$proxy==0,1,NA)
st<-ifelse(df$proxy==1,2,st)
st<-ifelse(df$dead==1 & is.na(st),3,st)
st<-ifelse(is.na(st),4,st)
table(df$wave,st)
df$status<-c("eligible","proxy","dead","attrit")[st]
df<-df[df$wave<=11,]


save(df,file="/home/bd/Dropbox/projects/hrs/cognition_attrition/wd/df.Rdata")



## x<-read.csv("/home/bd/Dropbox/projects/hrs/cognition_attrition/stata/working_data/cog_002_attr_clean.csv")


## df<-x[,c("hhidpn","year","ageR","d4","d4_next","rcogtot")]
## df<-df[df$d4=="responded, have cog measure",]
## df$rcogtot<-as.numeric(ifelse(df$rcogtot %in% c(".n",""),NA,df$rcogtot))
## df<-df[!is.na(df$rcogtot),]
## df<-df[df$d4_next!="responded, hrs/ahead overlap",]
## df$d4_next<-ifelse(df$d4_next=="","??",df$d4_next)

## by(df$rcogtot,df$d4_next,summary)
## df<-df[df$year<2016,]
## df<-df[df$d4_next!="??",]
## by(df$year,df$d4_next,summary)

## par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(3,3,1,12))
## plot(NULL,xlim=c(50,90),ylim=c(10,25),xlab='age',ylab='cogtot')
## L<-split(df,df$d4_next)
## for (i in 1:length(L)) {
##     z<-L[[i]]
##     z<-z[z$age>=50 & z$age<=90,]
##     z<-aggregate(z$rcogtot,list(z$age),mean,na.rm=TRUE)
##     lines(z)
##     mtext(side=4,las=2,line=.1,at=z[nrow(z),2],names(L)[i])
## }
## ##look at this. https://www.pnas.org/content/early/2020/06/16/1918455117
## df$rcogtot->df$cog.l0
## NULL->df$rcogtot
## ifelse(df$d4_next=="responded, have cog measure",1,0)->nex
## mm<-aggregate(nex,list(df$age),mean)
## plot(mm,type='l',xlim=c(50,90),xlab='age',ylab="Proportion with cognitive measure at next wave")
## #df<-df[df$ageR>60,]

## table(df$year,df$d4_next)

## ##add lagged cognition
## for (i in 1:5) {
##     tmp<-df[,c("hhidpn","year","cog.l0")]
##     tmp$year<-tmp$year+2*i
##     names(tmp)[3]<-paste("cog.l",i,sep='')
##     df<-merge(df,tmp,all.x=TRUE)
## }

## index<-grep("^cog.l",names(df))
## tmp<-df[,index]
## tmp<-cbind(tmp,NA)
## nl<-apply(tmp,1,function(x) which(is.na(x))[1])
## df$nlag<-nl-2

## save(df,file="/home/bd/Dropbox/projects/hrs/cognition_attrition/wd/df.Rdata")

## makewide<-function(df,ncog=2) {
##     df<-df[df$d4_next %in% c("responded, used proxy","responded, have cog measure"),]
##     L<-split(df,df$hhidpn)
##     f<-function(x,ncog) {
##         if (nrow(x)<ncog) {
##             return(NULL)
##         } else {
##             offset<-ifelse(ncog>1,ncog,0)
##             x<-x[order(x$year),]
##             out<-list()
##             for (i in 1:(nrow(x)-ifelse(ncog==1,0,ncog-1))) {
##                 tmp<-x[i:(i+max(offset-1,0)),]
##                 years<-tmp$year[1]+2*(0:(ncog-1))
##                 if (nrow(tmp)==ncog && all(tmp$year==years)) out[[as.character(i)]]<-c(tmp$hhidpn[1],tmp$year[1],tmp$ageR[nrow(tmp)],tmp$rcogtot[1:nrow(tmp)],tmp$d4_next[nrow(tmp)]) 
##             }
##             return(do.call("rbind",out))
##         }
##     }
##     L<-lapply(L,f,ncog=ncog)
##     x<-data.frame(do.call("rbind",L))
##     names(x)<-c("hhidpn","y1","age1",paste("cog",1:ncog,sep=""),"status")
##     x$status<-ifelse(x$status=="responded, used proxy",1,0)
##     x$age1<-as.numeric(x$age1)
##     for (i in 1:ncog) x[[paste("cog",i,sep='')]]<-as.numeric(x[[paste("cog",i,sep='')]])
##     x
## }




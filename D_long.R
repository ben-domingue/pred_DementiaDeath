## source("/home/bd/Dropbox/projects/hrs/cognition_attrition/src/00_bigfun.R")
## load(file="/home/bd/Dropbox/projects/hrs/cognition_attrition/wd/df.Rdata")

## ########################################################################################
## ##baseline
## out<-list()
## for (age in c(70,75)) {
##     tmp1<-df[abs(df$ageR-age)<1.5,]
##     tmp2<-df[abs(df$ageR-85)<2.5,]
##     tmp1<-tmp1[,c("hhidpn","cog.l0")]
##     tmp2<-tmp2[,c("hhidpn","year","d4_next","ageR")]
##     L<-split(tmp1,tmp1$hhidpn)
##     L<-lapply(L,function(x) x[sample(1:nrow(x),1),])
##     tmp1<-data.frame(do.call("rbind",L))
##     x<-merge(tmp2,tmp1)
##     for (st in c("responded, used proxy","attrited, alive","attrited, dead")) {
##         tmp<-x[x$d4_next %in% c(st,"responded, have cog measure"),]
##         tmp$status<-ifelse(tmp$d4_next==st,1,0)
##         0->tmp$nlag
##         out[[paste(st,age)]]<-bigfun(tmp,nlag=0,allmodels=FALSE)
##     }
## }

## zz<-lapply(out,proc) #no extenion: apm quantites, .1=p^m, .2=win


## tab<-do.call("rbind",zz)
## tab$van.1-tab$base.1 -> tab$delta
## tab2<-tab[,c("nlag","base.1","van","delta","van.2")]
## names(out)->tab2$nlag
## #tab[,2]-tab[,1] -> tab[,2]
## library(xtable)
## print(xtable(tab2,digits=3),include.rownames=FALSE)

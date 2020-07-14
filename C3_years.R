
bigfun_age<-function(df,st,nfold=5,allmodels=TRUE) {
    ##
    predfun<-function(x,allmodels) {
        x$inter<-x$age*x$cog #for splines
        #in/out
        oos<-x[x$oos,]
        x<-x[!x$oos,]
        ##Models
        ##baseline, no cognition
        #oos$mr<-mean(x$status)
        #mr<-aggregate(x$status,list(x$wave),mean,na.rm=TRUE)
        #names(mr)<-c("wave","mr")
        #oos<-merge(oos,mr)
        fm<-paste("status~factor(wave)")
        m<-glm(fm,x,family="binomial")
        oos$mr<-predict(m,oos,type="response")
        #vanilla
        fm<-paste("status~factor(wave)+age")
        m<-glm(fm,x,family="binomial")
        oos$vanilla<-predict(m,oos,type="response")
        ##ll
        ll<-function(x,p) {
            z<-log(x[[p]])*x$status+log(1-x[[p]])*(1-x$status)
            z<-sum(z)/length(z)
            exp(z)
        }    
        if (allmodels) {
            ##splines
            library(splines)
            spl<-list()
            for (nm in c("age")) {
                spl[[nm]]<-bs(x[[nm]])
                for (i in 1:ncol(spl[[nm]])) spl[[nm]][,i]->x[[paste(nm,"__",i,sep="")]]
            }
            foo<-grep("__",names(x))
            fm<-paste("status~factor(wave)+",paste(names(x)[foo],collapse="+"))
            m<-glm(fm,x,family="binomial")
            for (nm in names(spl)) {
                tmp<-predict(spl[[nm]],oos[[nm]])
                for (i in 1:ncol(tmp)) tmp[,i]->oos[[paste(nm,"__",i,sep="")]]
            }
            oos$spl<-predict(m,oos,type="response")
            c(ll(oos,'mr'),ll(oos,'vanilla'),ll(oos,"spl"))
        } else {
            c(ll(oos,'mr'),ll(oos,'vanilla'))
        }
    }
    ##
    out<-list()
    df$gr<-sample(1:nfold,nrow(df),replace=TRUE)
    for (ii in 1:nfold) {
        df$oos <- df$gr==ii
        out[[ii]]<-predfun(df,allmodels)
    }
    mat<-do.call("rbind",out)
    mat<-colMeans(mat) 
    names(mat)<-c("base","van","spl")[1:length(mat)]
    ##
    mat
}

proc_age<-function(x) {
    #
    gp<-Vectorize(getp)
    p<-list()
    for (i in 1:ncol(x)) p[[colnames(x)[i] ]]<-gp(x[,i])
    ##
    bet<-function(tp,ap) (tp-ap)/ap
    w<-list()
    allmod<-c("van","spl")
    test<-allmod %in% names(p)
    for (nm in allmod[test]) w[[nm]]<-bet(p[[nm]],p$base)
    data.frame(x,p,w)
}


########################################################################################
source("/home/bd/Dropbox/projects/hrs/cognition_attrition/src/00_bigfun.R")
load(file="/home/bd/Dropbox/projects/hrs/cognition_attrition/wd/df.Rdata")

maketab<-function(df) {
    L<-list()
    for (st in c("proxy","attrit","dead")) {
        tmp<-df[df$status %in% c(st,"eligible"),]
        tmp$status<-ifelse(tmp$status==st,1,0)
        L[[st]]<-bigfun_age(tmp)
    }
    tmp<-data.frame(do.call("rbind",L))
    tab<-proc_age(tmp) #zz<-lapply(L,proc) #no extenion: apm quantites, .1=p^m, .2=win
    tab$van.1-tab$base.1 -> tab$delta
    tab2<-tab[,c("base.1","van","delta","van.2","spl.2")]
    tab2
}
tab<-maketab(df)
library(xtable)
ii<-grep(".2",names(tab),fixed=TRUE)
print(xtable(tab[,ii],digits=4),include.rownames=TRUE)

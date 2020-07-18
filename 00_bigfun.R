

bigfun<-function(df,allmodels=TRUE,nfold=10) {
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
        fm<-paste("status~factor(wave)+age")
        m<-glm(fm,x,family="binomial")
        oos$mr<-predict(m,oos,type="response")
        #vanilla
        fm<-paste("status~factor(wave)+age+cog")
        m<-glm(fm,x,family="binomial")
        oos$vanilla<-predict(m,oos,type="response")
        ##ll
        ll<-function(x,p) {
            z<-log(x[[p]])*x$status+log(1-x[[p]])*(1-x$status)
            z<-sum(z)/length(z)
            exp(z)
        }    
        if (allmodels) {
            ##standard
            fm<-paste("status~factor(wave)+age*cog")
            m<-glm(fm,x,family="binomial")
            oos$std<-predict(m,oos,type="response")
            ##splines
            library(splines)
            spl<-list()
            for (nm in c("age","cog","inter")) {
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
            c(ll(oos,'mr'),ll(oos,'vanilla'),ll(oos,'std'),ll(oos,"spl"))
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
    names(mat)<-c("base","van","std","spl")[1:length(mat)]
    ##
    c(prevalence=mean(df$status,na.rm=TRUE),mat)
}


getp<-function(a) {
    f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
    nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
}

proc<-function(x) {
    #
    x$prevalence->prev
    NULL->x$prevalence
    gp<-Vectorize(getp)
    p<-list()
    for (i in 1:ncol(x)) p[[colnames(x)[i] ]]<-gp(x[,i])
    ##
    bet<-function(tp,ap) (tp-ap)/ap
    w<-list()
    allmod<-c("van","std","spl")
    test<-allmod %in% names(p)
    for (nm in allmod[test]) w[[nm]]<-bet(p[[nm]],p$base)
    data.frame(prev,x,p,w)
}


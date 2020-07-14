## pv<-seq(.5,.99,by=.001)
## y<-list()
## for (p in pv) {
##     for (i in 1:500) {
##         z<-rbinom(1000,1,p)
##         z<-log(p)*z+log(1-p)*(1-z)
##         z<-sum(z)/length(z)
##         y[[paste(p,i)]]<-c(p,exp(z))
##     }
## }
## y<-do.call("rbind",y)
## plot(y,pch=19,cex=.4); abline(0,1)

## z<-aggregate(y[,2],list(y[,1]),mean)
## lines(z,col='red',lwd=2)

## names(z)<-c("p","ll")
## z->coins
## save(coins,file="coins.Rdata")

## getp<-function(a) {
##     f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
##     nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
## }

    
## ## ##bets
## ## win<-list()
## ## for (p in seq(.5,.99,by=.005)) {
## ##     z<-rbinom(500000,1,p)
## ##     odds<-p/(1-p)
## ##     #house puts up $1
## ##     house<-1
## ##     me<-house*odds
## ##     w<-sum(z*(me+house))-length(z)*me
## ##     win[[as.character(p)]]<-c(p,w/length(z))
## ## }
## ## win<-do.call("rbind",win)
## ## plot(win)

## win<-function(p) {
##     bet<-function(tp,ap) (tp-ap)/ap
##     bet(p[,2],p[,1])
## }

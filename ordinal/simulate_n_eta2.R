#zz <- file("requiredN.Rout", open = "wt")
#sink(zz)
source("functions.R")

load("b_for_eta2.RData")

pvar<-b_for_eta2
pvar$pvar<-as.numeric(pvar$pvar)
pvar<-pvar[pvar$pvar<.225,]
pvar$b<-as.numeric(pvar$b)
what=unique(pvar$stat)
model=unique(pvar$model)

t<-Sys.time()

doFuture::registerDoFuture()
future::plan(future::multicore)
Rep=5*10^3
i<-1

if (what=="eta2") pv<-"pow_eta" else pv<-"pow_mod"

results<-lapply(1:nrow(pvar), function(i) {
   power<-.80
   info<-pvar[i,]
   class(info$b)<-c(class(info$b),model)
   start<-eta_power(as.numeric(info$pvar),power=power,df=1,p=c(.3,.3,.3))
   ne<-round(start$n)
   n<-round(start$n)
   ok<-FALSE
   onerun<-function(n) {
     .sample<-create_sample(b = info$b,ncovs = as.numeric(info$ncovs),rcovs = as.numeric(info$rcovs),N = n, what=what)
     o<-onetest(.sample)
     return(o)
   }

   j<-1
    while (!ok) {
      cat("trying ",n,"for eta",info$pvar)
      suppressMessages({
          reslist  <- foreach::foreach(1:Rep) %dorng% onerun(n)
      })
      resdata  <- as.data.frame(do.call(rbind,reslist))
      resdata  <- resdata[resdata$eta<.99,]
      pow<-mean(resdata$pow_eta)
      cat(" found", pow," with pvar",mean(resdata$r2), "\n")
      if (round(pow,digits = 2)>power)
            n=n-1
      if (round(pow,digits = 2)<power)
            n=n+1
      if (round(pow,digits = 2)==power)
            ok<-TRUE
      ## if it gets into a loop and the estimate is reasonable, take it
      if (j>20 && abs(delta)<.02)
        ok<-TRUE

      }
cbind(eta_pop=info$pvar,b=info$b,N=n,eN=ne)
})

requiredN<-as.data.frame(do.call(rbind,results))
save(requiredN,file="requiredN_eta2.Rdata")
cat("Time: ",Sys.time()-t)
#sink()

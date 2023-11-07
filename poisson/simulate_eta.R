tictoc::tic()
source("functions.R")
load("b_for_eta2.RData")
popvalues<-b_for_eta2
popvalues
### some simulations parameters
model<-"poisson"
R<-5*10^3
Ns<-c(25,50,75,100)
pvars<-seq(.025,.50,by=.025)
what<- "eta2"
#######

cat("Simulating",length(pvars)," values of",what,"for model ", model,", replications:",R, "\n")
cat("Working in ",getwd(),"\n")

doFuture::registerDoFuture()
future::plan(future::multisession)

## this runs for all Ns R replication for one value of var parameter
make_by_param<-function(info) {

  info$b<-as.numeric(info$b)
  class(info$b)<-c(model,"numeric")

  onerun<-function(n) {
    .sample<-create_sample(b = info$b,ncovs = as.numeric(info$ncovs),rcovs = as.numeric(info$rcovs),N = n,what=what)
    onetest(.sample)
  }

  ### this replicate for each N
  nrepls<-lapply(Ns,function(n) {

    repls<-replicate(R,onerun(n),simplify = F)
    message("replicating for N:",n,"")
    onelong<-as.data.frame(do.call(rbind,repls))
    res<-as.data.frame(rbind(apply(onelong,2,mean,na.rm=T),apply(onelong,2,sd,na.rm=T)))
    res$stat<-c("mean","sd")
    res$N<-n
    res$exp_par<-as.numeric(info$pvar)
    res$R<-R
    res
  })
  as.data.frame(do.call(rbind,nrepls))
}

reslist  <- foreach::foreach(i=1:nrow(popvalues)) %dorng%  make_by_param(popvalues[i,])

# use lapply for debugging
#reslist  <- lapply(1:nrow(popvalues) , function(i)  make_by_param(popvalues[i,]))

eta2_simdata<-as.data.frame(do.call(rbind,reslist))
save(eta2_simdata,file = "eta2_simdata.RData")
cat("Simulations ended\n")
tictoc::toc()




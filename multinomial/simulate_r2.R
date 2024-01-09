tictoc::tic()
source("functions.R")
### some simulations parameters
model<-"multinomial"
R<-5*10^3
Ns<-c(25,50,75,100)
what<- "r2"
####### filenames ###
bdata<-"b_for_r2.RData"
odata<- "r2_simdata.RData"
###
# this when used in RStudio
#bdata<-paste0(model,"/",bdata)
#odata<-paste0(model,"/",odata)
#

load(bdata)
pvars<-b_for_r2[1:16,]
cat("Simulating",length(Ns)," sample sizes for",what,"in model ", model,", replications:",R, "\n")
cat("Working in ",getwd(),"\n")

doFuture::registerDoFuture()
future::plan(future::multicore)

## this runs for all Ns R replication for one value of var parameter
make_by_param<-function(info) {

  info$b<-as.numeric(info$b)
  class(info$b)<-c(model,"numeric")
  message("Simulating for",what,"=",info$pvar)

  onerun<-function(n) {
    .sample<-create_sample(b = info$b,ncovs = as.numeric(info$ncovs),rcovs = as.numeric(info$rcovs),N = n)
    onetest(.sample)
  }

  ### this replicate for each N
  nrepls<-lapply(Ns,function(n) {
    message("replicating for N:",n,"")
    repls<-replicate(R,onerun(n),simplify = F)
    onelong<-as.data.frame(do.call(rbind,repls))
    res<-as.data.frame(rbind(apply(onelong,2,mean,na.rm=T),apply(onelong,2,sd,na.rm=T)))
    res$stat<-c("mean","sd")
    res$N<-n
    res$exp_par<-as.numeric(info$pvar)
    res$R<-R
    res$aR<-dim(onelong)[1]

    res
  })
  as.data.frame(do.call(rbind,nrepls))
}

reslist  <- foreach::foreach(i=1:nrow(pvars)) %dorng%  make_by_param(pvars[i,])

# use lapply for debugging
#reslist  <- lapply(1:nrow(pvars) , function(i)  make_by_param(pvars[i,]))

r2_simdata<-as.data.frame(do.call(rbind,reslist))
save(r2_simdata,file = odata)
cat("Simulations ended\n")
tictoc::toc()


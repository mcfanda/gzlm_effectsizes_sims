source("functions.R")
tictoc::tic()
find_param<-function(pvar) {
  res<-list(b=1)
  reslist<-list()
  for (i in seq_len(R)) {
  res<-findb(pvar,b=res$b,ncovs = ncovs,rcovs = rcovs,what=what,model=model, debug = F,N = Npop)
  reslist[[length(reslist)+1]]<-res
  }
  res<-as.data.frame(do.call(rbind,reslist))
  c(pvar=pvar,model=model,b=mean(res$b),ncovs=ncovs,rcovs=rcovs,stat=what)
}
### parameters
Npop<-10^4
R<-10^2
ncovs<-3
rcovs<-.3
what="r2"
pvars<-seq(.025,.50,by=.025)
model="normal"
#####
cat("finding",length(pvars)," values of",what,"for model ", model,"\n")

doFuture::registerDoFuture()
future::plan(future::multisession)

#params  <- lapply(pvars, function(x) find_param(x)) #used for debugging

params  <- foreach::foreach(x=pvars) %dorng%  find_param(x)
b_for_r2<-as.data.frame(do.call(rbind,params))
save(b_for_r2,file="b_for_r2.RData")

### parameters
pvars<-seq(.025,.50,by=.025)

what="eta2"
#####
params  <- foreach::foreach(x=pvars) %dorng%  find_param(x)
#params  <- lapply(pvars, function(x) find_param(x)) #used for debugging

cat("finding",length(pvars)," values of",what,"for model ", model,"\n")

b_for_eta2<-as.data.frame(do.call(rbind,params))
save(b_for_eta2,file="b_for_eta2.RData")

cat("Simulations ended\n")
tictoc::toc()

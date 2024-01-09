### find_b.R ####
# Find the linear parameters yielding a model with the required R^2 or eta^2
# Author: Marcello Gallucci

source("functions.R")

## define which model to simulate
model="binomial"


find_param<-function(pvar) {
  res<-list(b=1)
  reslist<-list()
  ## repeat the search R times
  for (i in seq_len(R)) {
  try({
  if (is.na(res$b)) res$b<-1
  ### we use the findb() function defined in `functions.R`
  res<-findb(pvar,b=res$b,ncovs = ncovs,rcovs = rcovs,what=what,model=model, debug = F,N = Npop)
  reslist[[length(reslist)+1]]<-res
  })
  }
  res<-as.data.frame(do.call(rbind,reslist))
  cat("found ",mean(res$b)," for ",what,"=",pvar,"\n")
  # return the mean of the R search results
  c(pvar=pvar,model=model,b=mean(res$b),ncovs=ncovs,rcovs=rcovs,stat=what)
}
### Find parameters for R^2
Npop<-10^4
R<-1*10^3
ncovs<-3
rcovs<-.3
what="r2"
pvars<-seq(.025,.40,by=.025)
#####
cat("finding",length(pvars)," values of",what,"for model ", model,"\n")
doFuture::registerDoFuture()
future::plan(future::multicore)

params  <- foreach::foreach(x=pvars) %dorng%  find_param(x)

b_for_r2<-as.data.frame(do.call(rbind,params))
save(b_for_r2,file="b_for_r2.RData")

### Find parameters for eta^2
pvars<-seq(.025,.40,by=.025)
what="eta2"
#####
cat("finding",length(pvars)," values of",what,"for model ", model,"\n")

params  <- foreach::foreach(x=pvars) %dorng%  find_param(x)

# use lapply for debugging
#params  <- lapply(pvars, function(x)find_param(x))


b_for_eta2<-as.data.frame(do.call(rbind,params))
save(b_for_eta2,file="b_for_eta2.RData")

cat("Simulations ended\n")


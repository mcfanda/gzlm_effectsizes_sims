## these are shortcuts for running simulation in parallel
`%dorng%`<-doRNG::`%dorng%`
`%dopar%`<- foreach::`%dopar%`

# produce a dataset with y and continuous xs, link by the linkfunction defined by class of b. The covariates have correlation rcovs.
# This mimics a S3 method because the S3 method does not work with  foreach and dofuture

create_sample<-function(b, N=100,otherb=0, ncovs=1, rcovs=0, what="r2", k=3) {

  if ("binomial" %in% class(b) ) return(
                                .create_sample.binomial(b, N=N,otherb=otherb, ncovs=ncovs, rcovs=rcovs,what=what)
                                 )
  if ("poisson" %in% class(b) ) return(
    .create_sample.poisson(b, N=N,otherb=otherb, ncovs=ncovs, rcovs=rcovs, what=what)
  )

  if ("multinomial" %in% class(b) ) return(
    .create_sample.multinomial(b, N=N,otherb=otherb, ncovs=ncovs, rcovs=rcovs, what=what, k=k)
  )
  if ("ordinal" %in% class(b) ) {
    return(.create_sample.ordinal(b, N=N,otherb=otherb, ncovs=ncovs, rcovs=rcovs, what=what, k=k) )
  }
  if ("normal" %in% class(b) ) {
    asample<-.create_sample.normal(b, N=N,otherb=otherb, ncovs=ncovs, rcovs=rcovs, what=what)
    class(asample)<-c("normal","data.frame")
    return(asample)
  }

  stop("define a class for coefficient b")
}

.create_sample.binomial<-function(b, N=100,otherb=0, ncovs=1, rcovs=0, what="r2") {

  sigma <- matrix(rep(rcovs,ncovs^2),ncol=ncovs)
  diag(sigma)<-1
  if (what=="r2") vb<-as.numeric(rep(b,ncovs))
  if (what=="eta2") {
                  vb<-c(b,rep(otherb,ncovs-1))
                  vb<-as.numeric(vb[1:ncovs])
  }

  covs<-MASS::mvrnorm(N,mu = rep(0,ncol(sigma)),Sigma = sigma,empirical = T)
  l<-covs%*%as.numeric(vb)+rnorm(N)
  p<-1/(1+exp(-l))
  y <- rbinom(n = N, size = 1, prob = p)
  .data<-as.data.frame(cbind(y,covs))
   class(.data)<-c("binomial",class(.data))
  .data
  }

.create_sample.multinomial<-function(b, N=100,otherb=0, ncovs=1, rcovs=0, what="r2",k=3) {

  test<-0
  while(test!=k) {
  sigma <- matrix(rep(rcovs,ncovs^2),ncol=ncovs)
  diag(sigma)<-1
  if (what=="r2") vb<-rep(b,ncovs)
  if (what=="eta2") {
    vb<-c(b,rep(otherb,ncovs-1))
    vb<-as.numeric(vb[1:ncovs])
  }
  covs<-MASS::mvrnorm(N,mu = rep(0,ncol(sigma)),Sigma = sigma,empirical = T)
  int<-1/k
  lint<-log(int/(1-int))
  vb<-as.numeric(vb)
  l<-lapply(1:(k-1), function(x) covs%*%vb)
  l<- do.call(cbind,l)
  l<-l+rnorm(N)
  l<- cbind(lint,l)
  den<-apply(l,1,function(x) sum(exp(x)))
  p <- exp(l)/den
  y <- apply(p, MARGIN = 1, function(x) sample(x = 1:k, size = 1, prob = x))
  test<-length(table(y))
  }
  .data<-as.data.frame(cbind(y,covs))
  class(.data)<-c("multinomial",class(.data))
  .data
}

.create_sample.ordinal<-function(b, N=100,otherb=0, ncovs=1, rcovs=0, what="r2",k=3) {

  sigma <- matrix(rep(rcovs,ncovs^2),ncol=ncovs)
  diag(sigma)<-1
  if (what=="r2") vb<-rep(b,ncovs)
  if (what=="eta2") {
    vb<-c(b,rep(otherb,ncovs-1))
    vb<-as.numeric(vb[1:ncovs])
  }
  covs<-MASS::mvrnorm(N,mu = rep(0,ncol(sigma)),Sigma = sigma,empirical = T)
  l<- covs%*%vb
  l<-l+rnorm(N)
  s<-seq(0,1,by=1/k)
  y<-cut(l,quantile(l,probs = s))
  levels(y)<-1:k
  .data<-as.data.frame(cbind(y,covs))
  class(.data)<-c("ordinal",class(.data))
  .data
}


.create_sample.poisson<-function(b, N=100,otherb=0, ncovs=1, rcovs=0, what="r2") {

  sigma <- matrix(rep(rcovs,ncovs^2),ncol=ncovs)
  diag(sigma)<-1
  if (what=="r2") vb<-rep(b,ncovs)
  if (what=="eta2") {
    vb<-c(b,rep(otherb,ncovs-1))
    vb<-vb[1:ncovs]
  }
  covs<-MASS::mvrnorm(N,mu = rep(0,ncol(sigma)),Sigma = sigma,empirical = T)
  l<-covs%*%vb+rnorm(N)
#  l<-as.numeric(scale(l))
  mu <- exp(l)+2
  y <- rpois(n=N, lambda=mu)
  .data<-as.data.frame(cbind(y,covs))
  class(.data)<-c("poisson",class(.data))
  .data
}

.create_sample.normal<-function(b, N=100,otherb=0, ncovs=1, rcovs=0, what="r2") {

  sigma <- matrix(rep(rcovs,ncovs^2),ncol=ncovs)
  diag(sigma)<-1
  if (what=="r2") vb<-rep(b,ncovs)
  if (what=="eta2") {
    vb<-c(b,rep(otherb,ncovs-1))
    vb<-vb[1:ncovs]
  }
  covs<-MASS::mvrnorm(N,mu = rep(0,ncol(sigma)),Sigma = sigma,empirical = T)
  y<-as.numeric(covs%*%vb+rnorm(N))
  .data<-as.data.frame(cbind(y,covs))
  class(.data)<-c("normal",class(.data))
  .data
}

## estimate model b, eta, and inferential test for a given dataset
# This mimic a S3 method because the S3 method does not work with
# and foreach and dofuture

onetest<-function(data,family=NULL) {

  if ("binomial" %in% class(data) ) return(
    return(.onetest.default(data,family=binomial()) )
  )
  if ("poisson" %in% class(data) ) return(
    return(.onetest.default(data,family=poisson()) )
  )
  if ("multinomial" %in% class(data) ) return(
    return(.onetest.multinomial(data) )
  )
  if ("ordinal" %in% class(data) ) return(
    return(.onetest.ordinal(data) )
  )
  if ("normal" %in% class(data) ) return(
    return(.onetest.normal(data) )
  )

 stop("please define the model family as a class of the data ")
}

.onetest.default<-function(data,family=NULL) {

  if (is.null(family)) stop("please specify a family fo this simulation")
  N<-dim(data)[1]
  k<-ncol(data)-1
  baserate<-mean(data$y)
  form<-paste("y~",paste0("V",(1:k)+1,collapse = "+"))
  mod<-glm(form,family = family,data=data)
  if (!mod$converged) return(NULL)
  mod0<-glm(y~1,family = family,data=data)
  b<-mod$coefficients[2]
  a<-car::Anova(mod,type=3,test="LR")
  chi_eta<-a$`LR Chisq`[1]
  p_eta<-a$`Pr(>Chisq)`[1]
  pow_eta<-as.numeric(p_eta<.05)
  eta2<-chi_eta/(mod0$deviance)
  eps2<-(chi_eta-.5)/mod0$deviance
  adev<-mod$deviance/N
  adev0<- mod$null.deviance/N
  r2<-1-(mod$deviance/mod0$deviance)
  if (r2>.99) return(NULL)
  ar2<-1-((mod$deviance+k)/mod0$deviance)
  ar2df<-1-((mod$deviance*(N-1))/((N-k-1)*mod0$deviance))
  chi_mod<-as.numeric(anova(mod0,mod,test = "LRT")[2,4])
  p_mod<-as.numeric(anova(mod0,mod,test = "LRT")[2,5])
  pow_mod<-as.numeric(p_mod<=.05)
  lambda2<-r2*adev0
  alambda2<-ar2*adev0
  dflambda2<-ar2df*adev0
  elambda2<-eta2*adev0
  epslambda2<-eps2*adev0
  as.data.frame(cbind(b,baserate,eta2,eps2,chi_eta,p_eta,pow_eta,r2,ar2,ar2df,chi_mod,p_mod,pow_mod,adev,adev0,lambda2,alambda2,dflambda2,elambda2,epslambda2,N))
}


.onetest.multinomial<-function(data) {


  classes<-length(table(data$y))
  N<-dim(data)[1]
  p<-(classes-1)
  k<-(ncol(data)-1)
  baserate<-table(data$y)[1]/N
  df<-k*(classes-1)
  form<<-paste("y~",paste0("V",(1:k)+1,collapse = "+"))
  x<-capture.output({
    mod<-nnet::multinom(form,data=data)
    mod0<-nnet::multinom(y~1,data=data)
  })
  b<-coefficients(mod)[1,2]
  a<-car::Anova(mod,type=3,test="LR")
  chi_eta<-a$`LR Chisq`[1]
  p_eta<-a$`Pr(>Chisq)`[1]
  pow_eta<-as.numeric(p_eta<.05)
  eta2<-chi_eta/(mod0$deviance)

  eps2<-(chi_eta-p)/mod0$deviance
  adev<-mod$deviance/N
  adev0<- mod0$deviance/N
  r2<-1-(mod$deviance/mod0$deviance)
  if (r2>.999) return(NULL)
  ar2<-1-((mod$deviance+df)/mod0$deviance)
  ar2df<-1-((mod$deviance*(N-1))/((N-k-1)*mod0$deviance))
  chi_mod<-as.numeric(anova(mod0,mod)[2,6])
  p_mod<-as.numeric(anova(mod0,mod)[2,7])
  pow_mod<-as.numeric(p_mod<.05)
  lambda2<-r2*adev0
  alambda2<-ar2*adev0
  dflambda2<-ar2df*adev0
  elambda2<-eta2*adev0
  epslambda2<-eps2*adev0
  as.data.frame(cbind(b,baserate,eta2,eps2,chi_eta,p_eta,pow_eta,r2,ar2,ar2df,chi_mod,p_mod,pow_mod,adev,adev0,lambda2,alambda2,dflambda2,elambda2,epslambda2,N))
}


.onetest.ordinal<-function(data) {

  N<-dim(data)[1]
  k<-ncol(data)-1
  baserate<-table(data$y)[1]/N
  form<<-paste("y~",paste0("V",(1:k)+1,collapse = "+"))
  data$y<-factor(data$y)
  mod<-ordinal::clm(formula = form,data=data)
  mod0<-ordinal::clm(formula = y~1,data=data)
  yk<-nlevels(data$y)
  b<-coefficients(mod)[yk]
  a<-drop1(mod,test="Chi")
  dev1<-as.numeric(-2*logLik(mod))
  dev0<-as.numeric(-2*logLik(mod0))
  chi_eta<-a$LRT[2]
  p_eta<-a$`Pr(>Chi)`[2]
  pow_eta<-as.numeric(p_eta<.05)
  eta2<-chi_eta/dev0
  eps2<-(chi_eta-.5)/dev0
  adev<-dev1/N
  adev0<- dev0/N
  r2<-1-(dev1/dev0)
  if (r2>.99) return(NULL)
  ar2<-1-((dev1+k)/dev0)
  ar2df<-1-((dev1*(N-1))/((N-k-1)*dev0))
  chi_mod<-as.numeric(anova(mod0,mod)[2,4])
  p_mod<-as.numeric(anova(mod0,mod)[2,6])
  pow_mod<-as.numeric(p_mod<.05)
  lambda2<-r2*adev0
  alambda2<-ar2*adev0
  dflambda2<-ar2df*adev0
  elambda2<-eta2*adev0
  epslambda2<-eps2*adev0
  as.data.frame(cbind(b,baserate,eta2,eps2,chi_eta,p_eta,pow_eta,r2,ar2,ar2df,chi_mod,p_mod,pow_mod,adev,adev0,lambda2,alambda2,dflambda2,elambda2,epslambda2,N))
}

.onetest.normal<-function(data) {
  ### for OLS models, we use the chi_* fields to store the F-test. We keep these names
  ### for consistency with the other models runs
  N<-dim(data)[1]
  k<-ncol(data)-1
  baserate<-mean(data$y)
  form<<-paste("y~",paste0("V",(1:k)+1,collapse = "+"))
  mod<-lm(formula = form,data=data)
  mod0<-lm(formula = y~1,data=data)
  b<-coefficients(mod)[2]
  a<-car::Anova(mod)
  dev1<-(N-k-1)*sigma(mod)^2
  dev0<-(N-1)*sigma(mod0)^2
  r2<-1-(dev1/dev0)
  ar2<-1-(sigma(mod)^2/sigma(mod0)^2)
  chi_eta<-a$`F value`[1]
  p_eta<-a$`Pr(>F)`[1]
  pow_eta<-as.numeric(p_eta<=.05)
  eta2<-a$`Sum Sq`[1]/(a$`Sum Sq`[1]+dev1)
  eps2<-(a$`Sum Sq`[1]-(a$Df[1]*sigma(mod)))/(a$`Sum Sq`[1]+dev1)
  ### we use the ar2df slot here for gamma
  ar2df<-ar2
  adev<-sigma(mod)^2
  adev0<- sigma(mod0)^2
  chi_mod<-as.numeric(anova(mod0,mod)[2,5])
  p_mod<-as.numeric(anova(mod0,mod)[2,6])
  pow_mod<-as.numeric(p_mod<=.05)
  ## the following is useless for OLS models, we keep it for consistency with GzLM
  lambda2<-r2*adev0
  alambda2<-ar2*adev0
  dflambda2<-ar2df*adev0
  elambda2<-eta2*adev0
  epslambda2<-eps2*adev0
  as.data.frame(cbind(b,baserate,eta2,eps2,chi_eta,p_eta,pow_eta,r2,ar2,ar2df,chi_mod,p_mod,pow_mod,adev,adev0,lambda2,alambda2,dflambda2,elambda2,epslambda2,N))
}



## sample a sample of N cases from a given dataset and return the logistic regression results

### this makes a population with a given pvar parameter finding the appropriated b coefficients
### it is not meant to be efficient, it is meant to work
make_pop<-function(pvar,model,rcovs=0,ncovs=1, what=what,N=10^6,debug=FALSE) {
  ### first quickly find reasonable starting points
  first<-findb(pvar,N=10^3,b=1,otherb = 0.1, rcovs = rcovs, ncovs=ncovs,what=what,model = model)
  second<-findb(pvar,N=10^3,b=1,otherb = 0.1, rcovs = rcovs, ncovs=ncovs,what=what,model = model)
  ### then create the population dataset
  findb(pvar,N=N,b=mean(first$b,second$b),otherb = 0.1, rcovs = rcovs, ncovs=ncovs,what=what,data=T,model = model,debug=debug)
}

### this run one trials (one sample)
onetrial<-function(N) {

  onetest(onesample(pop,N))
}

## This function numerically find a b coefficient that guarantees the required eta2 or r2 squared
findb<-function(pvar,b=1,otherb=0,N=10^5,ncovs=3,rcovs=0,what="r2",dataout=FALSE,model="binomial",debug=FALSE) {

  vb<-b
  class(vb)<-c(model,class(vb))
  ok=0
  eps<-.5
  i<-0
  cat("using ", what, "for", model,"\n")
  while(ok==0) {
    i<-i+1
    .pop<-create_sample(vb, N, otherb,ncovs=ncovs,rcovs=rcovs,what = what)
    try({
      res<-onetest(.pop)
      })

    if (is.null(res))
         rpvar=1
    else
        rpvar<-res[[what]]

    eps=(pvar-rpvar)/2
    if (abs(eps)<.0001)
       ok<-1
    else {
      vb<-abs(vb+eps)
      if (vb<0) vb<-abs(eps)
    }
    ## when the function is used to find starting values, it may fail and so
    ## we go out after 10^3 tries
    if (i>10^3) ok=1

    class(vb)<-model
    if (debug) cat("Looking for",pvar," found ",rpvar,"coef", vb[1]," offset ",eps,"\n")
  }
  cat("Looking for",pvar," found b",vb[1]," \n")
  res$b<-vb
  if (dataout)
      return(.pop)
  else
     return(res)

}

##### estimate probabilities from logit b to pass to webPower::wp.logistic()

b_to_p<-function(n,b) {

  p0<-rep(.50,length(b))
  odd0<-p0/(1-p0)
  odd1<-exp(b)*odd0
  p1<-odd1/(1+odd1)
  WebPower::wp.logistic(n=n,p0=p0,p1=p1,family = "normal")
}

## this estimate the loglikelihood of a null model based on the p(Y=1)

ploglik0<-function(p) {
  if (length(p)==1) p<-c(p,1-p)
  lp<-log(p)
  sum(log(p)*p)
}


eta_power<-function(eta,power=NULL,n=NULL,p=.5,df=1) {

  if ( (is.null(power)+is.null(n) )!=1) stop("exactly one of eta and power must be NULL")
  l0<-ploglik0(p)
  es<--eta*2*l0
  wp<-pwr::pwr.chisq.test(N = n,sqrt(es),df=df,power=power)
#  wp<-WebPower::wp.sem.chisq(n=n,effect =es ,df=1,power=power)
  wp$n<-wp$N
  wp

}

startBookFig<- function (pathname,run=FALSE,font=12) {
  if (run==FALSE) return(NULL)
  par(family='serif')
  name<-pathname
  #par(pch=21,bg=0)
  #font=round(8+(size/80),0)
  # print(font)
  jpeg(file=name,width=14,height=25,units="cm", res=600, pointsize=font)

}


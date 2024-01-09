### make supplementary tables ###

# eta-squared, epsilon-squared and proportion converged  ##

## logistic
load("binomial/eta2_simdata.RData")

data<-eta2_simdata
mdata<-data[data$stat=="mean",]
var<-"eta2"
xdata<-mdata[,c(var,"exp_par","N")]
eta<-reshape(xdata,direction = "wide", v.names = c(var),timevar = "N",idvar = "exp_par")
var<-"eps2"
xdata<-mdata[,c(var,"exp_par","N")]
eps<-reshape(xdata,direction = "wide", v.names = c(var),timevar = "N",idvar = "exp_par")
mdata$conv<-mdata$aR/mdata$R
xdata<-mdata[,c("conv","exp_par","N")]
conv<-reshape(xdata,direction = "wide", v.names = c("conv"),timevar = "N",idvar = "exp_par")

tab1<-cbind(exp=eta$exp_par,e25=eta[,2],ep25=eps[,2],c25=conv[,2],e50=eta[,3],ep50=eps[,3],c50=conv[,3],e75=eta[,4],ep75=eps[,4],c75=conv[,4],e100=eta[,5],ep100=eps[,5],c100=conv[,5])
tab1

## multinomial
load("multinomial/eta2_simdata.RData")

data<-eta2_simdata
mdata<-data[data$stat=="mean",]
var<-"eta2"
xdata<-mdata[,c(var,"exp_par","N")]
eta<-reshape(xdata,direction = "wide", v.names = c(var),timevar = "N",idvar = "exp_par")
var<-"eps2"
xdata<-mdata[,c(var,"exp_par","N")]
eps<-reshape(xdata,direction = "wide", v.names = c(var),timevar = "N",idvar = "exp_par")
mdata$conv<-mdata$aR/mdata$R
xdata<-mdata[,c("conv","exp_par","N")]
conv<-reshape(xdata,direction = "wide", v.names = c("conv"),timevar = "N",idvar = "exp_par")

tab2<-cbind(exp=eta$exp_par,e25=eta[,2],ep25=eps[,2],c25=conv[,2],e50=eta[,3],ep50=eps[,3],c50=conv[,3],e75=eta[,4],ep75=eps[,4],c75=conv[,4],e100=eta[,5],ep100=eps[,5],c100=conv[,5])
tab2

## ordinal
load("ordinal/eta2_simdata.RData")

data<-eta2_simdata
mdata<-data[data$stat=="mean",]
var<-"eta2"
xdata<-mdata[,c(var,"exp_par","N")]
eta<-reshape(xdata,direction = "wide", v.names = c(var),timevar = "N",idvar = "exp_par")
var<-"eps2"
xdata<-mdata[,c(var,"exp_par","N")]
eps<-reshape(xdata,direction = "wide", v.names = c(var),timevar = "N",idvar = "exp_par")
mdata$conv<-mdata$aR/mdata$R
xdata<-mdata[,c("conv","exp_par","N")]
conv<-reshape(xdata,direction = "wide", v.names = c("conv"),timevar = "N",idvar = "exp_par")

tab3<-cbind(exp=eta$exp_par,e25=eta[,2],ep25=eps[,2],c25=conv[,2],e50=eta[,3],ep50=eps[,3],c50=conv[,3],e75=eta[,4],ep75=eps[,4],c75=conv[,4],e100=eta[,5],ep100=eps[,5],c100=conv[,5])
tab3

# R-squared, adjusted R-squared and proportion converged  ##

## logistic
load("binomial/eta2_simdata.RData")

data<-r2_simdata
mdata<-data[data$stat=="mean",]
var<-"r2"
xdata<-mdata[,c(var,"exp_par","N")]
eta<-reshape(xdata,direction = "wide", v.names = c(var),timevar = "N",idvar = "exp_par")
var<-"ar2"
xdata<-mdata[,c(var,"exp_par","N")]
eps<-reshape(xdata,direction = "wide", v.names = c(var),timevar = "N",idvar = "exp_par")
mdata$conv<-mdata$aR/mdata$R
xdata<-mdata[,c("conv","exp_par","N")]
conv<-reshape(xdata,direction = "wide", v.names = c("conv"),timevar = "N",idvar = "exp_par")

tab1<-cbind(exp=eta$exp_par,e25=eta[,2],ep25=eps[,2],c25=conv[,2],e50=eta[,3],ep50=eps[,3],c50=conv[,3],e75=eta[,4],ep75=eps[,4],c75=conv[,4],e100=eta[,5],ep100=eps[,5],c100=conv[,5])
tab1

## multinomial
load("multinomial/eta2_simdata.RData")

data<-r2_simdata
mdata<-data[data$stat=="mean",]
var<-"r2"
xdata<-mdata[,c(var,"exp_par","N")]
eta<-reshape(xdata,direction = "wide", v.names = c(var),timevar = "N",idvar = "exp_par")
var<-"ar2"
xdata<-mdata[,c(var,"exp_par","N")]
eps<-reshape(xdata,direction = "wide", v.names = c(var),timevar = "N",idvar = "exp_par")
mdata$conv<-mdata$aR/mdata$R
xdata<-mdata[,c("conv","exp_par","N")]
conv<-reshape(xdata,direction = "wide", v.names = c("conv"),timevar = "N",idvar = "exp_par")

tab2<-cbind(exp=eta$exp_par,e25=eta[,2],ep25=eps[,2],c25=conv[,2],e50=eta[,3],ep50=eps[,3],c50=conv[,3],e75=eta[,4],ep75=eps[,4],c75=conv[,4],e100=eta[,5],ep100=eps[,5],c100=conv[,5])
tab2

## ordinal
load("ordinal/eta2_simdata.RData")

data<-r2_simdata
mdata<-data[data$stat=="mean",]
var<-"r2"
xdata<-mdata[,c(var,"exp_par","N")]
eta<-reshape(xdata,direction = "wide", v.names = c(var),timevar = "N",idvar = "exp_par")
var<-"ar2"
xdata<-mdata[,c(var,"exp_par","N")]
eps<-reshape(xdata,direction = "wide", v.names = c(var),timevar = "N",idvar = "exp_par")
mdata$conv<-mdata$aR/mdata$R
xdata<-mdata[,c("conv","exp_par","N")]
conv<-reshape(xdata,direction = "wide", v.names = c("conv"),timevar = "N",idvar = "exp_par")

tab3<-cbind(exp=eta$exp_par,e25=eta[,2],ep25=eps[,2],c25=conv[,2],e50=eta[,3],ep50=eps[,3],c50=conv[,3],e75=eta[,4],ep75=eps[,4],c75=conv[,4],e100=eta[,5],ep100=eps[,5],c100=conv[,5])
tab3

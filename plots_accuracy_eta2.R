source("functions.R")

library(ggplot2)
load("normal/eta2_simdata.RData")

data<-eta2_simdata
data<-data[data$stat=="mean",]
names(data)
d<-reshape(data,varying = c("eta2","eps2"),direction = "long",v.names = "y")


d$time<-factor(d$time)
d$Index<-d$time
table(d$Index)
d$N<-factor(d$N)
levels(d$N)<-c("N=25","N=50","N=75","N=100")
p<-ggplot(d,aes(y=y,x=exp_par,col=Index))
p<-p+geom_smooth(se=FALSE)
#p<-p+geom_smooth(aes(y=ar2,col=N),linetype=2,linewidth=1.5)
p<-p+geom_abline(intercept = 0,slope = 1)
p<-p + ylab("Index value")+xlab('Population \u03b7\u00B2')
p<- p + ggtitle("Gaussian")
p<- p + scale_color_discrete(labels=c('\u03b7\u00B2','\u03b5\u00B2'))
p<- p + facet_wrap(~N,nrow = 1,ncol = 4)
p<-p+theme_classic()+  theme(plot.title = element_text(hjust = 0.5))
p<-p+theme(strip.text.x = element_text(size = 7))
p<-p+ylim(0,.6)

p1<-p
p1

tab1<-data[,c("N","exp_par","eta2","eps2")]
tab1$id<-rep(1:20,each=4)
tab1<-reshape(tab1,v.names =c("eta2","eps2"),direction = "wide",timevar = "N",idvar="id")

load("binomial/eta2_simdata.RData")

data<-eta2_simdata
data<-data[data$stat=="mean",]
d<-reshape(data,varying = c("eta2","eps2"),direction = "long",v.names = "y")

cbind(data$N,data$exp_par,data$aR/data$R)

d$time<-factor(d$time)
d$Index<-d$time
library(ggplot2)

  d$N<-factor(d$N)
  levels(d$N)<-c("N=25","N=50","N=75","N=100")
  p<-ggplot(d,aes(y=y,x=exp_par,col=Index))
  p<-p+geom_smooth(se=FALSE)
  #p<-p+geom_smooth(aes(y=ar2,col=N),linetype=2,linewidth=1.5)
  p<-p+geom_abline(intercept = 0,slope = 1)
  p<-p + ylab("Index value")+xlab('Population \u03b7\u00B2')
  p<- p + ggtitle("Logistic")
  p<- p + scale_color_discrete(labels=c('\u03b7\u00B2','\u03b5\u00B2'))
  p<- p + facet_wrap(~N,nrow = 1,ncol = 4)
  p<-p+theme_classic()+  theme(plot.title = element_text(hjust = 0.5))
  p<-p+theme(strip.text.x = element_text(size = 7))
  p<-p+ylim(0,.6)

  p2<-p
  p2

tab2<-data[,c("N","exp_par","eta2","eps2")]
tab2$id<-rep(1:20,each=4)
tab2<-reshape(tab2,v.names =c("eta2","eps2"),direction = "wide",timevar = "N",idvar="id")
round(tab2,digits = 3)
data<-NULL

load("multinomial/eta2_simdata.RData")

data<-eta2_simdata
data<-data[data$stat=="mean",]
names(data)
d<-reshape(data,varying = c("eta2","eps2"),direction = "long",v.names = "y")


d$time<-factor(d$time)
d$Index<-d$time
library(ggplot2)

d$N<-factor(d$N)
levels(d$N)<-c("N=25","N=50","N=75","N=100")
p<-ggplot(d,aes(y=y,x=exp_par,col=Index))
p<-p+geom_smooth(se=FALSE)
#p<-p+geom_smooth(aes(y=ar2,col=N),linetype=2,linewidth=1.5)
p<-p+geom_abline(intercept = 0,slope = 1)
p<-p + ylab("Index value")+xlab('Population \u03b7\u00B2')
p<- p + ggtitle("Multinomial")
p<- p + scale_color_discrete(labels=c('\u03b7\u00B2','\u03b5\u00B2'))
p<- p + facet_wrap(~N,nrow = 1,ncol = 4)
p<-p+theme_classic()+  theme(plot.title = element_text(hjust = 0.5))
p<-p+theme(strip.text.x = element_text(size = 7))
p<-p+ylim(0,.6)

p3<-p
p3

tab2<-data[,c("N","exp_par","eta2","eps2")]
tab2$id<-rep(1:20,each=4)
tab2<-reshape(tab2,v.names =c("eta2","eps2"),direction = "wide",timevar = "N",idvar="id")
round(tab2,digits = 3)




data<-NULL
load("ordinal/eta2_simdata.RData")

data<-eta2_simdata
data<-data[data$stat=="mean",]
names(data)

d<-reshape(data,varying = c("eta2","eps2"),direction = "long",v.names = "y")
d$time<-factor(d$time)


d$Index<-d$time
library(ggplot2)

d$N<-factor(d$N)
levels(d$N)<-c("N=25","N=50","N=75","N=100")
p<-ggplot(d,aes(y=y,x=exp_par,col=Index))
p<-p+geom_smooth(se=FALSE)
#p<-p+geom_smooth(aes(y=ar2,col=N),linetype=2,linewidth=1.5)
p<-p+geom_abline(intercept = 0,slope = 1)
p<-p + ylab("Index value")+xlab('Population \u03b7\u00B2')
p<- p + ggtitle("Ordinal")
p<- p + scale_color_discrete(labels=c('\u03b7\u00B2','\u03b5\u00B2'))
p<- p + facet_wrap(~N,nrow = 1,ncol = 4)
p<-p+theme_classic()+  theme(plot.title = element_text(hjust = 0.5))
p<-p+ylim(0,.6)
p<-p+theme(strip.text.x = element_text(size = 7))
p4<-p
p4


library(cowplot)
legend_b <- get_legend(
  p1 +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

startBookFig("../paper/figure2.jpg",TRUE,font =10)
plot_grid(p1+ theme(legend.position="none")+labs(x = NULL),
          p2+ theme(legend.position="none")+labs(x = NULL),
          p3+ theme(legend.position="none")+labs(x = NULL),
          p4+ theme(legend.position="none"), legend_b, ncol = 1,rel_heights = c(1,1,1,1,.2))
dev.off()


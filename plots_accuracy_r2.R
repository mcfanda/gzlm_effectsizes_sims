source("functions.R")

library(ggplot2)
load("normal/r2_simdata.RData")

data<-r2_normal_simdata
data<-data[data$stat=="mean",]
names(data)
d<-reshape(data,varying = c("r2","ar2"),direction = "long",v.names = "y")
d<-d[d$exp_par<.41,]
d$time<-factor(d$time)
d$Index<-d$time

d$N<-factor(d$N)
levels(d$N)<-c("N=25","N=50","N=75","N=100")
p<-ggplot(d,aes(y=y,x=exp_par,col=Index))
p<-p+geom_smooth()
#p<-p+geom_smooth(aes(y=ar2,col=N),linetype=2,linewidth=1.5)
p<-p+geom_abline(intercept = 0,slope = 1)
p<-p + ylab("Index value")+xlab(bquote('Population '~R^2))
p<- p + ggtitle("Gaussian")
p<- p + scale_color_discrete(labels=c(bquote(R^2),bquote('adj.'~R^2)))
p<- p + facet_wrap(~N,nrow = 1,ncol = 4)
p<-p+theme(strip.text.x = element_text(size = 7))

p<-p+theme_classic()+  theme(plot.title = element_text(hjust = 0.5))
p1<-p
p1
tab1<-data[,c("N","exp_par","r2","ar2")]
tab1$id<-rep(1:20,each=4)
tab1<-reshape(tab1,v.names =c("r2","ar2"),direction = "wide",timevar = "N",idvar="id")

load("binomial/r2_simdata.RData")
data<-r2_binomial_simdata
data<-data[data$stat=="mean",]
names(data)
d<-reshape(data,varying = c("r2","ar2"),direction = "long",v.names = "y")
d<-d[d$exp_par<.41,]

d$time<-factor(d$time)
d$Index<-d$time

d$N<-factor(d$N)
levels(d$N)<-c("N=25","N=50","N=75","N=100")
p<-ggplot(d,aes(y=y,x=exp_par,col=Index))
p<-p+geom_smooth()
#p<-p+geom_smooth(aes(y=ar2,col=N),linetype=2,linewidth=1.5)
p<-p+geom_abline(intercept = 0,slope = 1)
p<-p + ylab("Index value")+xlab(bquote('Population '~R^2))
p<- p + ggtitle("Logistic")
p<- p + scale_color_discrete(labels=c(bquote(R^2),bquote('adj.'~R^2)))
p<- p + facet_wrap(~N,nrow = 1,ncol = 4)
p<-p+theme(strip.text.x = element_text(size = 7))
p<-p+theme_classic()+  theme(plot.title = element_text(hjust = 0.5))
p2<-p
p2


load("multinomial/r2_simdata.RData")

data<-r2_simdata
data<-data[data$stat=="mean",]
names(data)
d<-reshape(data,varying = c("r2","ar2"),direction = "long",v.names = "y")
d<-d[d$exp_par<.41,]

d$time<-factor(d$time)
d$Index<-d$time

d$N<-factor(d$N)
levels(d$N)<-c("N=25","N=50","N=75","N=100")
p<-ggplot(d,aes(y=y,x=exp_par,col=Index))
p<-p+geom_smooth()
#p<-p+geom_smooth(aes(y=ar2,col=N),linetype=2,linewidth=1.5)
p<-p+geom_abline(intercept = 0,slope = 1)
p<-p + ylab("Index value")+xlab(bquote('Population '~R^2))
p<- p + ggtitle("Multinomial")
p<- p + scale_color_discrete(labels=c(bquote(R^2),bquote('adj.'~R^2)))
p<-p+theme(strip.text.x = element_text(size = 7))
p<- p + facet_wrap(~N,nrow = 1,ncol = 4)
p<-p+theme_classic()+  theme(plot.title = element_text(hjust = 0.5))
p3<-p
p3



load("ordinal/r2_simdata.RData")

data<-r2_simdata
data<-data[data$stat=="mean",]
names(data)
d<-reshape(data,varying = c("r2","ar2"),direction = "long",v.names = "y")
d<-d[d$exp_par<.41,]

d$time<-factor(d$time)
d$Index<-d$time

d$N<-factor(d$N)
levels(d$N)<-c("N=25","N=50","N=75","N=100")
p<-ggplot(d,aes(y=y,x=exp_par,col=Index))
p<-p+geom_smooth()
#p<-p+geom_smooth(aes(y=ar2,col=N),linetype=2,linewidth=1.5)
p<-p+geom_abline(intercept = 0,slope = 1)
p<-p + ylab("Index value")+xlab(bquote('Population '~R^2))
p<- p + ggtitle("Ordinal")
p<-p+theme(strip.text.x = element_text(size = 7))
p<- p + scale_color_discrete(labels=c(bquote(R^2),bquote('adj.'~R^2)))
p<- p + facet_wrap(~N,nrow = 1,ncol = 4)
p<-p+theme_classic()+  theme(plot.title = element_text(hjust = 0.5))
p4<-p
p4

legend_b <- get_legend(
  p1 +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

library(cowplot)
#startBookFig("../paper/figura2.jpg",TRUE,font =10)
startBookFig("../paper/figura1.jpg",TRUE,font =10)
plot_grid(p1+ theme(legend.position="none")+labs(x = NULL)+theme(strip.text.x = element_text(size = 7)),
          p2+ theme(legend.position="none")+labs(x = NULL)+theme(strip.text.x = element_text(size = 7)),
          p3+ theme(legend.position="none")+labs(x = NULL)+theme(strip.text.x = element_text(size = 7)),
          p4+ theme(legend.position="none")+theme(strip.text.x = element_text(size = 7)), legend_b, ncol = 1,rel_heights = c(1,1,1,1,.2))
dev.off()
#dev.off()

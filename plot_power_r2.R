library(ggplot2)
source("functions.R")
load("normal/r2_simdata.RData")

dd<-r2_normal_simdata[r2_normal_simdata$stat=="mean",]
dd<-dd[dd$exp_par<.41,]

dd$n<-dd$N
dd$N<-factor(dd$N)
lambda2<-dd$r2/(1-dd$r2)
p<-pwr::pwr.f2.test(u=3,v = dd$n-4,f2 = lambda2,sig.level = .05)
dd$est_powr<-p$power
lambda2<-dd$ar2/(1-dd$ar2)
p<-pwr::pwr.chisq.test(w=sqrt(lambda2),df=3,N=dd$n)
dd$est_powar<-p$power


levels(dd$N)<-c("N=25","N=50","N=75","N=100")
g<-ggplot(dd,aes(x=exp_par,y=pow_mod))+geom_smooth(aes(color="Actual"),se = FALSE)
g<-g+geom_smooth(aes(x=exp_par,y=est_powr,color="R\u00B2"),se=FALSE)
g<-g+geom_smooth(aes(x=exp_par,y=est_powar,color="adj R\u00B2"),se=FALSE )
g<-g +ggtitle("Gaussian")
g<-g+theme(strip.text.x = element_text(size = 7))

#g<-g+geom_smooth(aes(x=eta_pop,y=lpow,linetype="Wald"),se=FALSE, color="black")
g<-g + xlab('Population R\u00B2')+ylab("Power (1-\u03b2)")
g<-g + facet_wrap( ~N , ncol = 4,)+labs(color="Method")
g<- g + scale_color_manual(values=c("black","#00BFC4","#F8766D"))
g1<-g+theme_classic()+ theme(plot.title = element_text(hjust = 0.5))

g1


load("binomial/r2_simdata.RData")

dd<-r2_binomial_simdata[r2_binomial_simdata$stat=="mean",]
dd<-dd[dd$exp_par<.41,]
dd$n<-dd$N
dd$N<-factor(dd$N)
d0<-2*.5*log(.5)
d0<--2*d0
lambda2<-dd$r2*d0
p<-pwr::pwr.chisq.test(w=sqrt(lambda2),df=3,N=dd$n)
dd$est_powr<-p$power
lambda2<-dd$ar2*d0
p<-pwr::pwr.chisq.test(w=sqrt(lambda2),df=3,N=dd$n)
dd$est_powar<-p$power


levels(dd$N)<-c("N=25","N=50","N=75","N=100")
g<-ggplot(dd,aes(x=exp_par,y=pow_mod))
g<-g+geom_smooth(aes(color="Actual"),se = FALSE)
g<-g+geom_smooth(aes(x=exp_par,y=est_powr,color="R\u00B2"),se=FALSE )
g<-g+geom_smooth(aes(x=exp_par,y=est_powar,color="adj R\u00B2"),se=FALSE )
g<-g+theme(strip.text.x = element_text(size = 7))

#g<-g+geom_smooth(aes(x=eta_pop,y=lpow,linetype="Wald"),se=FALSE, color="black")
g<-g + xlab('Population R\u00B2')+ylab("Power (1-\u03b2)")
g<-g + facet_wrap( ~N , ncol = 4,)+labs(color="Method")
g<-g +ggtitle("Logistic")
g<- g + scale_color_manual(values=c("black","#00BFC4","#F8766D"))
#g<- g + scale_color_manual(values=c("black","#00BFC4","#F8766D"))
g2<-g+theme_classic()+ theme(plot.title = element_text(hjust = 0.5))

r2_simdata<-NULL
load("multinomial/r2_simdata.RData")

dd<-r2_simdata[r2_simdata$stat=="mean",]
dd<-dd[dd$exp_par<.41,]
dd$n<-dd$N
dd$N<-factor(dd$N)
d0<-3*.5*log(.5)
d0<--2*d0
lambda2<-dd$r2*d0
p<-pwr::pwr.chisq.test(w=sqrt(lambda2),df=6,N=dd$n)
dd$est_powr<-p$power
lambda2<-dd$ar2*d0
p<-pwr::pwr.chisq.test(w=sqrt(lambda2),df=6,N=dd$n)
dd$est_powar<-p$power


levels(dd$N)<-c("N=25","N=50","N=75","N=100")
g<-ggplot(dd,aes(x=exp_par,y=pow_mod))
g<-g+geom_smooth(aes(color="Actual"),se = FALSE)
g<-g+geom_smooth(aes(x=exp_par,y=est_powr,color="R\u00B2"),se=FALSE )
g<-g+geom_smooth(aes(x=exp_par,y=est_powar,color="adj R\u00B2"),se=FALSE )
g<-g+theme(strip.text.x = element_text(size = 7))

#g<-g+geom_smooth(aes(x=eta_pop,y=lpow,linetype="Wald"),se=FALSE, color="black")
g<-g + xlab('Population R\u00B2')+ylab("Power (1-\u03b2)")
g<-g + facet_wrap( ~N , ncol = 4,)+labs(color="Method")
g<-g +ggtitle("Multinomial")
g<- g + scale_color_manual(values=c("black","#00BFC4","#F8766D"))
g3<-g+theme_classic()+ theme(plot.title = element_text(hjust = 0.5))
g3

#g<-g + xlab('Population \u03b7\u00B2')+ylab("Power (1-\u03b2)")



r2_simdata<-NULL
load("ordinal/r2_simdata.RData")

dd<-r2_simdata[r2_simdata$stat=="mean",]
dd<-dd[dd$exp_par<.41,]
dd$n<-dd$N
dd$N<-factor(dd$N)
d0<-3*.5*log(.5)
d0<--2*d0
lambda2<-dd$r2*d0
p<-pwr::pwr.chisq.test(w=sqrt(lambda2),df=3,N=dd$n)
dd$est_powr<-p$power
dd$ar2<-ifelse(dd$ar2>0,dd$ar2,0)
lambda2<-dd$ar2*d0
p<-pwr::pwr.chisq.test(w=sqrt(lambda2),df=3,N=dd$n)
dd$est_powar<-p$power


levels(dd$N)<-c("N=25","N=50","N=75","N=100")
g<-ggplot(dd,aes(x=exp_par,y=pow_mod))
g<-g+ geom_smooth(aes(color="Actual"),se = FALSE)
g<-g+geom_smooth(aes(x=exp_par,y=est_powr,color="R\u00B2"),se=FALSE )
g<-g+geom_smooth(aes(x=exp_par,y=est_powar,color="adj R\u00B2"),se=FALSE )
g<-g+theme(strip.text.x = element_text(size = 7))

#g<-g+geom_smooth(aes(x=eta_pop,y=lpow,linetype="Wald"),se=FALSE, color="black")
g<-g + xlab('Population R\u00B2')+ylab("Power (1-\u03b2)")
g<-g + facet_wrap( ~N , ncol = 4,)+labs(color="Method")
g<-g +ggtitle("Ordinal")
g<- g + scale_color_manual(values=c("black","#00BFC4","#F8766D"))
g4<-g+theme_classic()+ theme(plot.title = element_text(hjust = 0.5))
g4

library(cowplot)

legend_b <- get_legend(
  g1 +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

startBookFig("../paper/figura3.jpg",TRUE,font =10)
plot_grid(g1+ theme(legend.position="none")+labs(x = NULL)+theme(strip.text.x = element_text(size = 7)),
          g2+ theme(legend.position="none")+labs(x = NULL)+theme(strip.text.x = element_text(size = 7)),
          g3+ theme(legend.position="none")+labs(x = NULL)+theme(strip.text.x = element_text(size = 7)),
          g4+ theme(legend.position="none")+theme(strip.text.x = element_text(size = 7)), legend_b, ncol = 1,rel_heights = c(1,1,1,1,.2))
dev.off()


#g<-g + xlab('Population \u03b7\u00B2')+ylab("Power (1-\u03b2)")

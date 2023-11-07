source("functions.R")

library(ggplot2)
load("binomial/requiredN.Rdata")
requiredN$model<-"Binomial"
dd<-requiredN
load("multinomial/requiredN.Rdata")
requiredN$model<-"Multinomial"
dd<-rbind(dd,requiredN)
load("ordinal/requiredN.Rdata")
requiredN$eN<-requiredN$eN
requiredN$model<-"Ordinal"
dd<-rbind(dd,requiredN)



g<-ggplot(dd,aes(x=eta_pop))
g<-g+geom_smooth(aes(y=eN,color="R\u00B2"),se=FALSE )
g<-g+geom_smooth(aes(y=N,color="Actual"),se=FALSE ,linetype=2)
g<-g+theme(strip.text.x = element_text(size = 7))
g<-g + xlab('Population R\u00B2')+ylab("Required N")
g<- g + scale_color_manual(values=c("black","#00BFC4"))
g<-g + facet_wrap( ~model , ncol = 4,)+labs(color="Model")
g<-g+ggtitle("Omnibus Test")
g<-g+theme_classic()+ theme(plot.title = element_text(hjust = 0.5))+labs(color="Method")
g1<-g
g1

dd<-NULL
load("binomial/requiredN_eta2.Rdata")
requiredN$model<-"Logistic"
dd<-requiredN
load("multinomial/requiredN_eta2.Rdata")
requiredN$model<-"Multinomial"
dd<-rbind(dd,requiredN)
load("ordinal/requiredN_eta2.Rdata")
requiredN$eN<-requiredN$eN
requiredN$model<-"Ordinal"
dd<-rbind(dd,requiredN)


g<-ggplot(dd,aes(x=eta_pop))
g<-g+geom_smooth(aes(y=eN,color="\u03b3\u00B2"),se=FALSE )
g<-g+geom_smooth(aes(y=N,color="Actual"),se=FALSE ,linetype=2)
g<-g+theme(strip.text.x = element_text(size = 7))
g<-g + xlab('Population \u03b7\u00B2')+ylab("Required N")
g<- g + scale_color_manual(values=c("black","#00BFC4"))
g<-g + facet_wrap( ~model , ncol = 4,)+labs(color="Model")
g<-g+ggtitle("Individual Predictor")
g<-g+theme_classic()+ theme(plot.title = element_text(hjust = 0.5))+labs(color="Method")
g2<-g

g2
library(cowplot)

startBookFig("../paper/figura5.jpg",TRUE,font =10)
plot_grid(g1+labs(x = NULL)+theme(strip.text.x = element_text(size = 7)),
          g2+theme(strip.text.x = element_text(size = 7)),  ncol = 1,rel_heights = c(1,1))
dev.off()


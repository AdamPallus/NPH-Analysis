

#This is just a little script for a plotting demo. 
#see transientscript.R for the code to generate these measurements

library(ggplot2)
library(relaimpo)
library(manipulate)
library(dplyr)
source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')
source('markEnhancement.R')

z<- readRDS('Measured Data for transdemoR.RDS')

p<- filter(z,saccade.type!='saccade.only')

goodsacs<- unique(p$sacnum)

nsac=length(goodsacs)
manipulate(ggplot(filter(z,sacnum==goodsacs[sac]))+
             geom_line(aes(counter,verg.velocity),color='purple')+
             geom_line(aes(counter,conj.velocity/10),color='pink')+
             geom_point(aes(counter,enhancenum*0-50))+
             geom_point(aes(counter,verg.velocity,color=transient.type),size=1,
                        data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
             geom_hline(yintercept = c(-2,2))+
             geom_line(aes(counter,verg.angle*5),color='darkgreen')+
             geom_vline(aes(xintercept = verg.onset))+
             coord_cartesian(ylim=c(-100,100)),
           sac=slider(1,nsac,step=1)
)
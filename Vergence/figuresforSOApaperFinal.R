source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')
library(dplyr)
library(ggplot2)
library(manipulate)

makefig1<- function(t,neuronChoice,starttime,stoptime,sd=10,maxplot=150){
  gc<- filter(t,neuron==neuronChoice)
  gc<-mutate(gc,time=row_number(), showrasters=replace(rasters,rasters<1,NA))
  gc<- filter(gc, time>=starttime,time<stoptime)
  gc<- mutate(gc,sdf=spikedensity(rasters,sd=sd))
  gs<- ggplot(gc)+
    geom_area(aes(time,sdf),alpha=1/10)+
    geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
    geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
    geom_point(aes(time,showrasters+50),shape='|')+
    theme_bw()+
    xlab('Time (ms)')+
    ylab('Vergence Velocity (deg/s)')+
    ylim(c(-100,maxplot))
}

t<- readRDS('SOA-NRTP.RDS')

fig1a <- makefig1(t,'Ozette-107',140000,160000,sd=15)
fig1b <- makefig1(t, 'Bee-218',120000,127500,sd=15)
fig1c <- makefig1(t,'Bee-215',90000,100000,sd=20,maxplot=250)

#------------- Modeling ---------------------
tp<-readRDS('ModelFitValues.RDS')
tp<- filter(tp,area=='SOA')

fig2<- ggplot(tp)+
  geom_point(aes(verg.angle,Slow.Vergence),size=3,alpha=0.2)+
  geom_vline(xintercept=0,linetype=2)+
  xlab('Sensitivity to Vergence Angle')+
  ylab('Sensitivity to Vergence Velocity')


fig3<- ggplot(tp)+
  geom_point(aes(Convergence,Slow.Vergence),size=3,alpha=0.2)+
  geom_abline(slope=1)+
  geom_vline(xintercept=0,linetype=2)+
  xlab('Vergence Velocity Sensitivity during Enhanced Convergence')+
  ylab('Vergence Velocity Sensitivity during Slow Vergence Movements')

ggsave('Figure1A.pdf',plot=fig1a,height=4)
ggsave('Figure1B.pdf',plot=fig1b,height=4)
ggsave('Figure1C.pdf',plot=fig1c,height=4)
ggsave('Figure2.pdf',plot=fig2)
ggsave('Figure3.pdf',plot=fig3)
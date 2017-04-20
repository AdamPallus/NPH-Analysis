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
# tp<-readRDS('ModelFitValues.RDS')
tp<- readRDS('ModelParams_20ms.RDS')
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

#------Behavior---------------
t<- readRDS('enhanceandsaccadesmarkedSOA.RDS')

t %>%
  filter(cellnum>100) %>%
  group_by(neuron,sacnum) %>%
  summarize(verg.amp=first(verg.amp),
            peak.verg.velocity=first(peak.verg.velocity),
            saccade.type=first(saccade.type),
            monkey=first(monkey),
            max.verg.velocity=max(verg.velocity),
            min.verg.velocity=min(verg.velocity),
            nosac=saccade.type=='no.saccade',
            max.conj.velocity=max(conj.velocity),
            r.amp=first(r.amp)) %>%
  filter(abs(peak.verg.velocity)<300,
         abs(verg.amp)<20)->
  g

ggplot(g)+
  geom_point(aes(verg.amp,peak.verg.velocity,color=saccade.type),alpha=1/20,size=2)+
  coord_cartesian(xlim=c(-20,20),ylim=c(-300,300))+
  facet_wrap(~monkey)
  
ggplot(g)+
  geom_point(aes(verg.amp,max.verg.velocity,color=nosac),alpha=1/20,size=2)+
  # coord_cartesian(xlim=c(-20,20),ylim=c(-300,300))+
  facet_wrap(~monkey)

ggplot(filter(g,saccade.type != 'diverging',nosac==F,r.amp>8,monkey=='Bee'))+
  geom_point(aes(verg.amp,max.verg.velocity),alpha=1/10,size=1.5)+
  geom_rect(xmin=-0.1, xmax=0.1,ymin=-10,ymax=100,alpha=1/50,color='red',fill=NA)+
  geom_rect(xmin=3.9, xmax=4.1,ymin=30,ymax=155,color='blue',fill=NA)+
  theme_bw()+
  xlab('Vergence Amplitude (deg)')+
  ylab('Maximum Vergence Velocity (deg/s)')+
  annotate('text',x=0,y=-20,label='Conjugate Saccades')+
  annotate('text',x=4,y=20,label='Converging Saccades (4 deg)')+
  ylim(c(-25,200))

gs<-ggplot(filter(g,saccade.type != 'diverging',nosac==F,r.amp>8,monkey=='Bee'))+
  geom_point(aes(verg.amp,max.verg.velocity),alpha=1/10,size=1.5)

gss<- gs+geom_rect(xmin=-0.1, xmax=0.1,ymin=-10,ymax=100,alpha=1/50,color='darkgray',fill=NA)+
  geom_rect(xmin=3.9, xmax=4.1,ymin=30,ymax=155,color='black',fill=NA,linetype=2)+
  xlab('Vergence Amplitude (deg)')+
  ylab('Maximum Vergence Velocity (deg/s)')+
  annotate('text',x=0,y=-20,label='Conjugate Saccades')+
  annotate('text',x=4,y=20,label='Converging Saccades (4 deg)')+
  ylim(c(-25,200))+
  theme_bw()
#---- 
#smaller file attempt
gp<- filter(g,saccade.type != 'diverging',nosac==F,r.amp>5,monkey=='Bee') 

gp %>%
  filter(max.verg.velocity<20) %>%
  sample_frac(size=0.20) ->
  gps

gp %>%
  filter(max.verg.velocity>=20) %>%
  rbind(gps)->
  gpa

ggplot(sample_frac(gp,.5))+
  geom_point(aes(verg.amp,max.verg.velocity),alpha=1/10,size=1.5)+
  geom_rect(xmin=-0.1, xmax=0.1,ymin=-10,ymax=100,alpha=1/50,color='darkgray',fill=NA)+
  geom_rect(xmin=3.9, xmax=4.1,ymin=30,ymax=155,color='black',fill=NA,linetype=2)+
  xlab('Vergence Amplitude (deg)')+
  ylab('Maximum Vergence Velocity (deg/s)')+
  annotate('text',x=0,y=-20,label='Conjugate Saccades')+
  annotate('text',x=4,y=20,label='Converging Saccades (4 deg)')+
  ylim(c(-25,200))+
  theme_bw()

  ggsave('Figure 9.PDF',height=6,width=8)

  # coord_cartesian(xlim=c(-20,20),ylim=c(-300,300))+
  # facet_wrap(~monkey)+
  # geom_abline(slope=7.5,color='pink',size=2)+
  # stat_smooth(method='lm',aes(verg.amp,max.verg.velocity))

ggplot(g)+
  geom_point(aes(verg.amp,min.verg.velocity),alpha=1/20,size=2)+
  # coord_cartesian(xlim=c(-20,20),ylim=c(-300,300))+
  facet_wrap(~monkey)


ggplot(filter(g,saccade.type == 'no.saccade'))+
  geom_point(aes(verg.amp,max.verg.velocity),color='green', alpha=1/10,data=filter(g,saccade.type=='conj'))+
  geom_point(aes(verg.amp,max.verg.velocity),alpha=1/5,size=2)+
  coord_cartesian(xlim=c(-2,5),ylim=c(-25,125))+
  facet_wrap(~monkey)

ggplot(g)+
  geom_boxplot(aes(saccade.type,max.verg.velocity))+
  facet_wrap(~monkey)

ggplot(filter(g,max.conj.velocity>150))+
  geom_boxplot(aes(saccade.type,max.verg.velocity))+
  facet_wrap(~monkey)

ggplot(g)+
  geom_point(aes(verg.amp,max.conj.velocity),alpha=1/20,size=2)+
  facet_wrap(~monkey)

library(manipulate)
library(dplyr)
library(ggplot2)

#These folders have SOA-NRTP.RDS split into SOA and NRTP and with the enhancements identified
#saves a lot of time to load these. Also has dynamic lead time calculated
#**For plotting, only load one at a time, obviously. Run all of the below on one cell at a time

t<- readRDS('enhancemarked.RDS')
t<- readRDS('enhancemarked-NRTP.RDS')


z<- filter(t,neuron=='Bee-211') #Strong burst/pause from SOA
z<- filter(t,neuron=='Bee-112') #A position cell SOA
z<- filter(t,neuron=='Bee-209') #far response cell with no burst SOA

z<- filter(t,neuron=='Bee-15') #Small burst-tonic NRTP
z<- filter(t,neuron=='Bee-33') #strong burst-tonic w/pauses NRTP
z<- filter(t,neuron=='Bee-09') #far response cell with no burst NRTP

z<- filter(t,neuron=='Ozette-110')

z<- z %>% group_by(time) %>% summarize_each(funs(first))

z<- mutate(z, verg.velocity=lev-rev,
           converge=enhance.type=='convergence',
           converge.velocity=replace(verg.velocity,!converge,NA),
           diverge=enhance.type=='divergence',
           diverge.velocity=replace(verg.velocity,!diverge,NA),
           showrasters=replace(rasters,rasters<1,NA))


windowsize<- 5000
manipulate(ggplot(filter(z,time>=window,time<window+windowsize))+
             geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
             geom_line(aes(time,(lep-rep)*5+100),color='darkgreen')+
             geom_line(aes(time,converge.velocity),color='darkblue',size=2,alpha=0.3)+
             geom_line(aes(time,diverge.velocity),color='darkred',size=2,alpha=0.3)+
             geom_point(aes(time,showrasters+50),shape='|')+
             ylim(c(-100,200))+
             theme_bw()
           ,
           window=slider(1,max(z$time-windowsize),step=windowsize))

##snapshots:
ggplot(filter(z,time>=51500,time<54000))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,slow.prediction),color='orange')+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  geom_line(aes(time,(lep-rep)*5-100),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  theme_bw()

ggplot(filter(z,time>=54500,time<57000))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  geom_line(aes(time,(lep-rep)*5-100),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  theme_bw()



#SUMMARY OF SACCADEs
source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')
bufferlength<-100
zz<- joinsaccadesuniform(z,buffer = bufferlength,threshold = 20)

zz %>%
  group_by(neuron,sacnum) %>%
  mutate(conj.h=(lep+rep)/2,
         conj.v=(lepV+repV)/2,
         verg.angle=(lep-rep)) %>%
  summarize(peak.conj.velocity=maxabs(conj.velocity),
            max.verg.velocity=max(verg.velocity),
            min.verg.velocity=min(verg.velocity),
            conj.h.amp=last(conj.h)-first(conj.h),
            conj.v.amp=last(conj.v)-first(conj.v),
            # conj.angle=atan2(conj.v,conj.h)*180/pi,
            r.amp=sqrt(conj.h.amp^2+conj.v.amp^2),
            verg.amp=last(verg.angle)-first(verg.angle)) %>%
  mutate(saccade.type='conj',
         saccade.type=replace(saccade.type,verg.amp< -2 & r.amp>2,'diverging'),
         saccade.type=replace(saccade.type,verg.amp> 2& r.amp>2,'converging'),
         saccade.type=replace(saccade.type,is.na(sacnum) | r.amp<2,'no.saccade')) ->
  sz



ggplot(filter(sz,saccade.type != 'no.saccade',abs(conj.v.amp)<2,
              peak.conj.velocity<800,max.verg.velocity<200))+
  geom_point(aes(peak.conj.velocity,max.verg.velocity,color=saccade.type))+
  stat_smooth(aes(peak.conj.velocity,max.verg.velocity,color=saccade.type),method='lm')
# 
# d<-filter(sz,saccade.type != 'no.saccade',abs(conj.v.amp)<2,peak.conj.velocity<800,max.verg.velocity<200)
# mconv= lm(max.verg.velocity~peak.conj.velocity,data=filter(d,saccade.type=='converging'))
# mconj= lm(max.verg.velocity~peak.conj.velocity,data=filter(d,saccade.type=='conj'))
# summary(mconv)
# summary(mconj)

gg<- filter(sz,verg.amp>0,verg.amp<20,max.verg.velocity<300)

# ggplot(gg)+geom_point(aes(verg.amp,max.verg.velocity,color=saccade.type))+stat_smooth(method='lm')

#This plot should be included. possibly use all the data -- requires rerunning above code. 
ggplot(filter(gg,saccade.type!='no.saccade'))+
  geom_point(aes(verg.amp,max.verg.velocity,color=peak.conj.velocity),alpha=0.7,size=3)+
  scale_color_continuous(low='black',high='orange')+
  # stat_smooth(method='lm',aes(verg.amp,max.verg.velocity))+
  xlab('Vergence Amplitude')+
  ylab('Maximum Vergence Velocity')

ggd<- filter(sz, verg.amp<0,verg.amp> -9, min.verg.velocity<300)
ggplot(filter(ggd,saccade.type!='no.saccade'))+
  geom_point(aes(verg.amp,min.verg.velocity,color=peak.conj.velocity),alpha=0.7,size=3)+
  scale_color_continuous(low='black',high='orange')+
  # stat_smooth(method='lm',aes(verg.amp,min.verg.velocity))+
  xlab('Vergence Amplitude')+
  ylab('Minimum Vergence Velocity')

#Next, manipulate through the saccades and choose an example saccade to zoom an annotate

p<- filter(sz,abs(verg.amp)<1,r.amp>4)
p<- filter(sz,verg.amp> 3)
goodsacs<- unique(p$sacnum)     
nsac=length(goodsacs)


manipulate(ggplot(filter(zz,sacnum==goodsacs[sac]))+
             # geom_area(aes(counter,sdf),alpha=1/10)+
             # geom_point(aes(counter,showrasters+100),shape='|')+
             geom_line(aes(counter-bufferlength,(lev-rev)),color='darkblue',alpha=1)+
             # geom_line(aes(counter,(lep+rep)/2))+
             geom_line(aes(counter-bufferlength,(lev+rev)/2),color='maroon',alpha=1,linetype=1)+
             geom_line(aes(counter-bufferlength,(levV+revV)/2),color='maroon',alpha=1,linetype=2)+
             # geom_line(aes(counter,enhance.velocity-100),size=2,color='darkblue')+
             geom_line(aes(counter-bufferlength,(lep-rep)*10-100),color='darkgreen')+
             xlab('Time(ms)')+
             ylab('Velocity (deg/s)')+
             theme_bw()+
             coord_cartesian(ylim=c(-200,200))
             
           ,
           sac=slider(2,nsac,step=1))


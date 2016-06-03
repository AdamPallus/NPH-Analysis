# Rereading Busettini, I realize that I did not appreciate the disinction he was making between
# vergence transients and vergence enhancement. He defines "transients" to be the paired positive
# and negative vergence velocity peaks observed during saccades to targets at the same distance.
# When a saccade is made in depth, enhancement occurs. In his view, this enhancement "covers up"
# the negative transient so we don't see it during these saccades. However, this is dependent on
# the timing of the saccades relative to the vergence. If the saccade starts "too soon," the 
# vergence enhancemnt hasn't had time to "build up" and thus the negative transient is visible.

# What we want to do in this script is evaluate the transients that occur during saccades
# to targets at the same depth. Then we can subtract this from the observed vergence velocity
# to obtain the vergence enhancement!!

# Planned programming procedure:
# 1) identify saccades with verg.amp < 1 (?) 
# 2) plot positive and negative transients against saccade peak velocity
# 3) fit an exponential model
# 4) use this model to predict the transients that would occur during saccades with verg.amp>1
# 5) subtract and observe
library(dplyr)
library(ggplot2)
source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')
t<-readRDS('SOA-NRTP.RDS')
z<- filter(t,neuron=='Bee-113')

bufferlength<- 200
saccade.length<- 150
z%>%
  group_by(neuron) %>%
  mutate(time=row_number()) %>%
  do(joinsaccadesuniform(.,buffer=bufferlength,threshold=20,saccade.length=saccade.length))->
  # do(joinsaccades(.,buffer=bufferlength,threshold=20))->
  z

#Once saccades have been marked, we measure lots of things about each saccade. 
#For this plot, we also remove all data that isn't part of a saccade or the buffer period
z %>%
  group_by(neuron) %>%
  mutate(issaccade=!is.na(sacnum)) %>%
  filter(issaccade) %>%
  group_by(neuron,sacnum) %>%
  summarize(#dur=n(),
    peak.conj.velocity=maxabs(conj.velocity),
    peak.R.H= maxabs(rev),
    peak.R.V= maxabs(revV),
    peak.L.H= maxabs(lev),
    peak.L.V= maxabs(levV),
    R.H.Amp=rep[n()]-rep[1],
    L.H.Amp=lep[n()]-lep[1],
    R.V.Amp=repV[n()]-repV[1],
    L.V.Amp=lepV[n()]-lepV[1],
    r.angle=atan2(R.V.Amp,R.H.Amp)*180/pi,
    r.amp=sqrt(R.H.Amp^2+R.V.Amp^2),
    nspikes=sum(rasters),
    vect.amp= (sqrt(R.H.Amp^2+R.V.Amp^2)+sqrt(L.H.Amp^2+L.V.Amp^2))/2,
    maxamp=max(abs(R.H.Amp),abs(R.V.Amp),abs(L.H.Amp),abs(L.V.Amp)),
    verg.amp=verg.angle[n()-bufferlength]-verg.angle[bufferlength+1],
    peak.verg.velocity= maxabs(verg.velocity),
    min.verg.trans = min(verg.velocity),
    max.verg.trans = max(verg.velocity),
    off.verg.velocity=min(abs(min.verg.trans),abs(max.verg.trans)),
    min.verg.angle=min(verg.angle),
    max.verg.angle=max(verg.angle),
    max.verg.velocity=max(verg.velocity),
    min.verg.velocity=min(verg.velocity),
    maxfr=max(sdf),
    # verg.amp= last(verg.angle)-first(verg.angle),
    verg.amp= verg.angle[n()-bufferlength]-verg.angle[bufferlength])->
  s
s<- filter(z,peak.conj.velocity<1500,min.verg.trans> -1000)


ggplot(filter(s,abs(verg.amp)<2,max.verg.angle>12,abs(r.angle)<20))+
  geom_point(aes(peak.conj.velocity,min.verg.trans))+
  geom_point(aes(peak.conj.velocity,max.verg.trans),color='darkgray')+
  coord_cartesian(xlim=c(0,1000),ylim=c(-300,300))+
  ylab('Pos & Neg peak vg vel')+
  ggtitle('Rightward saccades at near -OZETTE')

ggplot(filter(s,abs(verg.amp)<2,max.verg.angle<4,abs(r.angle)<20))+
  geom_point(aes(peak.conj.velocity,min.verg.trans))+
  geom_point(aes(peak.conj.velocity,max.verg.trans),color='darkgray')+
  coord_cartesian(xlim=c(0,1000),ylim=c(-300,300))+
  ylab('Pos & Neg peak vg vel')+
  ggtitle('Rightward saccades at far -Ozette')


#------------------------------

z %>%
  group_by(neuron) %>%
  mutate(issaccade=!is.na(sacnum)) %>%
  filter(issaccade) %>%
  group_by(neuron,sacnum) %>%
  mutate(#dur=n(),
    peak.conj.velocity=maxabs(conj.velocity),
    peak.R.H= maxabs(rev),
    peak.R.V= maxabs(revV),
    peak.L.H= maxabs(lev),
    peak.L.V= maxabs(levV),
    R.H.Amp=rep[n()]-rep[1],
    L.H.Amp=lep[n()]-lep[1],
    R.V.Amp=repV[n()]-repV[1],
    L.V.Amp=lepV[n()]-lepV[1],
    r.angle=atan2(R.V.Amp,R.H.Amp)*180/pi,
    r.amp=sqrt(R.H.Amp^2+R.V.Amp^2),
    nspikes=sum(rasters),
    vect.amp= (sqrt(R.H.Amp^2+R.V.Amp^2)+sqrt(L.H.Amp^2+L.V.Amp^2))/2,
    maxamp=max(abs(R.H.Amp),abs(R.V.Amp),abs(L.H.Amp),abs(L.V.Amp)),
    verg.amp=verg.angle[n()-bufferlength]-verg.angle[bufferlength+1],
    peak.verg.velocity= maxabs(verg.velocity),
    min.verg.trans = min(verg.velocity),
    max.verg.trans = max(verg.velocity),
    off.verg.velocity=min(abs(min.verg.trans),abs(max.verg.trans)), #this needs to be fixed because it's not the peak velocity, but the distance moved that determines the off direction. Redo this using verg.amp
    min.verg.angle=min(verg.angle),
    max.verg.angle=max(verg.angle),
    max.verg.velocity=max(verg.velocity),
    min.verg.velocity=min(verg.velocity),
    maxfr=max(sdf),
    # verg.amp= last(verg.angle)-first(verg.angle),
    verg.amp= verg.angle[n()-bufferlength]-verg.angle[bufferlength],
    counter=time-time[1]-bufferlength)->
  z

"It might be worth changing joinsaccadesuniform to give the time of the 
saccade offset. This would affect how we calculate vergence amplitude. 
It seems like some of the time, there is a really fast divergent transient
followed by a slower, longer convergent movement. Is this sneaking divergent
saccades into the saccade only bin? Maybe this isn't a big deal..."






ggplot(filter(z,verg.amp<1,r.amp>5,abs(r.angle)<20))+
  geom_line(aes(counter,conj.velocity,group=sacnum),alpha=0.5)+
  geom_line(aes(counter,verg.velocity,group=sacnum),color='hotpink')

verg.thresh<- 0.4
z$saccade.type<- 'saccade.only'
z$saccade.type[z$verg.amp< -verg.thresh]= 'diverging'
z$saccade.type[z$verg.amp> verg.thresh]= 'converging'
z$saccade.type<-as.factor(z$saccade.type)

ggplot(filter(z,abs(r.angle)<20,counter> -25,counter< 150))+
  geom_line(aes(counter,conj.velocity,group=interaction(neuron,sacnum)),alpha=0.2)+
  geom_line(aes(counter,verg.velocity-400,group=interaction(neuron,sacnum)),alpha=0.2,color='hotpink')+
  facet_wrap(~saccade.type,ncol=1)

ggplot(filter(z,abs(r.angle)<20,counter> -25,counter< 150))+
  geom_line(aes(counter,verg.velocity,group=interaction(neuron,sacnum)),alpha=0.2,color='hotpink')+
  facet_wrap(~saccade.type,ncol=1)+
  stat_smooth(aes(counter,verg.velocity))

z<- mutate(z,near.saccade=max.verg.angle>10)

z$near.saccade=as.factor(z$near.saccade)
levels(z$near.saccade)<- c('Far','Near')


ggplot(filter(z,abs(r.angle)<20,counter> -25,counter< 150))+
  geom_line(aes(counter,verg.velocity,group=interaction(neuron,sacnum)),alpha=0.2,color='hotpink')+
  facet_grid(near.saccade~saccade.type)+
  stat_smooth(aes(counter,verg.velocity))

z<- mutate(z,fast.saccade=as.factor(peak.conj.velocity>189))
levels(z$fast.saccade)<- c('Slow','Fast')

ggplot(filter(z,abs(r.angle)<20,counter> -25,counter< 150))+
  geom_line(aes(counter,verg.velocity,group=interaction(neuron,sacnum)),alpha=0.2,color='hotpink')+
  facet_grid(fast.saccade~saccade.type)+
  stat_smooth(aes(counter,verg.velocity))



z %>% filter(counter>-25,counter<150) %>%
  group_by(saccade.type,counter) %>%
  summarize(transient.template=mean(verg.velocity),
            saccade.template=mean(conj.velocity))->
  sz

ggplot(filter(sz))+
  geom_line(aes(counter,transient.template,color=saccade.type),size=2)

ggplot(filter(sz))+
  geom_line(aes(counter,verg.velocity,group=interaction(neuron,sacnum)),
            alpha=1/20,
            data=filter(z,saccade.type=='converging',counter>-25,counter<150))+
  geom_line(aes(counter,transient.template,color=saccade.type),size=2)




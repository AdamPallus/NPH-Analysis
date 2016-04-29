
library(dplyr)
library(ggplot2)

source('~/GitHub/NPH-Analysis/Vergence/joinsaccades.R')
source('~/GitHub/NPH-Analysis/Vergence/Adamhelperfunctions.R')
source('~/GitHub/NPH-Analysis/Vergence/RobustGaussianSurprise.R')

t<- readRDS('NRTPt.RDS')
t<- filter(t,neuron=='Bee-218')

bufferlength<- 200
t%>%
  # filter(as.numeric(cellnum)>200) %>%
  filter(as.numeric(cellnum) %in% c(202,205,215, 218)) %>%
  group_by(neuron) %>%
  do(joinsaccades(.,buffer=bufferlength,threshold=20))->
  d

d %>%
  group_by(neuron) %>%
  mutate(issaccade=!is.na(sacnum)) %>%
  filter(issaccade) %>%
  group_by(neuron,sacnum) %>%
  mutate(dur=n(),
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
         min.verg.angle=min(verg.angle),
         maxfr=max(sdf),
         counter=time-time[1]-bufferlength,
         verg.change=verg.angle-mean(verg.angle[1:50]),
         # verg.amp= last(verg.angle)-first(verg.angle),
         verg.amp= verg.angle[dur-bufferlength]-verg.angle[bufferlength],
         showrasters=replace(rasters,rasters<1,NA))->
  p

p$convergent<- as.factor(p$verg.amp>0)
levels(p$convergent)<- c('Divergent','Convergent')


p %>%  group_by(neuron) %>%
  filter(dur>(bufferlength*2+50),
         abs(verg.amp)>2,
         dur>500,
         dur<650,
         min.verg.angle> -5) %>%
  group_by(neuron,convergent) %>%
  mutate(nsaccades=length(unique(sacnum)),
         spiketimes=showrasters*counter) ->
  gtemp

gtemp%>%
  group_by(neuron,convergent,sacnum) %>%
  summarize(dur=first(dur)) %>%
  arrange(desc(dur)) %>%
  mutate(snum=row_number()) %>%
  left_join(gtemp,.)->
  g



gplot<- select(g, dur, counter, verg.angle, spiketimes,nsaccades, showrasters,snum,neuron,convergent, 
               rep,lep,repV,lepV,sacnum,sdf)
gs<-ggplot(gplot) + 
  theme_bw()+
  xlab('Time from Saccade Onset (ms)')+
  geom_histogram(aes(spiketimes,100*..ncount..),alpha=1,binwidth=10,fill='black',color='black')+
  # geom_freqpoly(aes(spiketimes,100*..ncount..),alpha=1,bins=40,color='darkred',size=1)+
  geom_point(aes(counter,showrasters* snum*2+110),shape='|',size=1,color='black')+
  facet_grid(neuron~convergent,scales='free_y',space='free_y')+
  # facet_wrap(~neuron,ncol=4,scales='free')+
  # facet_wrap(~neuron,ncol=4)+
  geom_vline(xintercept=0,color='red',size=1)+
  # geom_point(aes(counter,verg.change*10-250),color='darkgreen',size=1,shape='-')+
  geom_line(aes(counter,rep-50,group=sacnum),color='red',size=0.5,alpha=1/10)+
  geom_line(aes(counter,lep-50,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  geom_line(aes(counter,repV-100,group=sacnum),color='red',size=0.5,alpha=1/10)+
  geom_line(aes(counter,lepV-100,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  geom_line(aes(counter,verg.angle*10-300,group=sacnum),color='darkgreen',size=0.5,alpha=1/5)+
  coord_cartesian(xlim=c(-200,300),expand=FALSE)+
  theme(axis.text=element_text(size=16),
        panel.margin=unit(3,'lines'),
        strip.text=element_text(size=16),
        axis.title.x=element_text(size=16))+
  scale_y_continuous(breaks=c(-300,-250,-200,-150),labels=c(0,5,10,15))+
  ylab('Degrees')+
  expand_limits(y=-300)+
  geom_line(aes(counter,sdf-500,group=sacnum),color='orange',size=0.5,alpha=1/5)
# ylab('')



p %>%  group_by(neuron) %>%
  filter(dur>(bufferlength*2+50),
         abs(verg.amp)>2,
         dur>500,
         dur<650,
         min.verg.angle> -5) %>%
  group_by(neuron,convergent) %>%
  arrange(dur) %>%
  group_by(sacnum) %>%
  
  gs<-ggplot(gplot) + 
  theme_bw()+
  xlab('Time from Saccade Onset (ms)')+
  # geom_histogram(aes(spiketimes,100*..ncount..),alpha=1,binwidth=10,fill='black',color='black')+
  # geom_freqpoly(aes(spiketimes,100*..ncount..),alpha=1,bins=40,color='darkred',size=1)+
  # geom_point(aes(counter,showrasters* snum*2+110),shape='|',size=1,color='black')+
  facet_grid(neuron~convergent,scales='free_y',space='free_y')+
  # facet_wrap(~neuron,ncol=4,scales='free')+
  # facet_wrap(~neuron,ncol=4)+
  geom_vline(xintercept=0,color='red',size=1)+
  # geom_point(aes(counter,verg.change*10-250),color='darkgreen',size=1,shape='-')+
  geom_line(aes(counter,rep-50,group=sacnum),color='red',size=0.5,alpha=1/10)+
  geom_line(aes(counter,lep-50,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  # geom_line(aes(counter,repV-100,group=sacnum),color='red',size=0.5,alpha=1/10)+
  # geom_line(aes(counter,lepV-100,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  # geom_line(aes(counter,verg.angle*10-300,group=sacnum),color='darkgreen',size=0.5,alpha=1/5)+
  coord_cartesian(xlim=c(-200,300),expand=FALSE)+
  theme(axis.text=element_text(size=16),
        panel.margin=unit(3,'lines'),
        strip.text=element_text(size=16),
        axis.title.x=element_text(size=16))+
  scale_y_continuous(breaks=c(-300,-250,-200,-150),labels=c(0,5,10,15))+
  ylab('Degrees')+
  expand_limits(y=-300)+
  # geom_line(aes(counter,sdf-500,group=sacnum),color='orange',size=0.5,alpha=1/5)
  
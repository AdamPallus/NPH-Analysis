source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')


t<- readRDS('SOA-NRTP.RDS')


#First: raster histogram plots

t<- filter(t,neuron %in% c('Bee-101','Bee-211','Ozette-110','Ozette-118'))
t<- filter(t,neuron %in% c('Bee-112','Bee-211','Bee-33'))

#mark and measure saccades
bufferlength<- 400
{
t%>%
  group_by(neuron) %>%
  mutate(time=row_number()) %>%
  do(joinsaccadesuniform(.,buffer=bufferlength,threshold=20,saccade.length=150))->
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
}

#filter and summarize
{
p %>%  group_by(neuron) %>%
  filter(#dur>(bufferlength*2+50),
    abs(verg.amp)>5,
    #dur>500,
    #dur<650,
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

gtemp %>%
  group_by(neuron,convergent,sacnum) %>%
  summarise() %>%
  ungroup() %>%
  group_by(neuron,convergent) %>%
  sample_n(20,replace=FALSE) %>%
  mutate(plotthis=TRUE) ->
  randomsaccades
}

#plot all 4 cells in a column to variable "gs"
{
gplot<-g
gplot<- select(g, dur, counter, verg.angle, spiketimes,nsaccades, showrasters,snum,neuron,convergent, 
               rep,lep,repV,lepV,sacnum,sdf)
gs<-ggplot(gplot) + 
  theme_bw()+
  xlab('Time from Saccade Onset (ms)')+
  geom_histogram(aes(spiketimes,100*..ncount..),alpha=1,binwidth=10,fill='black',color='black')+
  # geom_freqpoly(aes(spiketimes,100*..ncount..),alpha=1,bins=40,color='darkred',size=1)+
  geom_point(aes(counter,showrasters* snum*8+110),shape='|',size=1,color='black')+
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
  coord_cartesian(xlim=c(-400,550),expand=FALSE)+
  theme(axis.text=element_text(size=16),
        panel.margin=unit(2,'lines'),
        strip.text=element_text(size=16),
        axis.title.x=element_text(size=16))+
  scale_y_continuous(breaks=c(-300,-250,-200,-150),labels=c(0,5,10,15))+
  ylab('Degrees')+
  expand_limits(y=-300)
}
ggsave('testrasters.PNG',height=15,width=8,plot=gs)
ggsave('testrasters.PDF',height=20,width=8,plot=gs)

#plot single cell -- expand to choose
{
gs<-ggplot(filter(g,neuron=='Ozette-118')) + 
  theme_bw()+
  xlab('Time from Saccade Onset (ms)')+
  geom_histogram(aes(spiketimes,100*..ncount..),alpha=1,binwidth=10,fill='black',color='black')+
  # geom_freqpoly(aes(spiketimes,100*..ncount..),alpha=1,bins=40,color='darkred',size=1)+
  geom_point(aes(counter,showrasters* snum*2+110),shape='|',size=1,color='black')+
  facet_grid(neuron~convergent,scales='free_y',space='free_y')+
  # facet_wrap(~neuron,ncol=4,scales='free')+
  # facet_wrap(~neuron,ncol=4)+
  geom_vline(xintercept=0,color='red',size=1)+
  # geom_line(aes(counter,verg.velocity-50,group=sacnum),color='purple',alpha=1/10)+
  # geom_point(aes(counter,verg.change*10-250),color='darkgreen',size=1,shape='-')+
  geom_line(aes(counter,rep-50,group=sacnum),color='red',size=0.5,alpha=1/10)+
  geom_line(aes(counter,lep-50,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  geom_line(aes(counter,repV-100,group=sacnum),color='red',size=0.5,alpha=1/10)+
  geom_line(aes(counter,lepV-100,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  geom_line(aes(counter,verg.angle*10-300,group=sacnum),color='darkgreen',size=0.5,alpha=1/5)+
  coord_cartesian(xlim=c(-400,550),expand=FALSE)+
  theme(axis.text=element_text(size=16),
        panel.margin=unit(2,'lines'),
        strip.text=element_text(size=16),
        axis.title.x=element_text(size=16))+
  scale_y_continuous(breaks=c(-300,-250,-200,-150),labels=c(0,5,10,15))+
  ylab('Degrees')+
  expand_limits(y=-300)
}

#######################################################################

#Next:single saccade demos

#Transients are timing?
bufferlength<- 50

zz<- joinsaccades(z,buffer=bufferlength,threshold=20)

zz %>%
  group_by(sacnum) %>%
  mutate(verg.amp= verg.angle[n()-bufferlength]-verg.angle[bufferlength],
         conj.position.h=(lep+rep)/2,
         conj.position.v=(lepV+repV)/2,
         h.amp=conj.position.h[n()-bufferlength]-conj.position.h[bufferlength],
         v.amp=conj.position.v[n()-bufferlength]-conj.position.v[bufferlength],
         r.amp=sqrt(h.amp^2+v.amp^2),
         counter=row_number(),
         plep=lep-first(conj.position.h),
         prep=rep-first(conj.position.h),
         plepV=lepV-first(conj.position.v),
         prepV=repV-first(conj.position.v)) %>%
  filter(r.amp>3)->
  zz

p<- filter(zz,verg.amp>3)
goodsacs<- unique(p$sacnum)     
nsac=length(goodsacs)


manipulate(ggplot(filter(zz,sacnum==goodsacs[sac]))+
             geom_area(aes(counter,sdf),alpha=1/10)+
             geom_point(aes(counter,showrasters+100),shape='|')+
             geom_line(aes(counter,(lev-rev)-50),color='darkblue',alpha=1)+
             # geom_line(aes(counter,(lev-rev)),color='green',alpha=1)+
             # geom_line(aes(counter,enhance.velocity-100),size=2,color='darkblue')+
             geom_line(aes(counter,(lep-rep)*5-50),color='darkgreen')
           # geom_line(aes(counter,lev-lag(rev,4)),color='red',linetype=2)+
           # geom_line(aes(counter,lag(lev,4)-rev),color='blue',linetype=2)+
           # geom_line(aes(counter,lev-lag(rev,3)),color='red',linetype=3)+
           # geom_line(aes(counter,lag(lev,3)-rev),color='blue',linetype=3)+
           # geom_line(aes(counter,lev-lag(rev,2)),color='red',linetype=4)+
           # geom_line(aes(counter,lag(lev,2)-rev),color='blue',linetype=4)+
           # geom_line(aes(counter,lev-lag(rev,1)),color='red',linetype=5)+
           # geom_line(aes(counter,lag(lev,1)-rev),color='blue',linetype=5)+
           # geom_line(aes(counter,lev-lag(rev,6)),color='red',linetype=6)+
           # geom_line(aes(counter,lag(lev,6)-rev),color='blue',linetype=6)+
           # geom_line(aes(counter,lev-lag(rev,9)),color='red',linetype=9)+
           # geom_line(aes(counter,lag(lev,9)-rev),color='blue',linetype=9)+
           # geom_point(aes(plep*10+200,100+plepV*10),color='blue',alpha=1/2)+
           # geom_point(aes(prep*10+200,100+prepV*10),color='red',alpha=1/2)+
           # geom_point(aes(200,100),size=3)
           # ylim(c(-100,200))
           ,
           sac=slider(2,nsac,step=1))




manipulate(ggplot(filter(z,time>=window,time<window+5000))+
             geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
             geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
             geom_line(aes(time,(lep-rep)*5-100),color='darkgreen')+
             geom_point(aes(time,showrasters+50),shape='|')+
             # geom_line(aes(time,slow.prediction),color='orange')+
             # geom_line(aes(time,(rev+lev)/2),color='darkred')+
             # geom_line(aes(time,(revV+levV)/2),color='red')+
             # theme_bw()+
             ylim(c(-100,200))
           ,
           window=slider(5000,max(z$time-5000),step=5000))



#------------- Modeling ---------------------
tp<-readRDS('ModelFitValues.RDS')
neuronchoice=c('Bee-112','Bee-211','Bee-33')
ggplot(tp)+
  geom_point(aes(verg.angle,Slow.Vergence,shape=area),size=3,alpha=0.2)+
  geom_point(aes(verg.angle,Slow.Vergence,shape=area),size=3,color='red',data=filter(tp,neuron %in% neuronchoice))+
  geom_abline(slope=1)+
  geom_vline(xintercept=0,linetype=2)+
  xlab('Sensitivity to Vergence Angle')+
  ylab('Sensitivity to Vergence Velocity')

ggplot(tp)+
  geom_point(aes(Convergence,Slow.Vergence,shape=area),size=3,alpha=0.2)+
  geom_abline(slope=1)+
  geom_vline(xintercept=0,linetype=2)+
  xlab('Vergence Velocity Sensitivity during Enhanced Convergence')+
  ylab('Vergence Velocity Sensitivity during Slow Vergence Movements')

ggplot(tp)+
  geom_density(aes(Slow.Vergence,fill=area),alpha=0.3)+
  theme_bw()+
  xlab('Sensitivity to Vergence Velocity (spk/s)/(deg/s)')


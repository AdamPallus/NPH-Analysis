#This is a special reduced file that generates the plots using previously saved data 
#See SOA-Modeling.Rmd for the code to start from the raw .csv files 

source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')
library(dplyr)
library(ggplot2)
library(manipulate)

t<- readRDS('SOA-NRTP.RDS')


#First: raster histogram plots

t<- filter(t,cellnum>100)
t<- filter(t,neuron %in% c('Bee-218','Ozette-111','Bee-208','Bee-103')) #bursts and pauses
t<- filter(t,neuron %in% c('Bee-216','Bee-105','Ozette-107','Ozette-108')) #no bursts no pauses
memory.size(32000) #increase memory to avoid out of memory errors -- WILL cause computer to freeze while processing
#mark and measure saccades

measureSaccades<- function(t,bufferlength){
  t%>%
  group_by(neuron) %>%
  mutate(time=row_number()) %>%
  do(joinsaccadesuniform(.,buffer=bufferlength,threshold=20,saccade.length=150)) %>%
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
  p
}

p<- measureSaccades(t=t,bufferlength = 400)

#filter and summarize
p %>%  group_by(neuron) %>%
  filter(#dur>(bufferlength*2+50),
    abs(verg.amp)>3,
    #dur>500,
    # dur < 850,
    min.verg.angle> -5) %>%
  group_by(neuron,convergent) %>%
  mutate(nsaccades=length(unique(sacnum)),
         spiketimes=showrasters*counter) ->
  gtemp

gtemp%>% #this little block arranges saccades by duration for plotting
  group_by(neuron,convergent,sacnum) %>%
  summarize(dur=first(dur)) %>%
  arrange(desc(dur)) %>%
  mutate(snum=row_number()) %>%
  left_join(gtemp,.)->
  g

gtemp<-NULL

#plot all 4 cells in a column to variable "gs"
makeRasterPlot<- function(g){
  gs<-ggplot(g) + 
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
cells2plot=c('Bee-218','Bee-103','Bee-208','Ozette-110')
# gs<-makeRasterPlot(g=filter(g,neuron %in% cells2plot))
gs<-makeRasterPlot(g=g)
ggsave('SOArasters.PNG',height=15,width=8,plot=gs)
ggsave('SOArasters.PDF',height=20,width=8,plot=gs)

gs<-makeRasterPlot(g=filter(g,neuron == 'Ozette-107'))
gs


#######################################################################

#Next:single saccade demos
ChosenCell='Bee-215'
gc<- filter(t,neuron==ChosenCell) #saved as a separate variable so it can be used later

p<- filter(gc,abs(verg.amp)>3) #saved as a separate variable so it can be repeated

goodsacs<- unique(p$sacnum)     
nsac=length(goodsacs)


manipulate(ggplot(filter(p,sacnum==goodsacs[sac]))+
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


gc<-mutate(gc,time=row_number(), showrasters=replace(rasters,rasters<1,NA))
window_size <- 10000
manipulate(ggplot(filter(gc,time>=window,time<window+window_size))+
             geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
             # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
             geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
             geom_point(aes(time,showrasters+50),shape='|')+
             # geom_line(aes(time,slow.prediction),color='orange')+
             # geom_line(aes(time,(rev+lev)/2),color='darkred')+
             # geom_line(aes(time,(revV+levV)/2),color='red')+
             # theme_bw()+
             ylim(c(-100,300))
           ,
           window=slider(window_size,max(gc$time-window_size),step=window_size))

#demo for Ozette-107 cell FIGURE 1A
ggplot(filter(gc,time>=140000,time<160000))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  theme_bw()+
  xlab('Time (ms)')+
  ylab('Vergence Velocity (deg/s)')+
  ylim(c(-100,150))
  ylim(c(-100,150))

#Bee-218 - Figure 1B - A burst+pause cell
ggplot(filter(gc,time>=120000,time<127500))+
  geom_area(aes(time,sdf),alpha=1/5)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=0.5)+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  theme_bw()+
  xlab('Time (ms)')+
  ylab('Vergence Velocity (deg/s)')+
  ylim(c(-100,150))

#Bee-215 - Figure 1C - A pause-no-burst cell
ggplot(filter(gc,time>=90000,time<100000))+
  geom_area(aes(time,sdf),alpha=1/5)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=0.5)+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  theme_bw()+
  xlab('Time (ms)')+
  ylab('Vergence Velocity (deg/s)')+
  ylim(c(-100,300))


#------------- Modeling ---------------------
tp<-readRDS('ModelFitValues.RDS')
tp<- filter(tp,area=='SOA')

neuronchoice=c('Bee-218','Ozette-111','Bee-208','Bee-103')

ggplot(tp)+
  geom_point(aes(verg.angle,Slow.Vergence,shape=area),size=3,alpha=0.2)+
  geom_text(aes(verg.angle,Slow.Vergence,label=neuron),size=3,color='red',data=filter(tp,neuron %in% neuronchoice))+
  geom_vline(xintercept=0,linetype=2)+
  xlab('Sensitivity to Vergence Angle')+
  ylab('Sensitivity to Vergence Velocity')

ggplot(tp)+
  geom_point(aes(verg.angle,Divergence,shape=area),size=3,alpha=0.2)+
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


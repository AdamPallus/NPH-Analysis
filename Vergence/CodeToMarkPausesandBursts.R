
library(dplyr)
library(ggplot2)

source('~/GitHub/NPH-Analysis/Vergence/joinsaccades.R')
source('~/GitHub/NPH-Analysis/Vergence/Adamhelperfunctions.R')
source('~/GitHub/NPH-Analysis/Vergence/RobustGaussianSurprise.R')


t<- readRDS('NRTPt.RDS')
t<- filter(t,neuron=='Bee-204')

spiketimes<- t$time[t$rasters==1]
isi<- spiketimes[-1]-spiketimes[-(length(spiketimes)-1)]
spiketimes<-spiketimes[-1]

p<- data.frame(spiketimes=spiketimes,isi=isi)
bp<- f.BPsummary(list(p),Pthresh=0.05)

bp$burst[[1]] %>%
  group_by(clusid) %>%
  summarise(start=min(start),end=max(end),
            duration=end-start,
            p=first(adjP),
            ymin=0,
            ymax=20) ->
  bursts

bp$pause[[1]] %>%
  group_by(clusid) %>%
  summarise(start=min(start),
            end=max(end),
            duration=end-start,
            p=first(adjP),
            ymin=0,
            ymax=20) ->
  pauses

t%>%
  # filter(as.numeric(cellnum)>200) %>%
  group_by(neuron) %>%
  do(joinsaccades(.,buffer=bufferlength,threshold=20))->
  d


expanddemo<- function(stimes){
  df<- data.frame(time=stimes$start:stimes$end,
                  duration=stimes$duration,
                  p=stimes$p)
  
}
pauses %>%
  group_by(clusid) %>%
  do(expanddemo(.)) %>%
  rename(pauses=clusid,pause.dur=duration, p.pause=p) ->
  xxx

d<- left_join(d,xxx,by='time')

bursts %>%
  group_by(clusid) %>%
  do(expanddemo(.)) %>%
  rename(bursts=clusid,burst.dur=duration, p.burst=p) ->
  xxx

d<- left_join(d,xxx,by='time')

d %>%
  mutate(showrasters=replace(rasters,rasters<1,NA)) %>%
  group_by(sacnum) %>% 
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
         pause.dur=sum(!is.na(pauses[abs(counter)<150]))) ->
  d

d %>%
  ungroup() %>%
  filter(abs(verg.amp)>2,
         pause.dur>0) %>%
  mutate(splot=dense_rank(sacnum)) ->
  dp

manipulate(
  ggplot(filter(dp,splot==currentsaccade))+
    geom_line(aes(counter,verg.angle),color='darkgreen')+
    geom_point(aes(counter,10*showrasters),shape='|',size=4)+
    geom_vline(aes(xintercept=13+counter*(pauses*0+1)),alpha=1/10)+
    geom_vline(aes(xintercept=13+counter*(bursts*0+1)),alpha=1/10,color='red')+
    geom_text(aes(y=8,x=0,label=paste('Pause Duration:',first(pause.dur))))+
    geom_text(aes(y=7,x=0,label=paste('Vergence Change:',first(verg.amp))))+
    geom_line(aes(counter,verg.velocity/10),color='maroon'),
  currentsaccade=slider(1,max(dp$splot,na.rm=T))
)

d %>% 
  group_by(sacnum) %>%
  summarize_each(funs(first)) ->
  dplot

randomsaccades<- sample(unique(dp$splot),40)

ggplot(filter(dp,splot %in% randomsaccades))+
  geom_vline(aes(xintercept=13+counter*(pauses*0+1)),alpha=1/20)+
  geom_vline(aes(xintercept=13+counter*(bursts*0+1)),alpha=1/20,color='red')+
  geom_line(aes(counter,verg.angle),color='darkgreen')+
  geom_point(aes(counter,10*showrasters),shape='|',size=4)+
  # geom_text(aes(y=8,x=0,label=paste('Pause Duration:',first(pause.dur))))+
  # geom_text(aes(y=7,x=0,label=paste('Vergence Change:',first(verg.amp))))+
  facet_wrap(~splot,ncol=5)
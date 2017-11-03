#To use this script, change, "Cells.to.Plot" to the desired cell numbers.
#Then run the rest of the script
#Go to line 114 if you want to adjust what is plotted. 

#If you want to sample 20 random saccades, set sample.saccades to TRUE. 
#This could error if there aren't enough saccades.

#Set Remove.Multiverg to TRUE if you want to filter out movements with 
#significant off-direction vergence during the saccade. 

#These files are required
library(dplyr)
library(ggplot2)

#functions located in other .R files
source('joinsaccades.R')
source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')
#Read in all the data for SOA and NRTP
# t<- readRDS('NRTPt.RDS')
# t<- readRDS('SOA-NRTP.RDS')
t<- loadnewcsv2(path="C:/Users/setup/Desktop/NRTP Vergence/SOASTRAB/patos/")
# t<- loadnewcsv2(path="C:/Users/setup/Desktop/NRTP Vergence/SOASTRAB/pil/")
###Only run above commands once####


#############################
########OPTIONS##############
# Cells.to.Plot = c(33)
# Cells.to.Plot=c('Bee-04','Bee-33','Bee-15') #,'Bee-112','Bee-211')
# Cells.to.Plot=c('Kopachuck-106','Kopachuck-110','Kopachuck-122', 'Kopachuck-126','Kopachuck-127')
# Cells.to.Plot=c('Bee-211','Bee-216')
Cells.to.Plot='Patos-101'
window.size= 8 #how close do the eyes need to be to target?
# Cells.to.Plot='Kopachuck-110'
# Cells.to.Plot= unique(t$neuron)
# sample.saccades= FALSE
# num.to.sample= 50
# Remove.Multiverg= FALSE
#############################
#############################





#length of time to plot before and after each saccade
bufferlength<- 400

#Choose which cells to plot, identify saccades, then expand the data frame to make sure saccades don't overlap.
#Basically if there is a saccade from 500-550 and 700-750, they will overlap if we try to use a buffer of 200ms
#To counter this, I duplicate any data that would overlap so we don't have to worry about it after this point.

t%>%
  # filter(cellnum %in% c(124,125,126,127))%>%
  # filter(as.numeric(cellnum)>200) %>%
  # filter(monkey=='Bee',as.numeric(cellnum) %in% Cells.to.Plot) %>%
  filter(neuron %in% Cells.to.Plot) %>%
  group_by(neuron) %>%
  mutate(time=row_number(),
         left.eye.distance=sqrt((thp-lep)^2+(tvp-lepV)^2),
         right.eye.distance=sqrt((thp-rep)^2+(tvp-repV)^2),
         fix.error=pmin(left.eye.distance,right.eye.distance),
         closesteye=left.eye.distance<right.eye.distance,
         # closesteye=abs(lep-thp)<abs(rep-thp),
         sdf=spikedensity(rasters,10)) %>%
  do(joinsaccadesuniform(.,buffer=bufferlength,threshold=20,saccade.length=150))->
  # do(joinsaccades(.,buffer=bufferlength,threshold=20))->
  d

#Once saccades have been marked, we measure lots of things about each saccade. 
#For this plot, we also remove all data that isn't part of a saccade or the buffer period
d %>%
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
    verg.amp=verg.angle[n()-bufferlength]-verg.angle[bufferlength+50], #increased this to 50
    peak.verg.velocity= maxabs(verg.velocity),
    min.verg.trans = min(verg.velocity),
    max.verg.trans = max(verg.velocity),
    off.verg.velocity=min(abs(min.verg.trans),abs(max.verg.trans)),
    min.verg.angle=min(verg.angle),
    maxfr=max(sdf),
    counter=time-time[1]-bufferlength,
    verg.change=verg.angle-mean(verg.angle[1:50]),
    # verg.amp= last(verg.angle)-first(verg.angle),
    verg.amp= verg.angle[n()-bufferlength]-verg.angle[bufferlength],
    # eyeswitch=closesteye[n()-bufferlength]!=closesteye[bufferlength],
    startingeye=closesteye[bufferlength-250],
    endingeye=closesteye[bufferlength+250],
    eyeswitch=startingeye !=endingeye,
    startingerror=fix.error[bufferlength-250],
    endingerror=fix.error[bufferlength+250],
    showrasters=replace(rasters,rasters<1,NA)) -> #%>%
  # filter(r.amp>4)->
  p




p %>%  group_by(neuron) %>%
  filter(saccade.dur>80, #Removes any saccade shorter than 80ms
         # abs(verg.amp)>4, #removes saccades with vergence change of less than 1.5 deg
         eyeswitch,
         startingerror<window.size,
         endingerror<window.size,
         saccade.dur<800)%>% #,
  #min.verg.angle> -5) %>% #removes trials with eye-coil problems (rare)
  # group_by(neuron,convergent) %>%
  
  mutate(n=n(),spiketimes=showrasters*counter) ->
  gtemp


gtemp%>%
  group_by(neuron,startingeye,sacnum) %>% 
  summarize(dur=first(saccade.dur)) %>% 
  arrange(desc(dur)) %>% #sorts by duration
  mutate(snum=row_number()) %>% #re-numbers saccades in their new order for plotting purposes
  left_join(gtemp,.)%>%
  mutate(converging=verg.amp>0)->
  g

g$startingeye<- as.factor(g$startingeye)
levels(g$startingeye)<- c('Left to Right', 'Right to Left')


ggplot(g) + 
  theme_bw()+ #Removes gray background 
  xlab('Time from Saccade Onset (ms)')+
  geom_histogram(aes(spiketimes,100*..ncount..),alpha=1,binwidth=15,fill='black',color='black')+
  # geom_freqpoly(aes(spiketimes,100*..ncount..),alpha=1,bins=40,color='darkred',size=1)+
  geom_point(aes(counter,showrasters* snum*5+110),shape='|',size=0.5,color='black')+
  # facet_grid(neuron~convergent,scales='free_y',space='free_y')+
  # facet_grid(convergent~neuron,scales='free_y',space='free_y')+
  # facet_wrap(~neuron,ncol=1)+
  # facet_wrap(~neuron,ncol=4,scales='free')+
  facet_wrap(~startingeye)+
  # facet_grid(startingeye~neuron)+
  # facet_grid(converging~startingeye)+
  geom_vline(xintercept=0,color='red',size=1)+
  # geom_point(aes(counter,verg.change*10-250),color='darkgreen',size=1,shape='-')+
  geom_line(aes(counter,rep-50,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  geom_line(aes(counter,lep-50,group=sacnum),color='red',size=0.5,alpha=1/10)+
  geom_line(aes(counter,(repV+lepV)/2-150,group=sacnum),color='violet',size=0.5,alpha=1/10)+
  geom_line(aes(counter,thp-50,group=sacnum),size=0.5,alpha=1/10)+
  # geom_line(aes(counter,tvp-150,group=sacnum),size=0.5,alpha=1/10)+
  # geom_line(aes(counter,repV-100,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  # geom_line(aes(counter,lepV-100,group=sacnum),color='red',size=0.5,alpha=1/10)+
  geom_line(aes(counter,verg.angle*10,group=sacnum),color='darkgreen',size=0.5,alpha=1/5)+
  # geom_line(aes(counter,sdf-500,group=sacnum),color='orange',size=0.5,alpha=1/5)
  # coord_cartesian(xlim=c(-200,300),expand=FALSE)+
  theme(axis.text=element_text(size=16),
        # panel.margin=unit(2,'lines'),
        strip.text=element_text(size=16),
        axis.title.x=element_text(size=16))+
  scale_y_continuous(breaks=c(-400,-300,-200),labels=c(-40,-30,-20))+ #re-labels so verg.angle is accurate
  ylab('Degrees')+
  expand_limits(y=c(-300))

# ggsave('Rastersall.PDF',height=15,width=8)

#request from mark: 
#plot converging and diverging saccades WITHOUT fixation switches:

p %>%  group_by(neuron) %>%
  filter(saccade.dur>80, #Removes any saccade shorter than 80ms
         # abs(verg.amp)>4, #removes saccades with vergence change of less than 1.5 deg
         !eyeswitch,
         # abs(verg.amp)<8,
         startingerror<window.size,
         endingerror<window.size,
         saccade.dur<800)%>% #,
  #min.verg.angle> -5) %>% #removes trials with eye-coil problems (rare)
  # group_by(neuron,convergent) %>%
  mutate(n=n(),spiketimes=showrasters*counter,
         converging='Conjugate',
         converging=replace(converging,verg.amp< -1,'Diverging'),
         converging=replace(converging,verg.amp>1,'Converging'))->
  gtemp


gtemp%>%
  group_by(neuron,converging,sacnum) %>% 
  summarize(dur=first(saccade.dur)) %>% 
  arrange(desc(dur)) %>% #sorts by duration
  mutate(snum=row_number()) %>% #re-numbers saccades in their new order for plotting purposes
  left_join(gtemp,.)->
  g


ggplot(g) + 
  theme_bw()+ #Removes gray background 
  xlab('Time from Saccade Onset (ms)')+
  geom_histogram(aes(spiketimes,100*..ncount..),alpha=1,binwidth=15,fill='black',color='black')+
  geom_point(aes(counter,showrasters* snum*5+110),shape='|',size=0.5,color='black')+
  facet_wrap(~converging)+
  geom_vline(xintercept=0,color='red',size=1)+
  geom_line(aes(counter,rep-50,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  geom_line(aes(counter,lep-50,group=sacnum),color='red',size=0.5,alpha=1/10)+
  geom_line(aes(counter,(repV+lepV)/2-150,group=sacnum),color='violet',size=0.5,alpha=1/10)+
  geom_line(aes(counter,verg.angle*10,group=sacnum),color='darkgreen',size=0.5,alpha=1/5)+
  geom_line(aes(counter,thp-50,group=sacnum),size=0.5,alpha=1/5,color='black')+
  theme(axis.text=element_text(size=16),
        strip.text=element_text(size=16),
        axis.title.x=element_text(size=16))+
  scale_y_continuous(breaks=c(-400,-300,-200),labels=c(-40,-30,-20))+ #re-labels so verg.angle is accurate
  ylab('Degrees')+
  expand_limits(y=c(-300))


gtemp%>%
  filter(converging != 'Conjugate') %>%
  group_by(neuron,converging,sacnum) %>% 
  summarize(dur=first(saccade.dur)) %>% 
  arrange(desc(dur)) %>% #sorts by duration
  mutate(snum=row_number()) %>% #re-numbers saccades in their new order for plotting purposes
  left_join(gtemp,.) %>% 
  filter(converging != 'Conjugate') ->
  g


ggplot(g) + 
  theme_bw()+ #Removes gray background 
  xlab('Time from Saccade Onset (ms)')+
  geom_histogram(aes(spiketimes,100*..ncount..),alpha=1,binwidth=15,fill='black',color='black')+
  geom_point(aes(counter,showrasters* snum*5+110),shape='|',size=0.5,color='black')+
  facet_wrap(~converging)+
  geom_vline(xintercept=0,color='red',size=1)+
  geom_line(aes(counter,rep-50,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  geom_line(aes(counter,lep-50,group=sacnum),color='red',size=0.5,alpha=1/10)+
  geom_line(aes(counter,thp-50,group=sacnum),size=0.5,alpha=1/5,color='black')+
  geom_line(aes(counter,(repV+lepV)/2-150,group=sacnum),color='violet',size=0.5,alpha=1/10)+
  geom_line(aes(counter,verg.angle*10,group=sacnum),color='darkgreen',size=0.5,alpha=1/5)+
  theme(axis.text=element_text(size=16),
        strip.text=element_text(size=16),
        axis.title.x=element_text(size=16))+
  scale_y_continuous(breaks=c(-400,-300,-200),labels=c(-40,-30,-20))+ #re-labels so verg.angle is accurate
  ylab('Degrees')+
  expand_limits(y=c(-300))





#end request from mark

#target check
tt<- mutate(t,closesteye=abs(lep-thp)<abs(rep-thp))
windowsize=10000
manipulate(
ggplot(filter(tt,time>starttime,time<starttime+windowsize))+
  geom_line(aes(time,thp),color='black')+
  geom_line(aes(time,rep),color='red')+
  geom_line(aes(time,lep),color='blue')+
  geom_line(aes(time,verg.angle),color='darkgreen')+
  geom_point(aes(time,closesteye*25)),
starttime=slider(1,nrow(t),step=windowsize-500))

windowsize=1000
manipulate(
  ggplot(filter(tt,time>starttime,time<starttime+windowsize))+
    geom_point(aes(rep,repV),color='red')+
    geom_point(aes(lep,lepV),color='blue')+
    geom_point(aes(thp,tvp),color='black'),
  starttime=slider(1,nrow(tt),step=windowsize))

goodsacs=unique(g$sacnum)
manipulate(
  ggplot(filter(g,sacnum==goodsacs[chosensac]))+
    geom_line(aes(time,verg.angle)),
  chosensac=slider(1,length(goodsacs)))
)


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
t<- readRDS('NRTPt.RDS')
t<- readRDS('SOA-NRTP.RDS')
###Only run above commands once####


#############################
########OPTIONS##############
# Cells.to.Plot = c(33)
# Cells.to.Plot=c('Bee-04','Bee-33','Bee-15') #,'Bee-112','Bee-211')
# Cells.to.Plot=c('Kopachuck-106','Kopachuck-110','Kopachuck-122', 'Kopachuck-126','Kopachuck-127')
Cells.to.Plot=c('Bee-211','Bee-216')
sample.saccades= TRUE
num.to.sample= 50
Remove.Multiverg= FALSE
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
    verg.amp=verg.angle[n()-bufferlength]-verg.angle[bufferlength+1],
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
    showrasters=replace(rasters,rasters<1,NA)) %>%
  filter(r.amp>4)->
  p

#This just gives a name to convergent and divergent saccades so the figure is automatically labled
# p$convergent<- as.factor(p$verg.amp>0)
# levels(p$convergent)<- c('Divergent','Convergent')

#New version of naming to include conjguate saccades:
p$convergent<- 'conjugate'
p$convergent<- replace(p$convergent,p$verg.amp> 0.5,'converging')
p$convergent<- replace(p$convergent,p$verg.amp< -0.5,'diverging')
p$convergent<- as.factor(p$convergent)

#This is the block where we decide which saccades to reject
if (Remove.Multiverg){
  p %>% group_by(neuron) %>%
    filter(off.verg.velocity<12) -> #removes "multi-directional" vergence movements
    p}

p %>%  group_by(neuron) %>%
  filter(saccade.dur>80, #Removes any saccade shorter than 80ms
    abs(verg.amp)>2, #removes saccades with vergence change of less than 1.5 deg
    saccade.dur<800,
    min.verg.angle> -500) %>% #removes trials with eye-coil problems (rare)
  group_by(neuron,convergent) %>%
  
  mutate(n=n(),spiketimes=showrasters*counter) ->
  gtemp


if (sample.saccades) {
  #This block chooses 20 random saccades of each type for each neuron
  gtemp %>%
    group_by(neuron,convergent,sacnum) %>%
    summarise() %>%
    ungroup() %>%
    group_by(neuron,convergent) %>%
    sample_n(num.to.sample,replace=TRUE) %>%
    mutate(plotthis=TRUE) ->
    randomsaccades
  
  #This block orders the saccades by duration
  gtemp%>%
    left_join(randomsaccades) %>%
    ##############Comment out next line if you don't want to sample###########
  filter(plotthis) %>% #only plots the 20 random saccades
    group_by(neuron,convergent,sacnum) %>% 
    summarize(dur=first(saccade.dur)) %>% 
    arrange(desc(dur)) %>% #sorts by duration
    mutate(snum=row_number()) %>% #re-numbers saccades in their new order for plotting purposes
    left_join(gtemp,.)-> #merges these new numbers with the full dataset. 
    #The join is only necessary because we used summarize above. Summarize gives one number for each saccade.
    #By joining back to the original set, we are just adding that one number to all 550 ms of each saccade
    g
}else{
  gtemp%>%
    group_by(neuron,convergent,sacnum) %>% 
    summarize(dur=first(saccade.dur)) %>% 
    arrange(desc(dur)) %>% #sorts by duration
    mutate(snum=row_number()) %>% #re-numbers saccades in their new order for plotting purposes
    left_join(gtemp,.)-> #merges these new numbers with the full dataset. 
    #The join is only necessary because we used summarize above. Summarize gives one number for each saccade.
    #By joining back to the original set, we are just adding that one number to all 550 ms of each saccade
    g
}

ggplot(g) + 
  theme_bw()+ #Removes gray background 
  xlab('Time from Saccade Onset (ms)')+
  geom_histogram(aes(spiketimes,100*..ncount..),alpha=1,binwidth=15,fill='black',color='black')+
  # geom_freqpoly(aes(spiketimes,100*..ncount..),alpha=1,bins=40,color='darkred',size=1)+
  geom_point(aes(counter,showrasters* snum*5+110),shape='|',size=0.5,color='black')+
  # facet_grid(neuron~convergent,scales='free_y',space='free_y')+
  # facet_grid(convergent~neuron,scales='free_y',space='free_y')+
  facet_wrap(~convergent)+
  # facet_wrap(~neuron,ncol=1)+
  # facet_wrap(~neuron,ncol=4,scales='free')+
  # facet_wrap(~neuron,ncol=4)+
  geom_vline(xintercept=0,color='red',size=1)+
  # geom_point(aes(counter,verg.change*10-250),color='darkgreen',size=1,shape='-')+
  geom_line(aes(counter,rep-50,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  geom_line(aes(counter,lep-50,group=sacnum),color='red',size=0.5,alpha=1/10)+
  geom_line(aes(counter,repV-100,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  geom_line(aes(counter,lepV-100,group=sacnum),color='red',size=0.5,alpha=1/10)+
  geom_line(aes(counter,verg.angle*10-300,group=sacnum),color='darkgreen',size=0.5,alpha=1/5)+
  # geom_line(aes(counter,sdf-500,group=sacnum),color='orange',size=0.5,alpha=1/5)
  # coord_cartesian(xlim=c(-200,300),expand=FALSE)+
  theme(axis.text=element_text(size=16),
        panel.margin=unit(2,'lines'),
        strip.text=element_text(size=16),
        axis.title.x=element_text(size=16))+
  scale_y_continuous(breaks=c(-300,-250,-200,-150),labels=c(0,5,10,15))+ #re-labels so verg.angle is accurate
  ylab('Degrees')+
  expand_limits(y=c(-300))

ggsave('Rastersall.PDF',height=15,width=8)

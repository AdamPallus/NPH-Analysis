library(dplyr)
library(ggplot2)

nrtp<- filter(readRDS('NRTPp.RDS'),as.numeric(cellnum)<100)


first<- function(x){ x[1]}

nrtp %>%
  group_by(neuron,s) %>%
  filter(!isfixation) %>%
  summarize_each(funs(first)) ->
  n


qplot(r.angle,maxfr,data=n)+
  facet_wrap(~neuron) + coord_polar()

qplot(verg.angle,maxfr,data=n)+facet_wrap(~neuron)

qplot(peak.verg.velocity,maxfr,data=n)+facet_wrap(~neuron)

nrtp %>%
  filter(neuron=='Bee-01',!isfixation,maxamp>3) %>%
  mutate(spiketimes=counter,
         spiketimes=replace(spiketimes,rasters<1,NA))->
  b

ggplot(aes(spiketimes),data=b)+geom_histogram(aes(fill=saccade.type),position='dodge')+coord_cartesian(xlim=c(0,400))

nrtp %>%
  filter(!isfixation,maxamp>3) %>%
  mutate(spiketimes=counter,
         spiketimes=replace(spiketimes,rasters<1,NA))->
  ball

ggplot(aes(spiketimes),data=ball)+
  geom_histogram(aes(fill=saccade.type),position='dodge')+
  coord_cartesian(xlim=c(0,400))+
  facet_wrap(~neuron,ncol=3,scales='free')

#The plot above makes a histogram of the spiketimes of the cells relative to the timing of each saccade. 
#What we should try to do next is to subtract the known effect of static potiion from these cells. 
#There is going to be noise with this method, but it could
#BIGGER PROBLEM: These histograms need to be scaled by number of trials! I don't know if ggplot can do this.
#we might need to manually calculate this if it is something we really want. Binning the data is the annoying part.

library(ggplot2)
library(dplyr)
library(tidyr)

#load all the .csv files in the data folder, then add a column naming the neuron, 
#using the file name as the default name, then put them all together in one long data frame
path<-"~/GitHub/NPH-Analysis/data/"
files <- list.files(path=path,pattern='*.csv')
t<-data.frame()
for (i in 1:length(files)) {
  temp <- read.csv(paste(path,files[i],sep=''))
  temp$neuron<-gsub('.csv','',files[i])
  t <-rbind(t,temp)
}

thresh=1.5 #points with velocity below threshold are considered fixation

#choose just the points of fixation, then bin the data into 1 degree bins (using round)
#and calculate the mean firing rate during all the times when the eye is at each position
t %>%
  filter(abs(rev)<thresh,abs(revV)<thresh,abs(lev)<thresh,abs(levV)<thresh) %>%
  mutate(R.Hep=round(rep),R.Vep=round(repV), L.Hep=round(lep),L.Vep=round(lepV)) %>%
  group_by(R.Hep,R.Vep,L.Hep,L.Vep,neuron) %>%
  summarize(fr=mean(sdf)) %>%
  ungroup(.) %>%
  #use tidyr functions to make columns for eye (left or right), vertrical and horizontal eye position
  mutate(time=row_number(fr)) %>%
  gather(temp,P,1:4) %>%
  separate(temp,c("Eye","HV")) %>%
  spread(HV,P) ->
  s
levels(s$Eye)<-c("Right Eye","Left Eye") #Change R/L into Right Eye/Left Eye

#Just show cells I want
s <- filter(s,neuron %in% c("Bee6","BeeX1","BeeX2","BeeX3a","BeeX3b","BeeY1"))


#Create a scaled firing rate by simply dividing by the maximum firing rate in any bin
s %>%
  group_by(neuron) %>%
  mutate(maxFR=max(fr),scaledFR=fr/maxFR) ->
  ss

#plot
qplot(Hep,Vep,data=ss,fill=scaledFR)+geom_tile()+facet_grid(neuron~Eye)+
  scale_fill_gradient(low='black',high='orange')


  
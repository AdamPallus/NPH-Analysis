library(ggplot2)
library(dplyr)
library(tidyr)

t1 <- read.csv("~/GitHub/NPH-Strabismus/Bee_Mark_2015_09_17_1440_Radial.csv")
t2 <- read.csv("~/GitHub/NPH-Strabismus/Patos_2014_03_27_1435_Radial.csv")
t3 <- read.csv("~/GitHub/NPH-Strabismus/Patos_2014_03_27_1439_Radial.csv")
t4 <- read.csv("~/GitHub/NPH-Strabismus/Patos_2014_04_03_1532_Radial.csv")
t5 <- read.csv("~/GitHub/NPH-Strabismus/Patos_2014_04_10_1428_Radial.csv")

t1$neuron <- "Bee6"
t2$neuron <- "Patos1"
t3$neuron <- "Patos2"
t4$neuron <- "Patos4"
t5$neuron <-"Patos5"

t<- rbind(t1,t2,t3,t4,t5)

thresh=1.5
t %>%
  filter(abs(rev)<thresh,abs(revV)<thresh,abs(lev)<thresh,abs(levV)<thresh) %>%
  mutate(R.Hep=round(rep),R.Vep=round(repV), L.Hep=round(lep),L.Vep=round(lepV)) %>%
  group_by(R.Hep,R.Vep,L.Hep,L.Vep,neuron) %>%
  summarize(fr=mean(sdf)) %>%
  ungroup(.) %>%
  mutate(time=row_number(fr)) %>%
  gather(temp,P,1:4) %>%
  separate(temp,c("Eye","HV")) %>%
  spread(HV,P) ->
  s
levels(s$Eye)<-c("Right Eye","Left Eye")

#qplot(HP,VP,data=s,fill=fr)+geom_tile()+facet_grid(neuron~Eye)

s %>%
  group_by(neuron) %>%
  mutate(maxFR=max(fr),scaledFR=fr/maxFR) ->
  ss
qplot(Hep,Vep,data=ss,fill=scaledFR)+geom_tile()+facet_grid(neuron~Eye)+
  scale_fill_gradient(low='black',high='orange')

  
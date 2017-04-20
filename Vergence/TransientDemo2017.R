library(manipulate)
library(ggplot2)
zz<- readRDS('TransientDemo.RDS')
goodsacs<- unique(zz$sacnum)     

p<- filter(z,verg.amp>4,neuron=='Bee-113')
goodsacs=unique(p$sacnum)
nsac=length(goodsacs)


manipulate(ggplot(filter(z,sacnum==goodsacs[sac]))+
             geom_line(aes(counter,(lev-rev)),color='darkblue',alpha=1)+
             geom_line(aes(counter,verg.velocity),color='darkblue',linetype=2)+
             geom_line(aes(counter,(lep-rep)*10),color='darkgreen')+
             geom_line(aes(counter,conj.velocity/10))+
             geom_label(x=100,y=0,aes(label=unique(verg.lead)))+
             geom_label(x=100,y=20,aes(label=paste('sacnum = ',sacnum)))+
             geom_vline(aes(xintercept=200-verg.lead))+
             geom_point(aes(counter,cverg*0+100),shape='X')+
             geom_point(aes(counter,realsaccade*10))
           ,
           sac=slider(1,nsac,step=1))

ggplot(filter(z,sacnum==1))+
  geom_line(aes(counter,(lev-rev)),color='darkblue',alpha=1)+
  geom_line(aes(counter,verg.velocity),color='darkblue',linetype=2)+
  geom_line(aes(counter,(lep-rep)*10),color='darkgreen')+
  geom_line(aes(counter,conj.velocity/10))+
  geom_label(x=100,y=0,aes(label=unique(verg.lead)))+
  geom_vline(aes(xintercept=min(cverg,na.rm=T)))+
  geom_point(aes(counter,cverg*0+100),shape='X')


z %>%
  group_by(sacnum) %>%
  mutate(verg.lead=200-min(cverg,na.rm=T))->
  z

z %>%
  # group_by(neuron) %>%
  mutate(issaccade=!is.na(sacnum)) %>%
  filter(issaccade,saccade.dur>20) %>%
  group_by(neuron,sacnum) %>%
  mutate(cverg=abs(verg.velocity)>3,
         cverg=replace(counter,cverg,NA),
         verg.lead=bufferlength-max(cverg[100:bufferlength],na.rm=T),
         verg.lead=replace(verg.lead,is.na(verg.lead),0))->
  z

o<-filter(t,neuron %in% c('Bee-205','Ozette-110'))

o<- filter(t,neuron=='Bee-211')
o<- mutate(o,sdflag=lag(sdf,25))
m<- lm(sdflag~verg.angle,data=o)
o<- ungroup(o)
o<- mutate(o,static.fr=predict(m,newdata=o),
           replace(static.fr,static.fr<0,0))

ggplot(filter(o,abs(verg.velocity)>5))+
  geom_point(aes(verg.velocity,sdflag-static.fr),alpha=1/50,size=2)+
  facet_wrap(~neuron)+
  coord_cartesian(x=c(-150,150))+
  stat_smooth(aes(verg.velocity,sdflag-static.fr),method='lm')+
  geom_vline(xintercept = c(-15,15))

ggplot(o)+
# ggplot(filter(o,abs(verg.velocity)>15))+
  geom_point(aes(verg.velocity,sdflag),alpha=1/50,size=2)+
  facet_wrap(~neuron)+
  coord_cartesian(x=c(-100,100))+
  stat_smooth(aes(verg.velocity,sdflag),method='lm',data=filter(o,verg.velocity>15))+
  stat_smooth(aes(verg.velocity,sdflag),method='lm',data=filter(o,verg.velocity< -15))+
  stat_smooth(aes(verg.velocity,sdflag),method='lm',data=filter(o,abs(verg.velocity)<15,verg.velocity>0))+
  geom_vline(xintercept = c(-15,15))

ggplot(o)+
  # ggplot(filter(o,abs(verg.velocity)>15))+
  geom_point(aes(verg.velocity,sdflag-static.fr),alpha=1/50,size=2)+
  facet_wrap(~neuron)+
  coord_cartesian(x=c(-150,150))+
  stat_smooth(aes(verg.velocity,sdflag-static.fr),method='lm',data=filter(o,verg.velocity>15))+
  stat_smooth(aes(verg.velocity,sdflag-static.fr),method='lm',data=filter(o,verg.velocity< -15))+
  stat_smooth(aes(verg.velocity,sdflag-static.fr),method='lm',data=filter(o,abs(verg.velocity)<15,verg.velocity>0))+
  geom_vline(xintercept = c(-15,15))

ggplot(o)+
  # ggplot(filter(o,abs(verg.velocity)>15))+
  geom_point(aes(real.verg.velocity,sdflag-static.fr),alpha=1/50,size=2)+
  facet_wrap(~neuron)+
  coord_cartesian(x=c(-150,150))+
  stat_smooth(aes(real.verg.velocity,sdflag-static.fr),method='lm',data=filter(o,real.verg.velocity>15))+
  stat_smooth(aes(real.verg.velocity,sdflag-static.fr),method='lm',data=filter(o,real.verg.velocity< -15))+
  stat_smooth(aes(real.verg.velocity,sdflag-static.fr),method='lm',data=filter(o,abs(real.verg.velocity)<15,real.verg.velocity>0))+
  geom_vline(xintercept = c(-15,15))


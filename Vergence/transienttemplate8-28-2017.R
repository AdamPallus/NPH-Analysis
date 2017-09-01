bufferlength=20
z %>% 
  group_by(neuron) %>%
  do(joinsaccadesuniform(.,buffer=bufferlength,threshold = 30,saccade.length=150))->
  z

z %>%
  group_by(neuron,sacnum) %>%
  mutate(saccade.dur=n()-2*bufferlength,
         verg.amp=last(verg.angle)-first(verg.angle),
         peak.conj.velocity=max(conj.velocity),
         R.H.Amp=last(rep)-first(rep),
         L.H.Amp=last(lep)-first(lep),
         R.V.Amp=last(repV)-first(repV),
         L.V.Amp=last(lepV)-first(lepV),
         r.angle=atan2(R.V.Amp,R.H.Amp)*180/pi,
         r.direction='upward',
         r.direction=replace(r.direction,r.angle<0,'downward'),
         r.direction=replace(r.direction,abs(r.angle)<45,'rightward'),
         r.direction=replace(r.direction,abs(r.angle)>135,'leftward'),
         r.amp=sqrt(R.H.Amp^2+R.V.Amp^2),
         npk=min(verg.velocity),
         ppk=max(verg.velocity))->
  zm

zm %>%
  group_by(neuron,sacnum) %>%
  summarize_each(funs(first))->
  zmp

conj.sacs.for.model=filter(zmp,r.amp>3,r.amp<50,abs(verg.amp)<0.5)

trans.mod=lm(npk~peak.conj.velocity:r.direction,data=conj.sacs.for.model)
trans.mod.complex=lm(npk~peak.conj.velocity*r.angle,data=conj.sacs.for.model)
zm %>%
  filter(abs(verg.amp)<0.5,!is.na(counter)) %>%
  group_by(counter) %>%
  summarize_each(funs(mean))->
  zms


zm %>%
  filter(abs(verg.amp)<0.5,
         abs(r.amp)>4,
         verg.velocity[30]<0,
         abs(verg.velocity[20])< 20,
         abs(verg.velocity[1])<20
  ) %>%
  ungroup()->
  zmp



ggplot(zmp)+
  geom_line(aes(counter,verg.velocity,
                group=interaction(neuron,sacnum)),
            alpha=0.1)+
  facet_wrap(~r.direction)

zmp %>%
  group_by(neuron,sacnum) %>%
  mutate(verg.velocity=verg.velocity/min(verg.velocity)*-1)%>%
  ggplot()+
  geom_line(aes(counter,verg.velocity,
                group=interaction(neuron,sacnum)),
            alpha=0.1)


zmp %>%
  select(counter,r.direction,verg.velocity) %>%
  group_by(counter,r.direction) %>%
  summarize_each(funs(mean))%>%
  rename(trans.template=verg.velocity)->
  mean.waveforms

# qplot(counter,norm.trans.template,color=r.direction,data=mean.waveforms)
# qplot(counter,trans.template,color=r.direction,data=mean.waveforms)
mean.waveforms %>%
  ungroup() %>%
  select(counter,trans.template) %>%
  group_by(counter) %>%
  summarize_each(funs(mean))%>%
  mutate(norm.trans.template=trans.template-trans.template[1],
         norm.trans.template=norm.trans.template/min(norm.trans.template)*-1)->
  mean.waveforms



qplot(counter,norm.trans.template,data=mean.waveforms)

qplot(counter,cumsum(norm.trans.template),data=mean.waveforms)

qplot(counter,norm.trans.template,data=filter(mean.waveforms,counter<150,counter>15))

mean.waveforms<- filter(mean.waveforms,counter<150,counter>15)



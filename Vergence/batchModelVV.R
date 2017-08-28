batchModelVV<- function(x,saccadebuffer=20,saccadethreshold=30,
                    model.form='verg.velocity~sdf20+verg.angle',
                    lagsdf=31){
  if (!('time' %in% names(x))){
    x<- mutate(x,time=row_number())
  }
  # message(c('Calculating',x$neuron[1]))
  parabolic_n<- 15
  x %>%
    mutate(verg.velocity=parabolicdiff(lep-rep,parabolic_n),
           rev=parabolicdiff(rep,parabolic_n),
           lev=parabolicdiff(lep,parabolic_n),
           revV=parabolicdiff(repV,parabolic_n),
           levV=parabolicdiff(lepV,parabolic_n),
           sdf=spikedensity(rasters,sd=15),
           sdf20=dplyr::lag(sdf,lagsdf),
           conj.velocity=sqrt(((rev+lev)/2)^2+((revV+levV)/2)^2)) ->
    x
  x<- mutate(x,saccadic=markSaccades(conj.velocity,
                                     buffer=saccadebuffer,
                                     threshold=saccadethreshold)>0)
  mod<- lm(model.form,data=filter(x,!saccadic,verg.velocity>0))
  x<- mutate(ungroup(x),predV=predict(mod,newdata=x),
             showrasters=replace(rasters,rasters<1,NA))
}

bufferlength=200

t %>%
  filter(monkey =='Bee') %>%
  group_by(neuron) %>%
  mutate(sdf20=lag(sdf,20),
         verg.velocity=lev-rev,
         conj.velocity=sqrt(((rev+lev)/2)^2+((revV+levV)/2)^2)) %>%
  mutate(saccadic=markSaccades(conj.velocity,
                               buffer=4,
                               threshold=20)>0) %>%
  do(m=lm('verg.velocity~sdf20+verg.angle',data=filter(.,!saccadic)))->
  tm

ttm<- glance(tm,'m')
arrange(ttm,desc(r.squared))

t %>%
  filter(monkey=='Bee') %>%
  # filter(neuron %inc% c('Bee-101','Bee-211')) %>%
  group_by(neuron) %>%
  do(batchModelVV(.,lagsdf=20,
                  model.form='verg.velocity~sdf20+verg.angle',
                  # model.form='sdf20~verg.velocity+verg.angle',
                  saccadebuffer=5,
                  saccadethreshold=20)) %>%
  group_by(neuron) %>%
  do(joinsaccades(.,buffer=200,threshold = 20))%>%
  group_by(neuron,sacnum) %>%
  mutate(saccade.dur=n()-2*bufferlength, 
         saccade.end=saccade.dur+bufferlength,
         peak.conj.velocity=maxabs(conj.velocity),
         peak.R.H= maxabs(rev),
         peak.R.V= maxabs(revV),
         peak.L.H= maxabs(lev),
         peak.L.V= maxabs(levV),
         R.H.Amp=rep[saccade.end]-rep[bufferlength],
         L.H.Amp=lep[saccade.end]-lep[bufferlength],
         R.V.Amp=repV[saccade.end]-repV[bufferlength],
         L.V.Amp=lepV[saccade.end]-lepV[bufferlength],
         conj.H.Amp=(R.H.Amp+L.H.Amp)/2,
         conj.V.Amp=(R.V.Amp+L.V.Amp)/2,
         r.angle=atan2(R.V.Amp,R.H.Amp)*180/pi,
         r.amp=sqrt(R.H.Amp^2+R.V.Amp^2),
         vect.amp= (sqrt(R.H.Amp^2+R.V.Amp^2)+sqrt(L.H.Amp^2+L.V.Amp^2))/2,
         maxamp=max(abs(R.H.Amp),abs(R.V.Amp),abs(L.H.Amp),abs(L.V.Amp)),
         saccadic.verg.amp=verg.angle[saccade.end]-verg.angle[bufferlength],
         total.verg.amp=sum(verg.velocity)/1000,
         mean.verg.amp=mean(verg.angle[saccade.end:n()]-mean(verg.angle[1:bufferlength])),
         peak.verg.velocity= maxabs(verg.velocity),
         min.verg.trans = min(verg.velocity),
         max.verg.trans = max(verg.velocity),
         off.verg.velocity=min(abs(min.verg.trans),abs(max.verg.trans)),
         min.verg.angle=min(verg.angle),
         max.verg.angle=max(verg.angle),
         max.verg.velocity=max(verg.velocity),
         min.verg.velocity=min(verg.velocity),
         initial.verg.angle=verg.angle[bufferlength],
         predicted.verg.amp=sum(predV)/1000,
         predicted.peak.verg.velocity=maxabs(predV))->
  t
t %>% 
  group_by(neuron,sacnum) %>%
  summarize_each(funs(first))->
  zp

ggplot(filter(zp,verg.amp>0,verg.amp<10),aes(group=neuron))+
  # geom_point(aes(verg.amp,peak.verg.velocity),color='black')+
  geom_point(aes(verg.amp,predicted.peak.verg.velocity),color='orange')


ggplot(filter(zp,total.verg.amp>0,total.verg.amp<10),aes(group=neuron))+
  geom_point(aes(total.verg.amp,peak.verg.velocity),color='black')+
  geom_point(aes(total.verg.amp,predicted.peak.verg.velocity),color='orange')+
  # stat_smooth(aes(verg.amp,predicted.peak.verg.velocity),color='orange',method='lm')+
  # stat_smooth(aes(verg.amp,peak.verg.velocity),color='black',method='lm')+
  ylim(c(-20,200))



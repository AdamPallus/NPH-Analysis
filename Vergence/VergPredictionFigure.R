#the purpose of this script is to make a plot comparing predicted peak vergence with actual peak vergence

bufferlength=200

z<- filter(t,neuron=='Bee-211')

z %>%
  mutate(sdf20=lag(sdf,20),
         verg.velocity=lev-rev,
         conj.velocity=sqrt(((rev+lev)/2)^2+((revV+levV)/2)^2)) %>%
  mutate(saccadic=markSaccades(conj.velocity,
                               buffer=4,
                               threshold=20)>0) %>%
  do(m=lm('verg.velocity~sdf20+verg.angle',data=filter(.,!saccadic)))->
  tm


verg.model=tm$m[[1]]
summary(verg.model)

z %>%
  mutate(sdf20=lag(sdf,20),
         time=row_number(),
         conj.velocity=sqrt(((rev+lev)/2)^2+((revV+levV)/2)^2)) %>%
  mutate(predV=predict(verg.model,newdata=.))->
  z

bufferlength<-200
z %>%
  do(joinsaccades(.,buffer=bufferlength,threshold=20)) %>%
  group_by(sacnum) %>%
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
         verg.amp=verg.angle[saccade.end]-verg.angle[bufferlength],
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
  zz

zz %>% 
  group_by(neuron,sacnum) %>%
  summarize_each(funs(first))->
  zp



zplot<- filter(zp,total.verg.amp>1,total.verg.amp<10,peak.verg.velocity>0)

ggplot(zplot,
       aes(peak.verg.velocity,predicted.peak.verg.velocity))+
  geom_point(aes(peak.verg.velocity,predicted.peak.verg.velocity))+
coord_fixed()+
  theme_minimal()+
  stat_smooth(method='lm',se = FALSE,color='black')

vergVfitmod=lm(predicted.peak.verg.velocity~peak.verg.velocity,
               data=zplot)
summary(vergVfitmod)
cor(zplot$peak.verg.velocity,zplot$predicted.peak.verg.velocity)

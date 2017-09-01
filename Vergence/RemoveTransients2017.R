library(ggplot2)
library(manipulate)
library(dplyr)
source('Adamhelperfunctions.R')
t<-readRDS('SOA-NRTP.RDS')
# z<- filter(t,neuron=='Bee-211')
z<- filter(t,monkey=='Bee')

# z<- mutate(z,time=row_number())

z %>%
  group_by(neuron) %>%
  # mutate(g=floor(time/200000)) %>%
  # group_by(g) %>%
  mutate(time=row_number(),
         rev=parabolicdiff(rep,20),
         lev=parabolicdiff(lep,20),
         revV=parabolicdiff(repV,20),
         levV=parabolicdiff(lepV,20),
         conj.velocity=sqrt(((rev+lev)/2)^2)+sqrt(((revV+levV)/2)^2),
         verg.velocity=lev-rev) ->
  z

bufferlength=20
z %>% 
  group_by(neuron) %>%
  do(joinsaccades(.,buffer=bufferlength,threshold = 30))->
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
         abs(verg.velocity[1])<20,
         saccade.dur<100
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
  rename(trans.template=verg.velocity)%>%
  group_by(r.direction) %>%
  mutate(norm.trans.template=trans.template/min(trans.template)*-1)->
  mean.waveforms

qplot(counter,norm.trans.template,color=r.direction,data=mean.waveforms)
qplot(counter,trans.template,color=r.direction,data=mean.waveforms)
mean.waveforms %>%
  ungroup() %>%
  select(counter,trans.template,norm.trans.template) %>%
  group_by(counter) %>%
  summarize_each(funs(mean))%>%
  mutate(norm.trans.template=norm.trans.template-norm.trans.template[20])->
  mean.waveforms

mean.waveforms%>%
  filter(counter<100,counter>20)->
    mean.waveforms

qplot(counter,norm.trans.template,data=mean.waveforms)

#now do it for real without the intermediate step
# zmp %>%
#   select(counter,verg.velocity) %>%
#   group_by(counter) %>%
#   summarize_each(funs(mean))%>%
#   rename(trans.template=verg.velocity)%>%
#   mutate(norm.trans.template=(trans.template-trans.template[1]), #make sure to start at zero velocity
#          norm.trans.template=norm.trans.template/min(norm.trans.template)*-1) %>%
#   filter(counter<125)->
#   mean.waveforms

qplot(counter,norm.trans.template,data=mean.waveforms)


zm<- filter(zm,cellnum>100)
zm<- left_join(zm,mean.waveforms,by='counter')



zm %>%
  filter(verg.amp>3,
         abs(r.amp)>4,
         # verg.velocity[30]<0,
         # abs(verg.velocity[20])< 20,
         # abs(verg.velocity[1])<20,
         saccade.dur<100
  ) %>%
  
  ungroup()->
  zmpp

zmpp<-mutate(zmpp,sacnum=paste(neuron,sacnum,sep='-'))

zmpp<- mutate(zmpp,npk=predict(trans.mod,newdata=zmpp))

zmpp %>%
  mutate(scaled.transient=norm.trans.template*npk*-1,
         scaled.transient=replace(scaled.transient,is.na(scaled.transient),0),
         notrans.verg.velocity=verg.velocity-scaled.transient)->
  zmpp

goodsacs=unique(zmpp$sacnum)

nsac=length(goodsacs)
manipulate(ggplot(filter(zmpp,sacnum==goodsacs[sac]))+
             geom_line(aes(counter,verg.velocity))+
             geom_line(aes(counter,norm.trans.template*npk*-1),color='hotpink')+
             geom_line(aes(counter,verg.velocity-norm.trans.template*npk*-1),color='darkgreen'),
           sac=slider(1,nsac,step=1)
)

manipulate(ggplot(filter(zmpp,sacnum==goodsacs[sac]))+
             geom_line(aes(counter,verg.velocity))+
             geom_line(aes(counter,scaled.transient),color='grey')+
             geom_line(aes(counter,notrans.verg.velocity),color='hotpink')+
             annotate('text',15,50,label='transient removed',color='hotpink')+
             annotate('text',15,45,label='predicted transient',color='grey')+
             annotate('text',15,40,label='actual vergence velocity'),
           sac=slider(1,nsac,step=1)
)

#this is really good. The next thing to try to do would be to take one neuron
#and calculate the vergence velocity prediction model and see how it does to compare the model
#with the transients and with the transients removed

b<- filter(zm,neuron=='Bee-113')

b %>%
  ungroup()%>%
  mutate(sdf20=lag(sdf,20)) %>%
  group_by(sacnum) %>%
  mutate(npk=predict(trans.mod,newdata=.)[1],
         scaled.transient=norm.trans.template*npk*-1,
         scaled.transient=replace(scaled.transient,is.na(scaled.transient),0),
         notrans.verg.velocity=verg.velocity-scaled.transient,
         showrasters=replace(rasters,rasters<1,NA))->
  b

b %>% group_by(time) %>% summarize_each(funs(first))->bb
bb$sacnum<- NULL
bb$counter<-NULL
bufferlength=200
bb %>%
  modelVV2(chosenCell='Bee-113',lagsdf=20,
         model.form='verg.velocity~sdf20+verg.angle',
         saccadebuffer=bufferlength) %>%
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
         r.direction='upward',
         r.direction=replace(r.direction,r.angle<0,'downward'),
         r.direction=replace(r.direction,abs(r.angle)<45,'rightward'),
         r.direction=replace(r.direction,abs(r.angle)>135,'leftward'))->
  bb

goodsacs=unique(filter(bb,r.amp>3,verg.amp>4)$sacnum)

goodsacs=unique(filter(bb,conj.H.Amp<1,conj.V.Amp>1,verg.amp>4)$sacnum)


nsac=length(goodsacs)
manipulate({
  bb.plot<-filter(bb,sacnum==goodsacs[sac])
  ggplot(bb.plot)+
    geom_line(aes(counter,verg.velocity))+
    geom_line(aes(counter,norm.trans.template*npk*-1),color='purple')+
    # geom_line(aes(counter,scaled.transient),color='grey')+
    geom_line(aes(counter,verg.velocity-norm.trans.template*npk*-1),color='hotpink',linetype=2)+
    geom_line(aes(counter,verg.velocity-norm.trans.template*npk*-1-predV),color='magenta',size=1)+
    # geom_line(aes(counter,notrans.verg.velocity),color='hotpink',linetype=2)+
    geom_line(aes(counter,predV),color='orange',size=1)+
    geom_point(aes(counter,showrasters+30),shape='|',size=3)+
    # geom_area(aes(counter,conj.velocity),alpha=0.2)+
    # geom_line(aes(counter,rep*10+100),color='red')+
    # geom_line(aes(counter,lep*10+100),color='blue')+
    # geom_line(aes(counter,(repV+repV)*5+100),color='purple')+
    # geom_line(aes(counter,rev),color='red')+
    # geom_line(aes(counter,lev),color='blue')+
    # geom_line(aes(counter,(revV+levV)/2),color='purple')+
    ylim(c(NA,150))+
    annotate('text',50,50,label='transient removed',color='hotpink')+
    annotate('text',50,45,label='predicted transient',color='purple')+
    annotate('text',50,40,label='actual vergence velocity')+
    annotate('text',50,35,label='predicted vergence velocity',color='orange')+
    annotate('text',50,30,label='predicted enhancement',color='magenta')+
    annotate('text',200,140,label='conjugate velocity')+
    annotate('text',150,100,label=paste(bb.plot$r.direction[1],'saccade',sep=' '))+
    annotate('text',150,95,label=paste(round(bb.plot$r.angle[1]),'deg',sep=' '))+
    annotate('text',100,-20,label=paste('sacnum=',sacnum,sep=''))
  # geom_label(x=150,y=100,aes(label=r.direction[1]))
},
sac=slider(1,nsac,step=1)

)

bb %>%
  group_by(sacnum) %>%
  mutate(predicted.enhancement=verg.velocity-norm.trans.template*npk*-1-predV,
         r.direction='upward',
         r.direction=replace(r.direction,r.angle<0,'downward'),
         r.direction=replace(r.direction,abs(r.angle)<45,'rightward'),
         r.direction=replace(r.direction,abs(r.angle)>135,'leftward')) %>%
  summarize(peak.conj.velocity=peak.conj.velocity[1],
            peak.predicted.enhancement=max(predicted.enhancement,na.rm=T),
            peak.vergence.velocity=max(verg.velocity),
            verg.amp=verg.amp[1],
            r.direction=r.direction[1],
            r.amp=r.amp[1])->
  bs

qplot(peak.conj.velocity,peak.predicted.enhancement,color=r.direction,data=bs)
qplot(peak.vergence.velocity,peak.predicted.enhancement,color=r.direction,data=bs)+
  geom_abline()+annotate('text',125,150,label='Unity Line')

qplot(verg.amp,peak.predicted.enhancement,color=r.direction,data=bs)


manipulate({
  bb.plot<-filter(bb,sacnum==goodsacs[sac])
  ggplot(bb.plot)+
    geom_line(aes(counter,verg.velocity))+
    xlim(c(100,400))+
    theme_minimal()
  # geom_label(x=150,y=100,aes(label=r.direction[1]))
},
sac=slider(1,nsac,step=1)

)

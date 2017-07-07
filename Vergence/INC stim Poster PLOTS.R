#setup----
# s<- loadnewcsv2(path="C:/Users/setup/Desktop/NRTP Vergence/INCstim/")
s<- loadnewcsv2(path="C:/Users/setup/Desktop/NRTP Vergence/INCstim/HAMMING/")
# s<- filter(t,neuron %in% c('Kopachuck-501','Kopachuck-502'))
bufferlength=200
s %>%
  group_by(neuron) %>%
  mutate(time=row_number(),
         sdf=spikedensity(rasters,sd=10),
         stimes=markSaccades(sdf,buffer=bufferlength,threshold=120))->
  sx


# s<- mutate(s,time=row_number())
# s$stimes<- markSaccades(s$sdf,buffer=100,threshold=120)

sx %>%
  group_by(neuron) %>%
  filter(stimes>0) %>%
  mutate(showstim=replace(rasters,rasters<1,NA))%>%
  group_by(neuron,stimes) %>%
  mutate(stim.dur=n()-2*bufferlength,
    counter=time-first(time),
         repDIFF=rep-rep[counter==bufferlength],
         lepDIFF=lep-lep[counter==bufferlength],
         repVDIFF=repV-repV[counter==bufferlength],
         lepVDIFF=lepV-lepV[counter==bufferlength])->
  sp

#Make a plot----

d<- filter(sp,neuron=='Kopachuck-5112')

stimlength=d$stim.dur[1]
stimlength=ceiling(d$stim.dur[1]/10)*10
maxplot=bufferlength+stimlength


d<- mutate(d,peak.conj.velocity=
             max(conj.velocity[bufferlength:bufferlength+stimlength],
                                    na.rm=TRUE),
           IEP.H=(first(rep)+first(lep))/2,
           IEP.V=(first(repV)+first(lepV)/2))

ggplot(d,aes(group=stimes))+
  geom_line(aes(counter,repDIFF),color='red')+
  geom_line(aes(counter,lepDIFF),color='blue')+
  geom_line(aes(counter,repVDIFF-20),color='red',linetype=2)+
  geom_line(aes(counter,lepVDIFF-20),color='blue',linetype=2)

ggplot(d,aes(group=stimes))+
  geom_line(aes(counter,rev),color='red')+
  geom_line(aes(counter,lev),color='blue')+
  geom_line(aes(counter,revV-20),color='red',linetype=2)+
  geom_line(aes(counter,levV-20),color='blue',linetype=2)


ggplot(filter(d,peak.conj.velocity<bufferlength-20,counter<620,counter>100),aes(group=stimes))+
  geom_line(aes(counter,repDIFF),color='red')+
  geom_line(aes(counter,lepDIFF),color='blue')+
  geom_line(aes(counter,repVDIFF-20),color='darkred',linetype=1)+
  geom_line(aes(counter,lepVDIFF-20),color='darkblue',linetype=1)

ggplot(filter(d,peak.conj.velocity<190,counter<620,counter>100),aes(group=stimes))+
  geom_line(aes(counter,rev),color='red')+
  geom_line(aes(counter,lev),color='blue')+
  # geom_line(aes(counter,revV-20),color='darkred',linetype=1)+
  # geom_line(aes(counter,levV-20),color='darkblue',linetype=1)+
  ylim(-100,100)

d %>%
  group_by(counter) %>%
  summarize_each(funs(mean))->
  dmean

dp<- filter(d,peak.conj.velocity<190,counter<maxplot+20,counter>bufferlength-100)


ggplot(filter(dmean,peak.conj.velocity<190,counter<maxplot+50,counter>bufferlength-100),
       aes(group=stimes))+
  geom_line(aes(counter,repDIFF),color='red')+
  geom_line(aes(counter,lepDIFF),color='blue')+
  geom_line(aes(counter,repVDIFF),color='darkred')+
  geom_line(aes(counter,lepVDIFF),color='darkblue')


ggplot(filter(dmean,peak.conj.velocity<190,counter<maxplot+20,counter>bufferlength-100),
       aes(group=stimes))+
  geom_point(aes(repDIFF,repVDIFF),color='red')+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue')+
  geom_point(aes(repDIFF,repVDIFF,group=stimes),color='red',data=dp,alpha=1/30,size=1/2)+
  geom_point(aes(lepDIFF,lepVDIFF,group=stimes),color='blue',data=dp,alpha=1/30,size=1/2)+
  theme_minimal()

ggplot(filter(dmean,peak.conj.velocity<190,counter<maxplot+100,counter>bufferlength-100),
       aes(group=stimes))+
  geom_line(aes(counter,lepDIFF-repDIFF),color='darkgreen')+
  geom_line(aes(counter,lepDIFF-repDIFF),color='darkgreen',data=dp,alpha=1/20)


dmp<- filter(dmean,peak.conj.velocity<190)
dmpS<- filter(dmp,counter<maxplot,counter>bufferlength)
ggplot(dmp,aes(group=stimes))+
  geom_point(aes(repDIFF,repVDIFF),color='red',alpha=1/30)+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue',alpha=1/30)+
  geom_point(aes(repDIFF,repVDIFF),color='red',data=dmpS)+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue',data=dmpS)+
  theme_minimal()

#batchprocess----
sp %>%
  group_by(neuron)%>%
  mutate(peak.conj.velocity=
           max(conj.velocity[bufferlength:bufferlength+stim.dur[1]],
               na.rm=TRUE),
         IEP.H=(first(rep)+first(lep))/2,
         IEP.V=(first(repV)+first(lepV)/2),
         stim=counter>bufferlength&counter<(bufferlength+stim.dur)) %>%
  filter(conj.velocity<190)->
  dall
  
dall %>%
  group_by(neuron,counter) %>%
  summarize_each(funs(mean))%>%
  group_by(neuron)%>%
  mutate(stim=counter>bufferlength&counter<(bufferlength+stim.dur))->
  dallmean

dallp<- filter(dall,peak.conj.velocity<190,counter<bufferlength+first(stim.dur)+20,
               counter>bufferlength-100)

xxx<- filter(dallmean,stim)
ggplot(filter(dallmean,stim))+
  geom_point(aes(repDIFF,repVDIFF),color='red')+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue')+
  facet_wrap(~neuron,ncol=4,scales='free')
  


ggplot(filter(dallmean,peak.conj.velocity<190,counter<bufferlength+first(stim.dur)+20,
              counter>bufferlength-100),
       aes(group=stimes))+
  geom_point(aes(repDIFF,repVDIFF),color='red')+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue')+
  geom_point(aes(repDIFF,repVDIFF,group=stimes),color='red',data=dallp,alpha=1/30,size=1/2)+
  geom_point(aes(lepDIFF,lepVDIFF,group=stimes),color='blue',data=dallp,alpha=1/30,size=1/2)+
  theme_minimal()+
  facet_wrap(~neuron,scales='free')


dmp<- filter(dmean,peak.conj.velocity<190)
dmpS<- filter(dmp,counter<maxplot,counter>bufferlength)
ggplot(dmp,aes(group=stimes))+
  geom_point(aes(repDIFF,repVDIFF),color='red',alpha=1/30)+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue',alpha=1/30)+
  geom_point(aes(repDIFF,repVDIFF),color='red',data=dmpS)+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue',data=dmpS)+
  theme_minimal()

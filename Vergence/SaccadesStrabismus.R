library(manipulate)
 #note: written using data in INCstrabismus folder; includes Bee (normal) -- some of which used the vergence array

t %>%
  group_by(neuron) %>%
  #filter(neuron=='Kopachuck-902') %>%
  # filter(monkey=='Kopachuck')%>%
  do(joinsaccadesuniform(.,buffer=50,threshold=30))->
  t
  
t %>%
  group_by(neuron) %>%
  mutate(sdf10=lag(sdf,10)) %>%
  # mutate(bin.velocity=cut(verg.velocity,c(seq(-200,200,by=20)))) %>%
  # filter(!is.na(sacnum)) %>%
  group_by(neuron, sacnum) %>%
  mutate(peak.H.vel=maxabs(lev+rev)/2,
         peak.V.vel=maxabs((levV+revV)/2),
         conj.h=(lep+rep)/2,
         conj.v=(lepV+repV)/2,
         conj.h.amp=last(conj.h)-first(conj.h),
         conj.v.amp=last(conj.v)-first(conj.v),
         conj.angle=atan2(conj.v,conj.h)*180/pi,
         peak.verg.velocity=maxabs(verg.velocity),
         verg.amp=last(verg.angle)-first(verg.angle),
         max.verg.velocity=max(verg.velocity),
         min.verg.velocity=min(verg.velocity),
         r.amp=sqrt(conj.h.amp^2+conj.v.amp^2),
         verg.dur=sum(abs(verg.velocity)>30),
         conv.dur=sum(verg.velocity>30),
         div.dur=sum(verg.velocity< -30),
         peak.FR=max(sdf10)) ->
  t

# tv<- filter(ts,conj.v.amp>5,abs(peak.V.vel)>200)

tv<-filter(t,abs(r.amp)>5,!is.na(sacnum))

# tv<- mutate(tv,showrasters=replace(rasters,rasters<1,NA))

goodsacs<- unique(tv$sacnum)     
nsac=length(goodsacs)



manipulate(ggplot(filter(tv,sacnum==goodsacs[currentsac]),aes(group=neuron))+
             # geom_line(aes(counter,sdf10,group=sacnum))+
             # geom_line(aes(counter,(levV+revV)/20),color='magenta')+ #conjugate vertical velocity
             # geom_line(aes(counter,(lepV+repV)/2),color='orange')+ #conjugate vertical position
             geom_line(aes(counter,(lev-rev)/10),color='darkblue')+ #vergence velocity
             geom_line(aes(counter,(lep-rep)),color='darkgreen')+
             geom_label(x=100,y=10,aes(label=paste('Saccade Amp= ',first(round(r.amp,2)))))+
             geom_label(x=100,y=9,aes(label=paste('Vertical amp= ',first(round(conj.v.amp,2)))))+
             geom_label(x=100,y=8,aes(label=paste('Horizonal amp= ',first(round(conj.h.amp,2)))))
           ,
           currentsac=slider(1,nsac,step=1))
velocitycells<-c(101,102,106,109,110,112,113,114,115,116,118,123,126,127)
tv<- filter(tv,cellnum %in% velocitycells)
cellnames<-unique(tv$neuron)

manipulate(ggplot(filter(tv,verg.amp>chosenAmp-1,verg.amp<chosenAmp+1,neuron==cellnames[chosenCell]),
                  aes(group=sacnum))+
             # geom_line(aes(counter,sdf10,group=sacnum))+
             # geom_line(aes(counter,(levV+revV)/20),color='magenta')+ #conjugate vertical velocity
             # geom_line(aes(counter,(lepV+repV)/2),color='orange')+ #conjugate vertical position
             geom_line(aes(counter,(lev-rev)/10),color='darkblue')+ #vergence velocity
             geom_line(aes(counter,(lep-rep)),color='darkgreen')+
             geom_line(aes(counter,sdf/10+20))+
             geom_label(x=10,y=10,aes(label=neuron))
             # geom_point(aes(counter,replace(rasters,rasters<1,NA)),shape='|')
           ,
           chosenAmp=slider(-10,10,step=1,initial = 0),
           chosenCell=slider(1,length(cellnames),step=1))

ggplot(filter(tv,verg.amp>4,verg.amp<5,neuron=='Kopachuck-101'))+
  geom_line(aes(counter,(lev-rev)/10,group=sacnum),color='darkblue')


tv %>%
  group_by(neuron,sacnum) %>%
  summarize_each(funs(first))->
  tp

# tp <- group_by(summarize_each(tv,funs(first))

pp<-qplot(conj.v.amp,verg.amp,data=tp)+
  stat_smooth(method='lm')+
  geom_label(x=0,y=0,aes(label=paste('R^2 ==',round(cor(conj.v.amp,verg.amp)^2,2))),parse=TRUE)

pp<-qplot(conj.h.amp,verg.amp,data=tp)+
  stat_smooth(method='lm')+
  geom_label(x=0,y=0,aes(label=paste('R^2 ==',round(cor(conj.h.amp,verg.amp)^2,2))),parse=TRUE)

pp<-ggplot(aes(conj.v.amp,verg.amp),data=tp)+
  geom_point(aes(color=conj.angle))+
  stat_smooth(method='lm')+
  geom_label(x=0,y=0,aes(label=paste('R^2 ==',round(cor(conj.v.amp,verg.amp)^2,2))),parse=TRUE)

pp<-ggplot(aes(conj.angle,verg.amp),data=tp)+
  geom_point(aes(color=r.amp))+
  stat_smooth(method='lm')+
  geom_label(x=0,y=0,aes(label=paste('R^2 ==',round(cor(conj.v.amp,verg.amp)^2,2))),parse=TRUE)

qplot(conj.v.amp,peak.verg.velocity,data=tp)
qplot(verg.amp,peak.verg.velocity,color=cellnum,data=tp)

ggplot(tp)+
  geom_point(aes(verg.amp,max.verg.velocity),alpha=1/10)+
  geom_point(aes(verg.amp,min.verg.velocity),alpha=1/10,color='darkgray')+
  annotate('text',x=-5,y=300,label='Maximum Vergence Velocity')+
  annotate('text',x=5,y=-200,label='Minimum Vergence Velocity')+
  facet_wrap(~monkey)+
  theme_bw()+
  ylab('Vergence Velocity (deg/s)')+
  xlab('Vergence Amplitude (deg)')+
  geom_point(x=10,y=75,size=3,color='orange')


ggplot(tp)+
  geom_point(aes(max.verg.velocity,min.verg.velocity,color=verg.amp))

ggplot(tp)+
  geom_point(aes(y=-1*min.verg.velocity-7.5*verg.amp,x=max.verg.velocity-7.5*verg.amp,color=verg.amp))

ggplot(tp)+
  geom_point(aes(verg.amp,peak.FR,color=verg.dur))+
  facet_wrap(~neuron,scales='free')

ggplot(tp)+
  geom_point(aes(verg.dur,peak.FR,color=verg.amp))+
  facet_wrap(~neuron,scales='free')

qplot(verg.dur,data=tp)+facet_wrap(~neuron)

ggplot(tp)+
  geom_point(aes(div.dur,peak.FR,color=verg.amp))+
  facet_wrap(~neuron,scales='free')

ggplot(tp)+
  geom_point(aes(verg.amp,div.dur,alpha=peak.FR))+
  geom_point(aes(verg.amp,conv.dur,alpha=peak.FR),color='maroon')



#----
currentsac=1

ggplot(filter(tv,sacnum==currentsac))+
  geom_line(aes(counter,sdf,group=sacnum))+
  geom_line(aes(counter,(levV+revV)/2),color='magenta')+
  facet_wrap(~neuron)


tp<- readRDS('modelparams.RDS')
names<-str_match(tp$neuron,"(^[a-zA-Z]+)-([0-9]+)")

tp$monkey<-names[,2]

ggplot(tpv)+
  coord_fixed()+geom_abline(slope=1)+
  geom_point(aes(Convergence,Slow.Vergence,color=verg.angle>0))
  



ggplot(tp)+
  # coord_fixed()+
  # geom_abline(slope=1)+
  # geom_text(aes(Convergence,Divergence,label=neuron),alpha=0.5,angle=45)+
  geom_point(aes(Slow.Velocity,verg.angle))+
  stat_smooth(aes(Slow.Velocity,verg.angle),method='lm')+
  xlab('Sensitivity to Slow Vergence Velocity')+
  ylab('Sensitivity to Static Vergence Angle')
  
  expand_limits(x= -1,y= -1)


#attempt to compare a near and far responce cell

t %>%
  filter(neuron %in% c('Bee-208','Bee-209','Bee-103')) %>%
  group_by(neuron) %>%
  mutate(rev=parabolicdiff(rep,n=20),
         lev=parabolicdiff(lep,n=20)) ->
  ts

ts<- filter(t,neuron=='Ozette-118')

ts<- mutate(ts,verg.velocity=lev-rev)

mf<- lm('sdflag~verg.angle+verg.velocity:enhance.type',data=filter(ts,neuron=='Bee-208'))
mf2<- lm('sdflag~verg.angle+verg.velocity:enhance.type',data=filter(ts,neuron=='Bee-209'))

mn<- lm('sdflag~verg.angle+verg.velocity:enhance.type',data=filter(ts,neuron=='Bee-103'))

ts %>%
  ungroup() %>%
  mutate(time2=row_number()) %>%
  mutate(predictnear=predict(mn,newdata=.),
         predictfar=predict(mf2,newdata=.),
         predictsum=predictnear-predictfar,
         predictsum=replace(predictsum,predictsum<0,0)) ->
  tt


manipulate(ggplot(filter(tt,time2>=window,time2<window+2000))+
             geom_area(aes(time2,sdf),alpha=1/10)+
             # geom_line(aes(time2,(rev+lev)/2))+
             geom_line(aes(time2,rep),color='red')+
             geom_line(aes(time2,lep),color='blue')+
             geom_line(aes(time2,(lep-rep)/2*10),color='darkgreen',size=1)+
             geom_line(aes(time2,verg.velocity),color='pink')
             # geom_point(aes(time,showslow*100))+
             # geom_hline(yintercept = c(208,233))+
             # geom_line(aes(time2,predictnear),color='orange')+
           # geom_line(aes(time2,predictfar),color='purple')+
             # geom_line(aes(time2,predictsum),color='cyan')
           ,
           # geom_line(aes(time,lag(lev,3)-rev),color='pink'),
           window=slider(1,max(tt$time2-2000),step=2000))


###
#That didn't work because I just don't have a good far-response cell that pauses
t<-readRDS('enhancemarked.RDS')
t %>%
  filter(neuron %in% c('Bee-215','Bee-103'))->
  ts

ts %>%
  filter(neuron=='Bee-215') %>%
  mutate(verg.velocity=rev-lev,
         verg.angle=rep-lep) ->
  invertedverg


# mf<- lm('sdflag~verg.angle+verg.velocity:enhance.type',data=invertedverg)
mf<- lm('sdflag~verg.angle+verg.velocity:enhance.type',data=filter(ts,neuron=='Bee-215'))
mn<- lm('sdflag~verg.angle+verg.velocity:enhance.type',data=filter(ts,neuron=='Bee-103'))

ts %>%
  ungroup() %>%
  mutate(time2=row_number()) %>%
  mutate(predictnear=predict(mn,newdata=.),
         predictfar=predict(mf,newdata=.),
         predictsum=predictnear-predictfar,
         predictsum=replace(predictsum,predictsum<0,0)) ->
  tt
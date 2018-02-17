#burst index


tt <- filter(t,neuron=='Bee-116')

calculateBI<- function(tt,delay=30,duration=50){
  
  tt %>%
    filter(dsnum>0) %>%
    group_by(dsnum) %>%
    summarize(saccade.end=last(time),
              saccade.start=first(time))->
    tcount
  
  #this is a really annoying way of summarizing over points outside of the grouping variable
  post.saccade<-tcount$dsnum #initialize
  during.saccade<-tcount$dsnum #initialize
  peak.during.saccade<-tcount$dsnum #initialize
  for (i in seq_along(tcount$saccade.end)){
    starttime=tcount$saccade.start[i]
    stoptime=tcount$saccade.end[i]
    during.saccade[i]=mean(tt$sdf10[starttime:stoptime])
    post.saccade[i]= mean(tt$sdf[stoptime+delay:stoptime+delay+duration])
    peak.during.saccade[i]=max(tt$sdf10[starttime:stoptime])
  }
  
  tcount$post.saccade=post.saccade
  tcount$during.saccade=during.saccade
  tcount$peak.during.saccade=peak.during.saccade
  tcount$burst.index=peak.during.saccade/post.saccade
  
  tt<- left_join(tt,tcount,by='dsnum')
  # 
  # tt %>% 
  #   filter(dsnum>0) %>% #saccades only
  #   group_by(dsnum) %>%
  #   summarize(during.saccade=mean(sdf),
  #             post.saccade=first(post.saccade),
  #             burst.index=during.saccade/post.saccade)->
  #   BI
  # 
  # tt<- left_join(tt,BI,by='dsnum')
}


ttx <- calculateBI(tt)

ttx%>% 
  filter(dsnum>0) %>%
  group_by(dsnum) %>%
  summarize(burst.index=first(burst.index),
            during.saccade=first(during.saccade),
            post.saccade=first(post.saccade),
            conj.vert=first(conj.vert))->
  BI


ggplot(filter(BI,post.saccade>0))+
  geom_histogram(aes(burst.index))

ggplot(BI)+
  geom_point(aes(conj.vert,burst.index))

ggplot(BI)+
  geom_point(aes(post.saccade,during.saccade))+
  geom_abline()


BI %>%
  filter(post.saccade>5) %>%
  summarzie(mean.BI=)


t %>%
  filter(monkey=='Bee') %>%
  group_by(neuron) %>%
  do(calculateBI(.))->
  tbee


t %>%
  filter(monkey=='DC') %>%
  group_by(neuron) %>%
  do(calculateBI(.))->
  tDC

explain(t %>%
  filter(monkey=='Kopachuck') %>%
  group_by(neuron) %>%
  do(calculateBI(.))->
  tkopa)

saveRDS(tkopa,'tkopa.RDS')
saveRDS(tbee,'tbee.RDS')
saveRDS(tDC,'tDC.RDS')

{
  t<-NULL
  t<- rbind(tbee,tkopa,tDC)
  saveRDS(t,'INCsaccadesmarkedBI.RDS')
}

tbee%>% 
  filter(dsnum>0) %>%
  group_by(neuron,dsnum) %>%
  summarize(dur=n(),
            burst.index=first(burst.index),
            during.saccade=first(during.saccade),
            post.saccade=first(post.saccade),
            conj.vert=first(conj.vert))->
  BIbee



tDC%>% 
  filter(dsnum>0) %>%
  group_by(neuron,dsnum) %>%
  summarize(dur=n(),
            burst.index=first(burst.index),
            during.saccade=first(during.saccade),
            post.saccade=first(post.saccade),
            conj.vert=first(conj.vert))->
  BIdc

BIdc %>%
  filter(dur>10) %>%
  group_by(neuron) %>%
  summarize(mean.post.saccade=mean(post.saccade,na.rm=T),
            mean.during.saccade=mean(during.saccade),
            n=n())->
  BIdcave

BIbee %>%
  filter(dur>10) %>%
  group_by(neuron) %>%
  summarize(mean.post.saccade=mean(post.saccade,na.rm=T),
            mean.during.saccade=mean(during.saccade),
            n=n())->
  BIbeeave



ggplot(BI %>% filter(dur>10))+
  geom_point(aes(post.saccade,during.saccade,color=neuron))+
  geom_abline()


ggplot(BIdc %>% filter(dur>10))+
  geom_point(aes(post.saccade,during.saccade,color=neuron))+
  geom_abline()


ggplot(BIdcave)+
  geom_point(aes(mean.post.saccade,mean.during.saccade))+
  geom_abline()

ggplot(BIbeeave)+
  geom_point(aes(mean.post.saccade,mean.during.saccade))+
  geom_abline()

#NEXT: add all of the saccade measuring to the summary so that we can average by saccade direction 
#we need this to identify cells that pause during off direction movements 
tbee%>% 
  filter(dsnum>0) %>%
  group_by(monkey,neuron,dsnum) %>%
  summarize(dur=n(),
            burst.index=first(burst.index),
            during.saccade=first(during.saccade),
            peak.during.saccade=first(peak.during.saccade),
            post.saccade=first(post.saccade),
            conj.vert=first(conj.vert),
            upward=conj.vert>0,
            sd.conj.velocity=sd(conj.velocity),
            mean.conj.velocity=mean(conj.velocity),
            sd.verg.velocity=sd(verg.velocity),
            spread=max(conj.velocity)-min(conj.velocity),
            qrange=quantile(conj.velocity,0.975)-quantile(conj.velocity,0.025),
            R.H.Amp=last(rep)-first(rep),
            R.V.Amp=last(repV)-first(repV),
            L.H.Amp=last(lep)-first(lep),
            L.V.Amp=last(lepV)-first(lepV),
            disjunctiveH=sign(R.H.Amp*L.H.Amp)<0,
            disjunctiveV=sign(R.V.Amp*L.V.Amp)<0,
            conj.H.Amp=(R.H.Amp+L.H.Amp)/2,
            conj.V.Amp=(R.V.Amp+L.V.Amp)/2,
            peak.conj.velocity=maxabs(conj.velocity),
            peak.H.Velocity=maxabs((rev+lev)/2),
            peak.V.Velocity=maxabs((revV+levV)/2),
            peak.RH.Velocity=maxabs(rev),
            peak.RV.Velocity=maxabs(revV),
            peak.LH.Velocity=maxabs(lev),
            peak.LV.Velocity=maxabs(levV),
            peakFR=max(sdf10),
            nspk=sum(rasters),
            avgFR=nspk/dur*1000,
            r.amp=sqrt(conj.H.Amp^2+conj.V.Amp^2),
            asleep=sd.conj.velocity>7.5 || dur>2000) ->
  BIbee

BIbee %>%
  filter(dur>10,abs(conj.V.Amp)>5,abs(conj.H.Amp)<2) %>%
  group_by(monkey,neuron,upward) %>%
  summarize(mean.post.saccade=mean(post.saccade,na.rm=T),
            mean.during.saccade=mean(peak.during.saccade),
            mean.burst.index=mean(peak.during.saccade-post.saccade,na.rm=T),
            n=n())->
  BIbeeave

ggplot(BIbeeave)+
  geom_point(aes(mean.post.saccade,mean.during.saccade,color=upward))+
  geom_abline()


ggplot(BIbeeave)+
  geom_line(aes(mean.post.saccade,mean.during.saccade,group=neuron,color=monkey))+
  geom_point(aes(mean.post.saccade,mean.during.saccade,color=upward))+
  geom_abline()#+
  # facet_wrap(~monkey,ncol=1)
  
ggplot(BIbeeave)+
  geom_histogram(aes(mean.burst.index))+
  facet_wrap(~upward,ncol = 1)

BIbee %>%
  filter(neuron=='Bee-103',dur>20)%>%
  ggplot()+
  geom_point(aes(conj.V.Amp,peak.during.saccade-post.saccade))->
  plt

  # geom_point(aes(post.saccade,peak.during.saccade,color=upward))

ggplotly(plt)


BIbee %>%
  filter(abs(conj.V.Amp)>5)%>%
  ggplot()+
  stat_smooth(aes(conj.V.Amp,peak.during.saccade-post.saccade,group=neuron),method='lm',se=FALSE)+
  geom_point(aes(conj.V.Amp,peak.during.saccade-post.saccade,color=neuron))

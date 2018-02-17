expandSaccades<- function(tt,buffer=80){
  
  maxtime<- max(tt$time)
  tt %>%
    filter(dsnum>0) %>%
    group_by(dsnum) %>%
    summarize(saccade.end=last(time),
              saccade.start=first(time)) %>%
    filter(saccade.end+buffer<maxtime) %>%
    mutate(saccade.end=saccade.end+80)-> 
    tcount
  
tcount$saccade.end<-tcount$saccade.end+buffer

jsac<- function(stimes){
  df<- data.frame(time=stimes[['saccade.start']]:stimes[['saccade.end']],
                  dsnum_extended=stimes[['dsnum']])
  return(df)
}

x<-rbindlist(apply(tcount,1,jsac))

tt<- left_join(tt,x,by='time')

}

t %>%
  group_by(neuron) %>%
  do(expandSaccades(.,buffer=80))->
  t

t %>%
  filter(dsnum_extended>0) %>%
  group_by(monkey,neuron,dsnum_extended) %>%
  mutate(durEXT=n()) %>%
  filter(durEXT>80,durEXT<300) %>%
  summarize(post.saccade=mean(sdf10[(n()-50):n()]),
            during.saccade=max(sdf10[1:(n()-80)]),
            dur=n())->
  ts


ggplot(ts) +
  geom_point(aes(post.saccade,during.saccade))+
  geom_abline(color='red')+
  facet_wrap(~monkey,ncol = 1)

ts %>%
  group_by(monkey,neuron) %>%
  summarize(mean.post.saccade=mean(post.saccade))


t%>% 
  filter(dsnum>0) %>%
  group_by(monkey,neuron,dsnum_extended) %>%
  filter(n()>80,n()<300) %>%
  summarize(dur=n(),
            peak.during.saccade=max(sdf10[1:(n()-80)]),
            post.saccade=mean(sdf10[(n()-50):n()]),
            burst.index= peak.during.saccade-post.saccade,
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
  BI

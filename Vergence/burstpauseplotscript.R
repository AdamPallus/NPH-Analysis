source('~/GitHub/NPH-Analysis/Vergence/markBurstsandPauses.R')

t<- readRDS('NRTPt.RDS')
t<- filter(t,neuron=='Bee-34')

pmin<- 0.2
bufferlength<- 250
d<- markBurstsandPauses(t,bufferlength=bufferlength, plimit=pmin,p0=0.05)

d %>%
  filter(!is.na(sacnum)) %>%
  group_by(sacnum) %>%
  mutate(pause.at.saccade=pauses[counter==20],
         p.pause.at.saccade=p.pause[counter==20],#use 20ms after saccade onset as the official pause
         pause.during.saccade=!is.na(pause.at.saccade),
         pause.dur=sum(pauses==pause.at.saccade,na.rm=T),
         pre.sac.nspk=sum(rasters[counter< -100]),
         post.sac.nspk=sum(rasters[counter> 100])) %>%
  ungroup() %>%
  filter(abs(verg.amp)>2,
         pause.during.saccade==T)%>%
  mutate(splot=dense_rank(sacnum)) ->
  dp

shiftpause<- 8
manipulate(
  ggplot(filter(dp,splot==currentsaccade))+
    geom_line(aes(counter,verg.angle),color='darkgreen')+
    geom_point(aes(counter,10*showrasters),shape='|',size=4)+
    geom_vline(aes(xintercept=shiftpause+counter*(pauses*0+1)),alpha=1/10,
               data=filter(dp,splot==currentsaccade,pauses==pause.at.saccade))+
    geom_vline(aes(xintercept=shiftpause+counter*(bursts*0+1)),alpha=1/10,color='red')+
    geom_text(aes(y=8,x=0,label=paste('Pause Duration:',first(pause.dur))))+
    geom_text(aes(y=7,x=0,label=paste('pre.nspk:',first(pre.sac.nspk))))+
    geom_text(aes(y=6,x=0,label=paste('post.nspk:',first(post.sac.nspk))))+
    # geom_text(aes(y=5,x=0,label=paste('p.pause:',first(p.pause.at.saccade))))+
    geom_line(aes(counter,verg.velocity/10),color='maroon'),
  currentsaccade=slider(1,max(dp$splot,na.rm=T))
)





d %>% 
  group_by(sacnum) %>%
  summarize_each(funs(first)) ->
  dplot

randomsaccades<- sample(unique(dp$splot),40)

# ggplot(filter(dp,splot %in% randomsaccades))+ #plot random
  geom_vline(aes(xintercept=13+counter*(pauses*0+1)),alpha=1/20,
             data=filter(dp,splot %in% randomsaccades, pauses==pause.at.saccade))+
  # geom_vline(aes(xintercept=13+counter*(bursts*0+1)),alpha=1/20,color='red')+
  geom_line(aes(counter,verg.angle),color='darkgreen')+
  geom_point(aes(counter,10*showrasters),shape='|',size=4)+
  # geom_text(aes(y=8,x=0,label=paste('Pause Duration:',first(pause.dur))))+
  # geom_text(aes(y=7,x=0,label=paste('Vergence Change:',first(verg.amp))))+
  facet_wrap(~splot,ncol=5)

  ggplot(filter(dp,!is.na(splot)))+ #plot all
    geom_vline(aes(xintercept=13+counter*(pauses*0+1)),alpha=1/20,
               data=filter(dp, pauses==pause.at.saccade))+
    # geom_vline(aes(xintercept=13+counter*(bursts*0+1)),alpha=1/20,color='red')+
    geom_line(aes(counter,verg.angle),color='darkgreen')+
    geom_point(aes(counter,10*showrasters),shape='|',size=4)+
    # geom_text(aes(y=8,x=0,label=paste('Pause Duration:',first(pause.dur))))+
    # geom_text(aes(y=7,x=0,label=paste('Vergence Change:',first(verg.amp))))+
    facet_wrap(~splot,ncol=5)
  
  
  
  
  d %>%
    filter(!is.na(sacnum)) %>%
    group_by(sacnum) %>%
    mutate(pause.at.saccade=pauses[counter==20],
           p.pause.at.saccade=p.pause[counter==20],#use 20ms after saccade onset as the official pause
           pause.during.saccade=!is.na(pause.at.saccade),
           pause.dur=length(pauses==pause.at.saccade),
           pre.sac.nspk=sum(rasters[counter< -100]),
           post.sac.nspk=sum(rasters[counter> 100])) %>%
    ungroup() %>%
    filter(abs(verg.amp)>2)%>%
    mutate(splot=dense_rank(sacnum)) ->
    dp
#Lots of little blocks to analyze bursts and pauses and plot

source('~/GitHub/NPH-Analysis/Vergence/markBurstsandPauses.R')
library(dplyr)
library(ggplot2)
library(manipulate)
library(tidyr)
t<- readRDS('NRTPt.RDS')
t$cellnum<- as.numeric(t$cellnum)

tt<- filter(t,cellnum %in% c(1, 4, 6, 7, 11, 12, 13, 14, 15, 16, 17, 18,19))

pmin<- 0.2
bufferlength<- 250
tt %>% group_by(neuron) %>%
  do(markBurstsandPauses(.,bufferlength=bufferlength, plimit=0.01, p0=0.05,pausethresh=0.995,burstthresh=0.05)) ->
  d

d %>%
  filter(!is.na(sacnum)) %>%
  group_by(sacnum) %>%
  mutate(pause.at.saccade=pauses[counter==20],
         p.pause.at.saccade=p.pause[counter==20],#use 20ms after saccade onset as the official pause
         pause.during.saccade=!is.na(pause.at.saccade),
         pause.dur=sum(pauses==pause.at.saccade,na.rm=T),
         pre.sac.nspk=sum(rasters[counter< -100]),
         post.sac.nspk=sum(rasters[counter> 100]),
         enhance.conv.dur=sum(verg.velocity>30),
         enhance.dive.dur=sum(verg.velocity< -30)) %>%
  ungroup() %>%
  filter(abs(verg.amp)>2,
         # pause.during.saccade==T) %>%
         verg.amp<10000) %>% #cheater code that shouldn't filter anything. switch with above line
  arrange(enhance.conv.dur) %>%
  mutate(splot=dense_rank(desc(enhance.conv.dur*1000))) ->
  dp

# manipulate(
#   ggplot(filter(dp,splot==currentsaccade))+
#     geom_line(aes(counter,verg.angle,group=splot),color='darkgreen')+
#     geom_point(aes(counter,10*showrasters),shape='|',size=4)+
#     geom_vline(aes(xintercept=counter*(pauses*0+1)),alpha=1/10,
#                data=filter(dp,splot==currentsaccade,pauses==pause.at.saccade))+
#     geom_vline(aes(xintercept=counter*(pauses*0+1)),alpha=1/10,
#                data=filter(dp,splot==currentsaccade,!pauses==pause.at.saccade),color='violet')+
#     geom_vline(aes(xintercept=counter*(bursts*0+1)),alpha=1/10,color='red')+
#     geom_text(aes(y=8,x=0,label=paste('Pause Duration:',first(pause.dur))))+
#     geom_text(aes(y=7,x=0,label=paste('pre.nspk:',first(pre.sac.nspk))))+
#     geom_text(aes(y=6,x=0,label=paste('post.nspk:',first(post.sac.nspk))))+
# 
#     # geom_text(aes(y=5,x=0,label=paste('p.pause:',first(p.pause.at.saccade))))+
#     geom_line(aes(counter,verg.velocity/10),color='maroon')+
#   currentsaccade=slider(1,max(dp$splot,na.rm=T))
# )


manipulate(
  ggplot(filter(dp,splot==currentsaccade))+
    geom_line(aes(counter,verg.angle,group=sacnum),color='darkgreen')+
    geom_point(aes(counter,10*showrasters),shape='|',size=4)+
    geom_vline(aes(xintercept=counter*(pauses*0+1)),alpha=1/10,
               data=filter(dp,splot==currentsaccade,pauses==pause.at.saccade))+
    geom_vline(aes(xintercept=counter*(bursts*0+1)),alpha=1/10,color='red')+
    geom_text(aes(y=8,x=0,label=paste('Pause Duration:',first(pause.dur))))+
    geom_text(aes(y=7,x=0,label=paste('pre.nspk:',first(pre.sac.nspk))))+
    geom_text(aes(y=6,x=0,label=paste('post.nspk:',first(post.sac.nspk))))+
    geom_text(aes(y=5,x=0,label=paste('p.burst:',min(p.burst,na.rm=T))))+
    geom_line(aes(counter,verg.velocity/10,group=sacnum),color='maroon')+
    facet_wrap(~neuron),
  currentsaccade=slider(1,max(dp$splot,na.rm=T))
)






d %>% 
  group_by(sacnum) %>%
  summarize_each(funs(first)) ->
  dplot

randomsaccades<- sample(unique(dp$splot),40)

ggplot(filter(dp,splot %in% randomsaccades))+ #plot random
  geom_vline(aes(xintercept=counter*(pauses*0+1)),alpha=1/20,
             data=filter(dp,splot %in% randomsaccades, pauses==pause.at.saccade))+
  geom_vline(aes(xintercept=counter*(pauses*0+1)),alpha=1/20,color='violet',
             data=filter(dp,splot %in% randomsaccades, pauses!=pause.at.saccade))+
  geom_vline(aes(xintercept=counter*(bursts*0+1)),alpha=1/20,color='red')+
  geom_line(aes(counter,verg.angle),color='darkgreen')+
  geom_point(aes(counter,10*showrasters),shape='|',size=4)+
  # geom_text(aes(y=8,x=0,label=paste('Pause Duration:',first(pause.dur))))+
  # geom_text(aes(y=7,x=0,label=paste('Vergence Change:',first(verg.amp))))+
  facet_wrap(~splot,ncol=5)

ggplot(filter(dp,!is.na(splot)))+ #plot all
  geom_vline(aes(xintercept=counter*(pauses*0+1)),alpha=1/20,
             data=filter(dp, pauses==pause.at.saccade))+
  geom_vline(aes(xintercept=counter*(bursts*0+1)),alpha=1/20,color='red')+
  geom_line(aes(counter,verg.angle),color='darkgreen')+
  geom_point(aes(counter,10*showrasters),shape='|',size=4)+
  # geom_text(aes(y=8,x=0,label=paste('Pause Duration:',first(pause.dur))))+
  # geom_text(aes(y=7,x=0,label=paste('Vergence Change:',first(verg.amp))))+
  facet_wrap(~splot,ncol=5)


##Below is code to run the pause detection and saccade expansion function on multiple  neurons at once. 
#I already tried to run it on all neurons over 100 and the script failed to allocate enough memory to complete. 
#So let's try with just those over 200. 
t %>%
  filter(monkey=='Bee', cellnum %in% c(1, 4, 6, 7, 11, 12, 13, 14, 15, 16, 17, 18,19)) %>%
  # filter(monkey=='Bee', as.numeric(cellnum)<18, as.numeric(cellnum)<100) %>%
  group_by(neuron) %>%
  do(markBurstsandPauses(.,bufferlength=250, plimit=0.01, p0=.2,pausethresh=0.995,burstthresh=0.05)) ->
  d

d %>% ungroup() %>%
  filter(!is.na(sacnum)) %>%
  group_by(neuron,sacnum) %>%
  mutate(pause.at.saccade=pauses[270],
         p.pause.at.saccade=p.pause[270],#use 20ms after saccade onset as the official pause
         pause.during.saccade=!is.na(pause.at.saccade),
         pause.dur=sum(pauses==pause.at.saccade,na.rm=T),
         pre.sac.nspk=sum(rasters[counter< -100]),
         post.sac.nspk=sum(rasters[counter> 100]),
         enhance.conv.dur=sum(verg.velocity>30),
         enhance.dive.dur=sum(verg.velocity< -30)) %>%
  ungroup() %>%
  filter(abs(verg.amp)>2,
         # pause.during.saccade==T) %>%
         verg.amp<10000) %>% #cheater code that shouldn't filter anything. switch with above line
  group_by(neuron) %>%
  mutate(splot=dense_rank(sacnum)) ->
  dp


dp %>%
  group_by(neuron,splot) %>%
  summarize(pause.during=first(pause.during.saccade),
            convergent=first(verg.amp>2),
            divergent= first(verg.amp< -2),
            burst.during=max(!is.na(bursts[150:350])) & max(burst.dur>10,na.rm=T)) %>%
  group_by(neuron) %>%
  summarize(nsac=max(splot,na.rm=T),
            npause=sum(pause.during==T),
            nburst=sum(burst.during==T),
            nconv=sum(convergent),
            ndiv=sum(divergent),
            all.burst=round(nburst/nsac*100),
            all.pause=round(npause/nsac*100),
            ncpause=round(100*sum(pause.during==T & convergent)/nconv),
            ndpause=round(100*sum(pause.during==T & divergent)/ndiv),
            ncburst=round(100*sum(burst.during==T & convergent)/nconv),
            ndburst=round(100*sum(burst.during==T & divergent)/ndiv)) ->
  ss

# ggplot(ss)+
  #   geom_point(aes(neuron,percent.pause),size=5)+
  #   geom_point(aes(neuron,ncpause),size=5,color='maroon')+
  #   geom_point(aes(neuron,ndpause),size=5,color='darkgreen')+
  #   geom_hline(yintercept = 20)
  # 
  # ggplot(ss)+
  #   geom_bar(aes(neuron,y=percent.pause),size=5,stat='identity')+
  #   geom_bar(aes(neuron,y=ncpause),size=5,fill='maroon',stat='identity')+
  #   geom_bar(aes(neuron,y=ndpause),size=5,fill='darkgreen',stat='identity')+
  #   geom_hline(yintercept = 20)
  
  ss %>%
    gather("Pauses","Percent.pause",8:10) %>%
    gather("Bursts","Percent.burst",7:9) ->
    sp
  
  
  ggplot(aes(neuron,Percent.pause),data=sp)+
    geom_bar(stat='identity',aes(fill=Pauses),position='dodge')+
    geom_hline(yintercept = 25)+
    scale_fill_discrete(name='Pauses',labels=(c("Overall Percentage",'Convergent Movements','Divergent Movements')))+
    theme(legend.position='top')
  
  ggplot(aes(neuron,Percent.burst),data=sp)+
    geom_bar(stat='identity',aes(fill=Bursts),position='dodge')+
    geom_hline(yintercept = 25)+
    scale_fill_discrete(name='Bursts',labels=(c("Overall Percentage",'Convergent Movements','Divergent Movements')))+
    theme(legend.position='top')
  
  ##---------------------------
  dp %>% 
    group_by(neuron, splot) %>%
    summarize(FR= max(sdf),
              Vamp=first(verg.amp),
              conD=first(enhance.conv.dur),
              divD=first(enhance.dive.dur),
              maxVergTrans=first(max.verg.trans),
              minVergTrans=first(min.verg.trans),
              peakVergVelocity=first(peak.verg.velocity),
              saccadeAmp=first(r.amp),
              vectAmp= first(vect.amp),
              pauseDur=first(pause.dur)) ->
    m
  
  b=calc.relimp(lm(FR~peakVergVelocity+
                     maxVergTrans+
                     minVergTrans+
                     vergAmp,
                   data=m))
  
  b=calc.relimp(lm(FR~maxVergTrans+
                     minVergTrans+
                     Vamp+
                     conD+
                     divD,
                   data=m))
  
  dp %>% 
    group_by(neuron, splot) %>%
    summarize(FR= max(sdf),
              Vamp=first(verg.amp),
              conD=first(enhance.conv.dur),
              divD=first(enhance.dive.dur),
              maxVergTrans=first(max.verg.trans),
              minVergTrans=first(min.verg.trans),
              peakVergVelocity=first(peak.verg.velocity),
              saccadeAmp=first(r.amp),
              vectAmp= first(vect.amp),
              pauseDur=first(pause.dur)) %>%
    group_by(neuron) ->
    m
    m %>% 
    do(b=calc.relimp(lm(FR~maxVergTrans+
                          minVergTrans+
                          Vamp+
                          conD+
                          divD,
                        data=.))) ->
    bb
  
  r <- data.frame()
  r2 <- NULL
  for (i in 1:nrow(bb)){
    b<- bb$b[[i]]$lmg
    r<-rbind(r,b)
    
    r2<- c(r2,bb$b[[i]]$R2)
    
  }
  
  r$neuron<-bb$neuron
  r$R2<- r2
  
  names(r)<- c(names(bb$b[[1]]$lmg),'neuron','R2')
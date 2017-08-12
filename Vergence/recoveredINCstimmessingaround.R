mean.LVlead=mean(LVlead))->
  latencytest
latencytest
accelthresh=100
svelocitiesmean %>%
  filter(counter>0)%>%
  mutate(RHlead=abs(reaDIFF)>accelthresh+abs(reaDIFF[50]),
         RHlead=replace(counter,!RHlead,NA),
         RVlead=abs(reaVDIFF)>accelthresh+abs(reaDIFF[50]),
         RVlead=replace(counter,!RVlead,NA),
         LHlead=abs(leaDIFF)>accelthresh+abs(reaDIFF[50]),
         LHlead=replace(counter,!LHlead,NA),
         LVlead=abs(leaVDIFF)>accelthresh+abs(reaDIFF[50]),
         LVlead=replace(counter,!LVlead,NA))%>%
  summarize(RHlead=min(RHlead,na.rm=T),
            RVlead=min(RVlead,na.rm=T),
            LHlead=min(LHlead,na.rm=T),
            LVlead=min(LVlead,na.rm=T))%>%
  group_by(neuron)%>%
  mutate(mean.RHlead=mean(RHlead),
         mean.RVlead=mean(RVlead),
         mean.LHlead=mean(LHlead),
         mean.LVlead=mean(LVlead))->
  latencytest
latencytest
qplot(LHlead,LVlead,data=latencytest)
qplot(LHlead,LVlead,data=latencytest)+ylim(0,50)+xlim(0,50)
qplot(LHlead,LVlead,data=latencytest)+ylim(0,50)+xlim(0,50)+geom_abline()
anova(LHlead~LVlead,data=filter(latencytest(LHlead<50,LVlead<50)))
anova(LHlead,LVlead,data=filter(latencytest(LHlead<50,LVlead<50)))
aov(LHlead,LVlead,data=filter(latencytest(LHlead<50,LVlead<50)))
aov(LHlead~LVlead,data=filter(latencytest(LHlead<50,LVlead<50)))
aov(LHlead~LVlead,data=filter(latencytest,LHlead<50,LVlead<50))
summary(aov(LHlead~LVlead,data=filter(latencytest,LHlead<50,LVlead<50)))
accelthresh=50
svelocitiesmean %>%
  filter(counter>0)%>%
  mutate(RHlead=abs(reaDIFF)>accelthresh+abs(reaDIFF[50]),
         RHlead=replace(counter,!RHlead,NA),
         RVlead=abs(reaVDIFF)>accelthresh+abs(reaDIFF[50]),
         RVlead=replace(counter,!RVlead,NA),
         LHlead=abs(leaDIFF)>accelthresh+abs(reaDIFF[50]),
         LHlead=replace(counter,!LHlead,NA),
         LVlead=abs(leaVDIFF)>accelthresh+abs(reaDIFF[50]),
         LVlead=replace(counter,!LVlead,NA))%>%
  summarize(RHlead=min(RHlead,na.rm=T),
            RVlead=min(RVlead,na.rm=T),
            LHlead=min(LHlead,na.rm=T),
            LVlead=min(LVlead,na.rm=T))%>%
  group_by(neuron)%>%
  mutate(mean.RHlead=mean(RHlead),
         mean.RVlead=mean(RVlead),
         mean.LHlead=mean(LHlead),
         mean.LVlead=mean(LVlead))->
  latencytest
qplot(LHlead,LVlead,data=latencytest)+ylim(0,50)+xlim(0,50)+geom_abline()
svelocitiesmean %>%
  filter(counter>0)%>%
  mutate(RHlead=abs(reaDIFF)>accelthresh,
         RHlead=replace(counter,!RHlead,NA),
         RVlead=abs(reaVDIFF)>accelthresh,
         RVlead=replace(counter,!RVlead,NA),
         LHlead=abs(leaDIFF)>accelthresh,
         LHlead=replace(counter,!LHlead,NA),
         LVlead=abs(leaVDIFF)>accelthresh,
         LVlead=replace(counter,!LVlead,NA))%>%
  summarize(RHlead=min(RHlead,na.rm=T),
            RVlead=min(RVlead,na.rm=T),
            LHlead=min(LHlead,na.rm=T),
            LVlead=min(LVlead,na.rm=T))%>%
  group_by(neuron)%>%
  mutate(mean.RHlead=mean(RHlead),
         mean.RVlead=mean(RVlead),
         mean.LHlead=mean(LHlead),
         mean.LVlead=mean(LVlead))->
  latencytest
qplot(LHlead,LVlead,data=latencytest)+ylim(0,50)+xlim(0,50)+geom_abline()
leaDIFF=leaVDIFF-leaVDIFF[counter==0]) %>%
  ungroup() %>%
  mutate(meanFIXH=mean(revDIFF[1:50]),
         meanFIXV=mean(revVDIFF[1:50]))->
  dlatency
sp %>%
  group_by(neuron,stimes) %>%
  mutate(rea=parabolicdiff(rev,parabolicsize),
         lea=parabolicdiff(lev,parabolicsize),
         reaV=parabolicdiff(revV,parabolicsize),
         leaV=parabolicdiff(levV,parabolicsize),
         reaDIFF=rea-rea[counter==0],
         reaVDIFF=reaV-reaVDIFF[counter==0]
         leaDIFF=lea-lea[counter==0]
         leaDIFF=leaV-leaV[counter==0]) %>%
  ungroup() %>%
  mutate(meanFIXH=mean(revDIFF[1:50]),
         meanFIXV=mean(revVDIFF[1:50]))->
  dlatency
sp %>%
  group_by(neuron,stimes) %>%
  mutate(rea=parabolicdiff(rev,parabolicsize),
         lea=parabolicdiff(lev,parabolicsize),
         reaV=parabolicdiff(revV,parabolicsize),
         leaV=parabolicdiff(levV,parabolicsize),
         reaDIFF=rea-rea[counter==0],
         reaVDIFF=reaV-reaVDIFF[counter==0],
         leaDIFF=lea-lea[counter==0],
         leaDIFF=leaV-leaV[counter==0])%>%
  ungroup() %>%
  mutate(meanFIXH=mean(revDIFF[1:50]),
         meanFIXV=mean(revVDIFF[1:50]))->
  dlatency
sp %>%
  group_by(neuron,stimes) %>%
  mutate(rea=parabolicdiff(rev,parabolicsize),
         lea=parabolicdiff(lev,parabolicsize),
         reaV=parabolicdiff(revV,parabolicsize),
         leaV=parabolicdiff(levV,parabolicsize),
         reaDIFF=rea-rea[counter==0],
         reaVDIFF=reaV-reaV[counter==0],
         leaDIFF=lea-lea[counter==0],
         leaDIFF=leaV-leaV[counter==0])%>%
  ungroup() %>%
  mutate(meanFIXH=mean(revDIFF[1:50]),
         meanFIXV=mean(revVDIFF[1:50]))->
  dlatency
sp %>%
  group_by(neuron,stimes) %>%
  mutate(rea=parabolicdiff(rev,parabolicsize),
         lea=parabolicdiff(lev,parabolicsize),
         reaV=parabolicdiff(revV,parabolicsize),
         leaV=parabolicdiff(levV,parabolicsize),
         reaDIFF=rea-rea[counter==0],
         reaVDIFF=reaV-reaV[counter==0],
         leaDIFF=lea-lea[counter==0],
         leaDIFF=leaV-leaV[counter==0])%>%
  ungroup()->
  dlatency
dlatency %>%
  group_by(neuron,counter) %>%
  filter(!interrupted)%>%
  summarize_each(funs(mean))->
  svelocitiesmean
ggplot(filter(svelocitiesmean, counter> -50,counter<150),
       aes(group=stimes))+
  # geom_line(aes(counter,revDIFF),color='red')+
  geom_line(aes(counter,leaDIFF),color='blue')+
  # geom_line(aes(counter,revVDIFF-200),color='red',linetype=1)+
  geom_line(aes(counter,leaVDIFF),color='blue',linetype=2)+
  theme_minimal()+
  geom_hline(yintercept = c(-100,100))+
  ylim(-200,200)
names(svelocitiesmean)

parabolicsize=5
sp %>%
  group_by(neuron,stimes) %>%
  mutate(rea=parabolicdiff(rev,parabolicsize),
         lea=parabolicdiff(lev,parabolicsize),
         reaV=parabolicdiff(revV,parabolicsize),
         leaV=parabolicdiff(levV,parabolicsize),
         reaDIFF=rea-rea[counter==0],
         reaVDIFF=reaV-reaV[counter==0],
         leaDIFF=lea-lea[counter==0],
         leaVDIFF=leaV-leaV[counter==0])%>%
  ungroup()->
  dlatency
dlatency %>%
  group_by(neuron,counter) %>%
  filter(!interrupted)%>%
  summarize_each(funs(mean))->
  svelocitiesmean
ggplot(filter(svelocitiesmean, counter> -50,counter<50),
       aes(group=stimes))+
  # geom_line(aes(counter,revDIFF),color='red')+
  geom_line(aes(counter,leaDIFF),color='blue')+
  # geom_line(aes(counter,revVDIFF-200),color='red',linetype=1)+
  geom_line(aes(counter,reaVDIFF),color='red',linetype=2)+
  theme_minimal()+
  geom_hline(yintercept = c(-100,100))

ggplot(filter(dlatency, counter> -50,counter<50),
       aes(group=stimes))+
  # geom_line(aes(counter,revDIFF),color='red')+
  geom_line(aes(counter,leaDIFF),color='blue')+
  # geom_line(aes(counter,revVDIFF-200),color='red',linetype=1)+
  geom_line(aes(counter,reaVDIFF),color='red',linetype=2)+
  theme_minimal()+
  geom_hline(yintercept = c(-100,100))

+
  ylim(-200,200)
ggplot(filter(svelocitiesmean,neuron=='Kopachuck-501', counter> -50,counter<150),
       aes(group=stimes))+
  # geom_line(aes(counter,revDIFF),color='red')+
  geom_line(aes(counter,leaDIFF),color='blue')+
  # geom_line(aes(counter,revVDIFF-200),color='red',linetype=1)+
  geom_line(aes(counter,leaVDIFF),color='blue',linetype=2)+
  theme_minimal()+
  geom_hline(yintercept = c(-100,100))
accelthresh=50
svelocitiesmean %>%
  filter(counter>0)%>%
  mutate(RHlead=abs(reaDIFF)>accelthresh,
         RHlead=replace(counter,!RHlead,NA),
         RVlead=abs(reaVDIFF)>accelthresh,
         RVlead=replace(counter,!RVlead,NA),
         LHlead=abs(leaDIFF)>accelthresh,
         LHlead=replace(counter,!LHlead,NA),
         LVlead=abs(leaVDIFF)>accelthresh,
         LVlead=replace(counter,!LVlead,NA))%>%
  summarize(RHlead=min(RHlead,na.rm=T),
            RVlead=min(RVlead,na.rm=T),
            LHlead=min(LHlead,na.rm=T),
            LVlead=min(LVlead,na.rm=T))%>%
  group_by(neuron)%>%
  mutate(mean.RHlead=mean(RHlead),
         mean.RVlead=mean(RVlead),
         mean.LHlead=mean(LHlead),
         mean.LVlead=mean(LVlead))->
  latencytest
qplot(LHlead,LVlead,data=latencytest)+ylim(0,50)+xlim(0,50)+geom_abline()
summary(aov(LHlead~LVlead,data=filter(latencytest,LHlead<50,LVlead<50)))
qplot(LHlead,LVlead,data=latencytest)+geom_abline()
accelthresh=100
svelocitiesmean %>%
  filter(counter>0)%>%
  mutate(RHlead=abs(reaDIFF)>accelthresh,
         RHlead=replace(counter,!RHlead,NA),
         RVlead=abs(reaVDIFF)>accelthresh,
         RVlead=replace(counter,!RVlead,NA),
         LHlead=abs(leaDIFF)>accelthresh,
         LHlead=replace(counter,!LHlead,NA),
         LVlead=abs(leaVDIFF)>accelthresh,
         LVlead=replace(counter,!LVlead,NA))%>%
  summarize(RHlead=min(RHlead,na.rm=T),
            RVlead=min(RVlead,na.rm=T),
            LHlead=min(LHlead,na.rm=T),
            LVlead=min(LVlead,na.rm=T))%>%
  group_by(neuron)%>%
  mutate(mean.RHlead=mean(RHlead),
         mean.RVlead=mean(RVlead),
         mean.LHlead=mean(LHlead),
         mean.LVlead=mean(LVlead))->
  latencytest
qplot(LHlead,LVlead,data=latencytest)+geom_abline()
summary(aov(LHlead~LVlead,data=filter(latencytest,LHlead<50,LVlead<50)))
qplot(LHlead,LVlead,data=latencytest)+geom_abline()+coord_fixed()
names(latencytest)
svelocitiesmean %>%
  filter(counter>0)%>%
  mutate(RHlead=abs(reaDIFF)>accelthresh,
         RHlead=replace(counter,!RHlead,NA),
         RVlead=abs(reaVDIFF)>accelthresh,
         RVlead=replace(counter,!RVlead,NA),
         LHlead=abs(leaDIFF)>accelthresh,
         LHlead=replace(counter,!LHlead,NA),
         LVlead=abs(leaVDIFF)>accelthresh,
         LVlead=replace(counter,!LVlead,NA))%>%
  summarize(RHlead=min(RHlead,na.rm=T),
            RVlead=min(RVlead,na.rm=T),
            LHlead=min(LHlead,na.rm=T),
            LVlead=min(LVlead,na.rm=T))%>%
  group_by(neuron)%>%
  mutate(mean.RHlead=mean(RHlead),
         mean.RVlead=mean(RVlead),
         mean.LHlead=mean(LHlead),
         mean.LVlead=mean(LVlead))%>%
  separate(neuron,c('mmm','sitenum'),remove=FALSE) %>%
  mutate(sitenum=str_sub(sitenum,1,3))->
  latencytest
latencytest%>%
  separate(neuron,c('mmm','sitenum'),remove=FALSE) %>%
  mutate(sitenum=str_sub(sitenum,1,3))%>%
  group_by(sitenum)%>%
  summarize(mean.RHlead=mean(RHlead),
            mean.RVlead=mean(RVlead),
            mean.LHlead=mean(LHlead),
            mean.LVlead=mean(LVlead))->
  ltp
svelocitiesmean %>%
  filter(counter>0)%>%
  mutate(RHlead=abs(reaDIFF)>accelthresh,
         RHlead=replace(counter,!RHlead,NA),
         RVlead=abs(reaVDIFF)>accelthresh,
         RVlead=replace(counter,!RVlead,NA),
         LHlead=abs(leaDIFF)>accelthresh,
         LHlead=replace(counter,!LHlead,NA),
         LVlead=abs(leaVDIFF)>accelthresh,
         LVlead=replace(counter,!LVlead,NA))%>%
  summarize(RHlead=min(RHlead,na.rm=T),
            RVlead=min(RVlead,na.rm=T),
            LHlead=min(LHlead,na.rm=T),
            LVlead=min(LVlead,na.rm=T))%>%
  group_by(neuron)%>%
  mutate(mean.RHlead=mean(RHlead),
         mean.RVlead=mean(RVlead),
         mean.LHlead=mean(LHlead),
         mean.LVlead=mean(LVlead))->
  latencytest
latencytest%>%
  separate(neuron,c('mmm','sitenum'),remove=FALSE) %>%
  mutate(sitenum=str_sub(sitenum,1,3))%>%
  group_by(sitenum)%>%
  summarize(mean.RHlead=mean(RHlead),
            mean.RVlead=mean(RVlead),
            mean.LHlead=mean(LHlead),
            mean.LVlead=mean(LVlead))->
  ltp
qplot(LHlead,LVlead,data=ltp)+geom_abline()+coord_fixed()
qplot(mean.LHlead,mean.LVlead,data=ltp)+geom_abline()+coord_fixed()
summary(aov(LHlead~LVlead,data=ltp)
        summary(aov(LHlead~LVlead,data=ltp))
        summary(aov(mean.LHlead~mean.LVlead,data=ltp))
        qplot(mean.LHlead,mean.LVlead,data=ltp)+geom_abline()+coord_fixed()+geom_label(aes(label=sitenum))
        
# t<-readRDS('KopachuckINC Wed Sep 13')
t<-readRDS('INCSTRAB-11-3-2017.RDS')

t %>%
  group_by(neuron) %>%
  mutate(dsnum=markSaccadesDouble(conj.velocity,threshold1=30,threshold2=15),
         sdf=spikedensity(rasters,10),
         sdf10=lag(sdf,10),
         conj.vert=(repV+lepV)/2,
         conj.vert.vel=(revV+levV)/2,
         conj.hor=(rep+lep)/2,
         conj.hor.v=(rev+lev)/2) %>%
  group_by(dsnum) %>%
  mutate(sd.conj.velocity=sd(conj.velocity),
         dur=n(),
         asleep=(sd.conj.velocity>7.5 & dsnum<0) || dur>2000)->
  t

#Saccades----
t %>%
  group_by(neuron) %>%
  mutate(time=row_number(),
         rasters10=lag(rasters,10)) %>%
  filter(dsnum>0) %>% #saccades only - no buffer
  group_by(neuron,dsnum) %>%
  summarize(dur=n(),
            peakFR=max(sdf10),
            nspklag=sum(rasters10),
            nspk=sum(rasters),
            R.H.Amp=last(rep)-rep[1],
            L.H.Amp=last(lep)-lep[1],
            R.V.Amp=last(repV)-repV[1],
            L.V.Amp=last(lepV)-lepV[1],
            conj.h.amp=(R.H.Amp+L.H.Amp)/2,
            conj.v.amp=(R.V.Amp+L.V.Amp)/2,
            r.amp=sqrt(conj.h.amp^2+conj.v.amp^2),
            maxamp=max(abs(R.H.Amp),abs(R.V.Amp),abs(L.H.Amp),abs(L.V.Amp))) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zp

qplot(conj.v.amp,peakFR,data=filter(zp,r.amp>3,dur<150,dur>50),color=neuron)+
  facet_wrap(~monkey)+
  theme(legend.position='none')

qplot(conj.v.amp,nspklag,data=filter(zp,r.amp>3,dur<150,dur>50),color=neuron)+
  facet_wrap(~monkey)+
  theme(legend.position='none')


ggplot(aes(conj.v.amp,nspklag),data=filter(zp,r.amp>3,dur<150,dur>50))+
  geom_point(aes(color=neuron,alpha=1/10))+
  stat_smooth(aes(group=neuron,color=neuron),method='lm',se=FALSE)+
  facet_wrap(~monkey)+
  theme(legend.position='none')+
  ylim(0,NA)+
  xlab('Vertical Saccade Amplitude')+
  ylab("Number of Spikes")

ggplot(aes(conj.h.amp,nspklag),data=filter(zp,r.amp>3,dur<150,dur>50))+
  geom_point(aes(color=neuron,alpha=1/10))+
  stat_smooth(aes(group=neuron,color=neuron),method='lm',se=FALSE)+
  facet_wrap(~monkey)+
  theme(legend.position='none')+
  ylim(0,NA)+
  xlab('Horizontal Saccade Amplitude')+
  ylab('Number of Spikes')

ggplot(aes(R.V.Amp,nspklag),data=filter(zp,r.amp>3,dur<150,dur>50))+
  geom_point(aes(color=neuron,alpha=1/10))+
  stat_smooth(aes(group=neuron,color=neuron),method='lm',se=FALSE)+
  facet_wrap(~monkey)+
  theme(legend.position='none')+
  ylim(0,NA)+
  xlim(-40,40)+
  xlab('Right Eye Vertical Saccade Amplitude')+
  ylab("Number of Spikes")

ggplot(aes(L.V.Amp,nspklag),data=filter(zp,r.amp>3,dur<150,dur>50))+
  geom_point(aes(color=neuron,alpha=1/10))+
  stat_smooth(aes(group=neuron,color=neuron),method='lm',se=FALSE)+
  facet_wrap(~monkey)+
  theme(legend.position='none')+
  ylim(0,NA)+
  xlim(-40,40)+
  xlab('Left Eye Vertical Saccade Amplitude')+
  ylab("Number of Spikes")

ggplot(aes(L.V.Amp,nspklag),data=filter(zp,r.amp>3,dur<150,dur>50,abs(conj.h.amp)<1))+
  geom_point(aes(color=neuron,alpha=1/10))+
  stat_smooth(aes(group=neuron,color=neuron),method='lm',se=FALSE)+
  facet_wrap(~monkey)+
  theme(legend.position='none')+
  ylim(0,NA)+
  xlim(-40,40)+
  xlab('Left Eye Vertical Saccade Amplitude')+
  ylab("Number of Spikes")+
  ggtitle('Only Vertical Saccades')


ggplot(aes(R.V.Amp,nspklag),data=filter(zp,r.amp>3,dur<150,dur>50,abs(conj.h.amp)<1))+
  geom_point(aes(color=neuron,alpha=1/10))+
  stat_smooth(aes(group=neuron,color=neuron),method='lm',se=FALSE)+
  facet_wrap(~monkey)+
  theme(legend.position='none')+
  ylim(0,NA)+
  xlim(-40,40)+
  xlab('Right Eye Vertical Saccade Amplitude')+
  ylab("Number of Spikes")+
  ggtitle('Only Vertical Saccades')


ggplot(data=filter(zp,r.amp>3,dur<150))+
  stat_smooth(aes(conj.h.amp,nspk,group=neuron),method='lm',se=FALSE,color='black')+
  facet_wrap(~monkey)+
  theme(legend.position='none')

#monotest
zp<- filter(zp,neuron %in% c('Kopachuck-913','Kopachuck-941'))
ggplot(data=filter(zp,R.V.Amp*L.V.Amp<0,dur>40,r.amp>1))+
  geom_point(aes(R.V.Amp,peakFR),color='red')+
  stat_smooth(aes(R.V.Amp,peakFR),color='red',method='lm',se=FALSE)+
  geom_point(aes(L.V.Amp,peakFR),color='blue')+
  stat_smooth(aes(L.V.Amp,peakFR),color='blue',method='lm',se=FALSE)+
  facet_wrap(~neuron,scales='free',ncol=5)+
  xlab('Vertical Saccade Amplitude (monocular)')+
  ggtitle('Saccades with disjunctive vertical components')

ggplot(data=filter(zp,dur>40,r.amp>1))+
  geom_point(aes(R.V.Amp,peakFR),color='red')+
  stat_smooth(aes(R.V.Amp,peakFR),color='red',method='lm',se=FALSE)+
  geom_point(aes(L.V.Amp,peakFR),color='blue')+
  stat_smooth(aes(L.V.Amp,peakFR),color='blue',method='lm',se=FALSE)+
  facet_wrap(~neuron)+
  xlab('Vertical Saccade Amplitude (monocular)')

zp %>%
  group_by(neuron) %>%
  filter(R.V.Amp*L.V.Amp<0,r.amp>1,dur>40)%>%
  do(tidy(lm(peakFR~R.V.Amp+L.V.Amp,data=.))) %>%
  select(neuron,term,estimate) %>%
  spread(term,estimate) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zpm
  
qplot(R.V.Amp,L.V.Amp,data=zpm)+facet_wrap(~monkey)
qplot(R.V.Amp,L.V.Amp,data=filter(zpm,monkey=='Kopachuck'))+coord_fixed()

ggplot(aes(R.V.Amp,L.V.Amp),data=filter(zp,monkey=='Kopachuck',R.V.Amp*L.V.Amp<0,
       neuron %in% c('Kopachuck-913','Kopachuck-941')))+
  geom_point(aes(color=peakFR))+
  facet_wrap(~neuron,scales='free')

#big plots

ggplot(aes(conj.v.amp,nspklag),data=filter(zp,r.amp>3,dur<150,dur>50))+
  geom_point(aes(color=monkey,alpha=1/10))+
  stat_smooth(aes(group=neuron,color=neuron),method='lm',se=FALSE)+
  facet_wrap(~neuron,ncol=5,scale='free_x')+
  theme(legend.position='none')+
  ylim(0,NA)+
  xlab('Vertical Saccade Amplitude')+
  ylab("Number of Spikes")

ggplot(aes(conj.h.amp,nspklag),data=filter(zp,r.amp>3,dur<150,dur>50))+
  geom_point(aes(color=monkey,alpha=1/10))+
  stat_smooth(aes(group=neuron,color=neuron),method='lm',se=FALSE)+
  facet_wrap(~neuron,ncol=5,scale='free_x')+
  theme(legend.position='none')+
  ylim(0,NA)+
  xlab('Horizontal Saccade Amplitude')+
  ylab("Number of Spikes")

#population estimates:

#Fixation----
t %>%
  group_by(neuron)%>%
  mutate(time=row_number()) %>%
  filter(dsnum<0) %>% #fixations only
  group_by(neuron,dsnum) %>%
  summarize(sd.conj.velocity=sd(conj.velocity),
            mean.conj.velocity=mean(conj.velocity),
            spread=max(conj.velocity)-min(conj.velocity),
            qrange=quantile(conj.velocity,0.975)-quantile(conj.velocity,0.025),
            dur=n(),
            starttime=first(time),
            c2eyes=cor(repV,lepV),
            meanFR=sum(rasters)/dur*1000,
            mean.V=mean(conj.vert),
            mean.H=mean(conj.hor),
            mean.R.H=mean(rep),
            mean.L.H=mean(lep),
            mean.R.V=mean(repV),
            mean.L.V=mean(lepV),
            asleep=sd.conj.velocity>7.5 || dur>2000)->
  zp

ggplot(filter(zp,dur>100,!asleep),aes(mean.V,meanFR,group=neuron))+
  stat_smooth(method='lm',se = FALSE,color='black')+
  facet_wrap(~mean.V>0,scales='free_x')

zp%>%
  group_by(neuron) %>%
  filter(dur>100,!asleep) %>%
  mutate(cor.pref.V=cor(mean.V,meanFR),
         cor.pref.H=cor(mean.H,meanFR),
         dir.pref.V=sign(cor.pref.V),
         dir.pref.H=sign(cor.pref.H),
         # signif.pref.V=cor.pref.V^2<0.1,
         # signif.pref.H=cor.pref.H^2<0.1)%>%
         signif.pref.V=cor.test(mean.V,meanFR)$p.value,
         signif.pref.H=cor.test(mean.H,meanFR)$p.value,
         signif.pref.R.V=cor.test(mean.R.V,meanFR)$p.value,
         signif.pref.R.H=cor.test(mean.R.H,meanFR)$p.value,
         signif.pref.L.V=cor.test(mean.L.V,meanFR)$p.value,
         signif.pref.L.H=cor.test(mean.L.H,meanFR)$p.value) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zp

ggplot(zp,aes(mean.V,meanFR,group=neuron))+
  stat_smooth(method='lm',se = FALSE,aes(color=monkey,linetype=signif.pref.V>0.05))+
  facet_wrap(~dir.pref.V)+
  scale_colour_manual(values=c('black','orange','hotpink'))+
  theme_minimal()+
  xlab('Mean conjugate Vertical Eye Position (deg)')+
  ylab('Mean Firing Rate (spks/s)')+
  # theme(legend.position="none")+
  ylim(0,NA)

zp$dir.pref.H<- as.factor(zp$dir.pref.H)
levels(zp$dir.pref.H)<- c('Leftward','Rightward')

ggplot(zp,aes(mean.H,meanFR,group=neuron))+
  stat_smooth(method='lm',se = FALSE,aes(color=monkey,linetype=signif.pref.H>0.001))+
  facet_wrap(~dir.pref.H)+
  scale_colour_manual(values=c('black','orange','hotpink'))+
  theme_minimal()+
  xlab('Mean Conjugate Horizontal Eye Position (deg)')+
  ylab('Mean Firing Rate (spks/s)')#+
  # theme(legend.position="none")+
  ylim(0,NA)

#exerimental:
# ggplot(filter(zp,mean.L.V*mean.R.V<0,monkey=='Kopachuck',cellnum %in% c(913,932)))+
#   geom_point(aes(mean.L.V,mean.R.V,color=meanFR,shape=as.factor(dir.pref.V)))+
#   # stat_smooth(aes(mean.L.V,mean.R.V),method='lm')+
#   facet_wrap(~neuron)+
#   geom_abline()
# 
# 
# ggplot(filter(zp,mean.L.V*mean.R.V<0,monkey=='Kopachuck',cellnum %in% c(913,932)))+
#   geom_point(aes(mean.L.V,meanFR),color='blue')+
#   geom_point(aes(mean.R.V,meanFR),color='red')+
#   facet_wrap(~neuron)
# 
# ggplot(filter(zp,monkey=='Kopachuck'))+
#   geom_point(aes(mean.L.V,meanFR),color='blue')+
#   geom_point(aes(mean.R.V,meanFR),color='red')+
#   facet_wrap(~neuron)
# 
# ggplot(filter(zp,mean.L.V*mean.R.V<0,monkey=='Bee'))+
#   geom_point(aes(mean.L.V,meanFR),color='blue')+
#   geom_point(aes(mean.R.V,meanFR),color='red')+
#   facet_wrap(~neuron)
# 
# ggplot(filter(zp,monkey=='Kopachuck'))+
#   geom_point(aes(mean.V,meanFR,color=mean.H))+
#   facet_wrap(~neuron)
#end experimental


zp %>%
  group_by(neuron) %>%
  do(mod=lm(meanFR~mean.V+mean.H,data=filter(.,!asleep,dur>100,meanFR>0)))->
  zm

zm %>%
  glance(mod) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zz

zm %>%
  tidy(mod)%>%
  select(neuron,term,estimate) %>%
  mutate(term=replace(term,term=='(Intercept)','b')) %>%
  spread(term,estimate)%>%
  mutate(dir.pref=atan2(mean.V,mean.H)*180/pi)->
  zt

zm %>%
  mutate(mean.V.imp=calc.relimp(mod)$lmg[1],
         mean.H.imp=calc.relimp(mod)$lmg[2])%>%
  select(-mod)%>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zzm

zzm %>%
  mutate(mean.V.w=mean.V/(mean.V+mean.H),
         mean.H.w=mean.H/(mean.V+mean.H),
         dir.pref=atan2(mean.V,mean.H)*180/pi)->
  zzm

zt<-left_join(zt,zzm,by='neuron')

zp %>%
  group_by(neuron) %>%
  summarize(signif.pref.V=first(signif.pref.V),
            signif.pref.H=first(signif.pref.H))->
  zps

zt<- left_join(zt,zps,by='neuron')

qplot(mean.H.imp,mean.V.imp,data=zzm,color=monkey)+
  geom_abline()+
  coord_fixed()+
  geom_text(aes(label=cellnum))



ggplot(filter(zt,signif.pref.V<0.001 | signif.pref.H<0.001))
  
ggplot(zt)+
  geom_segment(aes(yend=(mean.V.imp+mean.H.imp),xend=dir.pref,y=0,x=dir.pref),color='black',
               alpha=0.5,arrow=arrow(type='closed',length =unit(0.1,"inches")))+
  # geom_text(aes(dir.pref,R2+0.05,label=cellnum),alpha=1)+
  coord_polar(direction=-1,start=pi/2)+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  theme_minimal()+
  ggtitle('Direction preference and goodness of fit')+
  ylab('Goodness of Fit')+
  xlab('Preferred Direction')+
  facet_wrap(~monkey)


ggplot(zp,aes(mean.V,meanFR,group=neuron))+
  # geom_point()+
  # geom_point(aes(mean.H,meanFR),color='hotpink')+
  stat_smooth(method='lm',se = FALSE,aes(linetype=signif.pref.V>0.001),color='black')+
  stat_smooth(method='lm',se=FALSE,aes(mean.H,meanFR,linetype=signif.pref.H>0.001),color='hotpink')+
  facet_wrap(~dir.pref.H)+
  theme_minimal()+
  xlab('Mean Conjugate Eye Position (deg)')+
  ylab('Mean Firing Rate (spks/s)')+
  # theme(legend.position="none")+
  ylim(0,NA)+
  facet_wrap(~neuron,ncol=4)

ggplot(filter(zp,neuron=='Bee-118'))+
  geom_point(aes(mean.H,mean.V,color=meanFR),size=3)


ggplot(zp)+
  geom_point(aes(mean.H,mean.V,color=meanFR),size=3)+
  facet_wrap(~neuron,scales='free',ncol=4)

#MONOCULARfixation----

zp %>%
  group_by(neuron) %>%
  do(mod=lm(meanFR~mean.R.V+mean.L.V+mean.R.H+mean.L.H,
            data=filter(.,meanFR>0)))->
  zmono

zmono %>%
  glance(mod) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zzmono

zmono %>%
  tidy(mod)%>%
  select(neuron,term,estimate) %>%
  mutate(term=replace(term,term=='(Intercept)','b')) %>%
  spread(term,estimate)%>%
  mutate(dir.pref.R=atan2(mean.R.V,mean.R.H)*180/pi,
         dir.pref.L=atan2(mean.L.V,mean.L.H)*180/pi)->
  ztmono

zmono %>%
  mutate(mean.R.V.imp=calc.relimp(mod)$lmg[1],
         mean.L.V.imp=calc.relimp(mod)$lmg[2],
         mean.R.H.imp=calc.relimp(mod)$lmg[3],
         mean.L.H.imp=calc.relimp(mod)$lmg[4])%>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)%>%
  select(-mod)->
  zzmmono

ggplot(zzmmono)+
  geom_point(aes(mean.R.H.imp,mean.R.V.imp),color='red')+
  geom_point(aes(mean.L.H.imp,mean.L.V.imp),color='blue')+
  geom_segment(aes(x=mean.R.H.imp,xend=mean.L.H.imp,
                   y=mean.R.V.imp,yend=mean.L.V.imp))+
  facet_wrap(~monkey)+
  geom_abline()+
  theme_minimal()


ggplot(filter(zt,signif.pref.V<0.001 | signif.pref.H<0.001))+
  geom_segment(aes(yend=(mean.V.imp+mean.H.imp),xend=dir.pref,y=0,x=dir.pref),color='black',
               alpha=0.5,arrow=arrow(type='closed',length =unit(0.1,"inches")))+
  # geom_text(aes(dir.pref,R2+0.05,label=cellnum),alpha=1)+
  coord_polar(direction=-1,start=pi/2)+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  theme_minimal()+
  ggtitle('Direction preference and goodness of fit')+
  ylab('Goodness of Fit')+
  xlab('Preferred Direction')+
  facet_wrap(~monkey)
  
dirsignif<-filter(zt,signif.pref.V<0.001 | signif.pref.H<0.001)
dirsignif<-filter(zt,signif.pref.V<0.05 | signif.pref.H<0.05)
dirsignif %>%
  mutate(dir.pref=abs(90-abs(dir.pref)))->
  dirsignifx

t.test(dir.pref~monkey,data=dirsignifx)

ggplot(dirsignifx)+
  geom_segment(aes(yend=(mean.V.imp+mean.H.imp),xend=dir.pref,y=0,x=dir.pref),color='black',
               alpha=0.5,arrow=arrow(type='closed',length =unit(0.1,"inches")))+
  # geom_text(aes(dir.pref,R2+0.05,label=cellnum),alpha=1)+
  coord_polar(direction=-1,start=pi/2)+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  theme_minimal()+
  ggtitle('Direction preference and goodness of fit')+
  ylab('Goodness of Fit')+
  xlab('Preferred Direction')+
  facet_wrap(~monkey)

ggplot(filter(zp,neuron=='Kopachuck-942'))+
  geom_point(aes(mean.R.H,meanFR),color='red')+
  geom_point(aes(mean.L.H,meanFR),color='blue')

ggplot(zp,aes(mean.R.V,meanFR,group=neuron))+
  stat_smooth(method='lm',se = FALSE,aes(color=monkey,linetype=signif.pref.R.V>0.001))+
  facet_wrap(~dir.pref.V)+
  scale_colour_manual(values=c('black','orange'))+
  theme_minimal()+
  xlab('Mean conjugate Vertical Eye Position (deg)')+
  ylab('Mean Firing Rate (spks/s)')+
  theme(legend.position="none")+
  ylim(0,NA)

ggplot(zp,aes(mean.L.V,meanFR,group=neuron))+
  stat_smooth(method='lm',se = FALSE,aes(color=monkey,linetype=signif.pref.L.V>0.001))+
  facet_wrap(~dir.pref.V)+
  scale_colour_manual(values=c('black','orange'))+
  theme_minimal()+
  xlab('Mean conjugate Vertical Eye Position (deg)')+
  ylab('Mean Firing Rate (spks/s)')+
  theme(legend.position="none")+
  ylim(0,NA)

ggplot(zp,aes(mean.R.H,meanFR,group=neuron))+
  stat_smooth(method='lm',se = FALSE,aes(color=monkey,linetype=signif.pref.R.H>0.001))+
  facet_wrap(~dir.pref.V)+
  scale_colour_manual(values=c('black','orange'))+
  theme_minimal()+
  xlab('Mean conjugate Vertical Eye Position (deg)')+
  ylab('Mean Firing Rate (spks/s)')+
  theme(legend.position="none")+
  ylim(0,NA)

ggplot(zp,aes(mean.L.H,meanFR,group=neuron))+
  stat_smooth(method='lm',se = FALSE,aes(color=monkey,linetype=signif.pref.L.H>0.001))+
  facet_wrap(~dir.pref.V)+
  scale_colour_manual(values=c('black','orange'))+
  theme_minimal()+
  xlab('Mean conjugate Vertical Eye Position (deg)')+
  ylab('Mean Firing Rate (spks/s)')+
  theme(legend.position="none")+
  ylim(0,NA)


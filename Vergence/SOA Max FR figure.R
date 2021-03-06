# t<- readRDS('enhancemarked2.RDS')
t<- readRDS('SOA-NRTP.RDS')
t<- filter(t,cellnum>100)
# t %>% group_by(neuron) %>%
#   do(dynamiclead(p=.,formula='verg.angle',seq(5,60,by=5))) ->
#   t

t %>%
  group_by(neuron) %>%
  mutate(time=row_number(),
         sdf20= lag(sdf,20),
         near.response=cor(verg.angle,sdf)>0) ->
  t

t %>% group_by(neuron) %>%
  do(joinsaccades(.,buffer=20,threshold=20))->
  t

t %>%
  group_by(neuron) %>%
  # mutate(bin.velocity=cut(verg.velocity,c(seq(-200,200,by=20)))) %>%
  # filter(!is.na(sacnum)) %>%
  group_by(neuron, sacnum) %>%
  mutate(conj.h=(lep+rep)/2,
         conj.v=(lepV+repV)/2,
         conj.h.amp=last(conj.h)-first(conj.h),
         conj.v.amp=last(conj.v)-first(conj.v),
         conj.angle=atan2(conj.v,conj.h)*180/pi,
         peak.verg.velocity=maxabs(verg.velocity),
         r.amp=sqrt(conj.h^2+conj.v^2),
         verg.amp=last(verg.angle)-first(verg.angle),
         peak.FR=max(sdf20),
         saccade.type='conj',
         saccade.type=replace(saccade.type,verg.amp< -2 & r.amp>2,'diverging'),
         saccade.type=replace(saccade.type,verg.amp> 2& r.amp>2,'converging'),
         saccade.type=replace(saccade.type,is.na(sacnum) | r.amp<2,'no.saccade')) ->
  t

t %>%
  filter(abs(verg.amp)<25,r.amp>2) %>%
  group_by(neuron) %>%
  mutate(sig.converg=cor.test(peak.verg.velocity[peak.verg.velocity>0],
                              peak.FR[peak.verg.velocity>0])$p.value,
         sig.diverg=cor.test(peak.verg.velocity[peak.verg.velocity<0],
                             peak.FR[peak.verg.velocity<0])$p.value) %>%
  group_by(neuron,sacnum) %>%
  summarize(near.response=near.response[1],
            verg.amp=first(verg.amp),
            peak.verg.velocity=first(peak.verg.velocity),
            peak.FR=first(peak.FR),
            sig.converg=first(sig.converg),
            sig.diverg=first(sig.diverg))%>%
  filter(abs(peak.verg.velocity)<250,
         peak.FR>10) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  sp

qplot(verg.amp,peak.FR,data=sp)+facet_wrap(~neuron)

democells<-c('Bee-211')

spd<- filter(sp,neuron %in% democells)

ggplot(spd,aes(peak.verg.velocity,peak.FR))+
  geom_point(shape=21,fill='black')+
  # facet_wrap(~neuron)+
  stat_smooth(method='lm',data=filter(spd,peak.verg.velocity>0))+
  xlab('Peak Vergence Velocity (deg)')+
  ylab('Peak Firing Rate (spk/s)')+
  theme_bw()

ggsave('regressiondemo.PDF',height=5,width=5)

#----
#Plot only the on direction:
sp %>%
  mutate(diverging=peak.verg.velocity<0) %>%
  group_by(neuron) %>%
  do(m=lm(peak.FR~peak.verg.velocity:diverging,data=.))->
  spr

spr %>%
  mutate(rsq=summary(m)$r.squared) %>%
  dplyr::select(-m) ->
  rsq

sp %>%
  mutate(diverging=peak.verg.velocity<0) %>%
  group_by(neuron) %>%
  do(tidy(lm(peak.FR~peak.verg.velocity:diverging,data=.)))->
  spm


spm %>%
  dplyr::select(neuron,term,estimate) %>%
  # filter(term != '(Intercept)') %>%
  summarize(max.slope=max(estimate[2:3]),
            min.slope=min(estimate[2:3]),
            b=estimate[1],
            preferred.slope=maxabs(c(max.slope,min.slope)),
            off.slope=minabs(c(max.slope,min.slope)))%>%
  
    # preferred.slope=maxabs(estimate[term != '(Intercept)']),
            # off.slope=estimate[term != '(Intercept)',estimate!=preferred.slope],
            # b=estimate[term=='(Intercept)']) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE) %>%
  mutate(preferred.direction=sign(preferred.slope))->
  spms


ggplot(spms)+
  geom_point(aes(x=b,y=preferred.slope),alpha=0)+
  geom_abline(aes(slope=abs(preferred.slope),intercept=b),size=0.5,alpha=1)+
  coord_cartesian(ylim=c(0,500),xlim=c(0,150))+
  theme_minimal()+
  xlab('Peak Vergence Speed (deg/s) -- On Direction Saccades')+
  ylab('Peak Firing Rate (spks/s)')

# ggplot(spms)+
#   geom_point(aes(x=b,y=preferred.slope),alpha=0)+
#   geom_abline(aes(slope=preferred.slope,intercept=b,color=sign(preferred.slope)),size=0.5,alpha=1)+
#   coord_cartesian(ylim=c(0,500),xlim=c(0,150))+
#   theme_bw()+
#   xlab('Peak Vergence Speed (deg/s) -- On Direction Saccades')+
#   ylab('Peak Firing Rate (spks/s)')

ggplot(spms)+
  geom_point(aes(x=b,y=off.slope),alpha=0)+
  geom_abline(aes(slope=off.slope*sign(preferred.slope),intercept=b),size=0.5,alpha=1)+
  coord_cartesian(ylim=c(0,500),xlim=c(0,150))+
  theme_minimal()+
  xlab('Peak Vergence Speed (deg/s) -- Off Direction Saccades')+
  ylab('Peak Firing Rate (spks/s)')

ggplot(spms)+
  geom_point(aes(x=b,y=off.slope),alpha=0)+
  geom_abline(aes(slope=off.slope*sign(preferred.slope)*-1,intercept=b),size=0.5,alpha=1)+
  coord_cartesian(ylim=c(0,500),xlim=c(-150,0))+
  theme_minimal()+
  xlab('Peak Vergence Speed (deg/s) -- Off Direction Saccades')+
  ylab('Peak Firing Rate (spks/s)')

ggplot(spms)+
  geom_point(aes(x=b,y=off.slope),alpha=0)+
  geom_abline(aes(slope=abs(off.slope),intercept=b),size=1,alpha=1)+
  coord_cartesian(ylim=c(0,500),xlim=c(0,150))+
  theme_bw()+
  xlab('Peak Vergence Speed (deg/s)')+
  ylab('Peak Firing Rate (spks/s)')


ggsave('RefressionDemo.PDF',height=5,width=5)

#final versiodn: add off direction too for reviewer
# sp <- mutate(sp,diverging=cor(peak.verg.velocity,peak.FR)<0)

# sp <- mutate(sp,diverging=sign(cor(peak.verg.velocity,peak.FR)))


ggplot(aes(peak.verg.velocity,peak.FR,group=neuron),data=sp)+
  stat_smooth(method='lm',se=FALSE,color='black',data=filter(sp,peak.verg.velocity>0))+
  stat_smooth(method='lm',se=FALSE,color='black',data=filter(sp,peak.verg.velocity<0))+
  facet_wrap(~near.response)

ggplot(aes(peak.verg.velocity,peak.FR,group=neuron),data=sp)+
  stat_smooth(method='lm',se=FALSE,color='black')+
  facet_wrap(~near.response)+
  theme_minimal()
  

ggplot(aes(peak.verg.velocity,peak.FR,group=neuron),data=filter(sp,!diverging,peak.verg.velocity>0))+
  stat_smooth(method='lm',se=FALSE,color='black')+
  theme_minimal()+
  ylim(0,350)

ggplot(aes(peak.verg.velocity,peak.FR,group=neuron),
       data=filter(sp,!diverging,peak.verg.velocity<0))+
  stat_smooth(method='lm',se=FALSE,color='black')+
  theme_minimal()+
  ylim(0,350)


ggplot(aes(peak.verg.velocity,peak.FR,group=neuron),data=sp)+
  stat_smooth(method='lm',se=FALSE,color='black',
              data=filter(sp,!diverging,peak.verg.velocity>0))+ #on dir conv
  stat_smooth(method='lm',se=FALSE,color='hotpink',
              data=filter(sp,diverging,peak.verg.velocity<0))+ #on dir div
  ylim(0,350)+
  theme_minimal()
  

ggplot(aes(peak.verg.velocity,peak.FR,group=neuron),data=sp)+
  stat_smooth(method='lm',se=FALSE,color='black',
              data=filter(sp,!near.response,peak.verg.velocity<0))+ #off dir conv
  stat_smooth(method='lm',se=FALSE,color='hotpink',
              data=filter(sp,near.response,peak.verg.velocity>0))+ #off dir div
  ylim(0,350)+
  theme_minimal()

ggplot(aes(peak.verg.velocity,peak.FR,group=neuron),data=sp)+
  stat_smooth(method='lm',se=FALSE,aes(color=near.response),data=filter(sp,peak.verg.velocity<0))+
  stat_smooth(method='lm',se=FALSE,aes(color=near.response),data=filter(sp,peak.verg.velocity>0))


sp<- mutate(sp,peak.preferred.verg.velocity=peak.verg.velocity*diverging)

ggplot(aes(peak.preferred.verg.velocity,peak.FR,group=neuron),data=sp)+
  stat_smooth(method='lm',se=FALSE,color='black',data=filter(sp,peak.preferred.verg.velocity>0))+
  

ggplot(aes(peak.preferred.verg.velocity,peak.FR,group=neuron),data=sp)+
  stat_smooth(method='lm',se=FALSE,color='black',data=filter(sp,peak.preferred.verg.velocity<0))

#---- 
#Use stat_smooth, but only plot on direction
sp<-left_join(sp,select(spms,neuron,preferred.direction),by='neuron')

ggplot(data=filter(sp,peak.verg.velocity>0),
       aes(peak.verg.velocity,peak.FR,group=neuron))+
  # geom_point()+
  facet_grid(preferred.direction~monkey)+
  stat_smooth(method='lm')+
  xlab('Peak Vergence Velocity (deg)')+
  ylab('Peak Firing Rate (spk/s)')

sp %>%
  group_by(neuron) %>%
  summarize(right=cor(peak.verg.velocity[peak.verg.velocity>0],peak.FR[peak.verg.velocity>0]),
            left=cor(peak.verg.velocity[peak.verg.velocity<0],peak.FR[peak.verg.velocity<0]))->
  spss

#----
#exploration stuff
ggplot(data=filter(sp,peak.verg.velocity>0),aes(peak.verg.velocity,peak.FR,group=neuron))+
  # geom_point()+
  facet_wrap(~monkey)+
  stat_smooth(method='lm')+
  xlab('Peak Vergence Velocity (deg)')+
  ylab('Peak Firing Rate (spk/s)')

ggplot(data=filter(sp,verg.amp>0),aes(verg.amp,peak.FR,group=neuron))+
  # geom_point()+
  facet_wrap(~monkey)+
  stat_smooth(method='lm')+
  xlab('Vergence Amplitude (deg)')+
  ylab('Peak Firing Rate (spk/s)')

ggplot(filter(sp,abs(peak.verg.velocity)<250,peak.FR>10))+
  geom_point(aes(verg.amp,peak.FR))+
  facet_wrap(~neuron)+
  xlab('Vergence Amplitude (deg)')+
  ylab('Peak Firing Rate (spk/s)')

ggplot(filter(sp,neuron %in% democells,abs(peak.verg.velocity)<250,peak.FR>10))+
  geom_point(aes(verg.amp,peak.FR))+
  facet_wrap(~neuron)+
  xlab('Vergence Amplitude (deg)')+
  ylab('Peak Firing Rate (spk/s)')+
  coord_cartesian(xlim=c(-1,6))


t<- loadnewcsv2(path="C:/Users/setup/Desktop/NRTP Vergence/StrabVergence/")

t %>%
  mutate(dsnum=markSaccadesDouble(conj.velocity,threshold1=50,threshold2=20,
                                  driftcorrect = FALSE,markFixations = TRUE),
         # sdf=spikedensity(rasters,10),
         # sdf10=lag(sdf,10),
         conj.vert=(repV+lepV)/2,
         conj.vert.vel=(revV+levV)/2,
         conj.hor=(rep+lep)/2,
         conj.hor.v=(rev+lev)/2) ->
  t

t %>%
  group_by(neuron)%>%
  mutate(time=row_number()) %>%
  filter(dsnum<0) %>% #fixations only
  group_by(neuron,dsnum) %>%
  summarize(sd.conj.velocity=sd(conj.velocity),
            mean.conj.velocity=mean(conj.velocity),
            right.eye.distance=mean(sqrt((rep-thp)^2+(repV-tvp)^2)),
            left.eye.distance =mean(sqrt((lep-thp2)^2+(lepV-tvp2)^2)),
            right.eye.viewing=right.eye.distance<left.eye.distance & right.eye.distance<8,
            left.eye.viewing=left.eye.distance<right.eye.distance & left.eye.distance<8,
            dur=n(),
            starttime=first(time),
            # meanFR=sum(rasters)/dur*1000,
            mean.V=mean(conj.vert),
            mean.H=mean(conj.hor),
            mean.R.H=mean(rep),
            mean.L.H=mean(lep),
            mean.R.V=mean(repV),
            mean.L.V=mean(lepV),
            mean.T.V=mean(tvp),
            mean.T.H=mean((thp+thp2)/2),
            mean.T.verg=mean(thp2-thp),
            mean.E.verg=mean(lep-rep))->
  zp

zp$viewing.eye='off_target'
zp$viewing.eye[zp$right.eye.viewing]='right_eye_viewing'
zp$viewing.eye[zp$left.eye.viewing] ='left_eye_viewing'
zp$viewing.eye<-as.factor(zp$viewing.eye)


zp<- filter(zp,dur>150,
            abs(mean.conj.velocity)<5)

ggplot(zp,aes(mean.T.verg,mean.E.verg,color=mean.T.V))+
  geom_point()+
  stat_smooth(method='lm',se=FALSE,color='black')

ggplot(zp,aes(mean.T.verg,mean.E.verg,color=mean.T.V))+
  geom_point()+
  stat_smooth(method='lm',se=FALSE,color='black')


multiplot(
ggplot(zp,aes(mean.T.H,mean.T.V,color=mean.T.verg))+
  geom_point(size=3)+
  coord_fixed()+
  xlab('Horizontal Target Position')+
  ylab('Vertical Target Position')+
  labs(color='Required Vergence Angle to Converge')+
  theme(legend.position = 'bottom')
,

ggplot(zp,aes(mean.T.H,mean.T.V,color=mean.E.verg))+
  geom_point(size=3)+
  coord_fixed()+
  # scale_color_continuous(low='black',high='orange')+
  xlab('Horizontal Target Position')+
  ylab('Vertical Target Position')+
  labs(color='Strabismus Angle')+
  theme(legend.position = 'bottom')
)


multiplot(
  ggplot(zp,aes(mean.T.V,mean.E.verg,color=viewing.eye))+
    geom_point()+
    stat_smooth(method='lm')+
    xlab('Vertical Target Position')+
    ylab('Mean Strabismus Angle')+
    labs(color='Viewing Eye')+
    theme(legend.position = 'bottom')
  ,
  ggplot(zp,aes(mean.T.H,mean.E.verg,color=viewing.eye))+
    geom_point()+
    stat_smooth(method='lm')+
    xlab('Horizontal Target Position')+
    ylab('Mean Strabismus Angle')+
    labs(color='Viewing Eye')+
    theme(legend.position = 'bottom')
  ,
  ggplot(zp,aes(mean.T.verg,mean.E.verg,color=viewing.eye))+
    geom_point()+
    stat_smooth(method='lm')+
    xlab('Vergence Angle required by Target')+
    ylab('Mean Strabismus Angle')+
    labs(color='Viewing Eye')+
    theme(legend.position = 'bottom')
  
)


plotlist=list(
  ggplot(zp,aes(mean.T.V,mean.E.verg,color=viewing.eye))+
    geom_point()+
    stat_smooth(method='lm')+
    xlab('Vertical Target Position')+
    ylab('Mean Strabismus Angle')
  labs(color='Viewing Eye')
  ,
  ggplot(zp,aes(mean.T.H,mean.E.verg,color=viewing.eye))+
    geom_point()+
    stat_smooth(method='lm')+
    xlab('Horizontal Target Position')+
    ylab('Mean Strabismus Angle')
  labs(color='Viewing Eye')
  ,
  ggplot(zp,aes(mean.T.verg,mean.E.verg,color=viewing.eye))+
    geom_point()+
    stat_smooth(method='lm')+
    xlab('Vergence Angle required by Target')+
    ylab('Mean Strabismus Angle')
  labs(color='Viewing Eye')
)

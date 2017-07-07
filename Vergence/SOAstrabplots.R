ggplot(aes(mean.Verg.Angle,mean.Spikerate,group=neuron),data=filter(sp,cellnum>100,dur>40,monkey=='Kopachuck'))+
  # geom_point(size=2,alpha=1/2)+
  # geom_point(aes(color=mean.time),size=2,alpha=1/2)+
  # scale_color_continuous(low='black',high='orange')+
  # facet_wrap(~neuron,scales='free_x')+
  # facet_wrap(~neuron,ncol=2)+
  stat_smooth(method='lm',se=FALSE,color='black')+
  # geom_label(aes(0,100,label=paste('r = ',round(r,2))))+
  ggtitle('Firing Rate as a function of Vergence Angle during Fixations (Monkey K)')+
  xlab('Mean Vergence Angle (deg)')+
  ylab('Mean Firing Rate (spks/s)')+
  ylim(c(0,150))+
  theme_minimal()


summaryforplot %>%
  separate(neuron, c('monkey','cellnum2'),remove=FALSE) %>%
  filter(monkey=='Pilchuck',
         mean.Verg.Angle<20,
         mean.Verg.Angle>-30) %>%
  group_by(neuron) %>%
  # filter(mean.Verg.Vel<2, 
  #        dur>50,
  #        mean.Verg.Angle<50,
  #        mean.Verg.Angle> -30) %>%
  mutate(r=cor(mean.Verg.Angle,mean.Spikerate)) ->
  spp

ggplot(aes(mean.Verg.Angle,mean.Spikerate,group=neuron),
       data=filter(spp,dur>40))+
  geom_point(size=2,alpha=1/2)+
  # geom_point(aes(color=mean.time),size=2,alpha=1/2)+
  # scale_color_continuous(low='black',high='orange')+
  # facet_wrap(~neuron,scales='free_x')+
  facet_wrap(~neuron,ncol=2)+
  stat_smooth(method='lm',se=FALSE,color='black')+
  geom_label(aes(0,100,label=paste('r = ',round(r,2))))+
  ggtitle('Firing Rate as a function of Vergence Angle during Fixations')+
  xlab('Mean Vergence Angle (deg)')+
  ylab('Mean Firing Rate (spks/s)')+
  theme_minimal()

ggplot(aes(mean.Verg.Angle,mean.Spikerate,group=neuron),
       data=filter(spp,dur>40))+
  # geom_point(size=2,alpha=1/2)+
  # geom_point(aes(color=mean.time),size=2,alpha=1/2)+
  # scale_color_continuous(low='black',high='orange')+
  # facet_wrap(~neuron,scales='free_x')+
  # facet_wrap(~neuron,ncol=2)+
  stat_smooth(method='lm',se=FALSE,color='black')+
  # geom_label(aes(0,100,label=paste('r = ',round(r,2))))+
  ggtitle('Firing Rate as a function of Vergence Angle during Fixations (Monkey P)')+
  xlab('Mean Vergence Angle (deg)')+
  ylab('Mean Firing Rate (spks/s)')+
  ylim(c(0,150))+
  theme_minimal()

ggplot(aes(mean.Verg.Angle,mean.Spikerate,group=neuron),
       data=filter(sp,dur>40,monkey=='Kopachuck'))+
  geom_point(size=2,alpha=1/2)+
  # geom_point(aes(color=mean.time),size=2,alpha=1/2)+
  # scale_color_continuous(low='black',high='orange')+
  # facet_wrap(~neuron,scales='free_x')+
  facet_wrap(~neuron,ncol=2)+
  stat_smooth(method='lm',se=FALSE,color='black')+
  geom_label(aes(0,100,label=paste('r = ',round(r,2))))+
  ggtitle('Firing Rate as a function of Vergence Angle during Fixations')+
  xlab('Mean Vergence Angle (deg)')+
  ylab('Mean Firing Rate (spks/s)')+
  theme_minimal()

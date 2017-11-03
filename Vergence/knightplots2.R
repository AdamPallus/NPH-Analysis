ggplot(hpp,aes(abs(gaze.amp),abs(peak.gaze.velocity),color=block))+
  geom_point()+
  stat_smooth(method='lm')+
  xlab('Gaze shift amplitude (deg)')+
  ylab('Peak gaze shift speed (deg/s)')
# b. comparison of pre and post mean Gaze latency
ggplot(hpp,aes(abs(gaze.amp),gaze.onset/samplerate,color=block))+
  geom_point()+
  stat_smooth(method='lm')+
  ylab('Gaze shift latency (ms)')+
  xlab('Gaze shift amplitude (deg)')

# c. comparison of pre and post mean primary Gaze gain
ggplot(hpp,aes(abs(target.amp),abs(gaze.amp),color=block))+
  geom_point()+
  geom_abline()+
  stat_smooth(method='lm')+
  xlab('target amplitude (deg)')+
  ylab('Gaze shift amplitude (deg)')+
  annotate('text',30,80,label='First saccade only; both directions')
# 
# d. comparison of pre and post mean Head peak velocity
ggplot(hpp,aes(abs(gaze.amp),abs(peak.head.velocity),color=block))+
  geom_point()+
  stat_smooth(method='lm')+
  xlab('Gaze shift amplitude (deg)')+
  ylab('Peak head speed (deg/s)')
# e.comparison of pre and post mean Head latency

# f. comparison of pre and post mean head contribution to gaze\
ggplot(hpp,aes(abs(gaze.amp),abs(head.contribution),color=block))+
  geom_point()+
  stat_smooth(method='lm')+
  xlab('Gaze shift amplitude (deg)')+
  ylab('Head Contribution (deg)')

ggplot(filter(hpp,abs(head.contribution)<10),
       aes(abs(gaze.amp),abs(head.contribution),color=IEPs))+
  geom_point(size=3)+
  stat_smooth(method='lm')+
  xlab('Gaze shift amplitude (deg)')+
  ylab('Head Contribution (deg)')+
  ggtitle('Outliers removed')+
  facet_wrap(~block)+
  scale_color_continuous(low='black',high='orange')



ggplot(filter(hpp,!is.na(amp.bins)))+
  geom_boxplot(aes(amp.bins,gaze.onset/samplerate,color=block))+
  ylab('Gaze Shift Latency (ms)')+
  xlab('Gaze shift amplitude bins (deg)')

ggplot(filter(hpp,!is.na(amp.bins)))+
  geom_boxplot(aes(amp.bins,head.contribution,color=block))+
  ylab('Head Contribution (deg)')+
  xlab('Gaze shift amplitude bins (deg)')

ggplot(filter(hpp,!is.na(amp.bins),abs(head.contribution)<10))+
  geom_boxplot(aes(amp.bins,head.contribution,color=block))+
  ylab('Head Contribution (deg)')+
  xlab('Gaze shift amplitude bins (deg)')

ggplot(filter(hpp,!is.na(amp.bins.right)))+
  geom_boxplot(aes(amp.bins.right,gaze.onset/samplerate,color=block))+
  ylab('Gaze Shift Latency (ms)')+
  xlab('Gaze shift amplitude bins (deg)')

ggplot(filter(hpp,!is.na(amp.bins.right)))+
  geom_boxplot(aes(amp.bins.right,head.contribution,color=block))+
  ylab('Head Contribution (deg)')+
  xlab('Gaze shift amplitude bins (deg)')

ggplot(filter(hpp,!is.na(amp.bins.left)))+
  geom_boxplot(aes(amp.bins.left,gaze.onset/samplerate,color=block))+
  ylab('Gaze Shift Latency (ms)')+
  xlab('Gaze shift amplitude bins (deg)')

ggplot(filter(hpp,!is.na(amp.bins.left)))+
  geom_boxplot(aes(amp.bins.left,head.contribution,color=block))+
  ylab('Head Contribution (deg)')+
  xlab('Gaze shift amplitude bins (deg)')

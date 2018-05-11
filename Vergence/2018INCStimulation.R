#setup----
library(ggplot2)
library(dplyr)
library(knitr)
library(tidyr)
library(broom)
# library(grid)
library(relaimpo)
library(leaps)
library(stringr)
library(cladoRcpp)
library(boot)
library(ggpubr)
source('Adamhelperfunctions.R')
source('fixationanalysisfunctions.R')
select<- dplyr::select

# sp<-readRDS('DCstim-5-10-2018.RDS')
sp<-readRDS('KopachuckStim-5-10-2018.RDS')

s<- loadnewcsv2(path="C:/Users/setup/Desktop/NRTP Vergence/INCstim/DC/")
# s<- loadnewcsv2(path="C:/Users/setup/Desktop/NRTP Vergence/INCstim/HAMMING/")
s<- loadnewcsv2(path='C:/Users/setup/Desktop/NRTP Vergence/INCstim/')
# s<- filter(t,neuron %in% c('Kopachuck-501','Kopachuck-502'))
bufferlength=200
s %>%
  group_by(neuron) %>%
  mutate(time=row_number(),
         sdf=spikedensity(rasters,sd=10),
         stimes=markSaccades(sdf,buffer=bufferlength,threshold=120),
         saccades=markSaccadesDouble(conj.velocity,threshold1=200,threshold2=20))->
  sx


# s<- mutate(s,time=row_number())
# s$stimes<- markSaccades(s$sdf,buffer=100,threshold=120)
sx%>%
  filter(stimes>0) %>%
  group_by(neuron,stimes) %>%
  mutate(stim.time=min(time[rasters==1]), #time of first stim pulse
         counter=time-first(stim.time))%>% #count from time of first stim pulse
  summarize(stim.dur=(n()-(2*bufferlength)), #duration of stim (just remove buffer)
            stim.dur=round(stim.dur/50)*50, #We use round numbers 
            stim.time=first(stim.time))%>% 
  group_by(neuron)%>%
  summarize(stim.dur=first(stim.dur), #neuron here indicates different data files
            stim.time=first(stim.time))->
  stimulation.durations


sx%>% #make a table of stimulation durations for each train (there could be multiple durs in one file)
  filter(stimes>0) %>%
  group_by(neuron,stimes) %>%
  summarize(stim.dur=n())->
  sdurs

sx %>% #now go back to the original data with stimulation trains marked
  group_by(neuron) %>%
  filter(stimes>0) %>% #get rid of non-stim portions of the data
  mutate(showstim=replace(rasters,rasters<1,NA))%>% #for plotting
  group_by(neuron,stimes) %>% #for each stim train
  mutate(stim.dur=n()-(2*bufferlength), #automatically figure out stim duration
         stim.dur=round(stim.dur/50)*50, #round to nearest 50
         counter=time-min(time[rasters==1]), #start at first detected stim pulse
         repDIFF=rep-rep[counter==0], #align based on time of first pulse
         lepDIFF=lep-lep[counter==0],
         repVDIFF=repV-repV[counter==0],
         lepVDIFF=lepV-lepV[counter==0],
         stim=counter>0&counter<(stim.dur),
         interrupted=1 %in% sign(saccades[stim]),
         blink = max(abs(conj.velocity))>1000)->
  sp

#Plot----

ggplot(sp %>% filter(!blink,!interrupted,stim.dur==100),aes(group=stimes))+
  geom_line(aes(counter,repDIFF+10),color='red')+
  geom_line(aes(counter,lepDIFF+10),color='blue')+
  geom_line(aes(counter,repVDIFF-5),color='red',linetype=2)+
  geom_line(aes(counter,lepVDIFF-5),color='blue',linetype=2)+
  geom_segment(y=-2.5,yend=-2.5,x=0,xend=100,size=3,color='hotpink',alpha=0.2)+
  ylim(-20,20)+
  theme_minimal()+
  annotate('text',0,2,label='Horizontal')+
  annotate('text',0,-4,label='Vertical')+
  facet_wrap(~neuron)

sp %>%
  filter(!blink,!interrupted) %>%
  group_by(neuron,counter) %>%
  summarize_at(c('repDIFF','lepDIFF','repVDIFF','lepVDIFF','stim.dur'),mean)->
  sps


ggplot(sps)+
  geom_line(aes(counter,repDIFF+10),color='red')+
  geom_line(aes(counter,lepDIFF+10),color='blue')+
  geom_line(aes(counter,repVDIFF-5),color='red',linetype=2)+
  geom_line(aes(counter,lepVDIFF-5),color='blue',linetype=2)+
  geom_segment(y=-20,yend=-20,x=0,aes(xend=stim.dur),size=3,color='hotpink',alpha=0.2)+
  ylim(-20,20)+
  theme_minimal()+
  annotate('text',0,2,label='Horizontal')+
  annotate('text',0,-4,label='Vertical')+
  facet_wrap(~neuron)


poststim=25
sps %>%
  group_by(neuron) %>%
  mutate(stim.dur=round(stim.dur/50)*50) %>%
  summarize(mean_repDiff=repDIFF[counter==stim.dur+poststim],
            mean_repVDiff=repVDIFF[counter==stim.dur+poststim],
            mean_lepDiff=lepDIFF[counter==stim.dur+poststim],
            mean_lepVDiff=lepVDIFF[counter==stim.dur+poststim],
            stim.dur=first(stim.dur))->
  spss

multiplot(
  qplot(mean_repVDiff,mean_lepVDiff,data=spss)+geom_abline(),
  qplot(mean_repDiff,mean_lepDiff,data=spss)+
    geom_abline()
)


spss %>%
  group_by(neuron) %>%
  mutate(Rdir=atan2(mean_repVDiff,mean_repDiff),
         Ldir=atan2(mean_lepVDiff,mean_lepDiff),
         Ramp=sqrt(mean_repVDiff^2+mean_repDiff^2),
         Lamp=sqrt(mean_lepVDiff^2+mean_lepDiff^2))->
  spss


multiplot(
ggplot(spss)+
  geom_segment(aes(y=0,yend=1,x=Rdir*180/pi,xend=Rdir*180/pi))+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  theme_minimal()+
  theme(legend.position='none')+
  xlab('Direction preference')+
  ylab('Goodness of Fit'),
ggplot(spss)+
  geom_segment(aes(y=0,yend=1,x=Ldir*180/pi,xend=Ldir*180/pi))+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  theme_minimal()+
  theme(legend.position='none')+
  xlab('Direction preference')+
  ylab('Goodness of Fit')
)


multiplot(
  ggplot(spss %>% filter(stim.dur==100))+
    geom_segment(aes(y=0,yend=Lamp,x=Ldir*180/pi,xend=Ldir*180/pi),color='blue')+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    xlab('Left Eye Direction Preference')+
    ylab('Left Eye Mean Evoked Amplitude'),
  ggplot(spss %>% filter(stim.dur==100))+
    geom_segment(aes(y=0,yend=Ramp,x=Rdir*180/pi,xend=Rdir*180/pi),color='red')+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    xlab('Right Eye Direction preference')+
    ylab('Right Eye Mean Evoked Amplitude'),

  cols=2
)


multiplot(
  ggplot(spss)+
    geom_segment(aes(y=0,yend=Lamp,x=Ldir*180/pi,xend=Ldir*180/pi),color='blue')+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    xlab('Left Eye Direction Preference')+
    ylab('Left Eye Mean Evoked Amplitude'),
  ggplot(spss)+
    geom_segment(aes(y=0,yend=Ramp,x=Rdir*180/pi,xend=Rdir*180/pi),color='red')+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    xlab('Right Eye Direction preference')+
    ylab('Right Eye Mean Evoked Amplitude'),
  
  cols=2
)

#----

#'Here we are summarizing each stimulation. In above calculations, we calculated the mean first.
#'This will let us look at statistics and the effect of IEP

sp %>%
  group_by(neuron,stimes) %>%
  filter(!interrupted,!blink) %>%
  summarize(IEPRV=repV[counter==1],
            IEPRH=rep[counter==1],
            IEPLV=lepV[counter==1],
            IEPLH=lep[counter==1],
            RHamp=rep[counter==stim.dur]-IEPRH,
            RVamp=repV[counter==stim.dur]-IEPRV,
            LHamp=lep[counter==stim.dur]-IEPLH,
            LVamp=lepV[counter==stim.dur]-IEPLV)->
  spsummary

#'This plot shows the relationship between initial eye position 
#'and the amplitude of the evoked movement.
#'It looks like there is a relationship such that the eyes more when they start in the opposite
#'direction of the evoked movement. This makes sense to me as we are stimultaing an integrator which
#'means we would be adding some position signal, but this would eventually max out. We might 
#'expect to see this as a logarithmic function

multiplot(
spsummary %>%
  ggplot(aes(IEPLV,LVamp,group=neuron))+
  stat_smooth(method='lm',se=FALSE,color='black')+
  xlab('Initial Vertical Position of Left Eye')+
  ylab('Left Eye vertical evoked amplitude'),
spsummary %>%
  ggplot(aes(IEPRV,RVamp,group=neuron))+
  stat_smooth(method='lm',se=FALSE,color='black')+
  xlab('Initial Vertical Position of Right Eye')+
  ylab('Right Eye vertical evoked amplitude')
)


multiplot(
  spsummary %>%
    ggplot(aes(IEPLV,LVamp,group=neuron))+
    stat_smooth(method='lm',se=FALSE,color='blue')+
    xlab('Initial Vertical Position of Left Eye')+
    ylab('Left Eye vertical evoked amplitude'),
  spsummary %>%
    ggplot(aes(IEPRV,RVamp,group=neuron))+
    stat_smooth(method='lm',se=FALSE,color='red')+
    xlab('Initial Vertical Position of Right Eye')+
    ylab('Right Eye vertical evoked amplitude'),
  spsummary %>% filter(LHamp<6) %>%
    ggplot(aes(IEPLH,LHamp,group=neuron))+
    stat_smooth(method='lm',se=FALSE,color='blue')+
    xlab('Initial Horizontal Position of Left Eye')+
    ylab('Left Eye Horizontal evoked amplitude'),
  spsummary %>% filter(RHamp<10) %>%
    ggplot(aes(IEPRH,RHamp,group=neuron))+
    stat_smooth(method='lm',se=FALSE,color='red')+
    xlab('Initial Horizontal Position of Right Eye')+
    ylab('Right Eye Horizontal evoked amplitude'),
  spsummary %>% filter(LHamp<6) %>%
    ggplot(aes(IEPLV,LHamp,group=neuron))+
    stat_smooth(method='lm',se=FALSE,color='blue')+
    xlab('Initial Vertical Position of Left Eye')+
    ylab('Left Eye Horizontal evoked amplitude'),
  spsummary %>% filter(RHamp<10) %>%
    ggplot(aes(IEPRV,RHamp,group=neuron))+
    stat_smooth(method='lm',se=FALSE,color='red')+
    xlab('Initial Vertical Position of Right Eye')+
    ylab('Right Eye Horizontal evoked amplitude'),
  cols=3
)



spsummary %>%
  # filter(abs(RVamp/LVamp)<5)%>%
  ggplot(aes(neuron,RVamp/LVamp))+
  geom_boxplot()+
  ylim(-5,5)+
  geom_hline(yintercept = 1,color='hotpink')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ggtitle('Comparing Left and Right Eye Evoked Vertical Amplitude')+
  xlab('Stim location')

spsummary %>%
  ggplot(aes(neuron,RHamp/RVamp))+
  geom_boxplot()+
  ylim(-10,10)+
  geom_hline(yintercept = 1,color='hotpink')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ggtitle('Comparing Horizontal and Vertical components of right eye evoked movements')+
  xlab('Stim location')

spsummary %>%
  ggplot(aes(neuron,LHamp/LVamp))+
  geom_boxplot()+
  ylim(-10,10)+
  geom_hline(yintercept = 1,color='hotpink')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ggtitle('Comparing Horizontal and Vertical components of left eye evoked movements')+
  xlab('Stim location')

spsummary %>%
  select(-starts_with('IEP')) %>%
  gather(key=EYEHV,value=amplitude,-neuron,-stimes) %>%
  ggplot(aes(EYEHV,amplitude))+
  geom_boxplot()+
  facet_wrap(~neuron,scales='free_y')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

library(ggpubr)

spsummary %>%
  select(-starts_with('IEP')) %>%
  gather(key=EYEHV,value=amplitude,-neuron,-stimes) %>%
  ggboxplot(x = "EYEHV", y='amplitude',
            color = "EYEHV", palette = "npg",
            facet.by = "neuron",
            scales='free_y',
            short.panel.labs = TRUE) + 
  # stat_compare_means()+
  stat_compare_means(comparisons = list(c(1,3),c(2,4)),
                     label='p.signif')+
  theme(legend.position = 'bottom')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  geom_hline(yintercept = 0,alpha=0.2,linetype=2)

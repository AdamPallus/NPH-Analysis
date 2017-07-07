#setup----
# s<- loadnewcsv2(path="C:/Users/setup/Desktop/NRTP Vergence/INCstim/")
s<- loadnewcsv2(path="C:/Users/setup/Desktop/NRTP Vergence/INCstim/HAMMING/")
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
  mutate(stim.time=min(time[rasters==1]),
         counter=time-first(stim.time))%>%
  summarize(stim.dur=(n()-(2*bufferlength)),
            stim.dur=round(stim.dur/50)*50,
            stim.time=first(stim.time))%>%
  group_by(neuron)%>%
  summarize(stim.dur=first(stim.dur),
            stim.time=first(stim.time))->
  stimulation.durations


sx%>%
  filter(stimes>0) %>%
  group_by(neuron,stimes) %>%
  summarize(stim.dur=n())->
  sdurs

sx %>%
  group_by(neuron) %>%
  filter(stimes>0) %>%
  mutate(showstim=replace(rasters,rasters<1,NA))%>%
  group_by(neuron,stimes) %>%
  mutate(stim.dur=n()-(2*bufferlength), #automatically figure out stim duration
         stim.dur=round(stim.dur/50)*50, #round to nearest 50
         counter=time-min(time[rasters==1]), #start at first detected stim pulse
         repDIFF=rep-rep[counter==0],
         lepDIFF=lep-lep[counter==0],
         repVDIFF=repV-repV[counter==0],
         lepVDIFF=lepV-lepV[counter==0],
        stim=counter>0&counter<(stim.dur),
        interrupted=1 %in% sign(saccades[stim]))->
  sp



#Make a plot----

d<- filter(sp,neuron=='Kopachuck-501')

stimlength=d$stim.dur[1]
stimlength=ceiling(d$stim.dur[1]/10)*10
maxplot=bufferlength+stimlength


d<- mutate(d,peak.conj.velocity=
             max(conj.velocity[bufferlength:bufferlength+stimlength],
                                    na.rm=TRUE),
           IEP.H=(first(rep)+first(lep))/2,
           IEP.V=(first(repV)+first(lepV)/2))

ggplot(d,aes(group=stimes))+
  geom_line(aes(counter,repDIFF),color='red')+
  geom_line(aes(counter,lepDIFF),color='blue')+
  geom_line(aes(counter,repVDIFF-20),color='red',linetype=2)+
  geom_line(aes(counter,lepVDIFF-20),color='blue',linetype=2)

ggplot(filter(d,!interrupted,counter>-50,counter<150), #ON POSTER
       aes(group=stimes))+
  geom_line(aes(counter,repDIFF),color='red',size=2)+
  geom_line(aes(counter,lepDIFF),color='blue',size=2)+
  # geom_line(aes(counter,repVDIFF-20),color='red',linetype=1)+
  # geom_line(aes(counter,lepVDIFF-20),color='blue',linetype=1)+
  theme_minimal()+
  ylab('Change in Horizontal Eye Position')+
  xlab('Time (ms)')

d %>%
  group_by(stimes) %>%
  mutate(revDIFF=parabolicdiff(repDIFF,7),
         levDIFF=parabolicdiff(lepDIFF,7),
         revVDIFF=parabolicdiff(repVDIFF,7),
         levVDIFF=parabolicdiff(lepVDIFF,7)) %>%
  ungroup() %>%
  mutate(meanFIXH=mean(revDIFF[1:50]),
         meanFIXV=mean(revVDIFF[1:50]))->
  dlatency

dlatency %>%
  group_by(stimes)%>%
  filter(max(abs(revDIFF[150:200]))<100,
         max(abs(levDIFF[150:200]))<100,
         max(abs(revVDIFF[150:200]))<100,
         max(abs(levVDIFF[150:200]))<100)->
  dlatencytest
           

ggplot(filter(dlatencytest,!interrupted,counter>-50,counter<150),
       aes(group=stimes))+
  geom_line(aes(counter,revDIFF),color='red')+
  geom_line(aes(counter,levDIFF),color='blue')+
  geom_line(aes(counter,revVDIFF-200),color='red',linetype=1)+
  geom_line(aes(counter,levVDIFF-200),color='blue',linetype=1)+
  theme_minimal()
           
        
                               
sp %>%
  group_by(neuron,stimes) %>%
  mutate(revDIFF=parabolicdiff(repDIFF,7),
         levDIFF=parabolicdiff(lepDIFF,7),
         revVDIFF=parabolicdiff(repVDIFF,7),
         levVDIFF=parabolicdiff(lepVDIFF,7)) %>%
  ungroup() %>%
  mutate(meanFIXH=mean(revDIFF[1:50]),
         meanFIXV=mean(revVDIFF[1:50]))%>%
  filter(max(abs(revDIFF[150:200]))<100,
         max(abs(levDIFF[150:200]))<100,
         max(abs(revVDIFF[150:200]))<100,
         max(abs(levVDIFF[150:200]))<100)->
  dlatency


ggplot(filter(dlatency,!interrupted,counter>-50,counter<stim.dur+100),
       aes(group=stimes))+
  geom_line(aes(counter,revDIFF),color='red')+
  geom_line(aes(counter,levDIFF),color='blue')+
  geom_line(aes(counter,revVDIFF-200),color='red',linetype=1)+
  geom_line(aes(counter,levVDIFF-200),color='blue',linetype=1)+
  theme_minimal()+
  facet_wrap(~neuron,scales='free',ncol=4)






ggplot(d,aes(group=stimes))+
  geom_line(aes(counter,rev),color='red')+
  geom_line(aes(counter,lev),color='blue')+
  geom_line(aes(counter,revV-20),color='red',linetype=2)+
  geom_line(aes(counter,levV-20),color='blue',linetype=2)


ggplot(filter(d,peak.conj.velocity<bufferlength-20,counter<620,counter>100),aes(group=stimes))+
  geom_line(aes(counter,repDIFF),color='red')+
  geom_line(aes(counter,lepDIFF),color='blue')+
  geom_line(aes(counter,repVDIFF-20),color='darkred',linetype=1)+
  geom_line(aes(counter,lepVDIFF-20),color='darkblue',linetype=1)

ggplot(filter(d,peak.conj.velocity<190,counter<620,counter>100),aes(group=stimes))+
  geom_line(aes(counter,rev),color='red')+
  geom_line(aes(counter,lev),color='blue')+
  # geom_line(aes(counter,revV-20),color='darkred',linetype=1)+
  # geom_line(aes(counter,levV-20),color='darkblue',linetype=1)+
  ylim(-100,100)

d %>%
  group_by(counter) %>%
  summarize_each(funs(mean))->
  dmean

dp<- filter(d,peak.conj.velocity<190,counter<maxplot+20,counter>bufferlength-100)


ggplot(filter(dmean,peak.conj.velocity<190,counter<maxplot+50,counter>bufferlength-100),
       aes(group=stimes))+
  geom_line(aes(counter,repDIFF),color='red')+
  geom_line(aes(counter,lepDIFF),color='blue')+
  geom_line(aes(counter,repVDIFF),color='darkred')+
  geom_line(aes(counter,lepVDIFF),color='darkblue')


ggplot(filter(dmean,peak.conj.velocity<190,counter<maxplot+20,counter>bufferlength-100),
       aes(group=stimes))+
  geom_point(aes(repDIFF,repVDIFF),color='red')+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue')+
  geom_point(aes(repDIFF,repVDIFF,group=stimes),color='red',data=dp,alpha=1/30,size=1/2)+
  geom_point(aes(lepDIFF,lepVDIFF,group=stimes),color='blue',data=dp,alpha=1/30,size=1/2)+
  theme_minimal()

ggplot(filter(dmean,peak.conj.velocity<190,counter<maxplot+100,counter>bufferlength-100),
       aes(group=stimes))+
  geom_line(aes(counter,lepDIFF-repDIFF),color='darkgreen')+
  geom_line(aes(counter,lepDIFF-repDIFF),color='darkgreen',data=dp,alpha=1/20)


dmp<- filter(dmean,peak.conj.velocity<190)
dmpS<- filter(dmp,counter<maxplot,counter>bufferlength)
ggplot(dmp,aes(group=stimes))+
  geom_point(aes(repDIFF,repVDIFF),color='red',alpha=1/30)+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue',alpha=1/30)+
  geom_point(aes(repDIFF,repVDIFF),color='red',data=dmpS)+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue',data=dmpS)+
  theme_minimal()

#batchprocess----
# sp %>%
#   group_by(neuron,stimes)%>%
#   mutate(IEP.H=(first(rep)+first(lep))/2,
#          IEP.V=(first(repV)+first(lepV)/2),
#          stim=counter>0&counter<(stim.dur)) %>%
#   filter(conj.velocity<190)->
#   dall
  
sp %>%
  filter(!interrupted)%>%
  # filter(stimes<20) %>%
  group_by(neuron,counter) %>%
  summarize_each(funs(mean))->
  dallmean

sp %>%
  filter(!interrupted) ->
  dallraw
  
  
  
dallmean%>%
  group_by(neuron,stimes)%>%
  mutate(stim=counter>0&counter<stim.dur)->
  dallmean

#Histogram Maker:
sp %>%
  filter(!interrupted) %>% #remove trials w/saccades
  group_by(neuron,stimes) %>%
  summarize(R.amp=sqrt(repDIFF[counter==stim.dur]^2+repVDIFF[counter==stim.dur]^2),
            L.amp=sqrt(lepDIFF[counter==stim.dur]^2+lepVDIFF[counter==stim.dur]^2),
            RLratio=R.amp/L.amp,
            stim.dur=first(stim.dur))->
  ampHisto
qplot(RLratio,data=ampHisto,bins=10)+facet_wrap(~neuron,scales='free')

ampHisto %>%
  group_by(neuron) %>%
  summarize(RLratiomean=mean(RLratio),
            RLratioLow=t.test(RLratio)$conf.int[1],
            RLratioHigh=t.test(RLratio)$conf.int[2],
            nstims=n(),
            stim.dur=first(stim.dur))%>%
  arrange(nstims)->
  ampMeans

qplot(RLratiomean,data=ampMeans,binwidth=0.1)+
  geom_vline(xintercept = mean(ampMeans$RLratiomean))

ggplot(ampMeans,aes(nstims,RLratiomean))+
geom_point()+
  # geom_hline(yintercept = mean(ampMeans$RLratiomean))+
  theme_minimal()+
  geom_errorbar(aes(ymin=RLratioLow,ymax=RLratioHigh))

ggplot(ampMeans,
       aes(nstims,RLratiomean,color=neuron))+
  geom_point()+
  geom_hline(yintercept = mean(ampMeans$RLratiomean))+
  theme_minimal()+
  geom_errorbar(aes(ymin=RLratioLow,ymax=RLratioHigh))+
  xlab('Number of Stimulation-Evoked Movements')+
  ylab('Ratio of Right and Left Eye Evoked Movement Amplitude')+
  theme(legend.position="none")
  

mean(ampMeans$RLratio)

# 
# dallp<- filter(dall,peak.conj.velocity<190,counter<bufferlength+first(stim.dur)+20,
#                counter>bufferlength-100)


ggplot(filter(dallmean,neuron %in% c('Kopachuck-5101','Kopachuck-5102')))+
  geom_point(aes(repDIFF,repVDIFF),color='red')+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue')+
  geom_point(aes((lepDIFF+repDIFF)/2,(lepVDIFF+repVDIFF)/2))+
  facet_wrap(~neuron,ncol=4,scales='free')


###############
####PICK ONE OF THESE
##############
StimSites=unique(dallmean$neuron)
currentSite='Kopachuck-501'

manipulate({
  d=filter(dallmean,neuron == StimSites[currentSite])
  ggplot(d)+
    geom_point(aes(repDIFF,repVDIFF),color='darkred',alpha=1/20)+
    geom_point(aes(lepDIFF,lepVDIFF),color='darkblue',alpha=1/20)+
    geom_point(aes((lepDIFF+repDIFF)/2,(lepVDIFF+repVDIFF)/2),alpha=1/20,color='gray50')+
    
    geom_point(aes(repDIFF,repVDIFF),color='red',data=filter(d,stim))+
    geom_point(aes(lepDIFF,lepVDIFF),color='blue',data=filter(d,stim))+
    geom_point(aes((lepDIFF+repDIFF)/2,(lepVDIFF+repVDIFF)/2),data=filter(d,stim),
               color='black')+
    annotate("text",x=1,y=1,label=d$stim.dur[1])+
    ggtitle(StimSites[currentSite])+
    theme_minimal()+
    xlab("Mean Change in Horizontal Position (deg)")+
    ylab('Mean Change in Vertical Position (deg)')+
    coord_fixed()
  
},
currentSite=slider(1,length(StimSites)))

#ON POSTER#############

PlotSites=paste('Kopachuck',c(502,504,506,507,5092,5101),sep='-')
d=filter(dallmean,neuron %in% PlotSites,counter> -50)
d<- dallmean
ggplot(d,aes(label=stim.dur))+ #This goes on the poster
  geom_point(aes(repDIFF,repVDIFF),color='darkred',alpha=1/20)+
  geom_point(aes(lepDIFF,lepVDIFF),color='darkblue',alpha=1/20)+
  geom_point(aes((lepDIFF+repDIFF)/2,(lepVDIFF+repVDIFF)/2),alpha=1/20,color='gray50')+
  
  geom_point(aes(repDIFF,repVDIFF),color='red',data=filter(d,stim))+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue',data=filter(d,stim))+
  geom_point(aes((lepDIFF+repDIFF)/2,(lepVDIFF+repVDIFF)/2),data=filter(d,stim),
             color='black')+
  # geom_label(x=1,y=1)+
  theme_minimal()+
  geom_point(x=0,y=0,color='yellow',size=3)+
  xlab("Mean Change in Horizontal Position (deg)")+
  ylab('Mean Change in Vertical Position (deg)')+
  facet_wrap(~neuron,ncol=3,scales='free')+
  theme(aspect.ratio = 1)

ggplot(d,aes(label=stim.dur))+ #This goes on the poster
  geom_point(aes(repDIFF,repVDIFF),color='darkred',alpha=1/20)+
  geom_point(aes(lepDIFF,lepVDIFF),color='darkblue',alpha=1/20)+
  geom_point(aes((lepDIFF+repDIFF)/2,(lepVDIFF+repVDIFF)/2),alpha=1/20,color='gray50')+
  
  geom_point(aes(repDIFF,repVDIFF),color='red',data=filter(d,stim))+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue',data=filter(d,stim))+
  geom_point(aes((lepDIFF+repDIFF)/2,(lepVDIFF+repVDIFF)/2),data=filter(d,stim),
             color='black')+
  # geom_label(x=1,y=1)+
  theme_minimal()+
  geom_point(x=0,y=0,color='yellow',size=3)+
  xlab("Mean Change in Horizontal Position (deg)")+
  ylab('Mean Change in Vertical Position (deg)')+
  facet_wrap(~neuron,ncol=3,scales='free')+
  theme(aspect.ratio = 1)

#saved 7x10in



#doodles####
ggplot(filter(dall,stim,neuron %in% c('Kopachuck-5111','Kopachuck-5112')),aes(group=stimes))+
  geom_line(aes(repDIFF,repVDIFF),color='red')+
  geom_line(aes(lepDIFF,lepVDIFF),color='blue')+
  geom_line(aes((lepDIFF+repDIFF)/2,(lepVDIFF+repVDIFF)/2))+
  facet_wrap(~neuron,ncol=4,scales='free')


chosenSites=c('Kopachuck-5102')

dallmeanstim=filter(dallmean,stim,neuron %in% chosenSites)
dallstim=filter(dall,stim,neuron %in% chosenSites)
dallplot=filter(dall,neuron %in% chosenSites)

ggplot(dallplot)+
  geom_line(aes(counter,repDIFF,group=stimes),color='red')+
  geom_line(aes(counter,lepDIFF,group=stimes),color='blue')
  # geom_line(aes(repDIFF,repVDIFF,group=stimes),color='red')+
  # geom_line(aes(lepDIFF,lepVDIFF,group=stimes),color='blue')+
  # geom_line(aes(counter,(lepDIFF+repDIFF)/2,group=stimes))+
  # geom_line(aes(counter,(lepVDIFF+repVDIFF)/2,group=stimes),linetype=2)+
  # geom_line(aes(counter,(lep+rep)/2,group=stimes))+
  # geom_line(aes(counter,(lepV+repV)/2,group=stimes),linetype=2)+
  # geom_point(aes(counter,showstim))+
  # geom_line(aes(counter,(lepDIFF+repDIFF)/2),
  #           data=dallmeanstim,size=2,color='purple')+
  # geom_line(aes(counter,(lepVDIFF+repVDIFF)/2),color='hotpink',
  #           data=dallmeanstim,size=2)+
  # geom_point(aes(rep,repV),color='red')+
  # geom_point(aes(lep,lepV),color='blue')+
  facet_wrap(~stimes,ncol=4,scales='free')



ggplot(ddd)+
  geom_line(aes(counter,repDIFF),color='red')


ggplot(filter(dallmean,peak.conj.velocity<190,counter<bufferlength+first(stim.dur)+20,
              counter>bufferlength-100),
       aes(group=stimes))+
  geom_point(aes(repDIFF,repVDIFF),color='red')+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue')+
  geom_point(aes(repDIFF,repVDIFF,group=stimes),color='red',data=dallp,alpha=1/30,size=1/2)+
  geom_point(aes(lepDIFF,lepVDIFF,group=stimes),color='blue',data=dallp,alpha=1/30,size=1/2)+
  theme_minimal()+
  facet_wrap(~neuron,scales='free')


dmp<- filter(dmean,peak.conj.velocity<190)
dmpS<- filter(dmp,counter<maxplot,counter>bufferlength)
ggplot(dmp,aes(group=stimes))+
  geom_point(aes(repDIFF,repVDIFF),color='red',alpha=1/30)+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue',alpha=1/30)+
  geom_point(aes(repDIFF,repVDIFF),color='red',data=dmpS)+
  geom_point(aes(lepDIFF,lepVDIFF),color='blue',data=dmpS)+
  theme_minimal()

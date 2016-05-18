#In this script I will be trying to model the activity of these neurons. 
#Specifically, I will look at the vergence velocity related activity

#Plan: 1) calcualte verg.angle related firing rate (FR) during fixation
#      2) subtract this from observed FR during movements
#      3) Is there a different relationship between verg.velocity and FR when there is a saccade?


t<-readRDS('SOA-NRTP.RDS')

tt<- filter(t,cellnum %in% c(4, 15, 34, 35), monkey=='Bee')

tt %>%
  group_by(neuron) %>%
  mutate(s=markSaccades(conj.velocity,buffer=10,threshold=10),
         isfixation=s<0) %>%
  filter(isfixation) %>%
  group_by(neuron,s) %>%
  mutate(meanfr=mean(sdf),
         maxfr=max(sdf),
         R.Hor=mean(rep),
         R.Ver=mean(repV),
         L.Hor=mean(lep),
         L.Ver=mean(lepV),
         mean.Verg.Angle=mean(verg.angle),
         mean.Verg.Angle=replace(mean.Verg.Angle, mean.Verg.Angle<0, NA),
         max.Verg.Vel = max(verg.velocity),
         max.Verg.Ang = max(verg.angle),
         nspikes=sum(rasters),
         dur=n(),
         mean.Spikerate=sum(rasters)/dur*1000,
         R.H.Amp=rep[1]-rep[length(rep)],
         L.H.Amp=lep[1]-lep[length(lep)],
         R.V.Amp=repV[1]-repV[length(repV)],
         L.V.Amp=lepV[1]-lepV[length(lepV)],
         maxamp=max(abs(R.H.Amp),abs(R.V.Amp),abs(L.H.Amp),abs(L.V.Amp)))->
  m


m %>%
  filter(dur>200) %>%
  summarize(mean.Spikerate=mean.Spikerate[1],
            verg.angle=mean.Verg.Angle[1],
            dur=dur[1]) ->
  summaryforplot

summaryforplot %>%
  group_by(neuron) %>%
  do(m=lm(mean.Spikerate ~ verg.angle,data=.)) ->
  mm

x<- filter(tt,neuron=='Bee-04')
m<- mm$m[[1]]
x$expectedFR<- predict(m,newdata=x)
x<- mutate(x,adjustedFR=sdf-expectedFR)
x$adjustedFR<- replace(x$adjustedFR,x$adjustedFR<0,0)
ggplot(filter(x,time<5000))+geom_line(aes(time,adjustedFR))+geom_line(aes(time,verg.velocity),color='pink')

maxtime<-nrow(x)

manipulate(ggplot(filter(x,time>window,time<window+5000))+
             geom_line(aes(time,adjustedFR))+
             geom_line(aes(time,verg.velocity),color='pink')+
             geom_line(aes(time,rep*10),color='red')+
             geom_line(aes(time,lep*10),color='blue'),
           window=slider(0,maxtime-5000,step=5000)
)




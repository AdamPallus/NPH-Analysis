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

# x$expectedFR<- predict(m,newdata=x)

x<- mutate(x,time=row_number(),
           expectedFR= predict(m,newdat=x),
           adjustedFR=sdf-expectedFR)

x$adjustedFR<- replace(x$adjustedFR,x$adjustedFR<0,0)

m2<- lm(adjustedFR ~ verg.velocity,data=filter(x,abs(verg.velocity)>15))

x<- mutate(x, expectedFR2=predict(m2,newdata=x))
x$expectedFR2<- replace(x$expectedFR2,x$expectedFR2<0,0)

x<- mutate(x,adjustedFR2=expectedFR-expectedFR2)

x$adjustedFR2<- replace(x$adjustedFR2,x$adjustedFR2<0,0)



maxtime<-nrow(x)

manipulate(ggplot(filter(x,time>window,time<window+5000))+
             geom_line(aes(time,expectedFR2))+
             # geom_line(aes(time,adjustedFR),color='orange')+
             geom_line(aes(time,verg.velocity),color='pink')+
             geom_line(aes(time,rep*10),color='red')+
             geom_line(aes(time,lep*10),color='blue'),
           window=slider(0,maxtime-5000,step=5000)
)


##trying anew
z<- filter(t,neuron=='Bee-06')
m<- mm$m[[1]] #from above summary: model based on static FR ~ verg.angle

# z<- mutate(z,time=row_number(),
#            expectedFR= predict(m,newdat=z))

z<- mutate(z,time=row_number(),
           transient.type='none',
       transient.type=replace(transient.type, verg.velocity > 15,'convergence'),
       transient.type=replace(transient.type, verg.velocity < -15, 'divergence'))


# z$expectedFR<- replace(z$expectedFR,z$expectedFR<0,0)

# z<- mutate(z,adjustedFR=sdf-expectedFR)

# z$adjustedFR<- replace(x$adjustedFR,x$adjustedFR<0,0)

mcontrol<- lm(sdf~verg.angle+verg.velocity,data=z)

mtest<- lm(sdf~verg.velocity:transient.type,data=z)

# m4<- lm(sdf~verg.angle+verg.velocity:transient.type,data=z)

z<- mutate(z,econtrol=predict(mcontrol,newdata=z),
           etest=predict(mtest,newdata=z))



maxtime<-nrow(z)


manipulate(ggplot(filter(z,time>window,time<window+5000))+
             geom_line(aes(time,sdf))+
             geom_line(aes(time,econtrol),color='orange')+
             geom_line(aes(time,etest),color='purple')+
             geom_hline(yintercept=15)+
             geom_line(aes(time,verg.velocity),color='pink'),#+
           # geom_line(aes(time,rep*10),color='red')+
           # geom_line(aes(time,lep*10),color='blue'),
           # geom_line(aes(time,verg.angle*10),color='darkgreen'),
           window=slider(0,maxtime-5000,step=5000)
)





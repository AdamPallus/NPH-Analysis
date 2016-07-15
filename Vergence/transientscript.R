"This is a script to analyze the transients. Specifically, the timing and duration of the enhancement
and identification of the transients"


library(ggplot2)
library(relaimpo)
library(manipulate)
library(dplyr)
source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')
source('markEnhancement.R')
t<-readRDS('SOA.RDS')
# z<- filter(t,neuron=='Bee-113')

z<-filter(t,cellnum>200,cellnum<204,monkey=='Bee')
t<-NULL
z<- mutate(z,time=row_number())


z %>%
  mutate(g=floor(time/200000)) %>%
  group_by(g) %>%
  mutate(verg.velocity=parabolicdiff(verg.angle,20)) ->
  z



zz<-markEnhancement(v=z$verg.velocity,threshold2=12)
zz<- dplyr::select(zz,time,enhancenum)

z<- left_join(z,zz,by='time')
z<-ungroup(z)
z %>%
  mutate(verg.enhance=!is.na(enhancenum),
         transient.type='none',
         verg.direction=verg.velocity>0)->
  z

i<- z$verg.enhance & !z$verg.direction
z$transient.type[i]<- 'divergence'
i<- z$verg.enhance & z$verg.direction
z$transient.type[i]<- 'convergence'

bufferlength<- 200
saccade.length<- 150
z%>%
  # group_by(neuron) %>%
  mutate(time=row_number()) %>%
  do(joinsaccadesuniform(.,buffer=bufferlength,threshold=20,saccade.length=saccade.length))->
  # do(joinsaccades(.,buffer=bufferlength,threshold=20))->
  z

# z<- mutate(z,verg.velocity=parabolicdiff(verg.angle,10))

#Once saccades have been marked, we measure lots of things about each saccade. 
#For this plot, we also remove all data that isn't part of a saccade or the buffer period
z %>%
  # group_by(neuron) %>%
  mutate(issaccade=!is.na(sacnum)) %>%
  filter(issaccade) %>%
  group_by(sacnum) %>%
  # filter(counter>0,counter<saccade.dur) %>%
  summarize(saccade.dur=first(saccade.dur),
            saccade.end=saccade.dur+bufferlength,
            peak.conj.velocity=maxabs(conj.velocity),
            peak.R.H= maxabs(rev),
            peak.R.V= maxabs(revV),
            peak.L.H= maxabs(lev),
            peak.L.V= maxabs(levV),
            R.H.Amp=rep[saccade.end]-rep[bufferlength],
            L.H.Amp=lep[saccade.end]-lep[bufferlength],
            R.V.Amp=repV[saccade.end]-repV[bufferlength],
            L.V.Amp=lepV[saccade.end]-lepV[bufferlength],
            r.angle=atan2(R.V.Amp,R.H.Amp)*180/pi,
            r.amp=sqrt(R.H.Amp^2+R.V.Amp^2),
            vect.amp= (sqrt(R.H.Amp^2+R.V.Amp^2)+sqrt(L.H.Amp^2+L.V.Amp^2))/2,
            maxamp=max(abs(R.H.Amp),abs(R.V.Amp),abs(L.H.Amp),abs(L.V.Amp)),
            verg.amp=verg.angle[saccade.end]-verg.angle[bufferlength],
            mean.verg.amp=mean(verg.angle[saccade.end:n()]-mean(verg.angle[1:bufferlength])),
            peak.verg.velocity= maxabs(verg.velocity),
            min.verg.trans = min(verg.velocity),
            max.verg.trans = max(verg.velocity),
            off.verg.velocity=min(abs(min.verg.trans),abs(max.verg.trans)),
            min.verg.angle=min(verg.angle),
            max.verg.angle=max(verg.angle),
            max.verg.velocity=max(verg.velocity),
            min.verg.velocity=min(verg.velocity),
            # verg.amp= last(verg.angle)-first(verg.angle),
            verg.amp= verg.angle[n()]-verg.angle[1],
            initial.verg.angle=verg.angle[bufferlength])->
  s

s$saccade.dur<-NULL

z<-left_join(z,ungroup(s),by='sacnum')
z %>% 
  group_by(sacnum) %>% 
  mutate(counter=row_number()-bufferlength)-> 
  z

z %>% 
  # group_by(neuron,sacnum) %>% 
  group_by(sacnum) %>% 
  mutate(verg.change=verg.angle-verg.angle[bufferlength],
         norm.verg.change=verg.change/max(abs(verg.change)),
         norm.verg.velocity=verg.velocity/max(abs(verg.velocity)),
         verg150=verg.change[bufferlength+150])->
  z

verg.thresh<- 2
z$saccade.type<- 'saccade.only'
z$saccade.type[z$verg150< -verg.thresh]= 'diverging'
z$saccade.type[z$verg150> verg.thresh]= 'converging'
z$saccade.type<-as.factor(z$saccade.type)

z %>%
  filter(counter<250,counter>0) %>%
  group_by(sacnum) %>%
  mutate(c.trans.dur=sum(transient.type=='convergence'),
         d.trans.dur=sum(transient.type=='divergence'),
         enhance.start=first(counter[verg.enhance])) %>%
  summarise_each(funs(first))->
  zz

zz<- filter(zz,verg.amp<20,verg.amp> -20)

qplot(d.trans.dur,c.trans.dur,data=zz)
qplot(r.amp,c.trans.dur,data=zz)

qplot(verg.amp,c.trans.dur,data=zz)

qplot(c.trans.dur, max.verg.trans,data=zz)+facet_wrap(~saccade.type)+coord_cartesian(xlim=c(0,300))


qplot(min.verg.trans,d.trans.dur,color=max.verg.angle,data=zz)

z %>%
  group_by(enhancenum) %>%
  summarize(enhance.dur=n())->
  sss


maxtime<-max(z$time,na.rm=T)
windowsize<- 1000

manipulate(ggplot(filter(z,time>window,time<window+windowsize))+
             geom_line(aes(time,verg.velocity),color='purple')+
             geom_point(aes(time,enhancenum*0-50))+
             geom_hline(yintercept = c(-2,2))+
             geom_line(aes(time,verg.angle*5),color='darkgreen')+
             coord_cartesian(ylim=c(-100,100))+
             geom_line(aes(time,verg.accel/10)),
           window=slider(0,maxtime-windowsize,step=windowsize)
)


# z %>%
#   group_by(g) %>%
#   mutate(smooth.verg.velocity=parabolicdiff(verg.angle,30)) ->
#   z
# 
# z %>%
#   group_by(g) %>%
#   mutate(verg.accel=parabolicdiff(verg.velocity,5)) ->
#   z

z %>%
  filter(saccade.type!='saccade.only',counter> -100) %>%
  group_by(sacnum) %>%
  mutate(verg.ismoving=abs(verg.velocity)>3) %>%
  summarize(verg.onset=first(counter[verg.ismoving]),
            enhance.onset=first(counter[verg.enhance]),
            verg.lead=enhance.onset-verg.onset)->
  xxx

z<- left_join(z,xxx,by='sacnum')

# nsac=max(z$sacnum,na.rm=T)
# manipulate(ggplot(filter(z,sacnum==sac))+
#              geom_line(aes(counter,verg.velocity),color='purple')+
#              geom_point(aes(counter,enhancenum*0-50))+
#              geom_hline(yintercept = c(-3,3))+
#              geom_line(aes(counter,verg.angle*5),color='darkgreen')+
#              geom_vline(aes(xintercept = verg.onset))+
#              coord_cartesian(ylim=c(-100,100)),
#            sac=slider(1,nsac,step=1)
# )



p<- filter(z,saccade.type!='saccade.only')

goodsacs<- unique(p$sacnum)

nsac=length(goodsacs)
manipulate(ggplot(filter(z,sacnum==goodsacs[sac]))+
             geom_line(aes(counter,verg.velocity),color='purple')+
             geom_line(aes(counter,conj.velocity/10),color='pink')+
             geom_point(aes(counter,enhancenum*0-50))+
             geom_point(aes(counter,verg.velocity,color=transient.type),size=1,
                        data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
             geom_hline(yintercept = c(-2,2))+
             geom_line(aes(counter,verg.angle*5),color='darkgreen')+
             geom_vline(aes(xintercept = verg.onset))+
             coord_cartesian(ylim=c(-100,100)),
           sac=slider(1,nsac,step=1)
)

z %>%
  group_by(sacnum) %>%
  summarize_each(funs(first))->
  sz
sz<- filter(sz,abs(verg.amp)<20,abs(peak.verg.velocity)<1000)

ggplot(sz)+geom_point(aes(verg.lead,peak.verg.velocity,color=saccade.type),alpha=0.4)
qplot(verg.lead,peak.verg.velocity,data=sz,color=saccade.type)
qplot(verg.onset,max.verg.trans,data=sz,color=saccade.type)

ggplot(sz)+geom_point(aes(verg.onset,max.verg.trans),color='red')+
  geom_point(aes(verg.onset,min.verg.trans),color='blue')+
  geom_point(aes(verg.onset,peak.verg.velocity),alpha=0.6,shape='x',size=4)

ggplot(sz)+geom_point(aes(verg.onset,abs(min.verg.trans)-abs(max.verg.trans)))

p<- filter(z,saccade.type=='saccade.only')

z %>% 
  group_by(sacnum) %>%
  mutate(isdivergence.trans=transient.type=='divergence',
    transient.onset=first(counter[isdivergence.trans])) ->
  z

ggplot(z)+
  geom_line(aes(counter,verg.velocity,group=sacnum))

z %>% group_by(sacnum) %>%
  mutate(counter2=counter-transient.onset) ->
  z

z %>%
  ungroup() %>%
  group_by(monkey,counter2) %>%
  summarize(mean.transient=mean(verg.velocity,na.rm=T)) %>%
  group_by(monkey) %>%
  mutate(norm.transient=mean.transient/abs(min(mean.transient)))->
  s

z<- left_join(z,s,by='counter2')

goodsacs<- unique(z$sacnum)
nsac=length(goodsacs)
manipulate(ggplot(filter(z,sacnum==goodsacs[sac]))+
             geom_line(aes(counter2,verg.velocity),color='purple')+
             geom_line(aes(counter2,norm.transient*abs(predicted.min.trans)),color='pink')+
             geom_line(aes(counter2,verg.velocity-norm.transient*abs(predicted.min.trans)-100),color='darkred')+
             geom_point(aes(counter2,enhancenum*0-50))+
             geom_point(aes(counter2,verg.velocity,color=transient.type),size=1,
                        data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
             geom_hline(yintercept = c(-2,2))+
             geom_line(aes(counter2,verg.angle*5),color='darkgreen')+
             geom_vline(aes(xintercept = verg.onset))+
             coord_cartesian(ylim=c(-200,100)),
           sac=slider(1,nsac,step=1)
)
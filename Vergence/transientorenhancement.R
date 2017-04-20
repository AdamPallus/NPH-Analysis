# Rereading Busettini, I realize that I did not appreciate the disinction he was making between
# vergence transients and vergence enhancement. He defines "transients" to be the paired positive
# and negative vergence velocity peaks observed during saccades to targets at the same distance.
# When a saccade is made in depth, enhancement occurs. In his view, this enhancement "covers up"
# the negative transient so we don't see it during these saccades. However, this is dependent on
# the timing of the saccades relative to the vergence. If the saccade starts "too soon," the 
# vergence enhancemnt hasn't had time to "build up" and thus the negative transient is visible.

# What we want to do in this script is evaluate the transients that occur during saccades
# to targets at the same depth. Then we can subtract this from the observed vergence velocity
# to obtain the vergence enhancement!!

# Planned programming procedure:
# 1) identify saccades with verg.amp < 1 (?) 
# 2) plot positive and negative transients against saccade peak velocity
# 3) fit an exponential model
# 4) use this model to predict the transients that would occur during saccades with verg.amp>1
# 5) subtract and observe
library(dplyr)
library(ggplot2)
library(relaimpo)
source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')
source('markEnhancement.R')
t<-readRDS('SOA-NRTP.RDS')
# z<- filter(t,neuron=='Bee-113')

z<-filter(t,cellnum<15,monkey=='Bee')
z<- mutate(z,time=row_number())
zz<-markEnhancement(v=z$verg.velocity,threshold2=12)
zz<- dplyr::select(zz,time,enhancenum)

z<- left_join(z,zz,by='time')

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
  group_by(neuron,sacnum) %>%
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

# s<- filter(z,peak.conj.velocity<1500,min.verg.trans> -1000)


ggplot(filter(s,abs(mean.verg.amp)<1,max.verg.angle>12,r.amp>3))+
  geom_point(aes(peak.conj.velocity,min.verg.trans))+
  geom_point(aes(peak.conj.velocity,max.verg.trans),color='darkgray')+
  coord_cartesian(xlim=c(0,1000),ylim=c(-300,300))+
  ylab('Pos & Neg peak vg vel')+
  stat_smooth(aes(peak.conj.velocity,min.verg.trans),method='lm',formula='y~poly(x,2)')+
  ggtitle('Rightward saccades at near -Bee')

ggplot(filter(s,abs(mean.verg.amp)<1,max.verg.angle<5,abs(r.angle)<20,r.amp>3))+
  geom_point(aes(peak.conj.velocity,min.verg.trans))+
  geom_point(aes(peak.conj.velocity,max.verg.trans),color='darkgray')+
  coord_cartesian(xlim=c(0,1000),ylim=c(-300,300))+
  ylab('Pos & Neg peak vg vel')+
  ggtitle('Rightward saccades at far -Bee')+
  stat_smooth(aes(peak.conj.velocity,min.verg.trans),method='lm',formula='y~poly(x,2)')


ggplot(filter(s,abs(mean.verg.amp)<1,r.amp>3))+
  geom_point(aes(peak.conj.velocity,min.verg.trans))+
  geom_point(aes(peak.conj.velocity,max.verg.trans),color='darkgray')+
  coord_cartesian(xlim=c(0,1000),ylim=c(-300,300))+ 
  stat_smooth(aes(peak.conj.velocity,min.verg.trans),method='lm',formula='y~poly(x,2)')+
  stat_smooth(aes(peak.conj.velocity,max.verg.trans),method='lm',formula='y~poly(x,2)')+
  ylab('Pos & Neg peak vg vel')+
  ggtitle('All saccades with verg.amp < 1 -Bee')

f<- filter(s,abs(mean.verg.amp)<1,r.amp>5)

cor(f$peak.conj.velocity,f$min.verg.trans)^2
cor(f$peak.conj.velocity,f$max.verg.trans)^2
m<-lm(min.verg.trans~mean.verg.amp+poly(peak.conj.velocity,2)+poly(r.amp,2)+r.angle+initial.verg.angle,data=f)

b<-calc.relimp(m)
plot(b)

f<- ungroup(f)
f<- mutate(f,predicted.transient=predict(m,newdata=f))


ggplot(f)+geom_point(aes(peak.conj.velocity,min.verg.trans))+
  geom_point(aes(peak.conj.velocity,predicted.transient),color='purple')

ggplot(filter(f,abs(mean.verg.amp)<0.5))+geom_point(aes(peak.conj.velocity,min.verg.trans))+
  geom_point(aes(peak.conj.velocity,predicted.transient),color='purple')

f<- mutate(f,predict.error=predicted.transient-min.verg.trans,
           predict.error.percent=predict.error/predicted.transient)

ggplot(filter(f,abs(mean.verg.amp)<0.5))+
  geom_point(aes(peak.conj.velocity,min.verg.trans,size=abs(predict.error.percent),color=mean.verg.amp))

qplot(sacnum,predict.error,data=f)
qplot(sacnum,predict.error.percent,data=f)
i<- f$sacnum[abs(f$predict.error.percent)>1]

ff<-filter(f,sacnum %in% i)
qplot(peak.conj.velocity,min.verg.trans,data=f)+
  geom_point(aes(peak.conj.velocity,min.verg.trans),data=ff,color='hotpink',size=2)



m<-lm(abs(min.verg.trans)~poly(peak.conj.velocity,3),data=f)
summary(m)

#------------------------------
s$saccade.dur<-NULL

z<-left_join(z,s,by='sacnum')
z %>% 
  group_by(sacnum) %>% 
  mutate(counter=row_number()-bufferlength)-> 
  z
# z<-left_join(z,s,by=c('neuron','sacnum'))
# z %>% 
#   group_by(neuron,sacnum) %>% 
#   mutate(counter=row_number()-bufferlength)-> 
#   z



ggplot(filter(z,verg.amp>1,r.amp>5,abs(r.angle)<20))+
  geom_line(aes(counter,verg.angle*10-200,group=sacnum),color='darkgreen')+
  geom_line(aes(counter,conj.velocity,group=sacnum),alpha=0.5)+
  geom_line(aes(counter,verg.velocity,group=sacnum),color='hotpink')

verg.thresh<- 2
z$saccade.type<- 'saccade.only'
z$saccade.type[z$mean.verg.amp< -verg.thresh]= 'diverging'
z$saccade.type[z$mean.verg.amp> verg.thresh]= 'converging'
z$saccade.type<-as.factor(z$saccade.type)

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

ggplot(filter(z,counter> -25,counter< 150,r.amp>7))+
  # geom_line(aes(counter,conj.velocity,group=interaction(neuron,sacnum)),alpha=0.2)+
  geom_line(aes(counter,verg.change*20,group=sacnum),color='darkgreen',alpha=0.2)+
  geom_line(aes(counter,verg.velocity-400,group=sacnum),alpha=0.2,color='hotpink')+
  facet_wrap(~saccade.type,ncol=1)+
  # stat_smooth(aes(counter,verg.velocity-400))+
  geom_hline(yintercept=c(0,-400))

ggplot(filter(z,counter> -25,counter< 150,r.amp>7,abs(r.angle<20),saccade.end<280))+
  # geom_line(aes(counter,conj.velocity,group=interaction(neuron,sacnum)),alpha=0.2)+
  geom_line(aes(counter,verg.change*20,group=sacnum),color='darkgreen',alpha=0.2)+
  geom_line(aes(counter,verg.velocity-400,group=sacnum),alpha=0.2,color='hotpink')+
  facet_wrap(~saccade.type,ncol=1)+
  stat_smooth(aes(counter,verg.velocity-400))+
  geom_hline(yintercept=c(0,-400))



ggplot(filter(z,abs(r.angle)<60,counter> -25,counter< 150))+
  geom_line(aes(counter,verg.velocity,group=interaction(sacnum)),alpha=0.2,color='hotpink')+
  facet_wrap(~saccade.type,ncol=1)+
  stat_smooth(aes(counter,verg.velocity))

z<- mutate(z,near.saccade=max.verg.angle>10)

z$near.saccade=as.factor(z$near.saccade)
levels(z$near.saccade)<- c('Far','Near')

z$right.saccade=as.factor(abs(z$r.angle)>90)
levels(z$right.saccade)<- c('Left','Right')

ggplot(filter(z,counter> -25,counter< 150,r.amp>7))+
  # geom_line(aes(counter,conj.velocity,group=interaction(neuron,sacnum)),alpha=0.2)+
  geom_line(aes(counter,norm.verg.change,group=interaction(sacnum)),color='darkgreen',alpha=1/20)+
  geom_line(aes(counter,norm.verg.velocity-2,group=interaction(sacnum)),alpha=1/20,color='hotpink')+
  facet_grid(right.saccade~saccade.type)+
  geom_hline(yintercept=c(0,-2))

ggplot(filter(z,counter> -100,counter< 200,r.amp>7))+
  # geom_line(aes(counter,conj.velocity,group=interaction(neuron,sacnum)),alpha=0.2)+
  geom_line(aes(counter,verg.change*10,group=interaction(sacnum)),color='darkgreen',alpha=1)+
  geom_line(aes(counter,verg.velocity-evel-400,group=interaction(sacnum)),color='orange',alpha=1)+
  geom_line(aes(counter,evel-300,group=interaction(sacnum)),color='orange',alpha=1)+
  geom_line(aes(counter,verg.velocity-200,group=interaction(sacnum)),alpha=1,color='hotpink')+
  facet_grid(right.saccade~saccade.type)+
  geom_hline(yintercept=c(0,-200,-300,-400))

z %>%
  filter(counter<150,counter> -25,r.amp>7) %>%
  group_by(saccade.type,right.saccade,counter) %>%
  summarize(mean.norm.verg.velocity=mean(norm.verg.velocity),
            mean.verg.velocity=mean(verg.velocity)) %>%
  ungroup() %>%
  group_by(saccade.type,right.saccade) %>%
  mutate( m.n.v.v=mean.norm.verg.velocity/max(abs(mean.norm.verg.velocity))) ->
  template

ggplot(template)+geom_line(aes(counter,mean.norm.verg.velocity))+facet_grid(right.saccade~saccade.type)

ggplot(template)+geom_line(aes(counter,mean.verg.velocity))+facet_grid(right.saccade~saccade.type)


ggplot(template)+geom_line(aes(counter,mean.norm.verg.velocity))+facet_grid(right.saccade~saccade.type)


ggplot(template)+geom_line(aes(counter,m.n.v.v))+facet_grid(right.saccade~saccade.type)


template<- filter(template,saccade.type=='saccade.only')
z<-ungroup(z)
template<-ungroup(template)
template<- select(template,-saccade.type)
z<- left_join(z,template,by=c('counter','right.saccade'))

z$mean.norm.verg.velocity[is.na(z$mean.norm.verg.velocity)]=0

z<- mutate(z,adjusted.verg.velocity=norm.verg.velocity-mean.norm.verg.velocity)

#subtract the normalized average of the normalized
gs<-ggplot(filter(z,counter> -25,counter< 150,r.amp>7,!is.na(sacnum),!is.na(right.saccade)))+
  # geom_line(aes(counter,conj.velocity,group=interaction(neuron,sacnum)),alpha=0.2)+
  # geom_line(aes(counter,norm.verg.change,group=interaction(neuron,sacnum)),color='darkgreen',alpha=1/20)+
  geom_line(aes(counter,norm.verg.velocity-m.n.v.v-3,
                group=interaction(neuron,sacnum)),alpha=1/20,color='black')+
  geom_line(aes(counter,norm.verg.velocity,
                group=interaction(neuron,sacnum)),alpha=1/20,color='hotpink')+
  geom_line(aes(counter,m.n.v.v),size=2,data=template,alpha=0.5)+
  # geom_line(aes(counter,mean.norm.verg.velocity-2,
  #               group=interaction(neuron,sacnum)),size=2)+
  facet_grid(right.saccade~saccade.type)+
  geom_hline(yintercept=c(0,-3))

#subtract the average of the normalized (average does not peak at 1)
ggplot(filter(z,counter> -25,counter< 150,r.amp>7,!is.na(sacnum),!is.na(right.saccade)))+
  # geom_line(aes(counter,conj.velocity,group=interaction(neuron,sacnum)),alpha=0.2)+
  # geom_line(aes(counter,norm.verg.change,group=interaction(neuron,sacnum)),color='darkgreen',alpha=1/20)+
  geom_line(aes(counter,norm.verg.velocity-mean.norm.verg.velocity-3,
                group=interaction(neuron,sacnum)),alpha=1/20,color='black')+
  geom_line(aes(counter,norm.verg.velocity,
                group=interaction(neuron,sacnum)),alpha=1/20,color='hotpink')+
  geom_line(aes(counter,mean.norm.verg.velocity),size=2,data=template,alpha=0.5)+
  # geom_line(aes(counter,mean.norm.verg.velocity-2,
  #               group=interaction(neuron,sacnum)),size=2)+
  facet_grid(right.saccade~saccade.type)+
  geom_hline(yintercept=c(0,-3))


gs<-ggplot(filter(z,counter> -25,counter< 150,r.amp>7,!is.na(sacnum),!is.na(right.saccade)))+
  # geom_line(aes(counter,conj.velocity,group=interaction(neuron,sacnum)),alpha=0.2)+
  # geom_line(aes(counter,norm.verg.change,group=interaction(neuron,sacnum)),color='darkgreen',alpha=1/20)+
  geom_line(aes(counter,verg.velocity-mean.verg.velocity,
                group=interaction(neuron,sacnum)),alpha=1/20,color='black')+
  geom_line(aes(counter,verg.velocity-400,
                group=interaction(neuron,sacnum)),alpha=1/20,color='hotpink')+
  geom_line(aes(counter,mean.verg.velocity-400),size=2,data=template,alpha=0.5)+
  # geom_line(aes(counter,mean.norm.verg.velocity-2,
  #               group=interaction(neuron,sacnum)),size=2)+
  facet_grid(right.saccade~saccade.type)+
  geom_hline(yintercept=c(0,-400))



ggplot(filter(z,counter> -25,counter< 150,saccade.dur>50,saccade.type!='saccade.only'))+
  geom_line(aes(counter,verg.velocity,group=interaction(neuron,sacnum)),alpha=0.2,color='hotpink')+
  facet_grid(near.saccade~saccade.type)+
  stat_smooth(aes(counter,verg.velocity))

z<- mutate(z,fast.saccade=as.factor(peak.conj.velocity>189))
levels(z$fast.saccade)<- c('Slow','Fast')

ggplot(filter(z,abs(r.angle)<20,counter> -25,counter< 150))+
  geom_line(aes(counter,verg.velocity,group=interaction(neuron,sacnum)),alpha=0.2,color='hotpink')+
  facet_grid(fast.saccade~saccade.type)+
  stat_smooth(aes(counter,verg.velocity))



z %>% filter(counter>-25,counter<150) %>%
  group_by(saccade.type,counter) %>%
  summarize(transient.template=mean(verg.velocity),
            saccade.template=mean(conj.velocity))->
  sz

ggplot(filter(sz))+
  geom_line(aes(counter,transient.template,color=saccade.type),size=2)

ggplot(filter(sz))+
  geom_line(aes(counter,verg.velocity,group=interaction(neuron,sacnum)),
            alpha=1/20,
            data=filter(z,saccade.type=='converging',counter>-25,counter<150))+
  geom_line(aes(counter,transient.template,color=saccade.type),size=2)
 


#-------------------
#marking transients
#-------------------

z %>%
  filter(counter<250,counter>0) %>%
  group_by(sacnum) %>%
  mutate(c.trans.dur=sum(transient.type=='convergence' & enhancenum==first(enhancenum)),
            d.trans.dur=sum(transient.type=='divergence' & enhancenum==first(enhancenum)),
         enhance.start=first(counter[verg.enhance])) %>%
  summarise_each(funs(first))->
    zz


require(dplyr)
require(ggplot2)
source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')



z<-readRDS('SOA-NRTP.RDS')

# z<- filter(z,neuron %in% c('Bee-101','Bee-122'))
z<- filter(z,monkey=='Bee')
#mark saccades

#measure saccades
message('marking saccades...')
#mark saccades and create buffer around each saccade for analysis
bufferlength<- 200
saccade.length<- 150
z%>%
  # group_by(neuron) %>%
  mutate(time=row_number()) %>%
  do(joinsaccadesuniform(.,buffer=bufferlength,threshold=20,saccade.length=saccade.length))->
  # do(joinsaccades(.,buffer=bufferlength,threshold=20))->
  z

message('measuring saccades...')
#Once saccades have been marked, we measure lots of things about each saccade. 
#For this plot, we also remove all data that isn't part of a saccade or the buffer period
z %>%
  # group_by(neuron) %>%
  mutate(issaccade=!is.na(sacnum)) %>%
  filter(issaccade) %>%
  group_by(sacnum) %>%
  # filter(counter>0,counter<saccade.dur) %>%
  mutate(counter=row_number()-bufferlength) %>%
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
            # verg.amp= verg.angle[n()]-verg.angle[1],
            initial.verg.angle=verg.angle[bufferlength],
            verg150=verg.angle[bufferlength+150]-initial.verg.angle)->
  s

s$saccade.dur<-NULL

z<-left_join(z,ungroup(s),by='sacnum')
z %>% 
  group_by(sacnum) %>% 
  mutate(counter=row_number()-bufferlength)-> 
  z

#mark saccades as convergent, divergent or neither
verg.thresh<- 0.5
z$saccade.type<- 'saccade.only'
z$saccade.type[z$mean.verg.amp < -verg.thresh]= 'diverging'
z$saccade.type[z$mean.verg.amp > verg.thresh]= 'converging'
z$saccade.type<-as.factor(z$saccade.type)
#filter saccades with verg.amp < 0.5
# verg.thresh<- 0.5
# z$saccade.type<- 'saccade.only'
# z$saccade.type[z$verg150 < -verg.thresh]= 'diverging'
# z$saccade.type[z$verg150 > verg.thresh]= 'converging'
# z$saccade.type<-as.factor(z$saccade.type)

#group by neuron, counter
#summarize verg.velocity

z %>% 
  filter(saccade.type=='saccade.only',counter<200,r.amp>5,saccade.dur<100) %>%
  select(neuron,counter,verg.velocity) %>%
  group_by(neuron,counter) %>%
  summarize(template=mean(verg.velocity)) ->
  t

gp<- qplot(counter,template,data=t)+facet_wrap(~neuron)

ggsave('TestTransients.pdf',plot=gp,height=20,width=20)

qp<- ggplot(data=filter(z,counter<200,verg150<0.5,verg150> -0.5,r.amp>4,saccade.dur<100))+
  geom_line(aes(counter,verg.velocity,group=sacnum),alpha=1/15)+
  facet_wrap(~neuron)+
  coord_cartesian(ylim=c(-100,100))
ggsave('testtemplate2.pdf',plot=qp,height=20,width=20)


z %>% 
  ungroup() %>%
  filter(saccade.type=='saccade.only',counter<200,r.amp>5) %>%
  select(neuron,counter,verg.velocity) %>%
  group_by(counter) %>%
  summarize(template=mean(verg.velocity)) ->
  t

gp<- qplot(counter,template,data=t)+facet_wrap(~neuron)

ggsave('TestTransientsALL.pdf',plot=gp,height=20,width=20)

# qplot(counter, verg.velocity)+facet_wrap(~neuron)


# z<- readRDS('MeasuredOzetteBehavior.RDS')

z %>%
  mutate(g=floor(time/200000)) %>%
  group_by(g) %>%
  mutate(rev=parabolicdiff(rep,20),
         lev=parabolicdiff(lep,20),
         revV=parabolicdiff(repV,20),
         levV=parabolicdiff(lepV,20),
         conj.velocity=sqrt((rev^2+lev^2)/2)+sqrt((revV^2+levV^2)/2),
         verg.velocity=lev-rev) ->
  z


# z<- readRDS('MeasuredDataBee-smooth.RDS')
library(manipulate)
p<- filter(z, r.amp>4,saccade.dur<100,abs(verg.amp)>3)
p<- filter(z, r.amp>4,saccade.dur<100,abs(verg.amp)<1)
p<- filter(z, r.amp>4,saccade.dur<100,verg.amp> 4)

goodsacs<- unique(p$sacnum)

nsac=length(goodsacs)
manipulate(ggplot(filter(z,sacnum==goodsacs[sac]))+
             geom_line(aes(counter,verg.velocity),color='purple')+
             geom_line(aes(counter,conj.velocity),color='brown')+
             # geom_point(aes(counter,enhancenum*0-50))+
             # geom_point(aes(counter,verg.velocity,color=transient.type),size=1,
             # data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
             geom_hline(yintercept = c(-2,2))+
             geom_line(aes(counter,verg.angle*5),color='darkgreen')+
             # geom_vline(aes(xintercept = verg.onset))+
             # geom_line(aes(counter,real.verg.velocity),color='magenta')+
             # geom_hline(yintercept=c(-12,12))+
             # geom_point(aes(counter,showrasters+100),shape='|',size=3)+
             coord_cartesian(ylim=c(-100,200)),
           sac=slider(1,nsac,step=1)
)

p<- filter(z, r.amp>4,saccade.dur<100,verg.amp> 4)
goodsacs<- unique(p$sacnum)
#for converging saccades
zz<- filter(z,sacnum %in% goodsacs[c(25,101,109,111,321,676)])

ggplot(zz)+
  geom_line(aes(counter,verg.velocity),color='purple',size=1.5)+
  geom_line(aes(counter,conj.velocity),color='brown')+
  # geom_point(aes(counter,enhancenum*0-50))+
  # geom_point(aes(counter,verg.velocity,color=transient.type),size=1,
  # data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
  # geom_hline(yintercept = c(-2,2))+
  geom_line(aes(counter,verg.angle*10),color='darkgreen')+
  # geom_vline(aes(xintercept = verg.onset))+
  # geom_line(aes(counter,real.verg.velocity),color='magenta')+
  # geom_hline(yintercept=c(-12,12))+
  # geom_point(aes(counter,showrasters+100),shape='|',size=3)+
  coord_cartesian(ylim=c(-50,200),xlim=c(-100,200))+
  facet_wrap(~sacnum)+
  theme(strip.background = element_blank(),strip.text.x=element_blank())+
  xlab('Time (ms)')+
  ylab('')


p<- filter(z, r.amp>4,saccade.dur<100,verg.amp< -4)
goodsacs<- unique(p$sacnum)
#for diverging saccades
zz<- filter(z,sacnum %in% goodsacs[c(130,154,155,156,636,638)])
ggplot(zz)+
  geom_line(aes(counter,verg.velocity),color='purple',size=1.5)+
  geom_line(aes(counter,conj.velocity),color='brown')+
  # geom_point(aes(counter,enhancenum*0-50))+
  # geom_point(aes(counter,verg.velocity,color=transient.type),size=1,
  # data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
  # geom_hline(yintercept = c(-2,2))+
  geom_line(aes(counter,verg.angle*10-100),color='darkgreen')+
  # geom_vline(aes(xintercept = verg.onset))+
  # geom_line(aes(counter,real.verg.velocity),color='magenta')+
  # geom_hline(yintercept=c(-12,12))+
  # geom_point(aes(counter,showrasters+100),shape='|',size=3)+
  coord_cartesian(ylim=c(-200,100),xlim=c(-100,200))+
  facet_wrap(~sacnum)+
  theme(strip.background = element_blank(),strip.text.x=element_blank())+
  xlab('Time (ms)')+
  ylab('')

p<- filter(z, r.amp>4,saccade.dur<100,abs(verg.amp)<1)
goodsacs<- unique(p$sacnum)
#for transients only
zz<- filter(z,sacnum %in% goodsacs[c(5,6,1957,1960,1973,2232)])
ggplot(zz)+
  geom_line(aes(counter,verg.velocity),color='purple',size=1.5)+
  geom_line(aes(counter,conj.velocity),color='brown')+
  # geom_point(aes(counter,enhancenum*0-50))+
  # geom_point(aes(counter,verg.velocity,color=transient.type),size=1,
  # data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
  # geom_hline(yintercept = c(-2,2))+
  geom_line(aes(counter,verg.angle*10-100),color='darkgreen')+
  # geom_vline(aes(xintercept = verg.onset))+
  # geom_line(aes(counter,real.verg.velocity),color='magenta')+
  # geom_hline(yintercept=c(-12,12))+
  # geom_point(aes(counter,showrasters+100),shape='|',size=3)+
  coord_cartesian(ylim=c(-100,100),xlim=c(-100,200))+
  facet_wrap(~sacnum)+
  theme(strip.background = element_blank(),strip.text.x=element_blank())+
  xlab('Time (ms)')+
  ylab('')


library(ggplot2)
library(manipulate)
source('Adamhelperfunctions.R')
source('calculateTransient.R')
library(dplyr)
# z<- readRDS('NRTP.RDS')
z<- readRDS('SOA.RDS')
# d<- filter(z,neuron=='Bee-16')
d<- filter(z,neuron %in% c('Bee-101','Bee-107','Bee-112','Bee-202','Bee-205','Bee-211','Bee-215','Bee-218'))
# z<- filter(z,cellnum==101,monkey=='Bee') # just use some of the data for this test

d<- filter(z,monkey=='Bee')

d<- mutate(d,time=row_number())

message('Smoothing Velocity...')
#smooth out vergence velocity traces
# d %>%
#   mutate(g=floor(time/200000)) %>%
#   group_by(g) %>%
#   mutate(verg.velocity=parabolicdiff(verg.angle,20)) ->
#   d

d %>%
  mutate(g=floor(time/200000)) %>%
  group_by(g) %>%
  mutate(rev=parabolicdiff(rep,20),
         lev=parabolicdiff(lep,20),
         revV=parabolicdiff(repV,20),
         levV=parabolicdiff(lepV,20),
         conj.velocity=sqrt((rev^2+lev^2)/2)+sqrt((revV^2+levV^2)/2),
         verg.velocity=lev-rev) ->
  d



z <- calculateTransient(d)

s<- readRDS('transientstemplate.RDS')
s<- filter(s,monkey=='Bee')

z<- left_join(z,s,by=c('monkey','counter2'))
z<- mutate(z,initial.verg.angle=first(verg.angle))

z %>%
  group_by(sacnum) %>%
  summarize_each(funs(first))->
  sz
sz <- mutate(sz,rightward=abs(r.angle)<90)

# m<-lm(min.verg.trans~peak.conj.velocity:rightward:monkey,data=sz)
m<-lm(min.verg.trans~peak.conj.velocity:rightward,data=sz)
m<-lm(min.verg.trans~peak.conj.velocity:verg.angle+verg.amp,data=sz)


sz<-mutate(sz,predicted.min.trans=predict(m,newdata=sz))

sz<- dplyr::select(sz, sacnum,predicted.min.trans)


z<- left_join(z,sz,by='sacnum')

p<- filter(z,transient.onset>0,transient.onset<100,abs(counter2)<=200)


goodsacs<- unique(p$sacnum)
nsac=length(goodsacs)
manipulate(ggplot(filter(p,sacnum==goodsacs[sac]))+
             geom_area(aes(counter2,sdf))+
             # geom_line(aes(counter2,verg.velocity),color='purple')+
             # geom_line(aes(counter2,norm.transient*abs(predicted.min.trans)),color='pink')+
             geom_line(aes(counter2,verg.velocity-norm.transient*abs(predicted.min.trans)-50),color='darkred')+
             geom_line(aes(counter2,verg.velocity-50),color='purple')+
             # geom_point(aes(counter2,enhancenum*0-50))+
             # geom_point(aes(counter2,verg.velocity,color=transient.type),size=1,
                        # data=filter(p, sacnum==goodsacs[sac],enhancenum>0))+
             # geom_hline(yintercept = c(-2,2))+
             geom_line(aes(counter2,verg.angle*10-50),color='darkgreen',size=2)+
             # geom_vline(aes(xintercept = verg.onset))+
             coord_cartesian(ylim=c(-300,300)),
           sac=slider(1,nsac,step=1)
)


z %>% 
  group_by(sacnum) %>%
  mutate(sdflag=lag(sdf,25),
         real.verg.velocity=verg.velocity-norm.transient*abs(predicted.min.trans)) %>%
  ungroup()-> 
  z

# mtest<- lm(sdflag~verg.angle+real.verg.velocity,data=zz)
# 
# zz<- mutate(zz,etest=predict(mtest,newdata=zz))
# 
# p<- zz
# goodsacs<- unique(p$sacnum)
# nsac=length(goodsacs)
# manipulate(ggplot(filter(p,sacnum==goodsacs[sac]))+
#              geom_area(aes(counter2,sdflag))+
#              # geom_line(aes(counter2,verg.velocity),color='purple')+
#              # geom_line(aes(counter2,norm.transient*abs(predicted.min.trans)),color='pink')+
#              geom_line(aes(counter2,verg.velocity-norm.transient*abs(predicted.min.trans)-50),color='darkred')+
#              geom_line(aes(counter2,real.verg.velocity-50))+
#              geom_line(aes(counter2,verg.velocity-50),color='purple')+
#              # geom_point(aes(counter2,enhancenum*0-50))+
#              # geom_point(aes(counter2,verg.velocity,color=transient.type),size=1,
#              # data=filter(p, sacnum==goodsacs[sac],enhancenum>0))+
#              # geom_hline(yintercept = c(-2,2))+
#              geom_line(aes(counter2,verg.angle*10-50),color='darkgreen',size=2)+
#              # geom_vline(aes(xintercept = verg.onset))+
#              coord_cartesian(ylim=c(-300,300)),
#            sac=slider(1,nsac,step=1)
# )

d<- mutate(d,sdflag=lag(sdf,20))
p<- filter(z,abs(counter2)<150)
pp<- dplyr::select(p,time,real.verg.velocity)
d<- left_join(d,pp,by='time')
rvv=d$verg.velocity
missing=is.na(d$real.verg.velocity)
rvv[!missing]=d$real.verg.velocity[!missing]
d$real.verg.velocity=rvv

mcontrol<- lm(sdflag~verg.angle+verg.velocity,data=d)

mtest<- lm(sdflag~verg.angle+real.verg.velocity,data=d)

d<- mutate(d,econtrol=predict(mcontrol,newdata=d),
           econtrol=replace(econtrol,econtrol<0,0),
           etest=predict(mtest,newdata=d),
           etest=replace(etest,etest<0,0),
           showrasters=replace(rasters,rasters<1,NA))

dt<- filter(d,!is.na(sdflag))
cor(dt$econtrol,dt$sdflag)^2

cor(dt$etest,dt$sdflag)^2

dt<-filter(dt,abs(verg.velocity)>15)

cor(dt$econtrol,dt$sdflag)^2


cor(dt$etest,dt$sdflag)^2


d %>% 
  group_by(time) %>%
  summarize_each(funs(max))->
  d

maxtime<-nrow(d)
windowsize<- 5000

manipulate(ggplot(filter(d,time>window,time<window+windowsize))+
             geom_area(aes(time,sdflag))+
             # geom_area(aes(time,etest),fill='red',alpha=0.5)+
             # geom_area(aes(time,econtrol),fill='blue',alpha=0.5)+
             geom_line(aes(time,etest),color='red',alpha=0.5)+
             geom_line(aes(time,econtrol),color='blue',alpha=0.5)+
             # geom_area(aes(time,etest),fill='purple',alpha=.5)+

             geom_line(aes(time,real.verg.velocity-50),color='purple'),
             # geom_line(aes(time,verg.velocity-100),color='red'),
           

           window=slider(0,maxtime-windowsize,step=windowsize)
)



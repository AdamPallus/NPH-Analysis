#In this script I will be trying to model the activity of these neurons. 
#Specifically, I will look at the vergence velocity related activity

#Plan: 1) calcualte verg.angle related firing rate (FR) during fixation
#      2) subtract this from observed FR during movements
#      3) Is there a different relationship between verg.velocity and FR when there is a saccade?
source('Adamhelperfunctions.R')
library(dplyr)
library(ggplot2)
library(manipulate)
source('markEnhancement.R')

t<-readRDS('SOA-NRTP.RDS')
z<- filter(t,neuron=='Bee-06')
# m<- mm$m[[1]] #from above summary: model based on static FR ~ verg.angle

# z<- mutate(z,time=row_number(),
#            expectedFR= predict(m,newdat=z))

# z<- mutate(z,time=row_number(),
#            sdf=spikedensity(rasters,sd=30),
#            verg.velocity=parabolicdiff(verg.angle,20),
#            transient.type='none',
#        transient.type=replace(transient.type, verg.velocity > 15,'convergence'),
#        transient.type=replace(transient.type, verg.velocity < -15, 'divergence'),
#        verg.velocity.fixed=replace(verg.velocity, transient.type=='divergence',verg.velocity*-1))

z %>%
  mutate(time=row_number(),
         sdf=spikedensity(rasters,sd=25),
         verg.velocity=parabolicdiff(verg.angle,25),
         # verg.accel=parabolicdiff(verg.velocity,20),
         verg.direction=verg.angle>0) ->
  z

z<- left_join(z,markEnhancement(z$verg.velocity),by='time')

z %>%
  mutate(verg.enhance=!is.na(enhancenum),
         transient.type='none',
         verg.direction=verg.velocity>0)->
  z

i<- z$verg.enhance & !z$verg.direction
z$transient.type[i]<- 'divergence'
i<- z$verg.enhance & z$verg.direction
z$transient.type[i]<- 'convergence'

z<- mutate(z,is.convergent.trans=transient.type=='convergence')


##split verg.velocity into positive and negative
# z %>%
#   mutate(verg.velocity.positive=replace(verg.velocity,verg.velocity<0,0),
#          verg.velocity.negative=replace(verg.velocity,verg.velocity>0,0),
#          verg.velocity.negative=abs(verg.velocity.negative),
#          is.transient.positive=abs(verg.velocity.positive)>15,
#          is.transient.negative=abs(verg.velocity.negative)>15) ->
#   z
# 


# z$expectedFR<- replace(z$expectedFR,z$expectedFR<0,0)

# z<- mutate(z,adjustedFR=sdf-expectedFR)

# z$adjustedFR<- replace(x$adjustedFR,x$adjustedFR<0,0)


rnum<- seq(0,250,by=5)
m1<- rnum
m2<- rnum
count<- 0
for (i in rnum) {
  count<- count+1
  z<- mutate(z,sdflead=lag(sdf,i))
  mcontrol<- lm(sdflead~verg.angle+verg.velocity,data=z)
  # mtest<- lm(sdflead~verg.angle+verg.velocity:transient.type,data=z)
  mtest<- lm(sdflead~verg.angle+verg.velocity:is.convergent.trans,data=filter(z,!transient.type=='divergence'))
  m1[count]=summary(mcontrol)$r.squared
  m2[count]=summary(mtest)$r.squared
  # print(summary(mcontrol)$r.squared)
  # print(summary(mtest)$r.squared)
}

lagtest<-data.frame(shift=rnum, control=m1, test=m2)
qplot(shift,test,data=lagtest,geom='text',label=shift)

bestlag<- lagtest$shift[lagtest$test==max(lagtest$test)]
z<- mutate(z,sdflead=lag(sdf,bestlag))

# z<- mutate(z, sdflead=lag(sdf,150))
mcontrol<- lm(sdflead~verg.angle+verg.velocity,data=filter(z,transient.type=='none'))
# mtest<- lm(sdflead~verg.angle+verg.velocity:transient.type,data=z)
# mtest<- lm(sdflead~verg.angle+verg.velocity.positive:is.transient.positive+verg.velocity.negative,data=z)

# z<- mutate(z, verg.vel.smooth=as.numeric(smooth(verg.velocity,kind='3RS3R')))
# mtest<- lm(sdflead~verg.angle+verg.vel.smooth:transient.type,data=z)
# mtest<-lm(sdflead~verg.angle+verg.velocity:transient.type,data=z)

mtest<- lm(sdflead~verg.angle+verg.velocity:is.convergent.trans,data=filter(z,!transient.type=='divergence'))

# mtest<-lm(sdflead~verg.angle+verg.velocity:transient.type,data=filter(z,!transient.type=='divergence'))

# zz<- mutate(z,transient.type=replace(transient.type,transient.type=='divergence','none'))

mvel<-lm(verg.velocity~sdflead+verg.angle-1,
         data=filter(z,
                     # transient.type=='none',
                     abs(verg.velocity)>4),
         sdflead>10)

z<- mutate(z,econtrol=predict(mcontrol,newdata=z),
           etest=predict(mtest,newdata=z),
           etest=replace(etest,etest<0,0),
           evel=predict(mvel,newdata=z),
           verg.angle.shifted=lag(verg.angle,bestlag),
           showrasters=replace(rasters,rasters<1,NA))


mtest<- lm(sdflead~verg.angle+evel,
           data=filter(z,!transient.type=='divergence'))


etest.prob<- z$etest/1000


z <-mutate(z,predict.spikes1=rbinom(length(etest.prob),1,etest.prob),
           predict.spikes1=replace(predict.spikes1,predict.spikes1<1,NA),
           predict.spikes2=rbinom(length(etest.prob),1,etest.prob),
           predict.spikes2=replace(predict.spikes2,predict.spikes2<1,NA),
           predict.spikes3=rbinom(length(etest.prob),1,etest.prob),
           predict.spikes3=replace(predict.spikes3,predict.spikes3<1,NA))

r.squared=cor(z$sdflead[200:nrow(z)],z$etest[200:nrow(z)])
r.squared=cor(z$sdflead[200:nrow(z)],z$econtrol[200:nrow(z)])
print(paste('R-Squared is:',round(r.squared^2,3)))

maxtime<-nrow(z)
windowsize<- 5000

manipulate(ggplot(filter(z,time>window,time<window+windowsize))+
             geom_area(aes(time,sdflead))+
             geom_line(aes(time,evel-150),color='orange')+
             geom_line(aes(time,verg.velocity-evel-200),color='maroon')+
             # geom_area(aes(time,econtrol),fill='orange',alpha=.5)+
             geom_area(aes(time,etest),fill='purple',alpha=.5)+
             geom_hline(yintercept=-115)+
             geom_hline(yintercept=-85)+
             
             geom_line(aes(time,verg.velocity-100),color='red')+
             # geom_point(aes(time,verg.velocity-100),color='red',size=3,
             # data=filter(z, time>window,time<window+windowsize,enhancenum>0))+
             # geom_line(aes(time,verg.vel.smooth-100),color='black')+
             geom_point(aes(time,verg.velocity-100,color=transient.type),size=1,
                        data=filter(z, time>window,time<window+windowsize,enhancenum>0))+
             geom_point(aes(time,showrasters+200),shape='|',size=2)+
             # geom_point(aes(time,predict.spikes1+220),shape='|',size=2,color='purple')+
             # geom_point(aes(time,predict.spikes2+225),shape='|',size=2,color='purple')+
             # geom_point(aes(time,predict.spikes3+230),shape='|',size=2,color='purple')+
             
             # geom_point(aes(time,enhancenum*0-50,color=transient.type))+
             
           # geom_line(aes(time,rep*10),color='red')+
           # geom_line(aes(time,lep*10),color='blue'),
           geom_line(aes(time,verg.angle.shifted*5),color='darkgreen',size=1.5),
           window=slider(0,maxtime-windowsize,step=windowsize)
)


###looking at all the fits
p<- readRDS('TransientModel.RDS')
library(tidyr)
p %>% separate(neuron, c('monkey','cellnum'),remove=FALSE) %>%
mutate(celltype=as.factor(as.numeric(cellnum)>100))-> p

ggplot(aes(celltype,lag),data=p)+geom_boxplot()+geom_jitter(aes(color=celltype))



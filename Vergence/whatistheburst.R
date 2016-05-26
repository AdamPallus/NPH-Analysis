#In this script I will be trying to model the activity of these neurons. 
#Specifically, I will look at the vergence velocity related activity

#Plan: 1) calcualte verg.angle related firing rate (FR) during fixation
#      2) subtract this from observed FR during movements
#      3) Is there a different relationship between verg.velocity and FR when there is a saccade?
source('Adamhelperfunctions.R')
library(dplyr)
library(ggplot2)
library(manipulate)

t<-readRDS('SOA-NRTP.RDS')
z<- filter(t,neuron=='Bee-25')
# m<- mm$m[[1]] #from above summary: model based on static FR ~ verg.angle

# z<- mutate(z,time=row_number(),
#            expectedFR= predict(m,newdat=z))

z<- mutate(z,time=row_number(),
           sdf=spikedensity(rasters,sd=30),
           verg.velocity=parabolicdiff(verg.angle,20),
           transient.type='none',
       transient.type=replace(transient.type, verg.velocity > 15,'convergence'),
       transient.type=replace(transient.type, verg.velocity < -15, 'divergence'),
       verg.velocity.fixed=replace(verg.velocity, transient.type=='divergence',verg.velocity*-1))


#split verg.velocity into positive and negative
z %>%
  mutate(verg.velocity.positive=replace(verg.velocity,verg.velocity<0,0),
         verg.velocity.negative=replace(verg.velocity,verg.velocity>0,0),
         verg.velocity.negative=abs(verg.velocity.negative),
         is.transient.positive=abs(verg.velocity.positive)>15,
         is.transient.negative=abs(verg.velocity.negative)>15) ->
  z



# z$expectedFR<- replace(z$expectedFR,z$expectedFR<0,0)

# z<- mutate(z,adjustedFR=sdf-expectedFR)

# z$adjustedFR<- replace(x$adjustedFR,x$adjustedFR<0,0)


rnum<- c(0, 5, 10,15, 20,25, 30, 35, 40,45, 50, 55, 60)
rnum<- seq(0,250,by=5)
m1<- rnum
m2<- rnum
count<- 0
for (i in rnum) {
  count<- count+1
  z<- mutate(z,sdflead=lag(sdf,i))
  mcontrol<- lm(sdflead~verg.angle+verg.velocity,data=z)
  mtest<- lm(sdflead~verg.angle+verg.velocity:transient.type,data=z)
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
mcontrol<- lm(sdflead~verg.angle+verg.velocity,data=z)
# mtest<- lm(sdflead~verg.angle+verg.velocity:transient.type,data=z)
mtest<- lm(sdflead~verg.angle+verg.velocity.positive:is.transient.positive+verg.velocity.negative,data=z)

# z<- mutate(z, verg.vel.smooth=as.numeric(smooth(verg.velocity,kind='3RS3R')))
# mtest<- lm(sdflead~verg.angle+verg.vel.smooth:transient.type,data=z)


z<- mutate(z,econtrol=predict(mcontrol,newdata=z),
           etest=predict(mtest,newdata=z),
           verg.angle.shifted=lag(verg.angle,bestlag),
           showrasters=replace(rasters,rasters<1,NA))



maxtime<-nrow(z)
windowsize<- 1000

manipulate(ggplot(filter(z,time>window,time<window+windowsize))+
             geom_area(aes(time,sdflead))+
             geom_line(aes(time,econtrol),color='orange')+
             geom_area(aes(time,etest),fill='purple',alpha=.5)+
             geom_hline(yintercept=-115)+
             geom_hline(yintercept=-85)+
             
             geom_line(aes(time,verg.velocity-100),color='red')+
             # geom_line(aes(time,verg.vel.smooth-100),color='black')+
             
             geom_point(aes(time,showrasters+200),shape='|',size=2)+
             
           # geom_line(aes(time,rep*10),color='red')+
           # geom_line(aes(time,lep*10),color='blue'),
           geom_line(aes(time,verg.angle.shifted*3.3),color='darkgreen',size=1.5),
           window=slider(0,maxtime-windowsize,step=windowsize)
)


###looking at all the fits
p<- readRDS('TransientModel.RDS')
library(tidyr)
p %>% separate(neuron, c('monkey','cellnum'),remove=FALSE) %>%
mutate(celltype=as.factor(as.numeric(cellnum)>100))-> p

ggplot(aes(celltype,lag),data=p)+geom_boxplot()+geom_jitter(aes(color=celltype))



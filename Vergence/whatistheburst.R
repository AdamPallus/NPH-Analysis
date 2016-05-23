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
           transient.type='none',
       transient.type=replace(transient.type, verg.velocity > 15,'convergence'),
       transient.type=replace(transient.type, verg.velocity < -15, 'divergence'))


# z$expectedFR<- replace(z$expectedFR,z$expectedFR<0,0)

# z<- mutate(z,adjustedFR=sdf-expectedFR)

# z$adjustedFR<- replace(x$adjustedFR,x$adjustedFR<0,0)


rnum<- c(0, 5, 10,15, 20,25, 30, 35, 40,45, 50, 55, 60)
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

z<- mutate(z, sdflead=lag(sdf,10))
mcontrol<- lm(sdflead~verg.angle+verg.velocity,data=z)
mtest<- lm(sdflead~verg.angle+verg.velocity:transient.type,data=z)


z<- mutate(z,econtrol=predict(mcontrol,newdata=z),
           etest=predict(mtest,newdata=z))



maxtime<-nrow(z)


manipulate(ggplot(filter(z,time>window,time<window+5000))+
             geom_area(aes(time,sdflead))+
             # geom_line(aes(time,elog),color='orange')+
             geom_area(aes(time,etest),fill='purple',alpha=.5)+
             geom_hline(yintercept=-115)+
             geom_hline(yintercept=-85)+
             geom_line(aes(time,verg.velocity-100),color='pink'),#+
           # geom_line(aes(time,rep*10),color='red')+
           # geom_line(aes(time,lep*10),color='blue'),
           # geom_line(aes(time,verg.angle*10),color='darkgreen'),
           window=slider(0,maxtime-5000,step=5000)
)


###looking at all the fits
p<- readRDS('TransientModel.RDS')
library(tidyr)
p %>% separate(neuron, c('monkey','cellnum'),remove=FALSE) %>%
mutate(celltype=as.factor(as.numeric(cellnum)>100))-> p

ggplot(aes(celltype,lag),data=p)+geom_boxplot()+geom_jitter(aes(color=celltype))



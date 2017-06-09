
#Setup----
library(ggplot2)
library(knitr)
library(tidyr)
library(broom)
# library(grid)
library(relaimpo)
library(leaps)
#library(data.table)
library(stringr)
library(dplyr)
library(cladoRcpp)
select<-dplyr::select
# source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')
source('ModelEnhancement.R')
library(manipulate)

t<- readRDS('SOA-NRTP.RDS')

#Measure----
chosenCell='Bee-211'
z<- filter(t,neuron==chosenCell)
z<- ModelEnhancement(z)
#Plotting----
# p<- filter(z,verg.amp>4)
p<- filter(z,total.verg.amp< -4)
p<- mutate(p,showrasters=replace(rasters,rasters<1,NA))
# p<- filter(z,predict.ratio>1,total.verg.amp>2)
goodsacs=unique(p$sacnum)
nsac=length(goodsacs)



manipulate(ggplot(filter(p,sacnum==goodsacs[sac]))+
             geom_point(aes(counter,showrasters*100),shape='|')+
             # geom_line(aes(counter,lev),color='blue')+
             # geom_line(aes(counter,rev),color='red')+
             # geom_line(aes(counter,levV),color='blue',linetype=2)+
             # geom_line(aes(counter,revV),color='red',linetype=2)+
             geom_line(aes(counter,(lev-rev)),color='darkblue',alpha=1)+
             # geom_line(aes(counter,verg.velocity),color='darkblue',linetype=2)+
             geom_line(aes(counter,(lep-rep)*10),color='darkgreen')+
             geom_line(aes(counter,conj.velocity/10))+
             geom_label(x=100,y=0,aes(label=unique(verg.lead)))+
             geom_label(x=100,y=20,aes(label=paste('sacnum = ',sacnum)))+
             # geom_label(x=100,y=-30,aes(label=paste('angle = ',round(r.angle,2))))+
             # geom_label(x=100,y=40,aes(label=paste('predict ratio = ',round(sum(predV)/sum(verg.velocity),2))))+
             geom_vline(aes(xintercept=200-verg.lead))+
             geom_area(aes(counter,sdf),alpha=1/10)
             # geom_line(aes(counter,predV),color='orange')+
             # geom_line(aes(counter,verg.velocity-predV),color='hotpink')+
             # geom_line(aes(counter,predict.verg.change*10),color='darkred')
           ,
           sac=slider(1,nsac,step=1))

zs<- summarize_each(group_by(z,sacnum),funs(first))

zs<- filter(zs,abs(total.verg.amp)<15,abs(verg.amp)<15)

qplot(verg.amp,peakFR,data=zs)+
  ylab('Peak Firing Rate (spikes/s)')+
  xlab('Saccadic vergence change (deg)')

qplot(total.verg.amp,peakFR,data=zs)+
  ylab('Peak Firing Rate (spikes/s)')+
  xlab('Total vergence change (deg)')

ggplot(zs)+
  geom_point(aes(verg.amp,max.predict.velocity),color='black')+
  xlab('Saccadic vergence change (deg)')+
  ylab('Predicted Peak Vergence Velocity (deg/s)')

ggplot(zs)+
  geom_point(aes(total.verg.amp,max.predict.velocity),color='black')+
  xlab('Total vergence change (deg)')+
  ylab('Predicted Peak Vergence Velocity (deg/s)')

ggplot(zs)+
  geom_point(aes(verg.amp,max.predict.velocity),color='black')+
  geom_point(aes(verg.amp,max.verg.velocity),color='hotpink')+
  # coord_cartesian(xlim=c(0,8))+
  annotate('text',x=4,y=125,label='Actual',color='hotpink')+
  annotate('text',x=4,y=40,label='Predicted')+
  xlab('Saccadic vergence change (deg)')+
  ylab('Vergence Velocity (deg/s)')
  

qplot(total.verg.amp,max.predict.velocity,data=zs)+
  geom_point(aes(total.verg.amp,max.verg.velocity),color='hotpink')+
  # coord_cartesian(xlim=c(0,12))+
  annotate('text',x=4,y=125,label='Actual',color='hotpink')+
  annotate('text',x=4,y=40,label='Predicted')+
  xlab('Total vergence change (deg)')+
  ylab('Vergence Velocity (deg/s)')

qplot(total.verg.amp,max.verg.velocity-max.predict.velocity,data=zs)+
  xlab('Total Vergence Change (deg)')+
  ylab('Actual - Predicted peak vergence velocity')+
  ggtitle("Vergence Velocity not accounted for by model")


qplot(verg.amp,max.verg.velocity-max.predict.velocity,data=filter(zs,verg.amp>0))+
  stat_smooth(method='lm')+
  xlab('Saccadic Vergence Change (deg)')+
  ylab('Actual - Predicted peak vergence velocity')

qplot(total.verg.amp,max.verg.velocity-max.predict.velocity,data=filter(zs,total.verg.amp>0))+
  stat_smooth(method='lm')+
  xlab('Total Vergence Change (deg)')+
  ylab('Actual - Predicted peak vergence velocity')

qplot(verg.amp,max.verg.velocity-max.predict.velocity,data=filter(zs,verg.amp>1))+
  stat_smooth(method='lm')+
  xlab('Saccadic Vergence Change (deg)')+
  ylab('Actual - Predicted peak vergence velocity')

qplot(total.verg.amp,max.verg.velocity-max.predict.velocity,data=filter(zs,verg.amp>1))+
  stat_smooth(method='lm')+
  xlab('Total Vergence Change (deg)')+
  ylab('Actual - Predicted peak vergence velocity')

qplot(r,data=xxx)+
  xlab('Slope of vergence change ~ actual-predicted peak vergence velocity')+
  ylab('Number of cells')


qplot(r,data=filter(xxx,modR2>0.1),bins=15)+
  xlab('Slope of vergence change ~ actual-predicted peak vergence velocity')+
  ylab('Number of cells')


qplot(verg.amp,predict.ratio,data=filter(zs,verg.amp>1,abs(predict.ratio)<5))+
  stat_smooth()+
  ylab('Vergence change ratio: predicted:actual')+
  xlab('Saccadic Vergence Change (deg)')+
  ggtitle('Fraction of vergence change accounted for')

qplot(total.verg.amp,predict.ratio,data=filter(zs,total.verg.amp>1,abs(predict.ratio)<5))+
  stat_smooth()+
  ylab('Vergence change ratio: predicted:actual')+
  xlab('Total Vergence Change (deg)')+
  ggtitle('Fraction of vergence change accounted for')

qplot(total.verg.amp,predict.ratio,data=zs)+
  stat_smooth()+
  ylim(c(-1,10))+
  xlim(c(1,12))+
  ylab('Vergence change ratio: predicted:actual')+
  xlab('Total Vergence Change (deg)')+
  ggtitle('Fraction of vergence change accounted for')

zs<- mutate(zs,Direction=cut(abs(r.angle),c(0,45,135,180)))
levels(zs$Direction)<-c('Horizontal','Vertical','Horizontal')

qplot(total.verg.amp,log(predict.ratio),
      color=Direction,
      data=filter(zs,total.verg.amp>1,abs(predict.ratio)<5))+
  stat_smooth(method='lm')+
  ylab('Log(Vergence change ratio) predicted:actual')+
  xlab('Total Vergence Change (deg)')+
  ggtitle('Effect of Direction on model prediction')

zs<- mutate(zs,verg.enhance=max.verg.velocity-max.predict.velocity)

m<- lm(verg.enhance~verg.amp,data=filter(zs,verg.amp>0))
summary(m)

m<- lm(verg.enhance~total.verg.amp,data=filter(zs,total.verg.amp>0))
summary(m)


ggplot(p,aes(group=sacnum))+
  geom_line(aes(counter,(lev-rev)),color='darkblue',alpha=1)+
  geom_line(aes(counter,(lep-rep)*10),color='darkgreen')+
  geom_line(aes(counter,conj.velocity/10))+
  geom_vline(aes(xintercept=200-verg.lead))+
  geom_line(aes(counter,predV),color='orange')+
  # geom_line(aes(counter,verg.velocity-predV),color='hotpink')+
  geom_line(aes(counter,predict.verg.change*10),color='darkred')

ps<- summarize_each(group_by(p,sacnum),funs(first))

qplot(verg.lead,min.verg.velocity,data=ps)+
  xlab('Vergence Lead (ms)')+
  ylab('Minimum Vergence Velocity (deg/s) npk')


qplot(verg.lead,min.verg.velocity,data=ps)+
  geom_point(aes(verg.lead,max.verg.velocity),color='hotpink')+
  xlab('Vergence Lead (ms)')+
  ylab('Peak Vergence Velocity (deg/s)')+
  annotate('text',x=20,y=125,label='Positive',color='hotpink')+
  annotate('text',x=20,y=-40,label='Negative')

p %>%
  
  group_by(counter) %>%
  summarise_each(funs(mean))->
  pc

ggplot(pc)+
  geom_line(aes(counter,(lev-rev)),color='darkblue',alpha=1)+
  geom_line(aes(counter,(lep-rep)*10),color='darkgreen')+
  geom_line(aes(counter,conj.velocity),color='gray')+
  geom_vline(aes(xintercept=200-verg.lead))+
  geom_line(aes(counter,predV),color='orange')+
  # geom_line(aes(counter,verg.velocity-predV),color='hotpink')+
  geom_line(aes(counter,predict.verg.change*10),color='darkred')+
  coord_cartesian(ylim=c(0,120))+
  ggtitle('Average of saccades with saccadic vergence amp > 4')+
  annotate('text',x=160,y=100,label='Vergence Lead',color='black')+
  annotate('text',x=400,y=100,label='Vergence Angle',color='darkgreen')+
  annotate('text',x=400,y=60,label='Predicted Vergence Angle',color='darkred')+
  annotate('text',x=240,y=110,label='Pythagorean Velocity',color='gray')+
  annotate('text',x=260,y=10,label='Predicted Vergence Velocity',color='orange')+
  annotate('text',x=320,y=30,label='Vergence Velocity',color='darkblue')+
  xlab('Time (ms)')+
  ylab('Velocity (deg/s) and Position*10 (deg)')

# p2<- filter(z,total.verg.amp>4)
# p2 %>%
#   group_by(counter) %>%
#   summarise_each(funs(mean))->
#   pc
# 
# ggplot(pc)+
#   geom_line(aes(counter,(lev-rev)),color='darkblue',alpha=1)+
#   geom_line(aes(counter,(lep-rep)*10),color='darkgreen')+
#   geom_line(aes(counter,conj.velocity),color='gray')+
#   geom_vline(aes(xintercept=200-verg.lead))+
#   geom_line(aes(counter,predV),color='orange')+
#   # geom_line(aes(counter,verg.velocity-predV),color='hotpink')+
#   geom_line(aes(counter,predict.verg.change*10),color='darkred')+
#   coord_cartesian(ylim=c(0,120))+
#   ggtitle('Average of saccades with Total Vergence Amp > 4')+
#   annotate('text',x=160,y=100,label='Vergence Lead',color='black')+
#   annotate('text',x=400,y=100,label='Vergence Angle',color='darkgreen')+
#   annotate('text',x=400,y=60,label='Predicted Vergence Angle',color='darkred')+
#   annotate('text',x=240,y=110,label='Pythagorean Velocity',color='gray')+
#   annotate('text',x=260,y=10,label='Predicted Vergence Velocity',color='orange')+
#   annotate('text',x=320,y=30,label='Vergence Velocity',color='darkblue')+
#   xlab('Time (ms)')+
#   ylab('Velocity (deg/s) and Position*10 (deg)')



pp<- filter(p,total.predict.amp>5,total.predict.amp<7)
pr<- filter(p,total.verg.amp>5, total.verg.amp<7)

ggplot(pp,aes(group=sacnum))+
  # geom_line(aes(counter,(lev-rev)),color='darkblue',alpha=1)+
  # geom_line(aes(counter,(lep-rep)*10),color='darkgreen')+
  # geom_line(aes(counter,conj.velocity/10))+
  geom_vline(aes(xintercept=200-verg.lead))+
  geom_line(aes(counter,predV),color='orange')+
  # geom_line(aes(counter,verg.velocity-predV),color='hotpink')+
  geom_line(aes(counter,predict.verg.change*10),color='darkred')

ggplot(pr,aes(group=sacnum))+
  geom_line(aes(counter,(lev-rev)),color='darkblue',alpha=1)+
  geom_line(aes(counter,(lep-rep)*10),color='darkgreen')+
  # geom_line(aes(counter,conj.velocity/10))+
  geom_vline(aes(xintercept=200-verg.lead))

  # geom_line(aes(counter,predV),color='orange')+
  # geom_line(aes(counter,verg.velocity-predV),color='hotpink')+
  # geom_line(aes(counter,predict.verg.change*10),color='darkred')
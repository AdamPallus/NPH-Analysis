
chosenCell='Bee-218'
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

t<- readRDS('SOA-NRTP.RDS')

#Measure----
z<- filter(t,neuron==chosenCell)
# filter(cellnum>100,cellnum<125,monkey=='Bee')%>%

# d<- dynamiclead2(p=z,formula = 'verg.velocity~sdflag+verg.angle',lags=seq(0,40,by=2))

bufferlength<- 200
saccade.length<- 150
z%>%
  group_by(neuron) %>%
  mutate(sdf20=lag(sdf,26), #d comes from dynamiclead2 function
         lev=parabolicdiff(lep,20),
         rev=parabolicdiff(rep,20),
         levV=parabolicdiff(lepV,20),
         revV=parabolicdiff(repV,20),
         verg.velocity=lev-rev,
         conj.velocity=sqrt(((rev+lev)/2)^2+((revV+levV)/2)^2)) %>%
  # conj.velocity=sqrt((rev^2+lev^2)/2)+sqrt((revV^2+levV^2)/2)) %>%
  ungroup() %>%
  mutate(time=row_number()) %>%
  do(joinsaccadesuniform(.,buffer=bufferlength,threshold=20,saccade.length=saccade.length))->
  # do(joinsaccades(.,buffer=bufferlength,threshold=20))->
  z


z %>%
  group_by(sacnum) %>%
  mutate(realsaccade=counter>bufferlength & counter< bufferlength + saccade.dur) %>%
  ungroup()->
  z

mod<- lm('verg.velocity~sdf20+verg.angle',data=filter(z,!realsaccade,verg.velocity>0))


z<-mutate(z, predV=predict(mod,newdata=z))


z %>%
  # group_by(neuron) %>%
  mutate(issaccade=!is.na(sacnum)) %>%
  filter(issaccade,saccade.dur>20) %>%
  group_by(neuron,sacnum) %>%
  mutate(cverg=abs(verg.velocity)>3,
         cverg=replace(counter,cverg,NA)) %>%
  mutate(peakFR=max(sdf20),
         saccade.dur=first(saccade.dur), #was originally a summary
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
         initial.verg.angle=verg.angle[bufferlength],
         verg.lead=bufferlength-max(cverg[100:bufferlength],na.rm=T),
         verg.lead=replace(verg.lead,is.na(verg.lead),0),
         predict.ratio=sum(predV)/sum(verg.velocity))->
  z

z %>%
  group_by(sacnum) %>%
  mutate(predict.verg.change=cumsum(predV)/1000+first(verg.angle),
         max.predict.velocity=max(predV,na.rm=T),
         total.verg.amp=last(verg.angle)-first(verg.angle))->
  z
#Plotting----
p<- filter(z,verg.amp>4)
# p<- filter(z,predict.ratio>1,total.verg.amp>2)
goodsacs=unique(p$sacnum)
nsac=length(goodsacs)


manipulate(ggplot(filter(p,sacnum==goodsacs[sac]))+
             geom_line(aes(counter,lev),color='blue')+
             geom_line(aes(counter,rev),color='red')+
             geom_line(aes(counter,levV),color='blue',linetype=2)+
             geom_line(aes(counter,revV),color='red',linetype=2)+
             geom_line(aes(counter,(lev-rev)),color='darkblue',alpha=1)+
             geom_line(aes(counter,verg.velocity),color='darkblue',linetype=2)+
             geom_line(aes(counter,(lep-rep)*10),color='darkgreen')+
             geom_line(aes(counter,conj.velocity/10))+
             geom_label(x=100,y=0,aes(label=unique(verg.lead)))+
             geom_label(x=100,y=20,aes(label=paste('sacnum = ',sacnum)))+
             geom_label(x=100,y=-20,aes(label=paste('angle = ',round(r.angle,2))))+
             geom_label(x=100,y=25,aes(label=paste('predict ratio = ',round(sum(predV)/sum(verg.velocity),2))))+
             geom_vline(aes(xintercept=200-verg.lead))+
             geom_line(aes(counter,predV),color='orange')+
             geom_line(aes(counter,verg.velocity-predV),color='hotpink')+
             geom_line(aes(counter,predict.verg.change*10),color='darkred')
           ,
           sac=slider(1,nsac,step=1))

zs<- summarize_each(group_by(z,sacnum),funs(first))

qplot(total.verg.amp,peakFR,data=zs)
qplot(verg.amp,peakFR,data=zs)
qplot(verg.amp,max.predict.velocity,data=zs)+
  geom_point(aes(verg.amp,max.verg.velocity),color='hotpink')+
  coord_cartesian(xlim=c(0,8))

qplot(total.verg.amp,max.predict.velocity,data=zs)+
  geom_point(aes(total.verg.amp,max.verg.velocity),color='hotpink')+
  coord_cartesian(xlim=c(0,12))

qplot(total.verg.amp,max.verg.velocity-max.predict.velocity,data=zs)

qplot(verg.amp,max.verg.velocity-max.predict.velocity,data=filter(zs,verg.amp>0))+
        stat_smooth(method='lm')

qplot(verg.amp,predict.ratio,data=filter(zs,verg.amp>1,abs(predict.ratio)<5))+stat_smooth()
zs<- mutate(zs,angle.bins=cut(abs(r.angle),c(0,45,135,180)))

qplot(total.verg.amp,log(predict.ratio),
      color=angle.bins,
      data=filter(zs,total.verg.amp>1,abs(predict.ratio)<5))+
  stat_smooth(method='lm')

zs<- mutate(zs,verg.enhance=max.verg.velocity-max.predict.velocity)

m<- lm(verg.enhance~verg.amp,data=filter(zs,verg.amp>0))
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
qplot(verg.lead,min.verg.velocity,data=ps)

t %>%
  # filter(neuron %in% c('Bee-211','Bee-113')) %>%
  group_by(neuron) %>%
  do(CalculateEnhancement(.))->
  xxx

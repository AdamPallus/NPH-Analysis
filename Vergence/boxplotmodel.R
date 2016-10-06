


library(dplyr)
library(ggplot2)
library(manipulate)
source('~/GitHub/NPH-Analysis/Vergence/Adamhelperfunctions.R')

dynamiclead<-function(p,lags=seq(10,300,by=10)) {
  
  rsq<-NULL
  for (i in 1:length(lags)) {
    if (lags[i] > 0){
      p$sdflag<-dplyr::lag(p$sdf,lags[i])
    }
    else{
      p$sdflag<-dplyr::lead(p$sdf,lags[i]*-1)
    }
    
    rsq[i]<- summary(lm(sdflag~verg.angle+verg.velocity,data=p))$r.squared
  }
  return(rsq)
  # return(lags[rsq==max(rsq)])
}

# z<- readRDS('MeasuredDataBee-smooth.RDS')
# z<- readRDS('Measured and Smoothed Ozette.RDS')

xx<- dplyr::filter(z,neuron=='Ozette-118')

# xx$sdf<- spikedensity(xx$rasters,20)

xx<- mutate(xx,isslow= is.na(sacnum) & abs((lev+rev)/2) < 2,
            showslow=replace(isslow,isslow<1,NA),
            verg.velocity.lag=lag(lev,3)-rev) 

leadseq=seq(10,100,by=5)
r<- dynamiclead(xx, leadseq)
plot(leadseq,r)

leadtime<-leadseq[r==max(r)]

# leadtime<- 50

slowfit<- lm(lag(sdf,leadtime)~(verg.angle)+(verg.velocity),data=filter(xx,isslow))

summary(slowfit)

xx<- ungroup(xx)
xx<- mutate(xx,slow.prediction=predict(slowfit,newdata=xx),
            slow.prediction=replace(slow.prediction,slow.prediction<0,0),
            time=time-first(time))

xxx<- xx %>% group_by(time) %>% summarize_each(funs(first))

xxx$bin.velocity=cut(xxx$verg.velocity,c(seq(-140,140,by=20)))
xxx$bin.position=cut(xxx$verg.angle,c(seq(0,18,by=1)))
#This plots the ms by ms data vs model fit
ggplot(xx)+geom_point(aes(verg.velocity,lag(sdf,leadtime)),alpha=1/20)+
  geom_point(aes(verg.velocity,slow.prediction),alpha=1/20,color='orange')+
  scale_y_continuous(limits=c(0,NA))+
  # scale_x_continuous(limits=c(0,NA))+
  facet_wrap(~isslow,scales='free_x')

#this plots the binned comparison of model to real data
ggplot(filter(xxx,!is.na(bin.velocity)))+
  geom_boxplot(aes(bin.velocity,lag(sdf,leadtime)))+
  geom_boxplot(aes(bin.velocity,slow.prediction),fill='orange', alpha=1/2)


#This plots the static vergence sensitivity
ggplot(filter(xx,isslow))+geom_point(aes(verg.angle,lag(sdf,leadtime)),alpha=1/20)+
  geom_point(aes(verg.angle,slow.prediction),alpha=1/20,color='orange')+
  scale_y_continuous(limits=c(0,NA))+
  scale_x_continuous(limits=c(-2.5,18))


#this plots the binned comparison of model to real data
ggplot(filter(xxx,!is.na(bin.position)))+
  geom_boxplot(aes(bin.position,lag(sdf,leadtime)))+
  geom_boxplot(aes(bin.position,slow.prediction),fill='orange', alpha=1/2)


manipulate(ggplot(filter(xxx,time>=window,time<window+5000))+
             geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,(rev+lev)/2))+
             geom_line(aes(time,lev-rev),color='pink')+
             # geom_point(aes(time,showslow*100))+
             # geom_hline(yintercept = c(208,233))+
             geom_line(aes(time,slow.prediction),color='orange'),
           # geom_line(aes(time,lag(lev,3)-rev),color='pink'),
           window=slider(1,max(xx$time-5000),step=5000))
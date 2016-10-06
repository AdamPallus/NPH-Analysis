

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

library(dplyr)
library(ggplot2)
library(manipulate)



# z<- readRDS('MeasuredDataBee-smooth.RDS')
# z<- readRDS('Measured and Smoothed Ozette.RDS')
z %>% group_by(neuron,sacnum) %>% summarize(verg.angle=first(verg.angle),peakfr=max(sdf)) ->sfp
qplot(verg.angle,peakfr,data=sfp)+facet_wrap(~neuron)

xx<- dplyr::filter(z,neuron=='Bee-112')

# xx<- mutate(xx,isslow= is.na(counter) | counter < -50 | counter > 200,abs(verg.velocity<20))
xx<- mutate(xx,isslow= is.na(sacnum) & abs((lev+rev)/2) < 2,
            showslow=replace(isslow,isslow<1,NA),
            verg.velocity.lag=lag(lev,3)-rev) 

slowfit<- lm(lag(sdf,50)~(verg.angle)+(verg.velocity),data=filter(xx,isslow))

summary(slowfit)

xx<- ungroup(xx)
xx<- mutate(xx,slow.prediction=predict(slowfit,newdata=xx),
            slow.prediction=replace(slow.prediction,slow.prediction<0,0),
            time=time-first(time))

xxx<- xx %>% group_by(time) %>% summarize_each(funs(first))
                      
manipulate(ggplot(filter(xxx,time>=window,time<window+5000))+
             geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,(rev+lev)/2))+
             geom_line(aes(time,lev-rev),color='pink')+
             # geom_point(aes(time,showslow*100))+
             geom_hline(yintercept = c(208,233))+
  geom_line(aes(time,slow.prediction),color='orange'),
    # geom_line(aes(time,lag(lev,3)-rev),color='pink'),
  window=slider(1,max(xx$time-5000),step=5000))

ggplot(xx)+geom_point(aes(verg.velocity,lag(sdf,50)),alpha=1/20)+
  geom_point(aes(verg.velocity,slow.prediction),alpha=1/20,color='orange')+
  scale_y_continuous(limits=c(0,NA))+
  # scale_x_continuous(limits=c(0,NA))+
  facet_wrap(~isslow,scales='free_x')

ggplot(filter(xx,isslow))+geom_point(aes(verg.angle,lag(sdf,50)),alpha=1/20)+
  geom_point(aes(verg.angle,slow.prediction),alpha=1/20,color='orange')+
  scale_y_continuous(limits=c(0,NA))+
  scale_x_continuous(limits=c(-2.5,15))


xxx$bin.velocity=cut(xxx$verg.velocity,c(seq(-140,140,by=20)))

ggplot(filter(xxx,!is.na(bin.velocity)))+
  geom_boxplot(aes(bin.velocity,lag(sdf,50)))+
  geom_boxplot(aes(bin.velocity,slow.prediction),fill='orange', alpha=1/2)


xx<-mutate(xx,showrasters=replace(rasters,rasters<1,NA),
           cv=(rev+lev)/2)


xx %>%
  mutate(g=floor(time/200000)) %>%
  group_by(g) %>%
  mutate(rev=parabolicdiff(rep,20),
         lev=parabolicdiff(lep,20),
         revV=parabolicdiff(repV,20),
         levV=parabolicdiff(lepV,20),
         # conj.velocity=sqrt((rev^2+lev^2)/2)+sqrt((revV^2+levV^2)/2),
         verg.velocity=lev-rev) ->
  xx

p<- filter(xx,r.amp>4,saccade.dur<100,abs(verg.amp)>4)
p<- filter(xx,r.amp>4,saccade.dur<100,verg.amp>3)
# p<- filter(xx,r.amp>4)
# p<- filter(xx,r.amp>4,saccade.dur<150,verg.amp< -4)

goodsacs<- unique(p$sacnum)

nsac=length(goodsacs)

manipulate(ggplot(filter(xx,sacnum==goodsacs[sac]))+
             geom_line(aes(counter,verg.velocity),color='purple')+
             # geom_line(aes(counter,conj.velocity),color='brown')+
             geom_line(aes(counter,cv),color='brown')+
             geom_line(aes(counter,rev),color='red')+
             geom_line(aes(counter,lev),color='blue')+

             # geom_hline(yintercept = c(-2,2))+
             geom_line(aes(counter,verg.angle*5),color='darkgreen')+
             geom_point(aes(counter,showrasters*100),shape='|',size=3)+
             geom_area(aes(counter,sdf),alpha=0.2)+

             coord_cartesian(ylim=c(-300,300)),
           sac=slider(1,nsac,step=1)
)

#test timing hypothesis for transient


manipulate(ggplot(filter(xx,sacnum==goodsacs[sac]))+
             geom_line(aes(counter,lev-rev),color='purple',size=1.5)+
             # geom_line(aes(counter,conj.velocity),color='brown')+
             geom_line(aes(counter,(lev+rev)/2),color='brown')+
             geom_line(aes(counter,rev),color='red')+
             # geom_line(aes(counter,lag(rev,2)),color='red',linetype=2)+
             geom_line(aes(counter,lev),color='blue')+
             # geom_line(aes(counter,lev-lag(lev,3)),color='orange')+
             # geom_line(aes(counter,lag(rev,3)-rev),color='magenta')+
             # geom_line(aes(counter,verg.velocity-(lev-lag(lev,3))),color='orange',linetype=2)+
             # geom_line(aes(counter,verg.velocity-(lag(rev,3)-rev)),color='magenta',linetype=2)+
             geom_line(aes(counter,lev-lag(rev,4)),color='red',linetype=2)+
             geom_line(aes(counter,lag(lev,4)-rev),color='blue',linetype=2)+
             geom_line(aes(counter,lev-lag(rev,3)),color='red',linetype=3)+
             geom_line(aes(counter,lag(lev,3)-rev),color='blue',linetype=3)+
             geom_line(aes(counter,lev-lag(rev,2)),color='red',linetype=4)+
             geom_line(aes(counter,lag(lev,2)-rev),color='blue',linetype=4)+
             geom_line(aes(counter,lev-lag(rev,1)),color='red',linetype=5)+
             geom_line(aes(counter,lag(lev,1)-rev),color='blue',linetype=5)+
             geom_line(aes(counter,lev-lag(rev,6)),color='red',linetype=6)+
             geom_line(aes(counter,lag(lev,6)-rev),color='blue',linetype=6)+
             # geom_hline(yintercept = c(-2,2))+
             geom_line(aes(counter,verg.angle*5),color='darkgreen')+
             geom_point(aes(counter,showrasters*100),shape='|',size=3)+
             geom_area(aes(counter,sdf),alpha=0.2),#+
             
             # coord_cartesian(ylim=c(-100,100),xlim=c(-50,100)),
           sac=slider(1,nsac,step=1)
)

gp<- ggplot(filter(z,cellnum>100,cellnum<=111))+geom_point(aes(verg.velocity,lag(sdf,50)),alpha=1/30)+
  scale_y_continuous(limits=c(0,NA))+
  scale_x_continuous(limits=c(-200,200))+
  facet_wrap(~neuron,ncol=4)

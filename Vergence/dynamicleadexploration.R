dynamicleadTEST<-function(p,lags=seq(10,300,by=10),formula='rev+lev') {
  
  formula=paste('sdflag~',formula)
  
  rsq<-NULL
  for (i in 1:length(lags)) {
    if (lags[i] > 0){
      # p<- mutate(p,sdflag=lag(sdf,lags[i]))
      p$sdflag<-dplyr::lag(p$sdf,lags[i])
    }
    else{
      # p<- mutate(p,sdflag=lead(sdf,lags[i]*-1))
      p$sdflag<-dplyr::lead(p$sdf,lags[i]*-1)
    }
    
    rsq[i]<- summary(lm(formula=formula,data=filter(p,sdflag>10)))$r.squared
  }
  return(rsq)
  # # return(lags[rsq==max(rsq)])
  # bestlag=lags[rsq==max(rsq)]
  # p$dynamiclead<- bestlag
  # if (bestlag > 0){
  #   p$sdftest<-dplyr::lag(p$sdf,bestlag)
  # }
  # else{
  #   p$sdftest<-dplyr::lead(p$sdf,bestlag*-1)
  # }
  # return(p)
  
}

lags<-seq(5,200,by=5)
va<- dynamicleadTEST(filter(tx,time>104000),formula='verg.angle',lags=lags)
vv<- dynamicleadTEST(filter(tx,time>104000),formula='verg.angle+verg.velocity',lags=lags)
# vc<- dynamicleadTEST(filter(tx,time>104000),formula='verg.angle+verg.velocity',lags=lags)
qplot(lags,va)
qplot(lags,vv)

window_size=1000
manipulate(ggplot(filter(tx,time>=window,time<window+window_size))+
             geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
             # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
             geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
             geom_line(aes(time,rep+200),color='darkred')+
             geom_line(aes(time,lep+200),color='blue')+
             geom_line(aes(time,repV+220),color='darkred',linetype=2)+
             geom_line(aes(time,lepV+220),color='blue',linetype=2)+
             geom_line(aes(time,(lev+rev)/2),color='maroon')+
             geom_line(aes(time,(levV+revV)/2),color='maroon',linetype=2)+
             
             geom_point(aes(time,showrasters+50),shape='|')+
             # geom_line(aes(time,slow.prediction),color='orange')+
             # geom_line(aes(time,(rev+lev)/2),color='darkred')+
             # geom_line(aes(time,(revV+levV)/2),color='red')+
             # theme_bw()+
             ylim(c(-100,300))
           ,
           window=slider(window_size,max(tx$time-window_size),step=window_size))



#------figuring out the lag thing

ChosenCell='Bee-104'
gc<- filter(t,neuron==ChosenCell) #saved as a separate variable so it can be used later
gc<-mutate(gc,time=row_number(), showrasters=replace(rasters,rasters<1,NA))

gc<- mutate(gc,sdf20=lag(sdf,20))

m<- lm(sdf20~verg.angle+verg.velocity,data=gc)
lags<-seq(5,200,by=5)
lags<- seq(1,60,by=1)
d<- dynamicleadTEST(tx,formula='verg.angle+verg.velocity',lags=lags)
qplot(lags,d)

window_size <-500
step_size <- 200
manipulate(ggplot(filter(gc,time>=window,time<window+window_size))+
             geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
             # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
             geom_line(aes(time,(lep-rep)*10+50),color='darkgreen')+
             # geom_line(aes(time,rep+200),color='darkred')+
             # geom_line(aes(time,lep+200),color='blue')+
             # geom_line(aes(time,repV+220),color='darkred',linetype=2)+
             # geom_line(aes(time,lepV+220),color='blue',linetype=2)+
             # geom_line(aes(time,(lev+rev)/2),color='maroon')+
             # geom_line(aes(time,(levV+revV)/2),color='maroon',linetype=2)+
             
             geom_point(aes(time,showrasters+50),shape='|')+
             # geom_line(aes(time,slow.prediction),color='orange')+
             # geom_line(aes(time,(rev+lev)/2),color='darkred')+
             # geom_line(aes(time,(revV+levV)/2),color='red')+
             # theme_bw()+
             ylim(c(-100,300))
           ,
           window=slider(window_size,max(gc$time-window_size),step=step_size))

#---
t<- readRDS('enhancemarked2.RDS')

t<- filter(t,dynamiclead>50)
unique(t$neuron)
lags<-seq(-50,50,by=5)
t %>%
  group_by(neuron) %>%
  do(dynamicleadTEST(p=.,lags=lags,formula='verg.velocity'))->
  # do(dynamiclead(p=.,lags=lags,formula='verg.velocity'))->
  t
t %>%
  group_by(neuron) %>%
  summarize(lags=first(dynamiclead)) ->
  p
qplot(lags,data=p)

#-----------
ggplot(tp)+
  geom_point(aes(verg.angle,Slow.Vergence),size=3,alpha=0.2)+
  geom_vline(xintercept=0,linetype=2)+
  stat_smooth(aes(verg.angle,Slow.Vergence),data=filter(tp,verg.angle>0),method='lm')+
  xlab('Sensitivity to Vergence Angle')+
  ylab('Sensitivity to Vergence Velocity')+
  coord_fixed()

ggplot(tp)+
  geom_point(aes(Divergence,Slow.Vergence),size=3,alpha=0.2)+
  geom_abline(slope=1)+
  geom_vline(xintercept=0,linetype=2)+
  xlab('Vergence Velocity Sensitivity during Enhanced Divergence')+
  ylab('Vergence Velocity Sensitivity during Slow Vergence Movements')+
  coord_fixed()

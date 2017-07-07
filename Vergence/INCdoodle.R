library(manipulate)
z<- filter(t,neuron=='Kopachuck-912')

# bufferlength=10
# z %>%
#   mutate(stimes=markSaccades(conj.velocity,buffer=bufferlength,threshold=20))->
#   z

parabolic_n=10
z %>%
  ungroup()%>%
  mutate(lev=parabolicdiff(lep,parabolic_n),
         levV=parabolicdiff(lepV,parabolic_n),
         rev=parabolicdiff(rep,parabolic_n),
         revV=parabolicdiff(repV,parabolic_n),
         conj.velocity=sqrt(((rev+lev)/2)^2+((revV+levV)/2)^2),
         dsnum=markSaccadesDouble(conj.velocity),
         sdf=spikedensity(rasters,10),
         sdf10=lag(sdf,10),
         conj.vert=(repV+lepV)/2,
         conj.vert.vel=(revV+levV)/2,
         conj.hor=(rep+lep)/2,
         conj.hor.v=(rev+lev)/2) ->
  z

z%>%
  group_by(dsnum) %>%
  mutate(sd.conj.velocity=sd(conj.velocity),
         dur=n(),
         asleep=sd.conj.velocity>7.5 & dsnum<0 || dur>2000)->
  z

windowsize=10000
manipulate(
  {
    d<-filter(z,time>window,time<window+windowsize)
    d<-mutate(d,showrasters=replace(rasters,rasters<1,NA))
    d<-mutate(d,sdf=spikedensity(rasters,10))
    ggplot(d)+
      geom_line(aes(time,rep),color='red')+
      geom_line(aes(time,lep),color='blue')+
      geom_line(aes(time,rev/10),color='red',linetype=2)+
      geom_line(aes(time,lev/10),color='blue',linetype=2)+
      geom_point(aes(time,sign(dsnum)*9,color=asleep))+
      geom_point(aes(time,showrasters-20),shape='|',size=3)
    # +
      # geom_line(aes(time,sdf))
      
  },
  window=slider(1,nrow(z),initial = 1,step=windowsize)
)
  

#identify sleep using standard deviation of non-saccadic periods?

z %>% 
  group_by(stimes)%>%
  mutate(sdvel=sd(conj.velocity),
         dur=n()) ->
  z

markSaccadesDouble<- function(v, threshold1=60,threshold2=20){
  require(dplyr)
  datalength<-length(v)
  mindur<- 1
  i<-which(abs(v)>threshold2) #find all the times when speed is above the lower threshold
  sacoff<-which(diff(i)>mindur) #minimum duration of an accepted saccade
  sacon<-c(1,sacoff+1) #first saccade
  sacoff<-c(sacoff,length(i)) #end of last saccade
  event.onset<-i[sacon] #get actual times
  event.offset<-i[sacoff] 
  
  stimes<- data.frame(event.onset,event.offset)
  nsaccades=nrow(stimes)
  
  jsac<- function(stimes){
    summary(stimes)
    #input should be an array of length 2: c(onsettime,offsettime, saccade.number,saccade.dur)
    df<- data.frame(time=stimes[[1]]:stimes[[2]])
    df$event<- stimes[[4]]
    df$event.dur<- stimes[[3]]
    return(df)
    # return(stimes[[1]]:stimes[[2]])
  }
  
  stimes %>%
    mutate(dur=event.offset-event.onset,
           s=row_number())->
    stimes
  x<-rbindlist(apply(stimes,1,jsac))
 
  v<- data.frame(v=v)
  v<- mutate(v, time=row_number())
  
  xx<- left_join(v,x,by='time')
  
  xx %>%
    group_by(event) %>%
    summarize(max.vel=max(abs(v))) %>%
    filter(max.vel>threshold1)->
    xm
  
  xx %>%
    filter(event %in% unique(xm$event)) %>%
    dplyr::select(time,event) ->
    g
  

  stimes2<- filter(stimes,s %in% unique(xm$event))
  
  ftimes<-data.frame(fix.onset=c(1,stimes2$event.offset+1),
                     fix.offset=c(stimes2$event.onset-1,datalength))
  
  ftimes %>%
    filter(fix.onset>0,fix.offset>0)%>%
    mutate(dur=fix.offset-fix.onset,
           s=row_number()) %>%
    filter(fix.onset<datalength)->
    ftimes
  
  f<-rbindlist(apply(ftimes,1,jsac))
  
  f<- select(f,-event.dur)
  
  f$issaccade=FALSE
  g$issaccade=TRUE
 fg<-rbind(f,g)
 fg<- arrange(fg,time)
 
 fg$event[!fg$issaccade]=fg$event[!fg$issaccade]*-1
 
 if (length(fg$event)!=datalength){
   message('FAIL')
   message(length(fg$event))
   message(datalength)
   # message(paste('FAILED: ',length(fg$event,datalength,sep='-')))
   }
 else{
   message('SUCCESS')
 }
 
 fg$event

   
}

z$dsnum<-markSaccadesDouble(z$conj.velocity)
# v<-markSaccadesDouble(z$conj.velocity)



mod<- lm(sdf~lepV+repV+levV+revV,data=filter(z,!asleep))

mod2<- lm(sdf~lepV+repV+levV+revV,data=filter(z,asleep))


mod<- lm(sdf~conj.vert+conj.hor,data=z)
mod<- lm(sdf~conj.vert+conj.hor,data=filter(z,!asleep))

mod<- lm(sdf~conj.vert:asleep,data=z)
mod<- lm(sdf10~conj.vert:asleep+conj.hor:asleep,data=z)

summary(mod)

z %>%
  filter(dsnum<0) %>%
  group_by(dsnum) %>%
  summarize(sd.conj.velocity=sd(conj.velocity),
            mean.conj.velocity=mean(conj.velocity),
            spread=max(conj.velocity)-min(conj.velocity),
            qrange=quantile(conj.velocity,0.975)-quantile(conj.velocity,0.025),
            dur=n(),
            starttime=first(time),
            c2eyes=cor(repV,lepV),
            meanFR=sum(rasters)/dur*1000,
            mean.V=mean(conj.vert),
            mean.H=mean(conj.hor),
            mean.verg.angle=mean(verg.angle),
            asleep=sd.conj.velocity>7.5 || dur>2000)->
  zp

mod<- lm(meanFR~mean.V:asleep+mean.H:asleep,data=zp)

mod<- lm(meanFR~mean.V+mean.H,data=filter(zp,!asleep,dur>100))

qplot(mean.V,meanFR,data=zp)
qplot(mean.V,meanFR,color=asleep,data=filter(zp,meanFR>0,mean.V> -10))+stat_smooth(method='lm')

calculateBIquick<- function(tt,delay=20,duration=50){
  
  tt %>%
    filter(dsnum>0) %>%
    group_by(dsnum) %>%
    summarize(saccade.end=last(time),
              saccade.start=first(time))-> #gets rid of noise 
    tcount
  
  getPostSaccade<-function(saccade.end,tt){
    post.saccade=mean(tt$sdf10[saccade.end+delay:saccade.end+duration+delay])
  }
  
  tcount$post.saccade<-sapply(tcount$saccade.end,FUN = getPostSaccade,tt)
  
  tt<- left_join(tt,select(tcount,dsnum,post.saccade),by='dsnum')
  # select(tcount,dsnum,post.saccade)
}

#this takes 30 seconds per neuron
#which adds up to 30 minutes for the full data set
t %>%
  group_by(neuron) %>%
  do(calculateBIquick(.,20,50)) ->
  t


getPostSaccade<-function(saccade.end,tt){
  post.saccade=mean(tt$sdf10[saccade.end+20:saccade.end+50+20])
}

{
  start.time<-Sys.time()
  t %>%
    # filter(neuron=='Bee-115')%>%
    filter(dsnum>0) %>%
    group_by(dsnum) %>%
    mutate(saccade.end=last(time),
           saccade.start=first(time),
           post.saccade=getPostSaccade(saccade.end,tt))-> #gets rid of noise 
    t
  stop.time<- Sys.time()
  stop.time-start.time
  #it only took 7.7 minutes. did it work?
}

t%>% 
  filter(dsnum>0) %>%
  group_by(monkey,neuron,dsnum) %>%
  summarize(dur=n(),
            peak.during.saccade=max(sdf10),
            post.saccade=first(post.saccade),
            burst.index= peak.during.saccade-post.saccade,
            conj.vert=first(conj.vert),
            upward=conj.vert>0,
            sd.conj.velocity=sd(conj.velocity),
            mean.conj.velocity=mean(conj.velocity),
            sd.verg.velocity=sd(verg.velocity),
            spread=max(conj.velocity)-min(conj.velocity),
            qrange=quantile(conj.velocity,0.975)-quantile(conj.velocity,0.025),
            R.H.Amp=last(rep)-first(rep),
            R.V.Amp=last(repV)-first(repV),
            L.H.Amp=last(lep)-first(lep),
            L.V.Amp=last(lepV)-first(lepV),
            disjunctiveH=sign(R.H.Amp*L.H.Amp)<0,
            disjunctiveV=sign(R.V.Amp*L.V.Amp)<0,
            conj.H.Amp=(R.H.Amp+L.H.Amp)/2,
            conj.V.Amp=(R.V.Amp+L.V.Amp)/2,
            peak.conj.velocity=maxabs(conj.velocity),
            peak.H.Velocity=maxabs((rev+lev)/2),
            peak.V.Velocity=maxabs((revV+levV)/2),
            peak.RH.Velocity=maxabs(rev),
            peak.RV.Velocity=maxabs(revV),
            peak.LH.Velocity=maxabs(lev),
            peak.LV.Velocity=maxabs(levV),
            peakFR=max(sdf10),
            nspk=sum(rasters),
            avgFR=nspk/dur*1000,
            r.amp=sqrt(conj.H.Amp^2+conj.V.Amp^2),
            asleep=sd.conj.velocity>7.5 || dur>2000) ->
  BI

BI %>%
  filter(dur>10,abs(conj.V.Amp)>5,abs(conj.H.Amp)<2) %>%
  group_by(monkey,neuron,upward) %>%
  summarize(mean.post.saccade=mean(post.saccade,na.rm=T),
            mean.during.saccade=mean(peak.during.saccade),
            mean.burst.index=mean(peak.during.saccade-post.saccade,na.rm=T),
            n=n())->
  BIave

BI %>%
  filter(abs(conj.V.Amp)>5,abs(conj.V.Amp)<40)%>%
  ggplot()+
  stat_smooth(aes(conj.V.Amp,peak.during.saccade-post.saccade,group=neuron),method='lm',se=FALSE)+
  facet_wrap(~monkey)

BI %>%
  filter(abs(conj.H.Amp)>5,abs(conj.H.Amp)<40)%>%
  ggplot()+
  stat_smooth(aes(conj.H.Amp,peak.during.saccade-post.saccade,group=neuron),method='lm',se=FALSE)+
  facet_wrap(~monkey)



#Calculate post-saccade firing rate
#takes ~8 min to run
getPostSaccade<-function(saccade.end,tt,neuron){
  tt<- filter(tt,neuron==neuron)
  post.saccade=mean(tt$sdf10[saccade.end+20:saccade.end+50+20])
}

{
  start.time<-Sys.time()
  t %>%
    filter(neuron=='Bee-115') %>%
    filter(dsnum>0) %>%
    group_by(neuron,dsnum) %>%
    mutate(saccade.end=last(time),
           saccade.start=first(time),
           post.saccade=getPostSaccade(saccade.end,t,first(neuron)))-> 
    tx
  
  stop.time<-Sys.time()
  stop.time-mid.time()
}

#clearly this is not working in an efficient way
#My new idea is to alter the marksaccadesdouble function to enable a post-sac buffer period
#so we'll mark dsnum as the start and end of the saccades
#then we'll mark dsnum_long as start:end+buffer then we can just group_by dsnum_long

markSaccadesDouble<- function(v, threshold1=60,threshold2=20,min.dur=5,maxreject=1000,
                              post.sac.buffer=80){
  #This function is an R implementation of a two-threshold event marker
  #The algorithm works like this: Find all the times when velocity is above the high threshold
  #Extend this out until velocity is below the lower threshold
  
  #in practice, I'm identifying all the times that the saccades cross the low threshold and then rejecting 
  #any that don't meet the higher threshold
  #I'm also rejecting events that are below a certain duration
  
  #this algorithm also assigns positive ID numbers to the saccades and 
  #negative ID numbers to the non-saccades (fixations?)
  #after running this function, you can group_by(event) and measure the fixations or saccades as you wish
  require(dplyr)
  require(data.table) #for rbindlist - a fast version of do.call('rbind') that uses data.table
  datalength<-length(v)
  i<-which(abs(v)>threshold2) #find all the times when speed is above the lower threshold
  
  #For continuous numbers, diff=1. If there is a larger jump, it means there's a gap. 
  #That indicates another saccade
  sacoff<-which(diff(i)>1) #sacoff now contains the indices of the ends of all the saccades
  #sacon has all the indices of when the saccades start
  #After an offset, the next index will be the onset of the next saccade.
  #find the onsets by looking at the next index after the offset
  sacon<-c(1,sacoff+1) #first saccade always starts at first index
  sacoff<-c(sacoff,length(i)) #end of last saccade is always at the end
  event.onset<-i[sacon] #Convert from the indices to actual times
  event.offset<-i[sacoff] 
  
  #event.onset now has the time (in sample time) of all saccade onsets
  #set up stimes as a data.frame with two columns. Onset and offset. 
  stimes<- data.frame(event.onset,event.offset) 
  
  #this is a little function that works with the weirdness of R's "apply" family of functions
  #it just takes the onset and offset and returns the whole range. 
  #if you give it [10, 20], it will return [10 11 12 13 14 15 16 17 18 19 20]
  jsac<- function(stimes){
    summary(stimes)
    #input should be an array of length 4: c(onsettime,offsettime, saccade.number,saccade.dur)
    df<- data.frame(time=stimes[[1]]:stimes[[2]])
    df$event<- stimes[[4]]
    df$event.dur<- stimes[[3]]
    return(df)
    # return(stimes[[1]]:stimes[[2]])
  }
  
  stimes %>%
    mutate(dur=event.offset-event.onset, #calculate duration of each saccade
           s=row_number())-> #assign an ID number to each saccade
    stimes
  
  #Use "apply" to run the function "jsac" (above) on each line of the "stimes" data.frame
  #the result is the times of when all the saccades happen
  x<-rbindlist(apply(stimes,1,jsac)) 
  
  v<- data.frame(v=v) #Make the original velocity trace into a data.frame
  v<- mutate(v, time=row_number()) #add time to keep track
  
  #join the marked saccades and the velocity
  #the result is the velocity trace plus a row that just indicates whether there's a saccade
  #each saccade is identified by it's unique marker "event" that comes from df$event<- stimes[[4]] above 
  xx<- left_join(v,x,by='time')
  
  xx %>%
    group_by(event) %>% #This means we analyze each saccade individually
    summarize(max.vel=max(abs(v)), #calculate max velocity
              dur=n()) %>% #calculate duration
    filter(max.vel>threshold1, #reject all saccades that fail to exceed the large threshold
           max.vel<maxreject, #reject all saccades over the max threshold
           dur>min.dur)-> #reject all saccades that fail to exceed the minimum duration
    xm #xm is a summary which means it just lists the saccades and their measured values
  
  xx %>% #go back to the full data set and now reject all the saccades that were rejected above
    filter(event %in% unique(xm$event)) %>% 
    dplyr::select(time,event) -> #All we need is the time and the eventID
    g
  
  #this next part goes through and assigns an ID to all the non-saccade portions of the data
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
  
  #the code below isn't very elegant, but it just combines the fixations and saccades
  #and assigns negative IDs to the fixations and positive IDs to the saccades
  f$issaccade=FALSE 
  g$issaccade=TRUE
  fg<-rbind(f,g)
  fg<- arrange(fg,time)
  
  fg$event[!fg$issaccade]=fg$event[!fg$issaccade]*-1
  
  #This is a debugging message in case the result isn't the correct length
  #we have to return a vector of the same length as the input
  if (length(fg$event)!=datalength){
    message('FAIL')
    message(length(fg$event))
    message(datalength)
    # message(paste('FAILED: ',length(fg$event,datalength,sep='-')))
  }
  else{
    # message('SUCCESS')
  }
  
  fg$event #return just an array of the IDs of saccades and fixations
  
  
}

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
    # message('SUCCESS')
  }
  
  fg$event
  
  
}
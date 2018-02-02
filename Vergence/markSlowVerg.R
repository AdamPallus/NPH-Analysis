
markSlowVerg<- function(v, highthresh=6,lowthresh=2,rejectthresh=30,min.dur=5){
  
  require(dplyr)
  require(data.table) #for rbindlist - a fast version of do.call('rbind') that uses data.table
  datalength<-length(v)
  i<-which(abs(v)>lowthresh) #find all the times when speed is above the lower threshold
  
  sacoff<-which(diff(i)>1) #sacoff now contains the indices of the ends of all the saccades
 
  sacon<-c(1,sacoff+1) #first saccade always starts at first index
  sacoff<-c(sacoff,length(i)) #end of last saccade is always at the end
  event.onset<-i[sacon] #Convert from the indices to actual times
  event.offset<-i[sacoff] 
  

  stimes<- data.frame(event.onset,event.offset) 

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
  
  x<-rbindlist(apply(stimes,1,jsac)) 
  
  v<- data.frame(v=v) #Make the original velocity trace into a data.frame
  v<- mutate(v, time=row_number()) 
  xx<- left_join(v,x,by='time')
  
  xx %>%
    group_by(event) %>% #This means we analyze each saccade individually
    mutate(max.vel=max(abs(v)), #calculate max velocity
           dur=n()) %>% #calculate duration
    filter(max.vel>highthresh, #reject all saccades that fail to exceed the large threshold
           max.vel<rejectthresh, #reject all movements that exceed the rejection thresh (prob saccades)
           dur>min.dur)-> #reject all saccades that fail to exceed the minimum duration
    xm #xm is a summary which means it just lists the saccades and their measured values
  
  
  
  xx %>% #go back to the full data set and now reject all the saccades that were rejected above
    filter(event %in% unique(xm$event)) %>% 
    dplyr::select(time,event) -> #All we need is the time and the eventID
    g
  
  g<- left_join(select(xx,time),g,by='time')
  
  g$event #return just an array of the IDs of saccades and fixations
  
  
}

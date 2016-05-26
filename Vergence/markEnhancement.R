markEnhancement<- function(v, treshold1=15,threshold2=8){
  require(dplyr)
  
  mindur<-50
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
    df$enhancenum<- stimes[[4]]
    df$enhance.dur<- stimes[[3]]
    return(df)
    # return(stimes[[1]]:stimes[[2]])
  }
  
  stimes %>%
    mutate(dur=event.offset-event.onset,
           s=row_number())->
    stimes
  
  x<- do.call('rbind',apply(stimes,1,jsac))
  
  vv<- data.frame(v=v)
  vv<- mutate(vv, time=row_number())
  
  xx<- left_join(vv,x,by='time')
  
  xx %>%
    group_by(enhancenum) %>%
    summarize(max.vel=max(abs(v))) %>%
    filter(max.vel>threshold1)->
    xm
  
  xx %>%
    filter(enhancenum %in% xm$enhancenum) %>%
    select(time,enhancenum) ->
    g
  
  
}
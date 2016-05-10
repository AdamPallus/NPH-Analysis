
joinsaccadesuniform<-function(t,buffer=20,threshold=30,saccade.length=150){
  
  findSaccades<-function(ev,threshold=40){
    mindur<-50
    i<-which(abs(ev)>threshold) #find all the times when speed > threshold
    sacoff<-which(diff(i)>mindur) #minimum duration of an accepted saccade
    sacon<-c(1,sacoff+1) #first saccade
    sacoff<-c(sacoff,length(i)) #end of last saccade
    saccade.onset<-i[sacon] #get actual times
    saccade.offset<-i[sacoff] 
    
    return(data.frame(saccade.onset,saccade.offset))
  }
  
  jsac<- function(stimes){
    summary(stimes)
    #input should be an array of length 2: c(onsettime,offsettime, saccade.number,saccade.dur)
    df<- data.frame(time=stimes[[1]]:stimes[[2]])
    df$sacnum<- stimes[[4]]
    df$saccade.dur<- stimes[[3]]
    return(df)
    # return(stimes[[1]]:stimes[[2]])
  }
  
  stimes<-findSaccades(t$conj.velocity,threshold)
  stimes %>%
    mutate(dur=saccade.offset-saccade.onset,
           s=row_number(),
           saccade.onset=saccade.onset-buffer,
           ###########HERE IS WHERE i MAKE SACCADES UNIFORM #######
           saccade.offset=saccade.onset+saccade.length+2*buffer)->
    stimes
  
  x<- do.call('rbind',apply(stimes,1,jsac))
  x %>%
    group_by(sacnum) %>%
    mutate(counter=time-first(time)) ->
    x
  left_join(t,x,by='time')
}
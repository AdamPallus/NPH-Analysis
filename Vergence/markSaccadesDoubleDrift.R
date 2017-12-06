#Saccade onset was defined as the first point in time at which eye velocity exceeded 50°/s and 
#acceleration exceeded 10,000°/s2. Offset was defined as the first point
#in time at which either of two criteria were met: 
  #1) eye velocity fell below 50°s; or 
  #2) eye velocity fell below 100°/s and the absolute value of acceleration fell below 10,000°/s2

#This function is an R implementation of a two-threshold event marker
#The algorithm works like this: Find all the times when velocity is above the high threshold
#Extend this out until velocity is below the lower threshold

#in practice, I'm identifying all the times that the saccades cross the low threshold and then rejecting 
#any that don't meet the higher threshold
#I'm also rejecting events that are below a certain duration

#this algorithm also assigns positive ID numbers to the saccades and 
#negative ID numbers to the non-saccades (fixations?)
#after running this function, you can group_by(event) and measure the fixations or saccades as you wish


markSaccadesDoubleDrift<- function(v, threshold1=60,threshold2=20,min.dur=5){
  
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
    ungroup() %>%
    mutate(acc=parabolicdiff(v,7)) %>%
    mutate(event=replace(event,abs(v)<100 & abs(acc)<10000,NA)) ->
    xx
  
  xx %>%
    group_by(event) %>% #This means we analyze each saccade individually
    mutate(max.vel=max(abs(v)), #calculate max velocity
              dur=n()) %>% #calculate duration
    filter(max.vel>threshold1, #reject all saccades that fail to exceed the large threshold
           dur>min.dur)-> #reject all saccades that fail to exceed the minimum duration
    xm #xm is a summary which means it just lists the saccades and their measured values
  

  
  xx %>% #go back to the full data set and now reject all the saccades that were rejected above
    filter(event %in% unique(xm$event)) %>% 
    dplyr::select(time,event) -> #All we need is the time and the eventID
    g
  
  g<- left_join(select(xx,time),g,by='time')
  
  
  #This is a debugging message in case the result isn't the correct length
  #we have to return a vector of the same length as the input
  if (length(g$event)!=datalength){
    message('FAIL')
    message(length(g$event))
    message(datalength)
    # message(paste('FAILED: ',length(fg$event,datalength,sep='-')))
  }
  else{
    # message('SUCCESS')
  }
  
  g$event #return just an array of the IDs of saccades and fixations
  
  
}

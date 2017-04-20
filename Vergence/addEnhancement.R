"
Detects and marks enhanced vergence velocity
t %>%
  group_by(neuron) %>%
  do(addEnhancement(threshold1=50,threshold2=12)) ->
t
"

addEnhancement<- function(t, highthreshold=50,lowthreshold=12){
  require(dplyr)
 
  #subfunction: returns beginning and end times of possible enhancment
  findenhancement<-function(v,lowthreshold=12){ 
    mindur<-50
    i<-which(abs(v)>lowthreshold) #find all the times when speed is above the lower threshold
    sacoff<-which((diff(i)>mindur)) #minimum duration of an accepted enhancement
    sacon<-c(1,sacoff+1) #first enhancement
    sacoff<-c(sacoff,length(i)) #end of last enhancement
    event.onset<-i[sacon] #get actual times
    event.offset<-i[sacoff] 
    stimes<- data.frame(event.onset,event.offset)
  }
  #subfunction: reconfigures to long form: from [1, 50] to [1,2,3,4...,50]
  jsac<- function(stimes){ 
    summary(stimes)
    df<- data.frame(time=stimes[[1]]:stimes[[2]])
    df$enhancenum<- stimes[[4]]
    df$enhance.dur<- stimes[[3]]
    return(df)
  }
  
  stimes<- findenhancement(t$verg.velocity,lowthreshold=lowthreshold)
  nsaccades=nrow(stimes)
  #calcluate duration of each potential enhancement and assign it a number
  stimes %>%
    mutate(dur=event.offset-event.onset,
           s=row_number())->
    stimes
  #calls jsac function repeatedly to convert all movements to long form
  x<- do.call('rbind',apply(stimes,1,jsac))

  #joins list of possible enhancements with original data to find peak velocity
  xx<- left_join(t,x,by='time')
  #calcluate peak velocity and removes any that do not cross the higher threshold
  xx %>%
    group_by(enhancenum) %>%
    summarize(max.vel=max(abs(verg.velocity))) %>%
    filter(max.vel>highthreshold)->
    xm
  #go back to the long form and omit the rejected movements
  xx %>%
    filter(enhancenum %in% unique(xm$enhancenum)) %>%
    dplyr::select(time,enhancenum) ->
    g
  #join it back to the full dataset
  t<- left_join(t,g,by=c('time'))
  
  #determine whether it's convergence or divergence
  t %>%
    mutate(verg.enhance=!is.na(enhancenum),
           enhance.type='none',
           verg.direction=verg.velocity>0)->
    t
  #assign names and convert to factor format
  i<- t$verg.enhance & !t$verg.direction
  t$enhance.type[i]<- 'divergence'
  i<- t$verg.enhance & t$verg.direction
  t$enhance.type[i]<- 'convergence'
  t$enhance.type<- as.factor(t$enhance.type)
  t$verg.direction<- NULL
  t
}

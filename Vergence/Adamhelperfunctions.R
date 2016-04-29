
spikedensity<-function (rasters,sd=100) {
  gsize<- sd*10
  g<-dnorm(-gsize:gsize,mean=0,sd=sd)
  sdf<-convolve(rasters,g,type="open")
  sdf<-sdf[gsize:(length(sdf)-(gsize+1))]*1000
  sdf
}

dynamiclead<-function(p,lags=seq(10,300,by=10)) {
  
  rsq<-NULL
  for (i in 1:length(lags)) {
    if (lags[i] > 0){
      p$sdflag<-dplyr::lag(p$sdf,lags[i])
    }
    else{
      p$sdflag<-dplyr::lead(p$sdf,lags[i]*-1)
    }
    
    rsq[i]<- summary(lm(sdflag~rep+lep+repV+lepV,data=p))$r.squared
  }
  #return(rsq)
  return(lags[rsq==max(rsq)])
}

  
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

markSaccades<-function(ev,buffer=15,threshold=40){
  #this function finds and marks saccades given a velocity input
  stimes<-findSaccades(ev,threshold)
  
  #remove saccades without enough data at the end of the file, based on buffer size
  toolong<- stimes$saccade.offset> length(ev)-buffer
  tooshort<- stimes$saccade.onset<buffer+1
  stimes<- filter(stimes, !tooshort, !toolong)
  
  nsaccades=nrow(stimes)
  
  stimes$saccade.onset=stimes$saccade.onset-buffer
  stimes$saccade.offset=stimes$saccade.offset+buffer
  
  s<-1:length(ev)*0
  
  for (k in 1:nsaccades){
    s[stimes$saccade.onset[k]:stimes$saccade.offset[k]]<- k
    if(k>1){
      s[stimes$saccade.offset[k-1]:stimes$saccade.onset[k]]<-(k*-1)
    }
  }
  s[1:stimes$saccade.onset[1]]<- -1
  s[stimes$saccade.offset[nrow(stimes)]:length(s)]<- (nrow(stimes)*-1)-1
  return(s)
}

parabolicdiff <- function(pos,n=7){
  q <- sum(2*((1:n)^2))
  convoutput<- convolve(pos,c(-n:-1, 1:n),type="open")
  convoutput<- convoutput[(n*2):(length(pos)-((n*2)+1))]
  vels<- c(array(convoutput[1],dim=n*2),convoutput,array(convoutput[length(convoutput)],dim=n*2))
  vels <- vels/q*1000
}

maxabs<- function(x){
  m1<-max(x,na.rm=T)
  m2<-min(x,na.rm=T)
  if (abs(m1)>abs(m2)) {
    return(m1)
  } else{
    return(m2)
  }
}
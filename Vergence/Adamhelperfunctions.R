library(cladoRcpp)
library(dplyr)
library(data.table)

spikedensity<-function (rasters,sd=100) {
  gsize<- sd*10
  g<-dnorm(-gsize:gsize,mean=0,sd=sd)
  sdf<-rcpp_convolve(rasters,g)
  sdf<-sdf[gsize:(length(sdf)-(gsize+1))]*1000
  sdf
}

dynamiclead<-function(p,lags=seq(10,300,by=10),formula='rev+lev') {
  
  formula=paste('sdflag~',formula)
  
  rsq<-NULL
  for (i in 1:length(lags)) {
    if (lags[i] > 0){
      p$sdflag<-dplyr::lag(p$sdf,lags[i])
    }
    else{
      p$sdflag<-dplyr::lead(p$sdf,lags[i]*-1)
    }
    
    rsq[i]<- summary(lm(formula=formula,data=p))$r.squared
  }
  #return(rsq)
  # return(lags[rsq==max(rsq)])
  bestlag=lags[rsq==max(rsq)]
  p$dynamiclead<- bestlag
  if (bestlag > 0){
    p$sdflag<-dplyr::lag(p$sdf,bestlag)
  }
  else{
    p$sdflag<-dplyr::lead(p$sdf,bestlag*-1)
  }
  return(p)
  
}

findSaccades<-function(ev,threshold=40){
  mindur<-1
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
  convoutput<- rcpp_convolve(pos,c(-n:-1, 1:n))
  convoutput<- convoutput[(n*2):(length(pos)-((n*2)+1))]
  vels<- c(array(convoutput[1],dim=n*2),convoutput,array(convoutput[length(convoutput)],dim=n*2))
  vels <- vels/q*-1000
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

loadnewcsv<- function(referencefile=NULL,path="C:/Users/setup/Desktop/NRTP Vergence/",
                      keeptarget=FALSE){
  require(stringr)
  require(dplyr)
  #This function loads .csv files in a particular folder. They must have the same columns for rbind
  #Saves time by only reading the csv when necessary
  
  #get names of all files in path
  files <- list.files(path=path,pattern='*.csv')
  #extract neuron name eg. Bee-01
  names<-sapply(files, str_match,"^[a-zA-Z]+-[0-9]+",USE.NAMES=FALSE)
  # check for new cells
  if (!is.null(referencefile)){
    files<-files[!names %in% referencefile$neuron] #comparison
  }
  
  nfiles<-length(files)
  
  if (nfiles>0){
    message(c('New Files: ',files))
    loadedfiles <- lapply(paste(path,files,sep=''),read.csv)
    t<-data.frame()
    # t.old<-NULL
    for (i in 1:nfiles) {
      f<- files[i]
      message(paste('Loading:',f))
      temp=loadedfiles[[i]]
      names<-str_match(f,"(^[a-zA-Z]+)-([0-9]+)")
      temp$neuron<-names[1]
      temp$monkey<-names[2]
      temp$cellnum<-as.numeric(names[3])
      temp$sdf<-spikedensity(temp$rasters,sd=10)
      # leadtime<-dynamiclead(temp)
      message(paste('ms of data:',nrow(temp)))
      mutate(temp,
             # sdflag=lag(sdf,leadtime),
             conj.velocity=sqrt((rev^2+lev^2)/2)+sqrt((revV^2+levV^2)/2),
             # s=markSaccades(conj.velocity,buffer=10,threshold=10),
             # slong=markSaccades(conj.velocity,buffer=longbuffer,threshold=10),
             time=row_number(),
             verg.angle=lep-rep,
             # verg.velocity=parabolicdiff(verg.angle,7),
             verg.velocity=lev-rev)->
        temp
      
      t <-rbind(t,temp)
    }
    if (!keeptarget){
      t<- dplyr::select(t, -thp,-tvp,-time)
      if('tvp2' %in% names(t)){
        t<- dplyr::select(t,-thp2,tvp2)
      }
    }
  }else{
    message('********NO NEW CELLS********')
    t<-NULL
  }
  return(t)
}

joinsaccadesuniformOLD<-function(t,buffer=20,threshold=30,saccade.length=150){
  
  findSaccades<-function(ev,threshold=40){
    mindur<- 1
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

joinsaccades<-function(t,buffer=20,threshold=30){
  findSaccades<-function(ev,threshold=40){
    mindur<- 1
    i<-which(abs(ev)>threshold) #find all the times when speed > threshold
    sacoff<-which(diff(i)>mindur) #minimum duration of an accepted saccade
    sacon<-c(1,sacoff+1) #first saccade
    sacoff<-c(sacoff,length(i)) #end of last saccade
    saccade.onset<-i[sacon] #get actual times
    saccade.offset<-i[sacoff] 
    
    return(data.frame(saccade.onset,saccade.offset))
  }
  
  jsac<- function(stimes){
    #input should be an array of length 2: c(onsettime,offsettime)
    df<- data.frame(time=stimes[[1]]:stimes[[2]])
    df$sacnum<- stimes[[3]]
    return(df)
    # return(stimes[[1]]:stimes[[2]])
  }
  
  stimes<-findSaccades(t$conj.velocity,threshold)
  stimes %>%
    mutate(s=row_number(),
           saccade.onset=saccade.onset-buffer,
           saccade.offset=saccade.offset+buffer)->
    stimes
  x<- as.data.frame(rbindlist(apply(stimes,1,jsac)))
  # x<- do.call('rbind',apply(stimes,1,jsac))
  x %>%
    group_by(sacnum) %>%
    mutate(counter=time-first(time)) ->
    x
  left_join(t,x,by='time')
}

markEnhancement<- function(v, threshold1=15,threshold2=8){
  require(dplyr)
  
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
  
  
  v<- data.frame(v=v)
  v<- mutate(v, time=row_number())
  
  xx<- left_join(v,x,by='time')
  
  xx %>%
    group_by(enhancenum) %>%
    summarize(max.vel=max(abs(v))) %>%
    filter(max.vel>threshold1)->
    xm
  
  xx %>%
    filter(enhancenum %in% unique(xm$enhancenum)) %>%
    dplyr::select(time,v,enhancenum) ->
    g
  
  
}

makeRelImp<- function(summaryforplot,formula='mean.Spikerate~mean.Verg.Angle+mean.C.Ver',normalize=TRUE){
  require(tidyr)
  summaryforplot %>% 
    group_by(neuron) %>%
    do(m=lm(formula=formula,data=.))-> 
    mm
  
  r<- data.frame()
  r2 <- NULL
  if (length(mm$m[[1]]$coefficients)<3){
    message('Not enough parameters for importance calculation...')
    message('Returning r.squared only.')
    for (i in 1:nrow(mm)){
      r2[i]<- summary(mm$m[[i]])$r.squared
    }
    r<- data.frame(neuron=mm$neuron,R2=r2)
    r<- separate(r,neuron,c('monkey','cellnum'),remove=FALSE)
    return(r)
  }  
  for (i in 1:nrow(mm)){
    # message('Trying...')
    bb<- relaimpo::calc.relimp(mm$m[[i]],rela=normalize)
    b<- bb$lmg
    r<-rbind(r,b)
    r2<- c(r2,bb$R2)
    
  }
  r$neuron<-mm$neuron
  r$R2<- r2
  names(r)<- c(names(b),'neuron','R2')
  r<- separate(r,neuron,c('monkey','cellnum'),remove=FALSE)
  r
}

joinsaccadesuniform<-function(t,buffer=20,threshold=30,saccade.length=150){
  
  findSaccades<-function(ev,threshold=40){
    mindur<- 1
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
  
  x<- as.data.frame(rbindlist(apply(stimes,1,jsac)))
  x %>%
    group_by(sacnum) %>%
    mutate(counter=time-first(time)) ->
    x
  left_join(t,x,by='time')
}

dynamiclead2<-function(p,lags=seq(20,80,by=2),formula='verg.velocity~sdflag+verg.angle') {
  #rewrote this function to work with variables other than sdflag as the prediction
  rsq<-NULL
  for (i in 1:length(lags)) {
    if (lags[i] > 0){
      p$sdflag<-dplyr::lag(p$sdf,lags[i])
    }
    else{
      p$sdflag<-dplyr::lead(p$sdf,lags[i]*-1)
    }
    
    rsq[i]<- summary(lm(formula=formula,data=p))$r.squared
  }
  # return(rsq)
  return(lags[rsq==max(rsq)])
  # bestlag=lags[rsq==max(rsq)]
  
}

loadnewcsv2<- function(referencefile=NULL,path="C:/Users/setup/Desktop/NRTP Vergence/"){
  require(stringr)
  require(dplyr)
  #This function loads .csv files in a particular folder. They must have the same columns for rbind
  #Saves time by only reading the csv when necessary
  
  #get names of all files in path
  files <- list.files(path=path,pattern='*.csv')
  #extract neuron name eg. Bee-01
  names<-sapply(files, str_match,"^[a-zA-Z]+-[0-9]+",USE.NAMES=FALSE)
  # check for new cells
  if (!is.null(referencefile)){
    files<-files[!names %in% referencefile$neuron] #comparison
  }
  
  nfiles<-length(files)
  
  if (nfiles>0){
    message(c('New Files: ',files))
    loadedfiles <- lapply(paste(path,files,sep=''),read.csv)
    t<-data.frame()
    temp<- NULL
    # t.old<-NULL
    for (i in 1:nfiles) {
      f<- files[i]
      message(paste('Loading:',f))
      # temp[[i]]=loadedfiles[[i]]
      names<-str_match(f,"(^[a-zA-Z]+)-([0-9]+)")
      loadedfiles[[i]]$neuron<-names[1]
      loadedfiles[[i]]$monkey<-names[2]
      loadedfiles[[i]]$cellnum<-as.numeric(names[3])
      # loadedfiles[[i]]$sdf<-spikedensity(temp$rasters,sd=10)
      # leadtime<-dynamiclead(temp)
      message(paste('ms of data:',nrow(temp)))
      mutate(loadedfiles[[i]],
             # sdflag=lag(sdf,leadtime),
             conj.velocity=sqrt((rev^2+lev^2)/2)+sqrt((revV^2+levV^2)/2),
             # s=markSaccades(conj.velocity,buffer=10,threshold=10),
             # slong=markSaccades(conj.velocity,buffer=longbuffer,threshold=10),
             time=row_number(),
             verg.angle=lep-rep,
             # verg.velocity=parabolicdiff(verg.angle,7),
             verg.velocity=lev-rev)->
        loadedfiles[[i]]
      
      
    }
    t <-rbindlist(loadedfiles)
    # t<- dplyr::select(t, -thp,-tvp,-time)
  }else{
    message('********NO NEW CELLS********')
    t<-NULL
  }
  return(t)
}
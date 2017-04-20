
```{r helperfunctions}

spikedensity<-function (rasters,sd=100) {
  gsize<- sd*10
  g<-dnorm(-gsize:gsize,mean=0,sd=sd)
  sdf<-rcpp_convolve(rasters,g)
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

makeRelImp<- function(summaryforplot,formula='mean.Spikerate~mean.Verg.Angle+mean.C.Ver',normalize=TRUE){
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
  r
}

loadnewcsv<- function(referencefile=NULL,path="C:/Users/setup/Desktop/NRTP Vergence/PPRF/"){
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
    t<- dplyr::select(t, -thp,-tvp,-time)
  }else{
    message('********NO NEW CELLS********')
    t<-NULL
  }
  return(t)
}

```

```{r measureMovements}
t %>%
  group_by(neuron) %>%
  mutate(s=markSaccades(conj.velocity,buffer=10,threshold=10),
         isfixation=s<0) %>%
  filter(isfixation) %>% #This removes all saccades from the dataframe
  group_by(neuron,s) %>%
  mutate(meanfr=mean(sdf),
         maxfr=max(sdf),
         R.Hor=mean(rep),
         R.Ver=mean(repV),
         L.Hor=mean(lep),
         L.Ver=mean(lepV),
         # exatrope= monkey=='Pilchuck',
         mean.Verg.Angle=mean(verg.angle),
         mean.Verg.Vel=mean(verg.velocity),
         # mean.Verg.Angle=replace(mean.Verg.Angle, mean.Verg.Angle<0 & ~exatrope , NA),
         # mean.Verg.Angle=replace(mean.Verg.Angle, mean.Verg.Angle>0 & exatrope,NA ),
         # mean.Verg.Angle=replace(mean.Verg.Angle,abs(mean.Verg.Angle)>50, NA),
         max.Verg.Vel = max(verg.velocity),
         max.Verg.Ang = max(verg.angle),
         nspikes=sum(rasters),
         dur=n(),
         mean.Spikerate=sum(rasters)/dur*1000,
         R.H.Amp=rep[1]-rep[length(rep)],
         L.H.Amp=lep[1]-lep[length(lep)],
         R.V.Amp=repV[1]-repV[length(repV)],
         L.V.Amp=lepV[1]-lepV[length(lepV)],
         maxamp=max(abs(R.H.Amp),abs(R.V.Amp),abs(L.H.Amp),abs(L.V.Amp)))->
  m


```



```{r verg.fixations,fig.height=28,fig.width=13}
# m %>%
#   filter(dur>50,monkey=='Kopachuck',
#          mean.Spikerate>20) %>%
#   summarize(mean.Spikerate=mean.Spikerate[1],
#             mean.Verg.Angle=mean.Verg.Angle[1],
#             dur=dur[1]) ->
# summaryforplot
m %>%
  group_by(neuron,s) %>%
  # dplyr::filter(dur>50) %>%
  summarize(mean.Spikerate=mean.Spikerate[1],
            mean.H=mean((rep+lep)/2),
            mean.V=mean((repV+lepV)),
            mean.conj.velocity=mean(conj.velocity),
            mean.Verg.Angle=mean.Verg.Angle[1],
            mean.Verg.Vel=first(mean.Verg.Vel),
            dur=dur[1],
            mean.time=mean(time),
            cellnum=cellnum[1],
            maxfr=maxfr[1],
            max.verg.angle=max.Verg.Ang[1],
            max.verg.velocity=max.Verg.Vel[1]) ->
  summaryforplot


summaryforplot %>%
  separate(neuron, c('monkey','cellnum2'),remove=FALSE) %>%
  # filter(monkey=='Pilchuck') %>%
  group_by(neuron) %>%
  filter(abs(mean.V)<40,
         abs(mean.H)<40) %>%
  # filter(mean.Verg.Vel<2, 
  #        dur>50,
  #        mean.Verg.Angle<50,
  #        mean.Verg.Angle> -30) %>%
  mutate(rV=cor(mean.V,mean.Spikerate),
         rH=cor(mean.H,mean.Spikerate)) ->
  sp


ggplot(sp)+
  geom_point(aes(mean.H,mean.Spikerate))+
  stat_smooth(aes(mean.H,mean.Spikerate),method='lm')+
  facet_wrap(~neuron,ncol=2)+
  geom_label(aes(0,100,label=paste('r = ',round(rH,2))))

ggplot(sp)+
  geom_point(aes(mean.V,mean.Spikerate))+
  stat_smooth(aes(mean.V,mean.Spikerate),method='lm')+
  facet_wrap(~neuron,ncol=2)+
  geom_label(aes(0,100,label=paste('r = ',round(rV,2))))

```
We plot the firing rate of each cell during saccades, including 100ms before and after the movement. Convergent saccades have an amplitude change of +3, divergent, -3 and version saccades are the rest. 



```{r Strab}

#analyze non-saccadic portion of data
m %>%
  group_by(neuron,s) %>%
  # dplyr::filter(dur>50) %>%
  summarize(mean.Spikerate=mean.Spikerate[1],
            mean.Verg.Angle=mean.Verg.Angle[1],
            mean.Verg.Vel=first(mean.Verg.Vel),
            mean.R.Hor=first(R.Hor),
            mean.R.Ver=first(R.Ver),
            mean.L.Hor=first(L.Hor),
            mean.L.Ver=first(L.Ver),
            mean.C.Hor=(mean.R.Hor+mean.L.Hor)/2,
            mean.C.Ver=(mean.R.Ver+mean.L.Ver)/2,
            dur=dur[1],
            mean.time=mean(time),
            cellnum=cellnum[1],
            maxfr=maxfr[1],
            max.verg.angle=max.Verg.Ang[1],
            max.verg.velocity=max.Verg.Vel[1]) ->
  summaryforplot

summaryforplot %>%
  separate(neuron, c('monkey','cellnum2'),remove=FALSE) %>%
  # filter(monkey=='Pilchuck') %>%
  group_by(neuron) %>%
  filter(mean.Verg.Vel<1, 
         dur>50,
         mean.Verg.Angle<50,
         mean.Verg.Angle> -30) %>%
  mutate(r=cor(mean.Verg.Angle,mean.Spikerate)) ->
  sp
summaryforplotold<-summaryforplot
summaryforplot<- sp

#first, plot conjugate X-Y with color as firing rate 
ggplot(filter(summaryforplot,abs(mean.C.Ver)<25,abs(mean.C.Hor)<25))+
  geom_point(aes(mean.C.Hor,mean.C.Ver,color=mean.Spikerate),size=2)+
  facet_wrap(~neuron,ncol=2,scales='free')

# ggplot(filter(summaryforplot,abs(mean.C.Ver)<25,abs(mean.C.Hor)<25,dur>20,neuron=='Kopachuck-106'))+
#   geom_point(aes(mean.C.Hor,mean.C.Ver,color=mean.Spikerate),size=3)
# 
# ggplot(filter(summaryforplot,abs(mean.C.Ver)<25,abs(mean.C.Hor)<25,dur>20,neuron=='Kopachuck-106'))+
#   geom_point(aes(mean.C.Hor,mean.C.Ver,color=mean.Verg.Angle),size=3)

ggplot(filter(summaryforplot,abs(mean.C.Ver)<25,abs(mean.C.Hor)<25))+
  geom_point(aes(mean.C.Hor,mean.C.Ver,color=mean.Verg.Angle),size=2)+
  facet_wrap(~neuron,ncol=2,scales='free')


#Calculate horizontal conjugate position vs vergence
r<- makeRelImp(summaryforplot,formula='mean.Spikerate~mean.Verg.Angle+mean.C.Hor',norm=FALSE)

ggplot(aes(mean.C.Hor,mean.Verg.Angle),size=2,data=r)+
  geom_point()+
  geom_text(aes(label=neuron))+
  geom_abline(slope=1)+
  xlim(c(0,1))+
  ylim(c(0,1))

#Calculate vertical conjugate position vs vergence
r<- makeRelImp(summaryforplot,formula='mean.Spikerate~mean.Verg.Angle+mean.C.Ver',norm=FALSE)

ggplot(aes(mean.C.Ver,mean.Verg.Angle),size=2,data=r)+
  geom_point()+
  geom_text(aes(label=neuron))+
  geom_abline(slope=1)+
  xlim(c(0,1))+
  ylim(c(0,1))

#caclculate R^2 value for verg.angle
r<- makeRelImp(summaryforplot,formula='mean.Spikerate~mean.Verg.Angle',norm=FALSE)
ggplot(r)+geom_bar(aes(neuron,R2),stat='identity')


#compare left vs right eyes
#HORIZONTAL
r<- makeRelImp(summaryforplot,formula='mean.Spikerate~mean.R.Hor+mean.L.Hor',norm=FALSE)

ggplot(aes(mean.R.Hor,mean.L.Hor),size=2,data=r)+
  geom_point()+
  geom_text(aes(label=neuron))+
  geom_abline(slope=1)+
  xlim(c(0,1))+
  ylim(c(0,1))

#VERTICAL
r<- makeRelImp(summaryforplot,formula='mean.Spikerate~mean.R.Ver+mean.L.Ver',norm=FALSE)

ggplot(aes(mean.R.Ver,mean.L.Ver),size=2,data=r)+
  geom_point()+
  geom_text(aes(label=neuron))+
  geom_abline(slope=1)+
  xlim(c(0,1))+
  ylim(c(0,1))

#EYE POSITION
r<- makeRelImp(summaryforplot,formula='mean.Spikerate~mean.C.Hor+mean.C.Ver',norm=FALSE)
ggplot(aes(mean.C.Hor,mean.C.Ver),size=2,data=r)+
  geom_point()+
  geom_text(aes(label=neuron))+
  geom_abline(slope=1)+
  xlim(c(0,1))+
  ylim(c(0,1))

r<- makeRelImp(summaryforplot,formula='mean.Spikerate~mean.C.Hor+mean.C.Ver',norm=TRUE)
r<- mutate(r,prefDir=atan2(mean.C.Ver,mean.C.Hor)*180/pi)
kable(r)

```
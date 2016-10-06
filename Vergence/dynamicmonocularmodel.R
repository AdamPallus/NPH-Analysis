#This script loads all the files in the PPRF folder, calculates models using right, left and conjugate velocity.
#Then it plots using manipulate. Orange is the conjugate model. Left and right are blue and red, respectively. 

desiredcell='Ozette-501'

library(ggplot2)
library(knitr)
library(tidyr)
# library(broom)
# library(grid)
library(relaimpo)
library(leaps)
#library(data.table)
library(stringr)
library(dplyr)
source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')
library(manipulate)

dynamiclead<-function(p,lags=seq(10,300,by=10)) {
  
  rsq<-NULL
  for (i in 1:length(lags)) {
    if (lags[i] > 0){
      p$sdflag<-dplyr::lag(p$sdf,lags[i])
    }
    else{
      p$sdflag<-dplyr::lead(p$sdf,lags[i]*-1)
    }
    
    rsq[i]<- summary(lm(sdflag~rev+lev,data=p))$r.squared
  }
  #return(rsq)
  return(lags[rsq==max(rsq)])
}

path<- "C:/Users/setup/Desktop/NRTP Vergence/PPRF/"
buffer<- 20
longbuffer<- 200
files <- list.files(path=path,pattern='*.csv')
names<-sapply(files, str_match,"^[a-zA-Z]+-[0-9]+",USE.NAMES=FALSE)
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
           verg.velocity=lev-rev)->
      temp
    
    t <-rbind(t,temp)
  }
}

t2<- filter(t,neuron==desiredcell)
d<- dynamiclead(t2,lags=seq(1,50,by=1))
t2<- mutate(t2,lagsdf=lag(sdf,d))
m<- lm(lagsdf~rev+lev,data=t2)
m2<- lm(lagsdf~rev+lev,data=filter(t2,sdf>10))
summary(m)
summary(m2)
t2<- mutate(t2,predictsdf=predict(m2,newdata=t2))

t2<- mutate(t2, conjv=(rev+lev)/2)

mconj<- lm(lagsdf~conjv-1,data=filter(t2,lagsdf>10))
mipsi<-lm(lagsdf~rev-1,data=filter(t2,lagsdf>10))
mcontra<-lm(lagsdf~lev-1,data=filter(t2,lagsdf>10))

t2<- mutate(t2,predictconj=predict(mconj,newdata=t2),
            predictipsi=predict(mipsi,newdata=t2),
            predictcontra=predict(mcontra,newdata=t2))

summary(mconj)
summary(mipsi)
summary(mcontra)


stepsize=1000
manipulate(ggplot(filter(t2,time>=window,time<window+stepsize))+
             geom_area(aes(time,lagsdf),alpha=1/10)+
             geom_line(aes(time,(rev+lev)/2))+ #conj velocity
             
             geom_line(aes(time,predictconj),color='orange')+
           geom_line(aes(time,predictipsi),color='red')+
           geom_line(aes(time,predictcontra),color='blue')+
             coord_cartesian(ylim=c(0,500)),
           window=slider(1,max(t2$time-stepsize),step=stepsize))

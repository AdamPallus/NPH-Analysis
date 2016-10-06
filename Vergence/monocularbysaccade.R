library(ggplot2)
library(knitr)
library(tidyr)
library(broom)
# library(grid)
library(relaimpo)
library(leaps)
#library(data.table)
library(stringr)
library(dplyr)
source('joinsaccadesuniform.R')
source('joinsaccades.R')
source('Adamhelperfunctions.R')

bootci <- function(t,n=100,alpha=0.05,formula='lagsdf~rev+lev'){
  t %>%
    bootstrap(n) %>%
    do(tidy(lm(formula,.)))%>%
    group_by(term) %>%
    summarize(
      low=quantile(estimate, alpha / 2),
      high=quantile(estimate, 1 - alpha / 2)) ->
    ci
  return(ci)
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
tall<- t

t<- dplyr::filter(tall,neuron=='Ozette-507')

t<- mutate(t,conj.velocity=sqrt(((rev+lev)/2)^2+((revV+levV)/2)^2))

d<- dynamiclead(t,lags=seq(1,50,by=1))
t<- mutate(t,lagsdf=lag(sdf,d),
           lagrasters=lag(rasters,d))


bufferlength<- 20
saccade.length<- 120
t%>%
  group_by(neuron) %>%
  mutate(time=row_number()) %>%
  # do(joinsaccadesuniform(.,buffer=bufferlength,threshold=20,saccade.length=saccade.length))->
  do(joinsaccades(.,buffer=bufferlength,threshold=20))->
  # do(joinsaccades(.,buffer=bufferlength,threshold=20))->
  t

#Once saccades have been marked, we measure lots of things about each saccade. 
#For this plot, we also remove all data that isn't part of a saccade or the buffer period
t %>%
  group_by(neuron) %>%
  mutate(issaccade=!is.na(sacnum)) %>%
  filter(issaccade) %>%
  group_by(neuron,sacnum) %>%
  mutate(#dur=n(),
    peak.conj.velocity=maxabs(conj.velocity),
    peak.R.H= maxabs(rev),
    peak.R.V= maxabs(revV),
    peak.L.H= maxabs(lev),
    peak.L.V= maxabs(levV),
    R.H.Amp=rep[n()]-rep[1],
    L.H.Amp=lep[n()]-lep[1],
    R.V.Amp=repV[n()]-repV[1],
    L.V.Amp=lepV[n()]-lepV[1],
    r.angle=atan2(R.V.Amp,R.H.Amp)*180/pi,
    r.amp=sqrt(R.H.Amp^2+R.V.Amp^2),
    nspikes=sum(rasters),
    vect.amp= (sqrt(R.H.Amp^2+R.V.Amp^2)+sqrt(L.H.Amp^2+L.V.Amp^2))/2,
    maxamp=max(abs(R.H.Amp),abs(R.V.Amp),abs(L.H.Amp),abs(L.V.Amp)),
    verg.amp=verg.angle[n()-bufferlength]-verg.angle[bufferlength+1],
    peak.verg.velocity= maxabs(verg.velocity),
    min.verg.trans = min(verg.velocity),
    max.verg.trans = max(verg.velocity),
    off.verg.velocity=min(abs(min.verg.trans),abs(max.verg.trans)),
    min.verg.angle=min(verg.angle),
    max.verg.angle=max(verg.angle),
    max.verg.velocity=max(verg.velocity),
    min.verg.velocity=min(verg.velocity),
    maxfr=max(sdf),
    counter=time-time[1]-bufferlength,
    verg.change=verg.angle-mean(verg.angle[1:50]),
    # verg.amp= last(verg.angle)-first(verg.angle),
    verg.amp= verg.angle[n()-bufferlength]-verg.angle[bufferlength],
    showrasters=replace(rasters,rasters<1,NA))->
  t

#This just gives a name to convergent and divergent saccades so the figure is automatically labled
t$convergent<- as.factor(t$verg.amp>0)
levels(t$convergent)<- c('Divergent','Convergent')



# ci<- bootci(t,n=1999,alpha=0.01)
# 
# ci %>%
#   group_by(neuron) %>%
#   mutate(m=(low+high)/2) %>%
#   filter(term != '(Intercept)') ->
#   pci
# g<-ggplot(pci,aes(factor(term),m))
# 
# g+geom_errorbar(aes(ymin=low,ymax=high,color=neuron),size=2,alpha=1/2,width=0.2)+
#   geom_hline(yintercept = 0)+
#   coord_flip()+
#   ylab('Bootstrap conficence interval for parameter estimate')+
#   xlab('Term')

civ<- bootci(filter(t,abs(verg.amp)>2),n=1999,alpha=0.05)

civ %>%
  # group_by(neuron) %>%
  mutate(m=(low+high)/2) %>%
  filter(term != '(Intercept)') ->
  pci
g<-ggplot(pci,aes(factor(term),m))

g+geom_errorbar(aes(ymin=low,ymax=high),size=2,alpha=1/2,width=0.2)+
  geom_hline(yintercept = 0)+
  coord_flip()+
  ylab('Bootstrap conficence interval for parameter estimate')+
  xlab('Term')


#####################################################
t<- ungroup(t)
# mconj<- lm(lagsdf~conjv-1,data=filter(t2,lagsdf>10))
mipsi<-lm(lagsdf~rev,data=filter(t,abs(verg.amp)>2,R.H.Amp<0))
mcontra<-lm(lagsdf~lev,data=filter(t,abs(verg.amp)>2,L.H.Amp<0))

t<- mutate(t,#predictconj=predict(mconj,newdata=t2),
            predictipsi=predict(mipsi,newdata=t),
            predictcontra=predict(mcontra,newdata=t),
           predictipsi=replace(predictipsi,predictipsi<0,0),
           predictcontra=replace(predictcontra,predictcontra<0,0))

t<- mutate(t,showrasters=replace(lagrasters,lagrasters<1,NA))

p<- filter(t,r.amp>2,abs(verg.amp)>2)
# p<-filter(t,R.V.Amp>1,verg.amp< 0)

p<- filter(t, R.H.Amp+L.H.Amp < 0,r.amp>2)

# p<- filter(xx,r.amp>4,saccade.dur<100,verg.amp>3)
# p<- filter(xx,r.amp>4)
# p<- filter(xx,r.amp>4,saccade.dur<150,verg.amp< -4)

goodsacs<- unique(p$sacnum)

nsac=length(goodsacs)

manipulate(ggplot(filter(t,sacnum==goodsacs[sac]))+
             geom_line(aes(counter,verg.velocity),color='purple')+
             # geom_line(aes(counter,conj.velocity),color='brown')+
             geom_line(aes(counter,(lev+rev)/2),color='brown')+
              geom_line(aes(counter,rev),color='red')+
              geom_line(aes(counter,lev),color='blue')+
             geom_line(aes(counter,predictipsi),color='red',linetype=3)+
             geom_line(aes(counter,predictcontra),color='blue',linetype=3)+
             
             
             # geom_hline(yintercept = c(-2,2))+
             geom_line(aes(counter,verg.angle*5),color='darkgreen')+
             geom_point(aes(counter,showrasters*100),shape='|',size=3)+
             geom_area(aes(counter,lagsdf),alpha=0.2)
             
           #  coord_cartesian(ylim=c(0,400))
           
           ,
           sac=slider(1,nsac,step=1)
)

manipulate(ggplot(filter(t,sacnum==goodsacs[sac]))+
             # geom_line(aes(counter,verg.velocity),color='purple')+
             geom_line(aes(counter,rep),color='red')+
             geom_line(aes(counter,lep),color='blue')+
             geom_line(aes(counter,repV),color='maroon')+
             geom_line(aes(counter,lepV),color='cyan')+
            
             
             
             # geom_hline(yintercept = c(-2,2))+
             geom_line(aes(counter,verg.angle),color='darkgreen')
             # geom_point(aes(counter,showrasters*100),shape='|',size=3)+
             # geom_area(aes(counter,lagsdf),alpha=0.2)
           
           #  coord_cartesian(ylim=c(0,400))
           
           ,
           sac=slider(1,nsac,step=1)
)

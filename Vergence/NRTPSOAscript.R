
library(ggplot2)
library(dplyr)
library(knitr)
library(tidyr)
# library(broom)
# library(grid)
library(relaimpo)
library(leaps)
#library(data.table)
library(stringr)
source('joinsaccadesuniform.R')
source('Adamhelperfunctions.R')


#load all the .csv files in the data folder, then add a column naming the neuron, 
#using the file name as the default name, then put them all together in one long data frame

path<- "C:/Users/setup/Desktop/NRTP Vergence/"
buffer<- 20
longbuffer<- 200

#get names of all files in path
files <- list.files(path=path,pattern='*.csv')
#extract neuron name eg. Bee-01
names<-sapply(files, str_match,"^[a-zA-Z]+-[0-9]+",USE.NAMES=FALSE)
# check for new cells
message('Loading Saved Data')
t.old<-readRDS('SOA-NRTP.RDS')
files<-files[!names %in% t.old$neuron] #comparison
nfiles<-length(files)
# nfiles=1
if (nfiles>0){
  message(c('New Files: ',files))
  loadedfiles <- lapply(paste(path,files,sep=''),read.csv)
  t<-data.frame()
  
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
    mutate(temp,
           # sdflag=lag(sdf,leadtime),
           conj.velocity=sqrt((rev^2+lev^2)/2)+sqrt((revV^2+levV^2)/2),
           # s=markSaccades(conj.velocity,buffer=10,threshold=10),
           # slong=markSaccades(conj.velocity,buffer=longbuffer,threshold=10),
           time=row_number(),
           verg.angle=lep-rep,
           verg.velocity=parabolicdiff(verg.angle,7))->
      temp
    
    t <-rbind(t,temp)
  }
  t<- dplyr::select(t, -thp,-tvp,-time)
  t<- rbind(t.old,t)
  saveRDS(rbind(t),'SOA-NRTP-new.RDS')
  t.old<- NULL
}else{
  message('********NO NEW CELLS********')
  t<-t.old
  t.old<-NULL
}
t<- filter(t, monkey %in% c('Bee','Ozette'))
# t<- filter(t, monkey %in% c('Ozette'))


t$celltype<- as.factor(as.numeric(t$cellnum)>100)
levels(t$celltype)<- c("NRTP","SOA")

t %>%
  group_by(neuron) %>%
  mutate(s=markSaccades(conj.velocity,buffer=10,threshold=10),
         isfixation=s<0) %>%
  filter(isfixation) %>%
  group_by(neuron,s) %>%
  mutate(meanfr=mean(sdf),
         maxfr=max(sdf),
         R.Hor=mean(rep),
         R.Ver=mean(repV),
         L.Hor=mean(lep),
         L.Ver=mean(lepV),
         mean.Verg.Angle=mean(verg.angle),
         mean.Verg.Angle=replace(mean.Verg.Angle, mean.Verg.Angle<0, NA),
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


m %>%
  filter(dur>200) %>%
  summarize(mean.Spikerate=mean.Spikerate[1],
            mean.Verg.Angle=mean.Verg.Angle[1],
            dur=dur[1]) ->
  summaryforplot
m<- NULL


gp<- ggplot(aes(mean.Verg.Angle,mean.Spikerate),data=summaryforplot)+
  geom_point(size=2,alpha=1/5)+
  facet_wrap(~neuron)+
  stat_smooth(method='lm')+
  ggtitle('Firing Rate as a function of Vergence Angle during Fixations')

ggsave('RatePosition.pdf',plot=gp, height=28, width=13)

# qplot(mean.Verg.Angle,mean.Spikerate,data=summaryforplot)+
#   geom_point()+
#   stat_smooth(method='lm')+
#   facet_wrap(~neuron)+
#   ggtitle('Firing Rate as a function of Vergence Angle during Fixations')

bufferlength<- 200
saccade.length<- 150
t%>%
  group_by(neuron) %>%
  mutate(time=row_number()) %>%
  do(joinsaccadesuniform(.,buffer=200,threshold=20,saccade.length=saccade.length))->
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



#############
#Make raster plots
#############

#try to reduce memory footprint
t<- dplyr::select(t, neuron,min.verg.angle,off.verg.velocity,verg.amp,convergent,showrasters,counter,sacnum,
           saccade.dur,celltype,rep,lep,repV,lepV,verg.angle)

Remove.Multiverg<- TRUE
sample.saccades<- TRUE
#This is the block where we decide which saccades to reject
if (Remove.Multiverg){
  t %>% group_by(neuron) %>%
    filter(off.verg.velocity<14) -> #removes "multi-directional" vergence movements
    t}

t %>%  group_by(neuron) %>%
  filter(#saccade.dur>80, #Removes any saccade shorter than 80ms
    abs(verg.amp)>2, #removes saccades with vergence change of less than 1.5 deg
    # dur<800,
    min.verg.angle> -5) %>% #removes trials with eye-coil problems (rare)
  group_by(neuron,convergent) %>%
  mutate(n=n(),spiketimes=showrasters*counter) ->
  gtemp


if (sample.saccades) {
  #This block chooses 20 random saccades of each type for each neuron
  gtemp %>%
    group_by(neuron,convergent,sacnum) %>%
    summarise() %>%
    ungroup() %>%
    group_by(neuron,convergent) %>%
    sample_n(20,replace=TRUE) %>%
    mutate(plotthis=TRUE) ->
    randomsaccades
  
  #This block orders the saccades by duration
  gtemp%>%
    left_join(randomsaccades) %>%
    ##############Comment out next line if you don't want to sample###########
  filter(plotthis) %>% #only plots the 20 random saccades
    group_by(neuron,convergent,sacnum) %>% 
    summarize(dur=first(saccade.dur)) %>% 
    arrange(desc(dur)) %>% #sorts by duration
    mutate(snum=row_number()) %>% #re-numbers saccades in their new order for plotting purposes
    left_join(gtemp,.)-> #merges these new numbers with the full dataset. 
    #The join is only necessary because we used summarize above. Summarize gives one number for each saccade.
    #By joining back to the original set, we are just adding that one number to all 550 ms of each saccade
    g
}else{
  gtemp%>%
    group_by(neuron,convergent,sacnum) %>% 
    summarize(dur=first(saccade.dur)) %>% 
    arrange(desc(dur)) %>% #sorts by duration
    mutate(snum=row_number()) %>% #re-numbers saccades in their new order for plotting purposes
    left_join(gtemp,.)-> #merges these new numbers with the full dataset. 
    #The join is only necessary because we used summarize above. Summarize gives one number for each saccade.
    #By joining back to the original set, we are just adding that one number to all 550 ms of each saccade
    g
}

gp<- ggplot(filter(g,celltype=='NRTP')) + 
  theme_bw()+ #Removes gray background 
  xlab('Time from Saccade Onset (ms)')+
  geom_histogram(aes(spiketimes,100*..ncount..),alpha=1,binwidth=10,fill='black',color='black')+
  # geom_freqpoly(aes(spiketimes,100*..ncount..),alpha=1,bins=40,color='darkred',size=1)+
  geom_point(aes(counter,showrasters* snum*5+110),shape='|',size=0.5,color='black')+
  facet_grid(neuron~convergent,scales='free_y',space='free_y')+
  # facet_wrap(~neuron,ncol=4,scales='free')+
  # facet_wrap(~neuron,ncol=4)+
  geom_vline(xintercept=0,color='red',size=1)+
  # geom_point(aes(counter,verg.change*10-250),color='darkgreen',size=1,shape='-')+
  geom_line(aes(counter,rep-50,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  geom_line(aes(counter,lep-50,group=sacnum),color='red',size=0.5,alpha=1/10)+
  geom_line(aes(counter,repV-100,group=sacnum),color='blue',size=0.5,alpha=1/10)+
  geom_line(aes(counter,lepV-100,group=sacnum),color='red',size=0.5,alpha=1/10)+
  geom_line(aes(counter,verg.angle*10-300,group=sacnum),color='darkgreen',size=0.5,alpha=1/5)+
  # geom_line(aes(counter,sdf-500,group=sacnum),color='orange',size=0.5,alpha=1/5)
  # coord_cartesian(xlim=c(-200,300),expand=FALSE)+
  # theme(axis.text=element_text(size=16),
        # panel.margin=unit(2,'lines'),
        # strip.text=element_text(size=16),
        # axis.title.x=element_text(size=16))+
  scale_y_continuous(breaks=c(-300,-250,-200,-150),labels=c(0,5,10,15))+ #re-labels so verg.angle is accurate
  ylab('Degrees')+
  expand_limits(y=c(-300))
ggsave('TestRasters.pdf',height=40,width=8,plot=gp)


###################
#Plot and model peak vergence velocity vs vergence position
###################
t %>%
  group_by(neuron,sacnum) %>%
  summarize(peak.verg.velocity=first(peak.verg.velocity),
            maxfr=first(maxfr),
            verg.amp=first(verg.amp),
            max.verg.angle=first(max.verg.angle),
            max.verg.velocity=first(max.verg.velocity)) %>%
  filter(abs(peak.verg.velocity)<1500)->
  summaryforplot
# t<- NULL
gp<- ggplot(summaryforplot)+geom_point(aes(peak.verg.velocity,maxfr),alpha=1/3,size=1)+
  facet_wrap(~neuron,ncol=3,scales='free')+
  geom_hline(yintercept=200,size=1, alpha=0.5,color='orange')+
  theme(legend.position="top",
        axis.text=element_text(size=8),
        # panel.margin=unit(2,'lines'),
        strip.text=element_text(size=8),
        axis.title.x=element_text(size=8))


ggsave('peakvelocity.pdf',plot=gp, height=40, width=10)

summaryforplot %>% 
  group_by(neuron) %>%
  do(m=lm(maxfr~max.verg.angle+max.verg.velocity,data=.)) -> 
  mm

saveRDS(mm,'crashmm.RDS')

r<- data.frame()
r2 <- NULL

for (i in 1:nrow(mm)){
  # message('Trying...')
  bb<- relaimpo::calc.relimp(mm$m[[i]])
  b<- bb$lmg
  r<-rbind(r,b)
  r2<- c(r2,bb$R2)
  
}

r$neuron<-mm$neuron
r$R2<- r2

names(r)<- c(names(b),'neuron','R2')

r %>%
  separate(neuron, c('monkey','cellnum'),remove=FALSE) %>%
  mutate(celltype=as.factor(as.numeric(cellnum)>100))->
  r
levels(r$celltype)<- c("NRTP","SOA")

gp<- ggplot(aes(max.verg.angle,max.verg.velocity),data=r)+
  # geom_point(size=4,aes(color=R2,shape=verg.amp>max.verg.velocity))+
  geom_point(size=4,aes(shape=monkey,color=celltype))+
  # geom_point(size=4,aes(color=R2))+
  # scale_color_gradient(low='blue',high='red')+
  # geom_point(size=4,aes(color=verg.amp>max.verg.velocity))+
  geom_text(aes(label=neuron),check_overlap=FALSE,size=3,vjust=-1)+
  geom_abline(intercept=0,slope=1)+
  # coord_cartesian(xlim=c(-0.5,1),ylim=c(-0.5,1))+
  theme(legend.position="top")+
  ggtitle('Relative Importance')

ggsave('RelativeImportance.pdf',plot=gp,height=10,width=10)


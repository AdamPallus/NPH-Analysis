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
joinsaccades<-function(t,buffer=20,threshold=30){
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
x<- do.call('rbind',apply(stimes,1,jsac))
x %>%
group_by(sacnum) %>%
mutate(counter=time-first(time)) ->
x
left_join(t,x,by='time')
}
markEnhancement<- function(v, threshold1=15,threshold2=8){
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
#load all the .csv files in the data folder, then add a column naming the neuron,
#using the file name as the default name, then put them all together in one long data frame
t<- loadnewcsv(path="C:/Users/setup/Desktop/SOA/",referencefile=NULL)
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
loadnewcsv<- function(referencefile=NULL,path="C:/Users/setup/Desktop/SOA/"){
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
verg.velocity=parabolicdiff(verg.angle,7))->
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
bootciNOCONJ <- function(t,n=100,alpha=0.05,formula='lagsdf~rev+lev'){
neuron=t$neuron[1]
message(paste('Currently working on',neuron))
t %>%
ungroup() %>%
filter(saccade.type != 'conj') %>%
bootstrap(n) %>%
do(tidy(lm(formula,.)))%>%
group_by(term) %>%
summarize(
low=quantile(estimate, alpha / 2),
high=quantile(estimate, 1 - alpha / 2),
m=first(estimate)) ->
ci
ci$neuron<- neuron
return(ci)
}
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
joinsaccades<-function(t,buffer=20,threshold=30){
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
x<- do.call('rbind',apply(stimes,1,jsac))
x %>%
group_by(sacnum) %>%
mutate(counter=time-first(time)) ->
x
left_join(t,x,by='time')
}
markEnhancement<- function(v, threshold1=15,threshold2=8){
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
#load all the .csv files in the data folder, then add a column naming the neuron,
#using the file name as the default name, then put them all together in one long data frame
t<- loadnewcsv(path="C:/Users/setup/Desktop/SOA/",referencefile=NULL)
# t<- rbind(o,t)
# o<-NULL
t %>%
group_by(neuron) %>%
mutate(verg.velocity=parabolicdiff(verg.angle,7),
sdf = spikedensity(rasters,sd=10))->
t
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
# separate(neuron, c('monkey','cellnum2'),remove=FALSE) %>%
# filter(monkey=='Pilchuck') %>%
group_by(neuron) %>%
filter(mean.Verg.Vel<1,
dur>75,
mean.Verg.Angle<10,
mean.Verg.Angle> -30) %>%
mutate(r=cor(mean.Verg.Angle,mean.Spikerate)) ->
sp
# m<- NULL
# s.old<-readRDS('NRTPsummaryforplot.RDS')
# summaryforplot<-rbind(s.old,summaryforplot)
#
# saveRDS(rbind(s.old,summaryforplot),'NRTPsummaryforplot-new.RDS')
#
# summaryforplot<- readRDS('NRTPsummaryforplot-new.RDS')
ggplot(aes(mean.Verg.Angle,mean.Spikerate),data=sp)+
geom_point(size=2,alpha=1/2)+
# facet_wrap(~neuron,scales='free_x')+
facet_wrap(~neuron,ncol=2)+
stat_smooth(method='lm')+
geom_label(aes(0,100,label=paste('r = ',round(r,2))))+
ggtitle('Firing Rate as a function of Vergence Angle during Fixations')
#
# gp<-ggplot(aes(mean.Verg.Angle,mean.Spikerate),data=filter(sp,mean.Verg.Angle> -5))+
# geom_point(size=2,alpha=1/5)+
# # facet_wrap(~neuron,scales='free_x')+
# facet_wrap(~neuron,ncol=3)+
# stat_smooth(method='lm')+
# geom_label(aes(0,100,label=paste('r = ',round(r,2))))+
# ggtitle('Firing Rate as a function of Vergence Angle during Fixations')
# ggsave('TestRasterLabel.png',height=45,width=5,plot=gp)
# gp<- ggplot(filter(summaryforplot,dur>50,cellnum %in% c(1,25,28))) +
# geom_point(aes(mean.Verg.Angle,mean.Spikerate),size=2,alpha=1/5)+
#   # facet_wrap(~neuron,scales='free_x')+
#     facet_wrap(~neuron,ncol=2)+
#   stat_smooth(method='lm')+
#   ggtitle('Firing Rate as a function of Vergence Angle during Fixations')+
#   xlab('Mean Vergence Angle (deg)')+
#   ylab('Mean Firing Rate (spk/s)')+
#   theme_bw()
# ggsave('NRTP-RatePosition.PDF',plot=gp)
#
# gp<- ggplot(filter(summaryforplot,dur>50,cellnum %in% c(30))) +
# geom_point(aes(mean.Verg.Angle,mean.Spikerate),size=3,alpha=1/2)+
#   # facet_wrap(~neuron,scales='free_x')+
#     facet_wrap(~neuron,ncol=2)+
#   stat_smooth(method='lm')+
#   ggtitle('Firing Rate as a function of Vergence Angle during Fixations')+
#   xlab('Mean Vergence Angle (deg)')+
#   ylab('Mean Firing Rate (spk/s)')+
#   theme_bw()
# ggsave('NRTP-RatePosition-Single.PDF',plot=gp)
# qplot(mean.Verg.Angle,mean.Spikerate,data=summaryforplot)+
#   geom_point()+
#   stat_smooth(method='lm')+
#   facet_wrap(~neuron)+
#   ggtitle('Firing Rate as a function of Vergence Angle during Fixations')
summaryforplot %>%
# separate(neuron, c('monkey','cellnum2'),remove=FALSE) %>%
# filter(monkey=='Pilchuck') %>%
group_by(neuron) %>%
filter(mean.Verg.Vel<1,
dur>75,
mean.Verg.Angle<10,
mean.Verg.Angle> -30) %>%
mutate(r=cor(mean.Verg.Angle,mean.Spikerate)) ->
sp
ggplot(aes(mean.Verg.Angle,mean.Spikerate),data=sp)+
geom_point(size=2,alpha=1/2)+
# facet_wrap(~neuron,scales='free_x')+
facet_wrap(~neuron,ncol=2)+
stat_smooth(method='lm')+
geom_label(aes(0,100,label=paste('r = ',round(r,2))))+
ggtitle('Firing Rate as a function of Vergence Angle during Fixations')
ggplot(aes(mean.Verg.Angle,mean.Spikerate),data=sp)+
geom_point(size=2,alpha=1/2)+
# facet_wrap(~neuron,scales='free_x')+
facet_wrap(~neuron,ncol=2)+
stat_smooth(method='lm')+
geom_label(aes(0,100,label=paste('r = ',round(r,2))))+
ggtitle('Firing Rate as a function of Vergence Angle during Fixations')
library(checkpoint)
library(checkpoint)
checkpoint("2017-01-20")
library(dplyr)
install.packages('dplyr')
install.packages('dplyr')
install.packages("dplyr")
install.packages(c("RUnit", "survival"))
library(checkpoint)
checkpoint('2017-01-24')
install.pacakges(c('formatR','htmltools','caTools','bitops','jsonlite','base64enc','rprojroot','rmarkdown')
install.pacakges(c('formatR','htmltools','caTools','bitops','jsonlite','base64enc','rprojroot','rmarkdown'))
install.packages(c('formatR','htmltools','caTools','bitops','jsonlite','base64enc','rprojroot','rmarkdown'))
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
library(checkpoint)
checkpoint("2017-01-20")
options(repos = c(CRAN = "https://mran.revolutionanalytics.com/snapshot/2017-01-24"))

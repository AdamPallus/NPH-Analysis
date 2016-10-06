#This is such a mess. PLAN: make this an R Markdown file



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

path="C:/Users/setup/Desktop/NRTP Vergence/PPRF/"

t<- readRDS('PPRF.RDS')
n<- loadnewcsv(r=t,path=path)
t<- rbind(t,n)

#the lag of these pprf cells is 14-16. For simplicity we'll use 15. Change this if its a problem
#dplyr doesn't support lagging by groups using different variables
d=15
t<- mutate(t,
           cev=(rev+lev)/2,
           lagsdf=lag(sdf,d),
           lagrasters=lag(rasters,d)
           )

bufferlength<- 20
saccade.length<- 120
t%>%
  group_by(neuron) %>%
  mutate(time=row_number()) %>%
  # do(joinsaccadesuniform(.,buffer=bufferlength,threshold=20,saccade.length=saccade.length))->
  do(joinsaccades(.,buffer=bufferlength,threshold=20))->
  # do(joinsaccades(.,buffer=bufferlength,threshold=20))->
  t

t %>%
  group_by(neuron) %>%
  mutate(issaccade=!is.na(sacnum)) %>%
  filter(issaccade) %>%
  group_by(neuron,sacnum) %>%
  mutate(#dur=n(),
    peak.conj.velocity=maxabs(cev),
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
    maxfr=max(sdf),
    counter=time-first(time)-bufferlength,
    verg.change=verg.angle-mean(verg.angle[1:50]),
    # verg.amp= last(verg.angle)-first(verg.angle),
    verg.amp= verg.angle[n()-bufferlength]-verg.angle[bufferlength],
    showrasters=replace(lagrasters,lagrasters<1,NA))->
  t


ci <- data.frame()
n=unique(t$neuron)

#Disconjugate saccades test
#9/9/2016 note: I'm using sdf>10 as a filter, but it would be better to just accurately mark
#the end of saccades. Use joinsaccades instead of joinsaccades uniform. 
#Need to make sure the function stil works
for (i in 1:length(n)){
  t %>%
    # filter(neuron==n[i],abs(verg.amp)>2,r.amp>2,peak.conj.velocity>0,sdf>10 ) %>% 
    filter(neuron==n[i],abs(verg.amp)>2,r.amp>2,peak.conj.velocity>0,sdf>10 ) %>% 
    # bootci(n=1999,alpha=0.01,formula=f) ->
    bootci(n=1999,alpha=0.05,formula='lagsdf~rev+lev') ->
    temp
  temp$neuron<-n[i]
  ci<-rbind(ci,temp)
}

ci %>%
  group_by(neuron) %>%
  mutate(m=(low+high)/2) %>%
  filter(term != '(Intercept)') ->
  pci
g<-ggplot(pci,aes(factor(term),m))

g+geom_errorbar(aes(ymin=low,ymax=high,color=neuron),size=2,alpha=1/2,width=0.2)+
  geom_hline(yintercept = 0)+
  coord_flip()+
  ylab('Bootstrap conficence interval for parameter estimate')+
  xlab('Term')
  

#################################
#######experimental below#########
##############################

t6<- filter(t,neuron=='Ozette-504')
mipsi<-lm(lagsdf~rev,data=filter(t6,abs(verg.amp)>2,r.amp>2,sdf>10))
mcontra<-lm(lagsdf~lev,data=filter(t6,abs(verg.amp)>2,r.amp>2,sdf>10))
e<- lm(lagsdf~rev+lev,data=filter(t6,abs(verg.amp)>2,r.amp>2,sdf>10))
t6<- ungroup(t6)
t6<- mutate(t6,#predictconj=predict(mconj,newdata=t2),
           predictipsi=predict(mipsi,newdata=t6),
           predictcontra=predict(mcontra,newdata=t6),
           predictipsi=replace(predictipsi,predictipsi<0,0),
           predictcontra=replace(predictcontra,predictcontra<0,0))
p<- filter(t6,r.amp>2,abs(verg.amp)>2,peak.conj.velocity>0)
# p<- filter(xx,r.amp>4,saccade.dur<100,verg.amp>3)
# p<- filter(xx,r.amp>4)
# p<- filter(xx,r.amp>4,saccade.dur<150,verg.amp< -4)

goodsacs<- unique(p$sacnum)

nsac=length(goodsacs)

manipulate(ggplot(filter(t6,sacnum==goodsacs[sac]))+
             geom_line(aes(counter,verg.velocity),color='purple')+
             # geom_line(aes(counter,conj.velocity),color='brown')+
             geom_line(aes(counter,(lev+rev)/2),color='brown')+
             # geom_line(aes(counter,rev),color='red')+
             # geom_line(aes(counter,lev),color='blue')+
             geom_line(aes(counter,predictipsi),color='red',linetype=3)+
             geom_line(aes(counter,predictcontra),color='blue',linetype=3)+
             
             # geom_hline(yintercept = c(-2,2))+
             geom_line(aes(counter,verg.angle*10),color='darkgreen')+
             geom_point(aes(counter,showrasters*100),shape='|',size=3)+
             geom_area(aes(counter,lagsdf),alpha=0.2)+
             
             coord_cartesian(ylim=c(-75,300)),
           sac=slider(1,nsac,step=1)
)


  
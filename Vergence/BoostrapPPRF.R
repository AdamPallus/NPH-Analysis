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


t %>%
  group_by(neuron) %>%
  mutate(s=markSaccades(conj.velocity,buffer=10,threshold=10),
         isfixation=s<0) %>%
  filter(!isfixation) %>%
  group_by(neuron,s) %>%
  summarize(meanfr=mean(sdf),
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
         R.H.Amp=last(rep)-first(rep),
         L.H.Amp=last(lep)-first(lep),
         R.V.Amp=repV[1]-repV[length(repV)],
         L.V.Amp=lepV[1]-lepV[length(lepV)],
         maxamp=max(abs(R.H.Amp),abs(R.V.Amp),abs(L.H.Amp),abs(L.V.Amp)))->
  tt

ggplot(tt)+
  geom_point(aes(R.H.Amp,nspikes),alpha=1/4,color='orange')+
  geom_point(aes(L.H.Amp,nspikes),color='purple',alpha=1/4)+
  facet_wrap(~neuron)

ggplot(filter(tt,R.H.Amp+L.H.Amp>4))+geom_boxplot(aes(isconverging,nspikes,fill=neuron))+facet_wrap(~neuron)

m<- lm(nspikes~R.H.Amp+L.H.Amp,data=tt)
summary(m)
m<- lm(nspikes~R.H.Amp,data=tt)
summary(m)
m<- lm(nspikes~L.H.Amp,data=tt)
summary(m)

t<- mutate(t,lagsdf=lag(sdf,20))
m<- lm(lag(sdf,20) ~ lev+rev,data=t)
r<-relaimpo::calc.relimp(m)
t$conj.velocity=(t$lev+t$rev)/2
bic<-regsubsets(lagsdf ~ lev+rev+conj.velocity,data=t)

m<- lm(lagsdf ~ lev+rev,data=filter(t,!isfixation))

plot(calc.relimp(m))


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

t<- t %>% group_by(neuron) %>% mutate(lagsdf=lag(sdf,20))

ci <- data.frame()

n=unique(t$neuron)

for (i in 1:length(n)){
  t %>%
    filter(neuron==n[i],lagsdf>10) %>%
    # bootci(n=1999,alpha=0.01,formula=f) ->
    bootci(n=1999,alpha=0.01) ->
    temp
  temp$neuron<-n[i]
  ci<-rbind(ci,temp)
}
# ci<- bootci(t,n=19,alpha=0.05)




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


pci %>%
  separate(term,c('Direction','PV','HV'),remove=FALSE) %>%
  mutate(scale=9*as.numeric(PV=='V')+1,
         low=low*scale, high=high*scale,m=m*scale) ->
  ppci

  g<-ggplot(ppci,aes(factor(term),m))

g+geom_linerange(aes(ymin=low,ymax=high,color=neuron),size=2)+
  facet_grid(neuron~.)+
  geom_hline(x=0)+
  coord_flip()+
ylab('Coefficient: Velocity scaled by 10')




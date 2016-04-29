
setx<- function(t){
x<- dplyr::filter(t,as.numeric(cellnum) %in% c(6,211))


x %>%
  group_by(neuron) %>%
  mutate(sdf=spikedensity(rasters,sd=12),
      sdflag=lag(sdf,25),
      sdfstretch=(sdf+sdflag)/2,
         isSaccade=!isfixation,
         conj.position=sqrt((rep^2+lep^2)/2)+sqrt((repV^2+lepV^2)/2),
         converging=verg.velocity>0,
         verg.acceleration=parabolicdiff(verg.velocity,7)) %>%
  group_by(neuron,s) %>%
  mutate(verg.amp=last(verg.angle)-first(verg.angle),
         conj.amp=last(conj.position)-first(conj.position),
         peak.conj.velocity=maxabs(conj.velocity)) ->
  x
}

tryingtomodel<- function(x,leadtime=25){
xfit<-filter(x,sdflag>20,verg.amp>3)

m1<-lm(sdfstretch~verg.velocity,data=xfit)
# m2<-lm(sdflag~verg.velocity*isSaccade,data=xfit)
m2<-lm(sdfstretch~verg.velocity+verg.angle,data=xfit)

m3<-lm(sdflag~verg.velocity*isSaccade+conj.velocity,data=xfit)
m4<-lm(sdflag~verg.velocity*isSaccade+conj.amp,data=xfit)

x %>% ungroup() %>%
  mutate(m1=predict(m1,newdata=.),
         m2=predict(m2,newdata=.),
         m3=predict(m3,newdata=.),
         m4=predict(m4,newdata=.)) ->
  xplot

xplot$m1[xplot$m1<0]=0
xplot$m2[xplot$m2<0]=0
xplot$m3[xplot$m3<0]=0
xplot$m4[xplot$m4<0]=0


bufferlength<- 50
xplot%>%
  mutate(conj.velocity=sqrt((rev^2+lev^2)/2)+sqrt((revV^2+levV^2)/2),
         ss=markSaccades(conj.velocity,buffer=bufferlength,threshold=10),
         isfixation=ss<0,
         convergent=verg.amp>0) %>%
  group_by(neuron,ss) %>%
  mutate(dur=n(),
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
         verg.amp=verg.angle[n()]-verg.angle[1],
         peak.verg.velocity= maxabs(verg.velocity),
         min.verg.trans = min(verg.velocity),
         max.verg.trans = max(verg.velocity),
         maxfr=max(sdf),
         counter=time-time[1]-bufferlength,
         showrasters=replace(rasters,rasters<1,NA))->
  p

p$convergent<-as.factor(p$convergent)
levels(p$convergent)<- c('Divergent','Convergent')

p %>% filter(R.H.Amp>5,abs(verg.amp)> 1,convergent=='Divergent') %>%
  group_by(neuron) %>%
  mutate(spiketimes=showrasters*counter,
         convergent=verg.amp>0,
         nsaccades=length(unique(ss)),
         snum=dense_rank(dur))->
  g


ggp<-ggplot(filter(g,!isfixation)) + 
  # geom_point(aes(counter,verg.angle),color='darkgreen',size=1,shape='-')+
  # geom_point(aes(counter,verg.velocity/10),color='lightgreen',size=1,shape='-')+
  geom_histogram(aes(spiketimes),alpha=1/5,bins=80)+
  geom_point(aes(counter,showrasters*(nsaccades-snum+2)),shape='|',size=3)+
  # facet_grid(convergent~neuron)+
  facet_wrap(~neuron,ncol=1)+
  geom_vline(xintercept=0)+
  # geom_point(aes(counter,rep+50),color='red',size=0.5,alpha=1/5)+
  # geom_point(aes(counter,lep+50),color='blue',size=0.5,alpha=1/5)+
  coord_cartesian(xlim=c(-50,100))
  # geom_point(aes(counter,rev/10),color='red')+  
  # geom_point(aes(counter-leadtime,m1/10-50),color='blue',alpha=1/5,size=1)+
  # geom_point(aes(counter-leadtime,m2/10-50),color='orange',alpha=1/5,size=1)+
  # geom_point(aes(counter-leadtime,m3-300),color='magenta')+
  # geom_point(aes(counter-leadtime,m4-300),color='pink')+
  # geom_point(aes(counter,sdf/10-50),color='black',alpha=1/5,size=1/2)
return(ggp)
}

# facet_wrap(~s,ncol=5,scales='free')+
# geom_point(aes(counter,verg.velocity/10),color='pink')+


  
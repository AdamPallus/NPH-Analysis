t<- readRDS('enhancemarked.RDS')

z<- filter(t,neuron=='Bee-211')
z<- filter(t,neuron=='Bee-108')

z$sdfsmooth=spikedensity(rasters=z$rasters,sd = 20)
d<- z$dynamiclead[1]
z$sdflag<- lag(z$sdfsmooth,d)
slowfit<- lm('sdflag~verg.angle+verg.velocity:enhance.type',data=z)
simplefit<- lm('sdflag~verg.angle+verg.velocity',data=z)

improvement<- BIC(simplefit)-BIC(slowfit)
message(paste("BIC reduced by",round(improvement,1)))

z<- mutate(z,slow.prediction=predict(slowfit,newdata=z),
            slow.prediction=replace(slow.prediction,slow.prediction<0,0),
           simple.prediction=predict(simplefit,newdata=z),
           simple.prediction=replace(simple.prediction,simple.prediction<0,0))

z<- z %>% group_by(time) %>% summarize_each(funs(first))

z<- mutate(z, converge=as.numeric(enhance.type=='convergence'),
           converge=replace(converge,converge<1,NA))

z<- dplyr::mutate(z,converge=replace(converge,converge==0,NA))
z<- mutate(z, enhance.velocity=lev-rev,
           slow.velocity=replace(enhance.velocity,!is.na(converge),NA),
           enhance.velocity=replace(enhance.velocity,is.na(converge),NA),
           showrasters=replace(rasters,rasters<1,NA))



# manipulate(ggplot(filter(z,time>=window,time<window+5000))+
#              geom_area(aes(time,sdf),alpha=1/10)+
#              # geom_line(aes(time,(rev+lev)/2))+
#              geom_line(aes(time,lev-rev),color='pink',alpha=1)
#              # geom_point(aes(time,converge*-100))+
#              # geom_hline(yintercept=c(50,-50))
#              # geom_point(aes(time,showslow*100))+
#              # geom_hline(yintercept = c(208,233))+
#              # geom_line(aes(time,slow.prediction),color='orange')
#              ,
#            # geom_line(aes(time,lag(lev,3)-rev),color='pink'),
#            window=slider(1,max(z$time-5000),step=5000))

manipulate(ggplot(filter(z,time>=window,time<window+5000))+
             geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
             geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
             geom_line(aes(time,(lep-rep)*5-100),color='darkgreen')+
             geom_point(aes(time,showrasters+50),shape='|')+
             # geom_line(aes(time,slow.prediction),color='orange')+
             # geom_line(aes(time,(rev+lev)/2),color='darkred')+
             # geom_line(aes(time,(revV+levV)/2),color='red')+
             # theme_bw()+
             ylim(c(-100,200))
           ,
           window=slider(5000,max(z$time-5000),step=5000))

manipulate(ggplot(filter(z,time>=window,time<window+5000))+
             geom_area(aes(time,sdfsmooth),alpha=1/10)+
             geom_line(aes(time,slow.velocity),color='darkblue',alpha=1)+
             geom_line(aes(time,slow.prediction),color='orange',alpha=1)+
             geom_line(aes(time,simple.prediction),color='darkred')+
             geom_line(aes(time,enhance.velocity),size=2,color='darkblue',alpha=1/10)+
             geom_line(aes(time,(lep-rep)*5-100),color='darkgreen')+
             geom_point(aes(time,showrasters+50),shape='|')+
             # geom_line(aes(time,slow.prediction),color='orange')+
             # geom_line(aes(time,(rev+lev)/2),color='darkred')+
             # geom_line(aes(time,(revV+levV)/2),color='red')+
             # theme_bw()+
             ylim(c(-100,200))
           ,
           window=slider(5000,max(z$time-5000),step=5000))

##snapshots:
ggplot(filter(z,time>=51500,time<54000))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,slow.prediction),color='orange')+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  geom_line(aes(time,(lep-rep)*5-100),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  theme_bw()

ggplot(filter(z,time>=54500,time<57000))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  geom_line(aes(time,(lep-rep)*5-100),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  theme_bw()

#131000-134000 pause with spikes after
#145500-149000
#225500-229000 pause for non-enhanced then pause for enhanced
#252000-255000



#Transients are timing?
bufferlength<- 50

zz<- joinsaccades(z,buffer=bufferlength,threshold=20)

zz %>%
  group_by(sacnum) %>%
  mutate(verg.amp= verg.angle[n()-bufferlength]-verg.angle[bufferlength],
         conj.position.h=(lep+rep)/2,
         conj.position.v=(lepV+repV)/2,
         h.amp=conj.position.h[n()-bufferlength]-conj.position.h[bufferlength],
         v.amp=conj.position.v[n()-bufferlength]-conj.position.v[bufferlength],
         r.amp=sqrt(h.amp^2+v.amp^2),
         counter=row_number(),
         plep=lep-first(conj.position.h),
         prep=rep-first(conj.position.h),
         plepV=lepV-first(conj.position.v),
         prepV=repV-first(conj.position.v)) %>%
  filter(r.amp>3)->
  zz
goodsacs<- unique(zz$sacnum)     
nsac=length(goodsacs)

windowsize<-1000
manipulate(ggplot(filter(zz,sacnum==goodsacs[sac]))+
             # geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(counter,(lev-rev)-100),color='darkblue',alpha=1)+
             geom_line(aes(counter,(lev-rev)),color='green',alpha=1)+
             geom_line(aes(counter,enhance.velocity-100),size=2,color='darkblue')+
             geom_line(aes(counter,(lep-rep)*5-100),color='darkgreen')+
             geom_line(aes(counter,lev-lag(rev,4)),color='red',linetype=2)+
             geom_line(aes(counter,lag(lev,4)-rev),color='blue',linetype=2)+
             geom_line(aes(counter,lev-lag(rev,3)),color='red',linetype=3)+
             geom_line(aes(counter,lag(lev,3)-rev),color='blue',linetype=3)+
             geom_line(aes(counter,lev-lag(rev,2)),color='red',linetype=4)+
             geom_line(aes(counter,lag(lev,2)-rev),color='blue',linetype=4)+
             geom_line(aes(counter,lev-lag(rev,1)),color='red',linetype=5)+
             geom_line(aes(counter,lag(lev,1)-rev),color='blue',linetype=5)+
             geom_line(aes(counter,lev-lag(rev,6)),color='red',linetype=6)+
             geom_line(aes(counter,lag(lev,6)-rev),color='blue',linetype=6)+
           geom_line(aes(counter,lev-lag(rev,9)),color='red',linetype=9)+
             geom_line(aes(counter,lag(lev,9)-rev),color='blue',linetype=9)+
             geom_point(aes(plep*10+200,100+plepV*10),color='blue',alpha=1/2)+
             geom_point(aes(prep*10+200,100+prepV*10),color='red',alpha=1/2)+
             geom_point(aes(200,100),size=3)
             # ylim(c(-100,200))
           ,
           sac=slider(2,nsac,step=1))

#HEY! WHAT IF WE USE THE DIFFERENCE IN TIMING TO IDENTIFY WHEN ENHANCEMENT HAPPENS??
#transients during vertical saccades?

manipulate(ggplot(filter(zz,sacnum==goodsacs[sac]))+
             # geom_area(aes(time,sdf),alpha=1/10)+
            geom_point(aes(lep,lepV),color='blue')+
             geom_point(aes(rep,repV),color='red')

           # ylim(c(-100,200))
           ,
           sac=slider(1,nsac,step=1))





manipulate(ggplot(filter(t,time>=window,time<window+5000))+
             geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,(rev+lev)/2))+
             geom_line(aes(time,lev-rev),color='pink')+
             geom_point(aes(time,conv*-100))+
             # geom_point(aes(time,showslow*100))+
             # geom_hline(yintercept = c(208,233))+
             geom_line(aes(time,slow.prediction),color='orange'),
           # geom_line(aes(time,lag(lev,3)-rev),color='pink'),
           window=slider(1,max(z$time-5000),step=5000))


#SUMMARY OF SACCADEs
zz<- joinsaccadesuniform(z,buffer = 100,threshold = 20)

zz %>%
  group_by(neuron,sacnum) %>%
  mutate(conj.h=(lep+rep)/2,
         conj.v=(lepV+repV)/2,
         verg.angle=(lep-rep)) %>%
  summarize(peak.conj.velocity=maxabs(conj.velocity),
            max.verg.velocity=max(verg.velocity),
            min.verg.velocity=min(verg.velocity),
            conj.h.amp=last(conj.h)-first(conj.h),
            conj.v.amp=last(conj.v)-first(conj.v),
            # conj.angle=atan2(conj.v,conj.h)*180/pi,
            r.amp=sqrt(conj.h.amp^2+conj.v.amp^2),
            verg.amp=last(verg.angle)-first(verg.angle)) %>%
  mutate(saccade.type='conj',
         saccade.type=replace(saccade.type,verg.amp< -2 & r.amp>2,'diverging'),
         saccade.type=replace(saccade.type,verg.amp> 2& r.amp>2,'converging'),
         saccade.type=replace(saccade.type,is.na(sacnum) | r.amp<2,'no.saccade')) ->
    sz



ggplot(filter(sz,saccade.type != 'no.saccade',abs(conj.v.amp)<2,
              peak.conj.velocity<800,max.verg.velocity<200))+
  geom_point(aes(peak.conj.velocity,max.verg.velocity,color=saccade.type))+
  stat_smooth(aes(peak.conj.velocity,max.verg.velocity,color=saccade.type),method='lm')

d<-filter(sz,saccade.type != 'no.saccade',abs(conj.v.amp)<2,peak.conj.velocity<800,max.verg.velocity<200)
mconv= lm(max.verg.velocity~peak.conj.velocity,data=filter(d,saccade.type=='converging'))
mconj= lm(max.verg.velocity~peak.conj.velocity,data=filter(d,saccade.type=='conj'))
summary(mconv)
summary(mconj)

gg<- filter(sz,verg.amp>0,verg.amp<20,max.verg.velocity<300)

qplot(verg.amp,max.verg.velocity,data=gg)+stat_smooth(method='lm')
cor(gg$verg.amp,gg$max.verg.velocity)^2


sz$bin.peak.conj.velocity=cut(sz$peak.conj.velocity,c(seq(50,650,by=50)))
ggplot(filter(sz,saccade.type %in% c('converging','conj'),
              abs(conj.v.amp)<200,abs(conj.h.amp)>2,!is.na(bin.peak.conj.velocity),
              max.verg.velocity<300))+
  geom_boxplot(aes(bin.peak.conj.velocity,max.verg.velocity,fill=saccade.type))

sz$bin.peak.conj.velocity=cut(sz$peak.conj.velocity,c(seq(-650,0,by=50)))
ggplot(filter(sz,saccade.type %in% c('diverging','conj'),conj.v.amp<2,!is.na(bin.peak.conj.velocity)))+
  geom_boxplot(aes(bin.peak.conj.velocity,min.verg.velocity,fill=saccade.type))

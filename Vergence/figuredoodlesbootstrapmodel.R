manipulate()
gc<- filter(gc, time>=starttime,time<stoptime)
gc<- mutate(gc,sdf=spikedensity(rasters,sd=sd))
gs<- ggplot(gc)+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  theme_bw()+
  xlab('Time (ms)')+
  ylab('Vergence Velocity (deg/s)')+
  ylim(c(-100,maxplot))

ChosenCell='Ozette-1142'
gc<- filter(t,neuron==ChosenCell) #saved as a separate variable so it can be used later
gc<-mutate(gc,time=row_number(), showrasters=replace(rasters,rasters<1,NA))

window_size <- 5000
step_size <- 1000
manipulate(ggplot(filter(gc,time>=window,time<window+window_size))+
             geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
             # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
             geom_line(aes(time,(lep-rep)*10+50),color='darkgreen')+
             # geom_line(aes(time,rep+200),color='darkred')+
             # geom_line(aes(time,lep+200),color='blue')+
             # geom_line(aes(time,repV+220),color='darkred',linetype=2)+
             # geom_line(aes(time,lepV+220),color='blue',linetype=2)+
             # geom_line(aes(time,(lev+rev)/2),color='maroon')+
             # geom_line(aes(time,(levV+revV)/2),color='maroon',linetype=2)+
             
             geom_point(aes(time,showrasters+50),shape='|')+
             # geom_line(aes(time,slow.prediction),color='orange')+
             # geom_line(aes(time,(rev+lev)/2),color='darkred')+
             # geom_line(aes(time,(revV+levV)/2),color='red')+
             # theme_bw()+
             ylim(c(-100,300))
           ,
           window=slider(window_size,max(gc$time-window_size),step=step_size))


tp<- mutate(tp,Far_Response=verg.angle<0)
ggplot(aes(Convergence,Slow.Vergence),data=tp)+
  geom_point(aes(color=Far_Response),size=3,alpha=0.8)+
  geom_abline(slope=1)+
  geom_vline(xintercept=0,linetype=2)+
  xlab('Vergence Velocity Sensitivity during Enhanced Convergence')+
  ylab('Vergence Velocity Sensitivity during Slow Vergence Movements')+
  coord_fixed()+
  # geom_text(aes(label=neuron))
  theme(legend.position='top')


gx<- readRDS('bootstrapErrorBars.RDS')
tpg<- left_join(tp,gx,by='neuron')

ggplot(tpg,aes(x=Convergence,y=Slow.Vergence))+
  geom_point(aes(Convergence,Slow.Vergence,color=Far_Response),size=3,alpha=0.8)+
  geom_abline(slope=1)+
  geom_vline(xintercept=0,linetype=2)+
  geom_errorbarh(aes(xmin=conLOW,xmax=conHIGH))+
  geom_errorbar(aes(ymin=slowLOW,ymax=slowHIGH))+
  # geom_text(aes(label=neuron))+
  xlab('Vergence Velocity Sensitivity during Enhanced Convergence')+
  ylab('Vergence Velocity Sensitivity during Slow Vergence Movements')


#----------- craft 'bootstrapErrorBars.RDS'

ci<- readRDS('Bootstrap1999SOAnoconj.RDS')
ci %>%
  mutate(term=replace(term,term=='verg.velocity:enhance.typenone','Slow.Velocity'),
         term=replace(term,term=='verg.velocity:enhance.typeconvergence','Convergence'),
         term=replace(term,term=='verg.velocity:enhance.typedivergence','Divergence')) %>%
  # group_by(neuron) %>%
  filter(term != '(Intercept)') ->
  pci

vang<-pci$m[pci$term=='verg.angle']
con<-pci$m[pci$term=='Convergence']
slow<- pci$m[pci$term=='Slow.Velocity']
gx<- data.frame(vang=vang,con=con,slow=slow)
gx$neuron<- unique(pci$neuron)
gx$vangLOW<-pci$low[pci$term=='verg.angle']
gx$conLOW<-pci$low[pci$term=='Convergence']
gx$slowLOW<- pci$low[pci$term=='Slow.Velocity']
gx$vangHIGH<-pci$high[pci$term=='verg.angle']
gx$conHIGH<-pci$high[pci$term=='Convergence']
gx$slowHIGH<- pci$high[pci$term=='Slow.Velocity']

ggplot(aes(x=con,y=slow),data=gx)+
  geom_point()+
  geom_errorbarh(aes(xmin=conLOW,xmax=conHIGH))+
  geom_errorbar(aes(ymin=slowLOW,ymax=slowHIGH))+
  geom_abline(slope=1)

saveRDS(gx,'bootstrapErrorBars.RDS')

#----------Enhancement

#determine whether it's convergence or divergence -->
gc %>% 
  mutate(verg.enhance=!is.na(enhancenum), 
         enhance.type='none',
           verg.direction=verg.velocity>0)-> 
  gc
i<- gc$verg.enhance & !gc$verg.direction 
gc$enhance.type[i]<- 'divergence' 
i<- gc$verg.enhance & gc$verg.direction
gc$enhance.type[i]<- 'convergence' 
gc$verg.direction<- NULL 


#-------------AVERAGE SPIKERATE ------------
t %>%
  group_by(neuron) %>%
  summarize(mean.spikerate=mean(rasters),
            lag=first(dynamiclead),
            monkey=first(monkey),
            cellnum=first(cellnum)) ->
  k

ggplot(aes(mean.spikerate,lag),data=k)+
  geom_point(aes(color=monkey))+
  geom_text(aes(label=cellnum))

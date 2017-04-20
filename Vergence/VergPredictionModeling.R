t<- readRDS('SOA-NRTP.RDS')
source('addEnhancement.R')
modelVV<- function(t,chosenCell='Bee-113'){

  x<- ungroup(filter(t,neuron==chosenCell))
  if (!('time' %in% names(x))){
    x<- mutate(x,time=row_number())
  }
  x %>%
    mutate(verg.velocity=parabolicdiff(lep-rep,20),
           sdf=spikedensity(rasters,sd=20),
           sdf20=dplyr::lag(sdf,35)) ->
    # do(addEnhancement(.,lowthreshold=lowthreshold,highthreshold=highthreshold)) ->
    x
  
  x<- joinsaccades(x,buffer=2,threshold=20)
  
  # mod<- lm('verg.velocity~sdf20+verg.angle',data=filter(x,enhance.type=='none'))
  mod<- lm('verg.velocity~sdf20+verg.angle',data=filter(x,is.na(sacnum)))
  print(tidy(mod))
  # mod2<- lm('verg.velocity~sdf20+verg.angle',data=filter(x,enhance.type=='none',sdf20>30))
  message(paste('R-squared =', summary(mod)$r.squared))
  x<- dplyr::select(x,-sacnum,-counter)
  x<- mutate(x,predV=predict(mod,newdata=x),
             # predV2=predict(mod2,newdata=x),
             showrasters=replace(rasters,rasters<1,NA))
}

window_size=2000
step_size=1000

x<- modelVV(t,chosenCell='Bee-113')

manipulate(ggplot(filter(x,time>=window,time<window+window_size))+
             geom_point(aes(time,showrasters+30),shape='|')+
             # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
             # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
             # geom_line(aes(time,slowpredict),color='orange')+
             geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
             # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
             geom_line(aes(time,predV),color='orange')+
             # geom_line(aes(time,predV2),color='magenta')+
             geom_line(aes(time,verg.velocity),color='darkblue')+
             # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
             ylim(c(-150,150))
           ,
           window=slider(window_size,max(x$time-window_size),step=step_size))
#saccade demo in Bee-113
{
  ggplot(filter(x,time>5000,time<6000))+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkblue')

ggplot(filter(x,time>8000,time<9000))+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkblue')

ggplot(filter(x,time>12800,time<13800))+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkblue')

ggplot(filter(x,time>19250,time<20250))+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkblue')

ggplot(filter(x,time>22500,time<23500))+
  geom_point(aes(rep,repV),color='red')+
  geom_point(aes(lep,lepV),color='blue')+
  annotate('text',-4,-10,label='Start')

  geom_point(aes(time,showrasters+30),shape='|')+
  geom_line(aes(time,verg.angle*10+600),color='darkgreen')+
  geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkblue')+
  geom_line(aes(time,rev+50,color='red'))+
  geom_line(aes(time,lev+50,color='blue'))+
  geom_line(aes(time,rep*10+600,color='red'))+
  geom_line(aes(time,lep*10+600,color='blue'))

ggplot(filter(x,time>29000,time<30000))+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkblue')

ggplot(filter(x,time>39500,time<40500))+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkblue')


ggplot(filter(x,time>42500,time<43500))+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkblue')+
  geom_line(aes(time,rev+50,color='red'))+
  geom_line(aes(time,lev+50,color='blue'))

ggplot(filter(x,time>129500,time<130500))+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkblue')+
  geom_line(aes(time,rev+50,color='red'))+
  geom_line(aes(time,lev+50,color='blue'))

# 200000-200500
# 229000-230500
# 61000-62000
# 64000-65000
# 65500-66500
ggplot(filter(x,time>65500,time<66500))+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkblue')+
  geom_line(aes(time,rev+50,color='red'))+
  geom_line(aes(time,lev+50,color='blue'))

ggplot(filter(x,time>64000,time<65000))+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkblue')+
  geom_line(aes(time,rev+50,color='red'))+
  geom_line(aes(time,lev+50,color='blue'))
}

#saccade demo in Bee-103
11000
155502700

#-----looking----- at overall fit---
ggplot(aes(verg.velocity,predV),data=x)+
  geom_point(aes(color=enhance.type),alpha=1/5)+
  geom_abline(slope=1,color='red',size=2)+
  annotate("text",x=-10,y=15,label='Unity line y=x')+
  annotate('rect',xmin=12,xmax=100,ymin=-10,ymax=25,alpha=1/10)


#first attempt at a figure using Bee-113
#notice that this is a ~17 degree leftward, ~17degree upward saccade
#that moves gaze from a far target to a near target, 
#followed by a small, leftward corrective saccade. 

ggplot(filter(x,time>22500,time<23500))+
  geom_point(aes(rep,repV),color='red')+
  geom_point(aes(lep,lepV),color='blue3')+
  annotate('text',-4,-10,label='Initial Fixation')+
  annotate('text',-7,-2,label='Right Eye',color='red')+
  annotate('text',-6,4,label='Left Eye',color='blue3')+
  xlab('Horizontal Position (deg)')+
  ylab('Vertical Position (deg)')
ggsave('XYplot.PDF',height=5,width=5)

#here we show horizontal eye position and the vergence angle (lep-rep)
ggplot(filter(x,time>22500,time<23500))+
  # geom_point(aes(time,showrasters+30),shape='|')+
  geom_line(aes(time-22500,verg.angle),color='darkgreen')+
  # geom_line(aes(time,predV),color='orange',size=2)+
  # geom_line(aes(time,verg.velocity),color='darkblue')+
  # geom_line(aes(time,rev+50,color='red'))+
  # geom_line(aes(time,lev+50,color='blue'))+
  geom_line(aes(time-22500,rep),color='red')+
  geom_line(aes(time-22500,lep),color='blue3')+
  xlab('Time (ms)')+
  ylab('Horizontal Position (deg)')+
  annotate('text',500,7,label='Vergence', color='darkgreen')+
  annotate('text',500,-10,label='Left Eye Position',color='blue3')+
  annotate('text',500,-20,label='Right Eye Position',color='red')
ggsave('Positions.PDF')

ggplot(filter(x,time>22500,time<23500))+
  # geom_point(aes(time,showrasters+30),shape='|')+
  # geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time-22500,verg.velocity),color='darkgreen')+
  geom_line(aes(time-22500,rev),color='red')+
  geom_line(aes(time-22500,lev),color='blue3')+
  ylab('Velocity (deg/s)')+
  xlab('Time (ms)')+
  annotate('text',330,120,label='Vergence',color='darkgreen')+
  annotate('text',360,-120,label='Right Eye',color='red')+
  annotate('text',160,-120,label='Left Eye',color='blue3')
  
ggsave('Velocities.PDF')

ggplot(filter(x,time>22500,time<23500))+
  # geom_area(aes(time,sdf),alpha=1/10)+
  geom_point(aes(time-22500,showrasters+30),shape='|')+
  geom_line(aes(time-22500,predV),color='orange',size=2)+
  geom_line(aes(time-22500,verg.velocity),color='darkgreen')+
  ylab('Velocity (deg/s)')+
  xlab('Time (ms)')+
  annotate('text',400,80,label='Vergence',color='darkgreen')+
  annotate('text',180,15,label='Model Prediction',color='orange')+
  annotate('text',550,40,label='Spike Rasters',color='black')
  
ggsave('CellandModel.PDF')
  
ggplot(filter(x,time>22500,time<23500))+  
  geom_line(aes(time-22500,predV),color='orange',size=1,linetype=2,alpha=1)+
  # geom_point(aes(time,showrasters+30),shape='|')+
  # geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time-22500,verg.velocity),color='darkgreen')+
  geom_line(aes(time-22500,rev),color='red')+
  geom_line(aes(time-22500,lev),color='blue3')+
  geom_point(aes(time-22500,showrasters+30),shape='|')+
  ylab('Velocity (deg/s)')+
  xlab('Time (ms)')+
  annotate('text',330,120,label='Actual Vergence',color='darkgreen')+
  annotate('text',360,-120,label='Right Eye',color='red')+
  annotate('text',160,-120,label='Left Eye',color='blue3')+
  annotate('text',550,40,label='Spike Rasters',color='black')+
  annotate('text',160,17,label='Predicted Vergence',color='orange')
ggsave('CellandModelCondensed.PDF')


#-----saccades----
x<- joinsaccadesuniform(x,buffer=200,threshold=20)
x %>%
  group_by(sacnum) %>%
  mutate(conj.h=(lep+rep)/2,
         conj.v=(lepV+repV)/2,
         conj.h.amp=last(conj.h)-first(conj.h),
         conj.v.amp=last(conj.v)-first(conj.v),
         conj.angle=atan2(conj.v,conj.h)*180/pi,
         peak.verg.velocity=maxabs(verg.velocity),
         r.amp=sqrt(conj.h^2+conj.v^2),
         # verg.amp=last(verg.angle)-first(verg.angle),
         verg.amp=verg.angle[counter==350]-verg.angle[counter==150],
         saccade.type='conj',
         saccade.type=replace(saccade.type,verg.amp< -2 & r.amp>2,'diverging'),
         saccade.type=replace(saccade.type,verg.amp> 2& r.amp>2,'converging'),
         saccade.type=replace(saccade.type,is.na(sacnum) | r.amp<2,'no.saccade'))->
  x


p<- filter(x,verg.amp> 5,verg.amp< 5.5)

p %>% #the whole point of this little block is to renumber the selected saccades from 1..n
  group_by(sacnum) %>%
  summarize() %>%
  mutate(snum=row_number()) %>%
  left_join(p,.,by='sacnum') ->
  p
  

ggplot(filter(p,snum<7),aes(group=sacnum))+
  geom_point(aes(counter,showrasters*snum*4+150),shape='|')+
  # geom_line(aes(counter,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(counter,predV),color='orange',size=1)+
  geom_line(aes(counter,verg.velocity),color='darkblue')+
  geom_line(aes(counter,sdf))

ggplot(filter(p,snum<7),aes(group=sacnum))+
  geom_line(aes(counter,verg.angle),color='darkgreen')

goodsacs<- unique(p$sacnum)     
nsac=length(goodsacs)

manipulate(ggplot(filter(x,sacnum==goodsacs[sac]))+
             geom_point(aes(counter,showrasters+30),shape='|')+
             geom_line(aes(counter,verg.angle*10+0),color='darkgreen')+
             geom_line(aes(counter,predV),color='orange',size=2)+
             geom_line(aes(counter,verg.velocity),color='darkblue')+
             geom_line(aes(counter,conj.velocity))
           ,
           sac=slider(2,nsac,step=1))



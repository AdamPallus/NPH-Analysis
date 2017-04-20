t<- readRDS('SOA-NRTP.RDS')

window_size=2000
step_size=1000

modelVV<- function(t,chosenCell='Bee-113',saccadebuffer=20,saccadethreshold=30){
  #free parameters:
  #1) lag for sdf
  #2) width of sdf gaussian
  #3) width of parabolic diff kernel
  #5) saccade buffer and threshold
  
  x<- ungroup(filter(t,neuron==chosenCell))
  if (!('time' %in% names(x))){
    x<- mutate(x,time=row_number())
  }
  x %>%
    mutate(verg.velocity=parabolicdiff(lep-rep,25),
           sdf=spikedensity(rasters,sd=20),
           sdf20=dplyr::lag(sdf,33)) ->
    x
  
  x<- joinsaccades(x,buffer=saccadebuffer,threshold=saccadethreshold)
  
  # mod<- lm('verg.velocity~sdf20+verg.angle',data=filter(x,enhance.type=='none'))
  mod<- lm('verg.velocity~sdf20+verg.angle',data=filter(x,is.na(sacnum),verg.velocity>0))
  print(tidy(mod))
  # mod2<- lm('verg.velocity~sdf20+verg.angle',data=filter(x,enhance.type=='none',sdf20>30))
  message(paste('R-squared =', summary(mod)$r.squared))
  x<- dplyr::select(x,-sacnum,-counter)
  x<- mutate(x,predV=predict(mod,newdata=x),
             # predV2=predict(mod2,newdata=x),
             showrasters=replace(rasters,rasters<1,NA))
}
makeVergPlot<- function(x,starttime=22500,stoptime=23500,savename=NULL){
  
  print(ggplot(filter(x,time>starttime,time<stoptime))+
          geom_point(aes(rep,repV),color='red')+
          geom_point(aes(lep,lepV),color='blue3')+
          annotate('text',4,-10,label='Initial Fixation')+
          annotate('text',4,2,label='Right Eye',color='red')+
          annotate('text',15,1,label='Left Eye',color='blue3')+
          xlab('Horizontal Position (deg)')+
          ylab('Vertical Position (deg)')+
          coord_fixed())
  if (!is.null(savename)){
    ggsave(paste(savename,'XYPositions.PDF',sep='-'),height=5,width=5)
  }
  
  #here we show horizontal eye position and the vergence angle (lep-rep)
  print(ggplot(filter(x,time>starttime,time<stoptime))+
          # geom_point(aes(time,showrasters+30),shape='|')+
          geom_line(aes(time-starttime,verg.angle),color='darkgreen')+
          # geom_line(aes(time,predV),color='orange',size=2)+
          # geom_line(aes(time,verg.velocity),color='darkblue')+
          # geom_line(aes(time,rev+50,color='red'))+
          # geom_line(aes(time,lev+50,color='blue'))+
          geom_line(aes(time-starttime,rep),color='red')+
          geom_line(aes(time-starttime,lep),color='blue3')+
          xlab('Time (ms)')+
          ylab('Horizontal Position (deg)')+
          annotate('text',250,7,label='Vergence', color='darkgreen')+
          annotate('text',250,19,label='Left Eye Position',color='blue3')+
          annotate('text',250,12,label='Right Eye Position',color='red'))
  if (!is.null(savename)){
    ggsave(paste(savename,'Positions.PDF',sep='-'))
  }
  
  # print(ggplot(filter(x,time>starttime,time<stoptime))+
  #         # geom_point(aes(time,showrasters+30),shape='|')+
  #         # geom_line(aes(time,predV),color='orange',size=2)+
  #         geom_line(aes(time-starttime,verg.velocity),color='darkgreen')+
  #         geom_line(aes(time-starttime,rev),color='red')+
  #         geom_line(aes(time-starttime,lev),color='blue3')+
  #         ylab('Velocity (deg/s)')+
  #         xlab('Time (ms)')+
  #         annotate('text',330,120,label='Vergence',color='darkgreen')+
  #         annotate('text',360,-120,label='Right Eye',color='red')+
  #         annotate('text',160,-120,label='Left Eye',color='blue3'))
  # 
  # # ggsave('Velocities.PDF')
  
  print(ggplot(filter(x,time>starttime,time<stoptime))+
          # geom_area(aes(time,sdf),alpha=1/10)+
          geom_point(aes(time-starttime,showrasters+30),shape='|')+
          geom_line(aes(time-starttime,predV),color='orange',size=1,linetype=2)+
          geom_line(aes(time-starttime,verg.velocity),color='darkgreen')+
          ylab('Velocity (deg/s)')+
          xlab('Time (ms)')+
          annotate('text',450,80,label='Vergence',color='darkgreen')+
          annotate('text',180,15,label='Model Prediction',color='orange')+
          annotate('text',550,40,label='Spike Rasters',color='black'))
  if (!is.null(savename)){
    ggsave(paste(savename,'CellandModel.PDF',sep='-'))
  }
  
  print(ggplot(filter(x,time>starttime,time<stoptime))+  
          geom_line(aes(time-starttime,predV),color='orange',size=1,linetype=2,alpha=1)+
          # geom_point(aes(time,showrasters+30),shape='|')+
          # geom_line(aes(time,predV),color='orange',size=2)+
          geom_line(aes(time-starttime,verg.velocity),color='darkgreen')+
          geom_line(aes(time-starttime,rev),color='red')+
          geom_line(aes(time-starttime,lev),color='blue3')+
          geom_point(aes(time-starttime,showrasters+30),shape='|')+
          ylab('Velocity (deg/s)')+
          xlab('Time (ms)')+
          annotate('text',460,80,label='Actual Vergence',color='darkgreen')+
          annotate('text',600,-120,label='Right Eye',color='red')+
          annotate('text',600,-200,label='Left Eye',color='blue3')+
          annotate('text',750,40,label='Spike Rasters',color='black')+
          annotate('text',450,17,label='Predicted Vergence',color='orange'))
  if (!is.null(savename)){
    ggsave(paste(savename,'CellandModelandVelocity.PDF',sep='-'))
  }
}

x<- modelVV(t,chosenCell='Bee-113',saccadebuffer=10,saccadethreshold=20)

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


#-----saccades----
buffer=400
x<- joinsaccadesuniform(x,buffer=buffer,threshold=20)

x %>%
  group_by(sacnum) %>%
  mutate(conj.h=(lep+rep)/2,
         conj.v=(lepV+repV)/2,
         conj.h.amp=last(conj.h)-first(conj.h),
         conj.v.amp=last(conj.v)-first(conj.v),
         conj.angle=atan2(conj.v,conj.h)*180/pi,
         peak.verg.velocity=maxabs(verg.velocity),
         r.amp=sqrt(conj.h^2+conj.v^2),
         max.model=max(predV[counter>buffer & counter<buffer+150]),
         max.sdf=max(sdf20[counter<buffer & counter<buffer+150]),
         # verg.amp=last(verg.angle)-first(verg.angle),
         verg.amp=verg.angle[counter==buffer+150]-verg.angle[counter==buffer-50])->
  x


p<- filter(x,verg.amp> 7,verg.amp< 66)
# p<- filter(x,max.model>30)

p %>% #the whole point of this little block is to renumber the selected saccades from 1..n
  group_by(sacnum) %>%
  summarize() %>%
  mutate(snum=row_number()) %>%
  left_join(p,.,by='sacnum') ->
  p

#the filter(p,snum<7) is just to limit number of saccades plotted
ggplot(filter(p,snum<10),aes(group=sacnum))+  
  # geom_area(aes(counter,sdf),fill='black',color='black',alpha=1/30,position='identity')+
  geom_point(aes(counter,showrasters*snum*4+150),shape='|')+
  geom_line(aes(counter,verg.angle*10+0),color='darkgreen')+
  geom_line(aes(counter,predV),color='orange',size=1)+
  geom_line(aes(counter,verg.velocity),color='darkblue')+
  ylab('')

sp<- summarise_each(x,funs(first))

qplot(verg.amp,max.model,data=sp)

qplot(verg.amp,max.sdf,data=sp)
qplot(verg.amp,r.amp,data=sp)


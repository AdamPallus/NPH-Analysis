ggplot(filter(x,time>229000,time<230000))+  
  geom_line(aes(time-starttime,predV),color='orange',size=1,linetype=2,alpha=1)+
  # geom_point(aes(time,showrasters+30),shape='|')+
  # geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time-starttime,verg.velocity),color='darkgreen')+
  geom_line(aes(time-starttime,rev),color='red')+
  geom_line(aes(time-starttime,lev),color='blue3')+
  geom_point(aes(time-starttime,showrasters+30),shape='|')


ggplot(filter(x,time> 283750,time<284750))+  
  geom_line(aes(time,predV),color='orange',size=1,linetype=2,alpha=1)+
  # geom_point(aes(time,showrasters+30),shape='|')+
  # geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkgreen')+
  geom_line(aes(time,rev/10),color='red')+
  geom_line(aes(time,lev/10),color='blue3')+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_hline(yintercept=0)+
  ylab('Velocity (deg/s)')+
  xlab('Time (ms)')+
  annotate('rect',xmin=284018,xmax=284090,ymin=-100,ymax=100,alpha=1/10)
  

ggplot(filter(x,time> 295500,time<296500))+  
  geom_line(aes(time,predV),color='orange',size=1,linetype=2,alpha=1)+
  # geom_point(aes(time,showrasters+30),shape='|')+
  # geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkgreen')+
  geom_line(aes(time,rev),color='red')+
  geom_line(aes(time,lev),color='blue3')+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_hline(yintercept=0)+
  ylab('Velocity (deg/s)')+
  xlab('Time (ms)')

ggplot(filter(x,time> 245500,time<247000))+  
  geom_line(aes(time,predV),color='orange',size=1,linetype=2,alpha=1)+
  geom_line(aes(time,verg.angle*10),color='darkgreen')+
  # geom_point(aes(time,showrasters+30),shape='|')+
  # geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkgreen')+
  geom_line(aes(time,rev),color='red')+
  geom_line(aes(time,lev),color='blue3')+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_hline(yintercept=0)+
  ylab('Velocity (deg/s)')+
  xlab('Time (ms)')

ggsave('SlowVerg.PDF')
  
ggplot(filter(x,time>245500,time<247000))+
  geom_point(aes(rep,repV),color='red')+
  geom_point(aes(lep,lepV),color='blue3')+
  coord_fixed()+
  # annotate('text',-4,-10,label='Initial Fixation')+
  # annotate('text',-7,-2,label='Right Eye',color='red')+
  # annotate('text',-6,4,label='Left Eye',color='blue3')+
  xlab('Horizontal Position (deg)')+
  ylab('Vertical Position (deg)')

ggsave('slowvergXY.PDF',height=5,width=5)


ggplot(filter(x,time> 343500,time<344500))+  
  geom_line(aes(time,predV),color='orange',size=1,linetype=2,alpha=1)+
  # geom_point(aes(time,showrasters+30),shape='|')+
  # geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkgreen')+
  # geom_line(aes(time,rev),color='red')+
  # geom_line(aes(time,lev),color='blue3')+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_hline(yintercept=0)+
  ylab('Velocity (deg/s)')+
  xlab('Time (ms)')


ggplot(filter(x,time> 394750,time<395750))+  
  geom_line(aes(time,predV),color='orange',size=1,linetype=2,alpha=1)+
  # geom_point(aes(time,showrasters+30),shape='|')+
  # geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time,verg.velocity),color='darkgreen')+
  geom_line(aes(time,verg.angle*10),color='darkgreen')+
  geom_line(aes(time,rev/10),color='red')+
  geom_line(aes(time,lev/10),color='blue3')+
  geom_point(aes(time,showrasters+30),shape='|')+
  geom_hline(yintercept=0)+
  ylab('Velocity (deg/s)')+
  xlab('Time (ms)')


makeVergPlot<- function(x,starttime=22500,stoptime=23500){

print(ggplot(filter(x,time>starttime,time<stoptime))+
  geom_point(aes(rep,repV),color='red')+
  geom_point(aes(lep,lepV),color='blue3')+
  annotate('text',-4,-10,label='Initial Fixation')+
  annotate('text',-7,-2,label='Right Eye',color='red')+
  annotate('text',-6,4,label='Left Eye',color='blue3')+
  xlab('Horizontal Position (deg)')+
  ylab('Vertical Position (deg)'))

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
  annotate('text',500,7,label='Vergence', color='darkgreen')+
  annotate('text',500,-10,label='Left Eye Position',color='blue3')+
  annotate('text',500,-20,label='Right Eye Position',color='red'))
# ggsave('Positions.PDF')

print(ggplot(filter(x,time>starttime,time<stoptime))+
  # geom_point(aes(time,showrasters+30),shape='|')+
  # geom_line(aes(time,predV),color='orange',size=2)+
  geom_line(aes(time-starttime,verg.velocity),color='darkgreen')+
  geom_line(aes(time-starttime,rev),color='red')+
  geom_line(aes(time-starttime,lev),color='blue3')+
  ylab('Velocity (deg/s)')+
  xlab('Time (ms)')+
  annotate('text',330,120,label='Vergence',color='darkgreen')+
  annotate('text',360,-120,label='Right Eye',color='red')+
  annotate('text',160,-120,label='Left Eye',color='blue3'))

# ggsave('Velocities.PDF')

print(ggplot(filter(x,time>starttime,time<stoptime))+
  # geom_area(aes(time,sdf),alpha=1/10)+
  geom_point(aes(time-starttime,showrasters+30),shape='|')+
  geom_line(aes(time-starttime,predV),color='orange',size=2)+
  geom_line(aes(time-starttime,verg.velocity),color='darkgreen')+
  ylab('Velocity (deg/s)')+
  xlab('Time (ms)')+
  annotate('text',400,80,label='Vergence',color='darkgreen')+
  annotate('text',180,15,label='Model Prediction',color='orange')+
  annotate('text',550,40,label='Spike Rasters',color='black'))

# ggsave('CellandModel.PDF')

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
  annotate('text',330,120,label='Actual Vergence',color='darkgreen')+
  annotate('text',360,-120,label='Right Eye',color='red')+
  annotate('text',160,-120,label='Left Eye',color='blue3')+
  annotate('text',550,40,label='Spike Rasters',color='black')+
  annotate('text',160,17,label='Predicted Vergence',color='orange'))
# ggsave('CellandModelCondensed.PDF')
}


#get z from "VergPreditionDoodle.R'
  bufferlength=200
  window=249250
  window_size=500
  d=filter(z,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,rep+100),color='red')+
    geom_line(aes(time,lep+100),color='blue')+
    geom_line(aes(time,repV+100),color='red',linetype=2)+
    geom_line(aes(time,lepV+100),color='blue',linetype=2)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))


  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_line(aes(time,conj.velocity),color='magenta')+
    ylim(c(NA,200))

d2<- ungroup(filter(z,sacnum==378))

d2<- ungroup(filter(z,sacnum==27))

A<-ggplot(d2)+
  # geom_point(aes(counter,showrasters+30),shape='|')+
  geom_line(aes(counter-bufferlength,verg.velocity),color='darkblue',size=2)+
  geom_line(aes(counter-bufferlength,conj.velocity),color='magenta',size=2)+
  geom_vline(xintercept=c(bufferlength,139)-bufferlength,linetype=3,size=2)+
  geom_vline(xintercept = c(41,213)-bufferlength,linetype=3,size=2)+
  ylim(c(NA,150))+
  xlab('Time (ms)')+
  ylab('Velocity (deg/s)')

A
ggsave('Figure3A_1.PDF',plot = A)

ggsave('Figure3A_2.PDF',plot=A+theme_void())
+
  theme_void()

B<-ggplot(d2)+
  # geom_point(aes(counter,showrasters+30),shape='|')+
  geom_line(aes(counter-bufferlength,verg.angle),color='darkgreen',size=2)+
  geom_line(aes(counter-bufferlength,rep),color='red',size=2)+
  geom_line(aes(counter-bufferlength,lep),color='blue',size=2)+
  geom_line(aes(counter-bufferlength,(repV+lepV)/2),color='darkviolet',linetype=2,size=2)+
  geom_vline(aes(xintercept=c(bufferlength,saccade.end))-bufferlength,linetype=3,size=2)+
  geom_vline(xintercept = c(41,213)-bufferlength,linetype=3,size=2)+
  ylim(c(NA,15))+
  xlab('Time (ms)')+
  ylab('Position (deg)')

B

ggsave('Figure3B_1.PDF',plot = B)

ggsave('Figure3B_2.PDF',plot=B+theme_void())

#Model----
C<-ggplot(d2)+
  geom_point(aes(counter-bufferlength,showrasters+30),shape='|',size=3)+
  geom_line(aes(counter-bufferlength,verg.velocity),color='darkblue',size=2)+
  geom_line(aes(counter-bufferlength,conj.velocity),color='magenta',size=2)+
  geom_vline(xintercept=c(0,first(d2$saccade.dur)),linetype=3,size=2)+
  # geom_vline(xintercept = c(41,213)-bufferlenagth,linetype=3,size=2)+
  geom_line(aes(counter-bufferlength,predV),color='orange',size=2)+
  ylim(c(NA,50))+
  xlab('Time (ms)')+
  ylab('Velocity (deg/s)')

C
ggsave('Figure3C_1.PDF',plot = C)

ggsave('Figure3C_2.PDF',plot=C+theme_void())
+
  theme_void()

D<-ggplot(d2)+
  # geom_point(aes(counter,showrasters+30),shape='|')+
  geom_line(aes(counter,verg.angle),color='darkgreen',size=2)+
  geom_line(aes(counter,rep),color='red',size=2)+
  geom_line(aes(counter,lep),color='blue',size=2)+
  geom_line(aes(counter,(repV+lepV)/2),color='darkviolet',linetype=2,size=2)+
  geom_vline(xintercept=c(bufferlength,bufferlength+first(d2$saccade.dur)),linetype=3,size=2)+
  # geom_vline(xintercept = c(41,213)-bufferlength,linetype=3,size=2)+
  geom_line(aes(counter,cumsum(predV)/1000+first(verg.angle)),color='orange',size=2)+
  # ylim(c(NA,15))+
  xlab('Time (ms)')+
  ylab('Position (deg)')

D

ggsave('Figure3B_1.PDF',plot = D)

ggsave('Figure3B_2.PDF',plot=D+theme_void())


#all saccades----
zp<- summarize_each(z,funs(first))
zp<-filter(zp,r.amp>3)

qplot(verg.amp,predicted.verg.amp,data=zp)+geom_abline()+
  xlab('Actual Vergence Amplitude (deg)')+
  ylab('Predicted Vergence Amplitude (deg)')


#searchforexamples----
chosenSac=3
goodsacs=unique(z$sacnum[z$r.amp>2&z$verg.amp>2])
manipulate({
d2<- filter(z,sacnum==goodsacs[chosenSac])  
verg.start=max(d2$counter[d2$counter<200&abs(d2$verg.velocity)<3])
verg.end=min(d2$counter[d2$counter>200+first(d2$saccade.dur)&abs(d2$verg.velocity)<3])
real.amp=d2$verg.angle[verg.end]-d2$verg.angle[verg.start]
predict.amp=sum(d2$predV[verg.start:verg.end])/1000
ggplot(d2)+
  geom_point(aes(counter,showrasters+30),shape='|',size=3)+
  geom_line(aes(counter,verg.velocity),color='darkblue',size=2)+
  # geom_line(aes(counter,conj.velocity),color='magenta',size=2)+
  geom_vline(xintercept=c(200,200+first(d2$saccade.dur)),linetype=3,size=2)+
  geom_vline(xintercept = c(verg.start,verg.end),linetype=3,size=2,color='darkblue')+
  # geom_vline(xintercept = c(41,213),linetype=3,size=2)+
  geom_line(aes(counter,predV),color='orange',size=2)+
  geom_hline(yintercept = c(-3,3))+
  ylim(c(NA,150))+
  xlab('Time (ms)')+
  ylab('Velocity (deg/s)')+
  annotate('text',x=100,y=30,label=paste('real.amp=',round(real.amp,2)))+
  annotate('text',x=100,y=25,label=paste('predict.amp=',round(predict.amp,2)))+
  annotate('text',x=100,y=20,label=paste('sacnum=',d2$sacnum[1]))+
  geom_line(aes(counter,verg.angle*10),color='darkgreen')+
  geom_line(aes(counter,cumsum(predV)/100+d2$verg.angle[1]*10),color='orange',size=1)+
  geom_line(aes(counter,target.verg*10))
},
chosenSac=slider(1,length(goodsacs),initial=1))
# chosenSac=slider(1,max(z$sacnum,na.rm=T),initial=3))

potentialSacs=c(16,20,27,205)


#Check the saccade at 112000
bufferlength=200
window=111790
window_size=1000
d=filter(z,time>=window,time<window+window_size)
ggplot(d)+
  geom_point(aes(time,showrasters+30),shape='|')+
  # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
  # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
  # geom_line(aes(time,slowpredict),color='orange')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
  geom_line(aes(time,predV),color='orange')+
  # geom_line(aes(time,predV2),color='magenta')+
  geom_line(aes(time,verg.velocity),color='darkblue')+
  geom_area(aes(time,conj.velocity),alpha=1/3)+
  geom_line(aes(time,rep+100),color='red')+
  geom_line(aes(time,lep+100),color='blue')+
  geom_line(aes(time,repV+100),color='red',linetype=2)+
  geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
  geom_line(aes(time,target.verg*10))+
  # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
  ylim(c(NA,200))


 #Saccade 344
{
d2<- filter(z,sacnum==344)  
verg.start=max(d2$counter[d2$counter<200&abs(d2$verg.velocity)<3])
verg.end=min(d2$counter[d2$counter>200+first(d2$saccade.dur)&abs(d2$verg.velocity)<3])
real.amp=d2$verg.angle[verg.end]-d2$verg.angle[verg.start]
predict.amp=sum(d2$predV[verg.start:verg.end])/1000
d2<- mutate(d2,sdfplot=spikedensity(rasters,sd=10))
A<-ggplot(d2)+
  geom_point(aes(counter,showrasters+30),shape='|',size=3)+
  geom_line(aes(counter,verg.velocity),color='darkblue',size=2)+
  # geom_line(aes(counter,conj.velocity),color='magenta',size=2)+
  geom_vline(xintercept=c(200,200+first(d2$saccade.dur)),linetype=3,size=2)+
  geom_vline(xintercept = c(verg.start,verg.end),linetype=3,size=2,color='darkblue')+
  # geom_vline(xintercept = c(41,213),linetype=3,size=2)+
  ylim(c(NA,120))+
  xlab('Time (ms)')+
  ylab('Velocity (deg/s)')

B<-ggplot(d2)+
  geom_point(aes(counter,showrasters+30),shape='|',size=3)+
  geom_line(aes(counter,verg.velocity),color='darkblue',size=2)+
  # geom_line(aes(counter,conj.velocity),color='magenta',size=2)+
  geom_vline(xintercept=c(200,200+first(d2$saccade.dur)),linetype=3,size=2)+
  geom_vline(xintercept = c(verg.start,verg.end),linetype=3,size=2,color='darkblue')+
  # geom_vline(xintercept = c(41,213),linetype=3,size=2)+
  geom_line(aes(counter,predV),color='orange',size=2)+
  ylim(c(NA,50))+
  xlab('Time (ms)')+
  ylab('Velocity (deg/s)')


  
B1<-ggplot(d2)+
  # geom_point(aes(counter,showrasters+10),shape='|',size=3)+
  geom_line(aes(counter,verg.angle),color='darkgreen',size=2)+
  # geom_line(aes(counter,rep),color='red')+
  # geom_line(aes(counter,lep),color='blue')+
  # geom_line(aes(counter,(lepV+repV)/2),color='purple',linetype=2)+
  # # geom_line(aes(counter,conj.velocity),color='magenta',size=2)+
  geom_vline(xintercept=c(200,200+first(d2$saccade.dur)),linetype=3,size=2)+
  geom_vline(xintercept = c(verg.start,verg.end),linetype=3,size=2,color='darkgreen')+
  geom_line(aes(counter,cumsum(predV)/1000+first(verg.angle)),color='orange',size=2)+
  # geom_vline(xintercept = c(41,213),linetype=3,size=2)+
  ylim(c(3,12))+
  xlab('Time (ms)')+
  ylab('Position (deg)')

C1<-ggplot(d2)+
  geom_point(aes(counter,showrasters+10),shape='|',size=3)+
  geom_line(aes(counter,verg.angle),color='darkgreen',size=2)+
  # geom_line(aes(counter,rep),color='red')+
  # geom_line(aes(counter,lep),color='blue')+
  # geom_line(aes(counter,(lepV+repV)/2),color='purple',linetype=2)+
  # # geom_line(aes(counter,conj.velocity),color='magenta',size=2)+
  geom_vline(xintercept=c(200,200+first(d2$saccade.dur)),linetype=3,size=2)+
  geom_vline(xintercept = c(verg.start,verg.end),linetype=3,size=2,color='darkgreen')+
  # geom_vline(xintercept = c(41,213),linetype=3,size=2)+
  ylim(c(0,20))+
  xlab('Time (ms)')+
  ylab('Position (deg)')

C2<-ggplot(d2)+
  # geom_point(aes(counter,showrasters+10),shape='|',size=3)+
  # geom_line(aes(counter,verg.angle),color='darkgreen',size=2)+
  geom_line(aes(counter,rep),color='red',size=2)+
  geom_line(aes(counter,lep),color='blue',size=2)+
  # geom_line(aes(counter,(lepV+repV)/2),color='purple',linetype=2)+
  # # geom_line(aes(counter,conj.velocity),color='magenta',size=2)+
  geom_vline(xintercept=c(200,200+first(d2$saccade.dur)),linetype=3,size=2)+
  geom_vline(xintercept = c(verg.start,verg.end),linetype=3,size=2,color='darkgreen')+
  # geom_vline(xintercept = c(41,213),linetype=3,size=2)+
  ylim(c(-30,-10))+
  xlab('Time (ms)')+
  ylab('Position (deg)')


C3<-ggplot(d2)+
  # geom_point(aes(counter,showrasters+10),shape='|',size=3)+
  # geom_line(aes(counter,verg.angle),color='darkgreen',size=2)+
  # geom_line(aes(counter,rep),color='red')+
  # geom_line(aes(counter,lep),color='blue')+
  geom_line(aes(counter,(lepV+repV)/2),color='purple',linetype=2,size=2)+
  # # geom_line(aes(counter,conj.velocity),color='magenta',size=2)+
  geom_vline(xintercept=c(200,200+first(d2$saccade.dur)),linetype=3,size=2)+
  geom_vline(xintercept = c(verg.start,verg.end),linetype=3,size=2,color='darkgreen')+
  # geom_vline(xintercept = c(41,213),linetype=3,size=2)+
  ylim(c(-8,12))+
  xlab('Time (ms)')+
  ylab('Position (deg)')

C4<-ggplot(d2)+
  geom_area(aes(counter,sdfplot),alpha=1)+
  geom_point(aes(counter,showrasters+20),shape='|',size=3,color='white')+
  # geom_line(aes(counter,verg.angle),color='darkgreen',size=2)+
  # geom_line(aes(counter,rep),color='red')+
  # geom_line(aes(counter,lep),color='blue')+
  # geom_line(aes(counter,(lepV+repV)/2),color='purple',linetype=2,size=2)+
  # # geom_line(aes(counter,conj.velocity),color='magenta',size=2)+
  geom_vline(xintercept=c(200,200+first(d2$saccade.dur)),linetype=3,size=2)+
  geom_vline(xintercept = c(verg.start,verg.end),linetype=3,size=2,color='darkgreen')+
  # ylim(c(-8,12))+
  xlab('Time (ms)')+
  ylab('Firing Rate (spks/s)')

z %>%
  group_by(sacnum) %>%
  mutate(peakFR=max(sdf))->
  z


zp<- summarize_each(z,funs(first))
zp<-filter(zp,r.amp>3,predicted.verg.amp<100)

#use Rstudio to output these pdfs to preserve circular points
D<-qplot(verg.amp,predicted.verg.amp,data=zp)+
  geom_abline()+
  theme_bw()+
  xlab('')+
  ylab('')+
  geom_point(x=7,y=3.79,size=2,color='red')+
  coord_fixed()+
    xlim(c(-8,10))

#use Rstudio to output these pdfs
E<- qplot(verg.amp,peakFR,data=zp)+
  geom_point(x=7.08,y=206,size=2,color='red')+
  theme_bw()+
  xlab('')+
  ylab('')+
  xlim(c(-8,10))

R2ampFR<-cor(zp$verg.amp[zp$verg.amp>0],zp$peakFR[zp$verg.amp>0])^2
  

  }

ggsave('VergVel.PDF',height=4,width=12,plot=A+theme_bw())
ggsave('VergVelmodel.PDF',height=4,width=12,plot=B+theme_bw())
ggsave('VergAngle.PDF',height=4,width=12,plot=C1+theme_bw())
ggsave('Heyepos.PDF',height=4,width=12,plot=C2+theme_bw())
ggsave('Veyepos.PDF',height=4,width=12,plot=C3+theme_bw())
ggsave('SDF.PDF',height=4,width=12,plot=C4+theme_bw())

ggsave('VergAngleBLANK.PDF',height=4,width=12,plot=C1+theme_void())
ggsave('HeyeposBLANK.PDF',height=4,width=12,plot=C2+theme_void())
ggsave('VeyeposBLANK.PDF',height=4,width=12,plot=C3+theme_void())
ggsave('SDFBLANK.PDF',height=4,width=12,plot=C4+theme_void())
ggsave('VergVelBLANK.PDF',height=4,width=12,plot=A+theme_void())
ggsave('VergVelmodelBLANK.PDF',height=4,width=12,plot=B+theme_void())

ggsave('VergPositionModel.PDF',height=4,width=12,plot=B1)
ggsave('VergPositionModelBLANK.PDF',height=4,width=12,plot=B1+theme_void())

ggsave('PredictedAmplitudes.PDF',height=5,width=5,plot=D)

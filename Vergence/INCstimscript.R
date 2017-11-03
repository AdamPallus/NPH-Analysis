s<- loadnewcsv2(path="C:/Users/setup/Desktop/NRTP Vergence/INCstim/")

# s<- filter(t,neuron %in% c('Kopachuck-501','Kopachuck-502'))
s<- filter(s,monkey=='DC')
bufferlength=200
s %>%
  group_by(neuron) %>%
  mutate(time=row_number(),
  sdf=spikedensity(rasters,sd=10),
  stimes=markSaccades(sdf,buffer=bufferlength,threshold=120))->
  sx
  

# s<- mutate(s,time=row_number())
# s$stimes<- markSaccades(s$sdf,buffer=100,threshold=120)

sx %>%
  group_by(neuron) %>%
  filter(stimes>0) %>%
  mutate(showstim=replace(rasters,rasters<1,NA))%>%
  group_by(neuron,stimes) %>%
  mutate(counter=time-first(time),
         repDIFF=rep-rep[counter==bufferlength],
         lepDIFF=lep-lep[counter==bufferlength],
         repVDIFF=repV-repV[counter==bufferlength],
         lepVDIFF=lepV-lepV[counter==bufferlength])->
  sp


ggplot(sp,aes(group=stimes))+
  geom_point(aes(counter,showstim*stimes+10),shape='|')+
  geom_line(aes(counter,repDIFF),color='red')+
  geom_line(aes(counter,lepDIFF),color='blue')+
  geom_line(aes(counter,repVDIFF-20),color='red',linetype=2)+
  geom_line(aes(counter,lepVDIFF-20),color='blue',linetype=2)+
  facet_wrap(~neuron,ncol=3)

ggsave('StimPlots.PDF',height=20,width=8)

sp %>%
  group_by(neuron,counter) %>%
  summarize_each(funs(mean))->
  spm

ggplot(spm)+
  geom_point(aes(counter,showstim*stimes-5),data=sp,shape='|')+
  geom_line(aes(counter,repDIFF,group=stimes),color='red',alpha=1/5,data=sp)+
  geom_line(aes(counter,lepDIFF,group=stimes),color='blue',alpha=1/5,data=sp)+
  geom_line(aes(counter,repVDIFF-10,group=stimes),color='red',linetype=2,alpha=1/5,data=sp)+
  geom_line(aes(counter,lepVDIFF-10,group=stimes),color='blue',linetype=2,alpha=1/5,data=sp)+
  geom_line(aes(counter,repDIFF),color='red',size=2)+
  geom_line(aes(counter,lepDIFF),color='blue',size=2)+
  geom_line(aes(counter,repVDIFF-10),color='red',linetype=2,size=2)+
  geom_line(aes(counter,lepVDIFF-10),color='blue',linetype=2,size=2)+
  theme_bw()+
  coord_cartesian(ylim=c(-15,10))+
  # annotate('rect',xmin=100,xmax=150,ymin=-12,ymax=6,alpha=1/5)+
  # annotate('text',50,5,label='Horizontal Position')+
  # annotate('text',50,-5,label='Vertical Position - 10 degrees')+
  # annotate('text',125,5.5,label='Stimulation')+
  ylab('Position Change (deg)')+
  facet_wrap(~neuron,scale='free',ncol=2)

  ggsave('INCstim.PDF',height=20,width=8)
  
  ggplot(spm)+
    geom_point(aes(counter,showstim*stimes-5),data=sp,shape='|')+
    geom_line(aes(counter,rev),color='red',size=1)+
    geom_line(aes(counter,lev),color='blue',size=1)+
    geom_line(aes(counter,revV-10),color='red',linetype=2,size=1)+
    geom_line(aes(counter,levV-10),color='blue',linetype=2,size=1)+
    theme_bw()+
    # coord_cartesian(ylim=c(-15,10))+
    # annotate('rect',xmin=100,xmax=150,ymin=-12,ymax=6,alpha=1/5)+
    # annotate('text',50,5,label='Horizontal Position')+
    # annotate('text',50,-5,label='Vertical Position - 10 degrees')+
    # annotate('text',125,5.5,label='Stimulation')+
    ylab('Position Change (deg)')+
    facet_wrap(~neuron,scale='free',ncol=2)
  
  

  
  ggplot(sp,aes(group=stimes))+
    # geom_point(aes(counter,showstim*stimes+10),shape='|')+
    geom_line(aes(counter,rep),color='red')+
    geom_line(aes(counter,lep),color='blue')+
    geom_line(aes(counter,repV-50),color='red',linetype=2)+
    geom_line(aes(counter,lepV-50),color='blue',linetype=2)+
    annotate('rect',xmin=100,xmax=150,ymin=-80,ymax=50,alpha=1/5,fill='pink')+
    ylab('Position (deg)')+
    facet_wrap(~neuron,scale='free',ncol=2)

  ggsave('INCstim1a.PDF',height=20,width=8)  


chosensite='Kopachuck-5084'

ggplot(filter(sp,neuron==chosensite),aes(group=stimes))+
  # geom_point(aes(counter,showstim*stimes+10),shape='|')+
  geom_line(aes(counter,rep),color='red')+
  geom_line(aes(counter,lep),color='blue')+
  geom_line(aes(counter,repV-50),color='red',linetype=2)+
  geom_line(aes(counter,lepV-50),color='blue',linetype=2)+
  annotate('rect',xmin=200,xmax=600,ymin=-80,ymax=50,alpha=1/5,fill='pink')+
  geom_hline(yintercept = c(0,-50))+
  ylab('Position (deg)')+
  facet_wrap(~stimes,ncol=3)

ggsave('INCstim1-separate.PDF',height=15,width=8)


#plan: gather rep/lep and repV/lepV so we can have Horiz (l/r) and Vert (l/r)

sp %>%
  group_by(neuron,stimes) %>%
  gather("H.Eye","H.Position",rep,lep) %>%
  gather("V.Eye","V.Position",repV,lepV)->
  x
ggplot(filter(x,neuron==chosensite))+
  geom_point(aes(counter,showstim*stimes+10),shape='|')+
  geom_line(aes(counter,H.Position,color=H.Eye))+
  # annotate('rect',xmin=200,xmax=600,ymin=-80,ymax=50,alpha=1/5,fill='pink')+
  # geom_hline(yintercept = c(0,-50))+
  ylab('Position (deg)')+
  # facet_wrap(~stimes,ncol=3)
  facet_grid()

chosensite='Kopachuck-5062'
spt<- filter(sp,neuron==chosensite)
# spt<- dplyr::select(spt,1:10)

spt %>%
  group_by(neuron,stimes) %>%
  gather("Component","Velocity",c(3,5,7,9))%>%
  gather("ComponentP","Position",2:5) %>%
  mutate(Component=as.factor(Component),
         is.Vertical=ComponentP %in% c('lepV','repV'),
         is.Vertical=as.factor(is.Vertical))->
  x
levels(x$is.Vertical)<- c("Horizontal",'Vertical')

# levels(x$Component)<- c("LH","LV","RH","RV")
# levels(x$ComponentV)<- c("LH","LV","RH","RV")

ggplot(x)+
  geom_line(aes(counter,Position,color=ComponentP))+
  facet_grid(is.Vertical~stimes)+
  geom_vline(xintercept = 200)






#----


ggplot(filter(sp,neuron==chosensite),aes(group=stimes))+
  # geom_point(aes(counter,showstim*stimes+10),shape='|')+
  geom_line(aes(counter,rev),color='red')+
  geom_line(aes(counter,lev),color='blue')+
  geom_line(aes(counter,revV),color='red',linetype=2)+
  geom_line(aes(counter,levV),color='blue',linetype=2)+
  annotate('rect',xmin=200,xmax=600,ymin=-80,ymax=50,alpha=1/5,fill='pink')+
  geom_hline(yintercept = 0)+
  coord_cartesian(ylim=c(-100,100),xlim=c(100,800))+
  ylab('Position (deg)')
  # facet_wrap(~stimes,ncol=3)


spx<- filter(sp,neuron==chosensite)
manipulate(ggplot(filter(spx,stimes==currentStim))+
             geom_point(aes(time,showstim),shape='|')+
             geom_line(aes(time,repV),color='red')+
             geom_line(aes(time,lepV),color='blue')
           ,
           currentStim=slider(1,max(sp$stimes),step=1))



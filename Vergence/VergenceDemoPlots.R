#Go to VergPreditionDoodle.r FIRST

MakeFigure<-function(d,labelloc=288000,zoom=TRUE){
  Hpositions<- ggplot(d)+
    geom_line(aes(time,(rep)),color='red',size=1)+
    geom_line(aes(time,(lep)),color='blue',size=1)+
    theme_minimal()+xlab('')+theme(axis.text.x = element_blank())+
    ylab('Horizontal\nEye Positon (deg)')
  
  Vpositions<- ggplot(d)+
    geom_line(aes(time,(((repV+lepV)/2))),color='violet',size=1)+
    theme_minimal()+xlab('')+theme(axis.text.x = element_blank())+
    ylab('Vertical\nEye Position (deg)')
  
  VergPositions<- ggplot(d)+
    geom_line(aes(time,lep-rep),color='darkgreen',size=1)+
    theme_minimal()+xlab('')+theme(axis.text.x = element_blank())+
    ylab('Vergence Position\n(deg)')
  
  VergVelocities<-ggplot(d)+theme_minimal()+
    geom_line(aes(time,verg.velocity),color='blue',alpha=1)+
    geom_line(aes(time,predV),color='orange')+
    # ylim(c(-10,30))+
    xlab('')+theme(axis.text.x = element_blank())+
    ylab('Vergence Velocity\n(deg/s)')+
    annotate('text',x=labelloc,y=-5,label='Predicted',color='orange')
  
  if (zoom){
    VergVelocities<-VergVelocities+ylim(c(-10,30))
  }
  
  sdf<- ggplot(d)+theme_minimal()+
    geom_area(aes(time,sdf/2),alpha=1)+
    geom_point(aes(time,showrasters+5),shape='|',color='lightgrey',size=3)+
    ylab('Firing Rate (spk/s)')
  xlab('Time (ms)')
  
  # multiplot(Hpositions,Vpositions,VergPositions,VergVelocities,sdf)
  # p<- list(Hpositions,Vpositions,VergPositions,VergVelocities,sdf)
  grid.arrange(arrangeGrob(Hpositions,Vpositions,VergPositions,VergVelocities,sdf,ncol=1,
                           heights=c(.5,.3,.5,1,.5)),padding=0.2)
}

z<-mutate(z,showrasters=replace(rasters,rasters<1,NA))



window=287000 #Bee-25
# window=334500 
window_size=2000
d=filter(z,time>=window,time<window+window_size)
d<- group_by(d,time) %>% summarize_each(funs(first))




window_size=10000
manipulate({
  d=filter(z,time>=window,time<window+window_size)
  d<- group_by(d,time) %>% summarize_all(funs(first))
  d<- mutate(d,saccadic=replace(saccadic,!saccadic,NA))
  MakeFigure(d,window)
},
window=slider(window_size,max(z$time-window_size),step=window_size))

#Bee-33----

z %>%
  filter(time>=109700,
         time<112500) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(112000)

z %>%
  filter(time>=232000,
         time<236000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(233000,FALSE)

#Bee-15----
z %>%
  filter(time>=117500,
         time<120000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(119000)

z %>%
  filter(time>=171000,
         time<173000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(172000)

z %>%
  filter(time>=203000,
         time<205000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(204000)



#Bee-25 ----
z %>%
  filter(time>=287000,
         time<289000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(288000)

z %>%
  filter(time>=271000,
         time<273000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(271000)


z %>%
  filter(time>=294500,
         time<296000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(295000)




#Bee-215----
z %>%
  filter(time>=35000,
         time<37999) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(36000)

z %>%
  filter(time>=93000,
         time<96000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(93000)

z %>%
  filter(time>=682000,
         time<684000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(683000)

z %>%
  filter(time>=747500,
         time<750000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(748000)

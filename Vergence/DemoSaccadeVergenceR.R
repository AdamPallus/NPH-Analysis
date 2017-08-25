#Bee-211----
#Go to VergPreditionDoodle.r 
z<-mutate(z,showrasters=replace(rasters,rasters<1,NA))

d<- group_by(z,time) %>% summarize_each(funs(first))

window_size=2000


#Conjugate
window=467500
ggplot(filter(d,time>=window,time<window+window_size))+
  geom_line(aes(time,((lep-rep)-(lep-rep)[1])*5+15+150),color='darkgreen',size=1)+
  geom_line(aes(time,(rep-rep[1])*5+150),color='red',size=1)+
  geom_line(aes(time,(lep-lep[1])*5+150),color='blue',size=1)+
  geom_line(aes(time,(((repV+lepV)/2)-((repV+lepV)/2)[1])*5+150-15),color='violet',size=1)+
  geom_line(aes(time,verg.velocity*2+100),color='blue',alpha=1)+
  geom_line(aes(time,predV*2+100),color='orange')+
  geom_area(aes(time,sdf/2),alpha=1)+
  geom_point(aes(time,showrasters+5),shape='|',color='lightgrey',size=3)+
  annotate('segment',x=window,xend=window,y=0,yend=50)+
  annotate('text',x=window,y=45,label='100 spk/s')+
  annotate('segment',x=window,xend=window,y=100,yend=120)+
  annotate('text',x=window,y=114,label='10 deg/s')+
  annotate('segment',x=window,xend=window,y=150,yend=175)+
  annotate('text',x=window,y=170,label='5 deg')+
  annotate('segment',x=window,xend=window+100,y=75,yend=75)+
  annotate('text',x=window,y=75,label='100 ms')+
  theme_minimal()+
  ylab('')

#slow verg
window=543500
ggplot(filter(d,time>=window,time<window+window_size))+
  geom_line(aes(time,((lep-rep)-(lep-rep)[1])*5+15+150),color='darkgreen',size=1)+
  geom_line(aes(time,(rep-rep[1])*5+150),color='red',size=1)+
  geom_line(aes(time,(lep-lep[1])*5+150),color='blue',size=1)+
  geom_line(aes(time,(((repV+lepV)/2)-((repV+lepV)/2)[1])*5+150-15),color='violet',size=1)+
  geom_line(aes(time,verg.velocity*2+100),color='blue',alpha=1)+
  geom_line(aes(time,predV*2+100),color='orange')+
  geom_area(aes(time,sdf/2),alpha=1)+
  geom_point(aes(time,showrasters+5),shape='|',color='lightgrey',size=3)+
  annotate('segment',x=window,xend=window,y=0,yend=50)+
  annotate('text',x=window,y=45,label='100 spk/s')+
  annotate('segment',x=window,xend=window,y=100,yend=120)+
  annotate('text',x=window,y=114,label='10 deg/s')+
  annotate('segment',x=window,xend=window,y=150,yend=175)+
  annotate('text',x=window,y=170,label='5 deg')+
  annotate('segment',x=window,xend=window+100,y=75,yend=75)+
  annotate('text',x=window,y=75,label='100 ms')+
  theme_minimal()+
  ylab('')

#converging saccade
window=45000 #Bee-211
ggplot(filter(d,time>=window,time<window+window_size))+
  geom_line(aes(time,((lep-rep)-(lep-rep)[1])*5+15+150),color='darkgreen',size=1)+
  geom_line(aes(time,(rep-rep[1])*5+150),color='red',size=1)+
  geom_line(aes(time,(lep-lep[1])*5+150),color='blue',size=1)+
  geom_line(aes(time,(((repV+lepV)/2)-((repV+lepV)/2)[1])*5+150-15),color='violet',size=1)+
  geom_line(aes(time,verg.velocity*2+100),color='blue',alpha=1)+
  geom_line(aes(time,predV*2+100),color='orange')+
  geom_area(aes(time,sdf/2),alpha=1)+
  geom_point(aes(time,showrasters+5),shape='|',color='lightgrey',size=3)+
  annotate('segment',x=window,xend=window,y=0,yend=50)+
  annotate('text',x=window,y=45,label='100 spk/s')+
  annotate('segment',x=window,xend=window,y=100,yend=120)+
  annotate('text',x=window,y=114,label='10 deg/s')+
  annotate('segment',x=window,xend=window,y=150,yend=175)+
  annotate('text',x=window,y=170,label='5 deg')+
  annotate('segment',x=window,xend=window+100,y=75,yend=75)+
  annotate('text',x=window,y=75,label='100 ms')+
  theme_minimal()+
  ylab('')


#------
#converging saccade
window=51000 #Bee-211
ggplot(filter(d,time>=window,time<window+window_size))+
  geom_line(aes(time,((lep-rep)-(lep-rep)[1])*5+15+150),color='darkgreen',size=1)+
  geom_line(aes(time,(rep-rep[1])*5+150),color='red',size=1)+
  geom_line(aes(time,(lep-lep[1])*5+150),color='blue',size=1)+
  geom_line(aes(time,(((repV+lepV)/2)-((repV+lepV)/2)[1])*5+150-15),color='violet',size=1)+
  geom_line(aes(time,verg.velocity*2+100),color='blue',alpha=1)+
  geom_line(aes(time,predV*2+100),color='orange')+
  geom_area(aes(time,sdf/2),alpha=1)+
  geom_point(aes(time,showrasters+5),shape='|',color='lightgrey',size=3)+
  annotate('segment',x=window,xend=window,y=0,yend=50)+
  annotate('text',x=window,y=45,label='100 spk/s')+
  annotate('segment',x=window,xend=window,y=100,yend=120)+
  annotate('text',x=window,y=114,label='10 deg/s')+
  annotate('segment',x=window,xend=window,y=150,yend=175)+
  annotate('text',x=window,y=170,label='5 deg')+
  annotate('segment',x=window,xend=window+100,y=75,yend=75)+
  annotate('text',x=window,y=75,label='100 ms')+
  theme_minimal()+
  ylab('')

#Bee-211----
#Go to VergPreditionDoodle.r 
z<-mutate(z,showrasters=replace(rasters,rasters<1,NA))


#PERFECT NON-SACCADIC VERGENCE
window=543500 #Bee-211
window_size=2000
d=filter(z,time>=window,time<window+window_size)
d<- group_by(d,time) %>% summarize_each(funs(first))

ggplot(d)+
  geom_area(aes(time,sdf),alpha=1/2)+
  geom_line(aes(time,lev-rev),color='blue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  # geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  # geom_line(aes(time,rep+100),color='red')+
  # geom_line(aes(time,lep+100),color='blue')+
  # geom_line(aes(time,repV+100),color='red',linetype=2)+
  # geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  # geom_line(aes(time,conj.velocity),color='hotpink')+
  geom_line(aes(time,predV),color='orange')+
  theme_minimal()

ggplot(d)+
  # geom_area(aes(time,sdf),alpha=1/2)+
  # geom_line(aes(time,lev-rev),color='blue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)),color='darkgreen',size=1)+
  # geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,rep),color='red',size=1)+
  geom_line(aes(time,lep),color='blue',size=1)+
  geom_line(aes(time,(repV+lepV)/2),color='violet',size=1)+
  # geom_line(aes(time,repV),color='red',linetype=1,size=2)+
  # geom_line(aes(time,lepV),color='blue',linetype=1,size=2)+
  # geom_line(aes(time,conj.velocity),color='hotpink')+
  # geom_line(aes(time,predV),color='orange')+
  ylim(-35,35)+
  theme_minimal()


ggplot(d)+
  # geom_area(aes(time,sdf),alpha=1/2)+
  # geom_line(aes(time,lev-rev),color='blue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)-(lep-rep)[1]+5),color='darkgreen',size=1)+
  # geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,rep-rep[1]),color='red',size=1)+
  geom_line(aes(time,lep-lep[1]),color='blue',size=1)+
  geom_line(aes(time,((repV+lepV)/2)-((repV+lepV)/2)[1]-5),color='violet',size=1)+
  geom_vline(xintercept=544535)+
  geom_vline(xintercept = 544565)+
  # geom_line(aes(time,repV),color='red',linetype=1,size=2)+
  # geom_line(aes(time,lepV),color='blue',linetype=1,size=2)+
  # geom_line(aes(time,conj.velocity),color='hotpink')+
  # geom_line(aes(time,predV),color='orange')+
  ylim(-10,10)+
  theme_minimal()
qplot(counter,conj.velocity,data=filter(d,sacnum==712))

ggplot(d)+
  # geom_area(aes(time,sdf),alpha=1/2)+
  geom_line(aes(time,verg.velocity),color='blue',alpha=1)+
  geom_vline(xintercept=544535)+
  geom_vline(xintercept = 544565)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  # geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  # geom_point(aes(time,showrasters+50),shape='|')+
  # geom_line(aes(time,rep+100),color='red')+
  # geom_line(aes(time,lep+100),color='blue')+
  # geom_line(aes(time,repV+100),color='red',linetype=2)+
  # geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  # geom_line(aes(time,conj.velocity),color='hotpink')+
  geom_line(aes(time,predV),color='orange')+
  # ylim(-10,30)+
  theme_minimal()


ggplot(d)+
  geom_area(aes(time,sdf),alpha=1)+
  # geom_line(aes(time,lev-rev),color='blue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  # geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+20),shape='|',color='white')+
  # geom_line(aes(time,rep+100),color='red')+
  # geom_line(aes(time,lep+100),color='blue')+
  # geom_line(aes(time,repV+100),color='red',linetype=2)+
  # geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  # geom_line(aes(time,conj.velocity),color='hotpink')+
  # geom_line(aes(time,predV),color='orange')+
  theme_minimal()

#alltogehernow----
ggplot(d)+
  geom_line(aes(time,((lep-rep)-(lep-rep)[1])*5+15+150),color='darkgreen',size=1)+
  geom_line(aes(time,(rep-rep[1])*5+150),color='red',size=1)+
  geom_line(aes(time,(lep-lep[1])*5+150),color='blue',size=1)+
  geom_line(aes(time,(((repV+lepV)/2)-((repV+lepV)/2)[1])*5+150-15),color='violet',size=1)+
  geom_line(aes(time,verg.velocity*2+100),color='blue',alpha=1)+
  geom_line(aes(time,predV*2+100),color='orange')+
  geom_area(aes(time,sdf/2),alpha=1)+
  geom_point(aes(time,showrasters+5),shape='|',color='lightgrey',size=3)+
  annotate('segment',x=544000,xend=544000,y=0,yend=50)+
  annotate('text',x=544000,y=45,label='100 spk/s')+
  annotate('segment',x=544000,xend=544000,y=100,yend=120)+
  annotate('text',x=544000,y=114,label='10 deg/s')+
  annotate('segment',x=544000,xend=544000,y=150,yend=175)+
  annotate('text',x=544000,y=170,label='5 deg')+
  annotate('segment',x=544000,xend=544100,y=75,yend=75)+
  annotate('text',x=543900,y=75,label='100 ms')+
  theme_minimal()+
  ylab('')




window=49000
window_size=2000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity))


window=72800
window_size=1000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity),color='hotpink')


window=131500
window_size=1000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity),color='hotpink')


window=139500
window_size=1500
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity),color='hotpink')



window=391500
window_size=1500
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity),color='hotpink')


window=400000
window_size=2000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity),color='hotpink')

window=433250
window_size=1500
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity),color='hotpink')


window=600000
window_size=5000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity),color='hotpink')




#Ozette-120----  
ChosenCell='Ozette-120'
gc<- filter(t,neuron==ChosenCell) #saved as a separate variable so it can be used later
gc<-mutate(gc,time=row_number(), showrasters=replace(rasters,rasters<1,NA))

gc<- mutate(gc,conj.velocity2=sqrt((rev+lev)^2/2)+sqrt((revV+levV)^2/2))

window=27500
window_size=2000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity),color='hotpink')+
  geom_line(aes(time,rep+100),color='red')+
  geom_line(aes(time,lep+100),color='blue')+
  geom_line(aes(time,repV+100),color='red',linetype=2)+
  geom_line(aes(time,lepV+100),color='blue',linetype=2)

window=105000
window_size=5000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity2),color='hotpink')+
  geom_line(aes(time,rep+100),color='red')+
  geom_line(aes(time,lep+100),color='blue')+
  geom_line(aes(time,repV+100),color='red',linetype=2)+
  geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  ylim(NA,300)

window=147000
window_size=5000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity2),color='hotpink')+
  geom_line(aes(time,rep+100),color='red')+
  geom_line(aes(time,lep+100),color='blue')+
  geom_line(aes(time,repV+100),color='red',linetype=2)+
  geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  ylim(NA,300)

window=168000
window_size=2000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity2),color='hotpink')+
  geom_line(aes(time,rep+100),color='red')+
  geom_line(aes(time,lep+100),color='blue')+
  geom_line(aes(time,repV+100),color='red',linetype=2)+
  geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  ylim(NA,300)

window=182000
window_size=3000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity2),color='hotpink')+
  geom_line(aes(time,rep+100),color='red')+
  geom_line(aes(time,lep+100),color='blue')+
  geom_line(aes(time,repV+100),color='red',linetype=2)+
  geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  ylim(NA,300)

window=211000
window_size=5000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity2),color='hotpink')+
  geom_line(aes(time,rep+100),color='red')+
  geom_line(aes(time,lep+100),color='blue')+
  geom_line(aes(time,repV+100),color='red',linetype=2)+
  geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  ylim(NA,150)

window=222500
window_size=1000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity2),color='hotpink')+
  geom_line(aes(time,rep+100),color='red')+
  geom_line(aes(time,lep+100),color='blue')+
  geom_line(aes(time,repV+100),color='red',linetype=2)+
  geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  ylim(NA,200)

window=247500
window_size=1000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity2),color='hotpink')+
  geom_line(aes(time,rep+100),color='red')+
  geom_line(aes(time,lep+100),color='blue')+
  geom_line(aes(time,repV+100),color='red',linetype=2)+
  geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  ylim(NA,200)

window=257000
window_size=2000
ggplot(filter(gc,time>=window,time<window+window_size))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,conj.velocity2),color='hotpink')+
  geom_line(aes(time,rep+100),color='red')+
  geom_line(aes(time,lep+100),color='blue')+
  geom_line(aes(time,repV+100),color='red',linetype=2)+
  geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  ylim(NA,200)

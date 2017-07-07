#Bee-211----
ChosenCell='Bee-211'
gc<- filter(t,neuron==ChosenCell) #saved as a separate variable so it can be used later
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
  geom_line(aes(time,(lep-rep)),color='darkgreen',size=2)+
  # geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,rep),color='red',size=2)+
  geom_line(aes(time,lep),color='blue',size=2)+
  geom_line(aes(time,(repV+lepV)/2),color='violet',size=2)+
  # geom_line(aes(time,repV),color='red',linetype=1,size=2)+
  # geom_line(aes(time,lepV),color='blue',linetype=1,size=2)+
  # geom_line(aes(time,conj.velocity),color='hotpink')+
  # geom_line(aes(time,predV),color='orange')+
  theme_minimal()

ggplot(d)+
  # geom_area(aes(time,sdf),alpha=1/2)+
  geom_line(aes(time,verg.velocity),color='blue',alpha=1)+
  # geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  # geom_line(aes(time,(lep-rep)*5+50),color='darkgreen')+
  # geom_point(aes(time,showrasters+50),shape='|')+
  # geom_line(aes(time,rep+100),color='red')+
  # geom_line(aes(time,lep+100),color='blue')+
  # geom_line(aes(time,repV+100),color='red',linetype=2)+
  # geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  geom_line(aes(time,conj.velocity),color='hotpink')+
  geom_line(aes(time,predV),color='orange')+
  ylim(-10,30)+
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

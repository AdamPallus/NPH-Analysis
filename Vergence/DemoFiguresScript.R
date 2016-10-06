t<- readRDS('enhancemarked.RDS')

z<- filter(t,neuron=='Bee-211')
slowfit<- lm('sdflag~verg.angle+verg.velocity:enhance.type',data=z)

z<- mutate(z,slow.prediction=predict(slowfit,newdata=z),
            slow.prediction=replace(slow.prediction,slow.prediction<0,0))

z<- z %>% group_by(time) %>% summarize_each(funs(first))

z<- mutate(z, converge=as.numeric(enhance.type=='convergence'),
           converge=replace(converge,converge<1,NA))

z<- dplyr::mutate(z,converge=replace(converge,converge==0,NA))
z<- mutate(z, enhance.velocity=lev-rev,
           enhance.velocity=replace(enhance.velocity,is.na(converge),NA),
           showrasters=replace(rasters,rasters<1,NA))



# manipulate(ggplot(filter(z,time>=window,time<window+5000))+
#              geom_area(aes(time,sdf),alpha=1/10)+
#              # geom_line(aes(time,(rev+lev)/2))+
#              geom_line(aes(time,lev-rev),color='pink',alpha=1)
#              # geom_point(aes(time,converge*-100))+
#              # geom_hline(yintercept=c(50,-50))
#              # geom_point(aes(time,showslow*100))+
#              # geom_hline(yintercept = c(208,233))+
#              # geom_line(aes(time,slow.prediction),color='orange')
#              ,
#            # geom_line(aes(time,lag(lev,3)-rev),color='pink'),
#            window=slider(1,max(z$time-5000),step=5000))

manipulate(ggplot(filter(z,time>=window,time<window+5000))+
             geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
             geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
             geom_line(aes(time,(lep-rep)*5-100),color='darkgreen')+
             geom_point(aes(time,showrasters+50),shape='|')+
             # geom_line(aes(time,slow.prediction),color='orange')+
             # geom_line(aes(time,(rev+lev)/2),color='darkred')+
             # geom_line(aes(time,(revV+levV)/2),color='red')+
             # theme_bw()+
             ylim(c(-100,200))
           ,
           window=slider(5000,max(z$time-5000),step=5000))

##snapshots:
ggplot(filter(z,time>=51500,time<54000))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,slow.prediction),color='orange')+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  geom_line(aes(time,(lep-rep)*5-100),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  geom_line(aes(time,enhance.velocity),size=2,color='darkblue')+
  theme_bw()

ggplot(filter(z,time>=54500,time<57000))+
  geom_area(aes(time,sdf),alpha=1/10)+
  geom_line(aes(time,lev-rev),color='darkblue',alpha=1)+
  geom_line(aes(time,(lep-rep)*5-100),color='darkgreen')+
  geom_point(aes(time,showrasters+50),shape='|')+
  theme_bw()


#Transients are timing?
bufferlength<- 200

zz<- joinsaccades(z,buffer=200,threshold=20)

zz %>%
  group_by(sacnum) %>%
  mutate(verg.amp= verg.angle[n()-bufferlength]-verg.angle[bufferlength]) %>%
  filter(verg.amp>3)->
  zz
goodsacs<- unique(zz$sacnum)     
nsac=length(goodsacs)

windowsize<-1000
manipulate(ggplot(filter(zz,sacnum==goodsacs[sac]))+
             # geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,(lev-rev)-100),color='darkblue',alpha=1)+
             geom_line(aes(time,enhance.velocity-100),size=2,color='darkblue')+
             geom_line(aes(time,(lep-rep)*5-100),color='darkgreen')+
             geom_line(aes(time,lev-lag(rev,4)),color='red',linetype=2)+
             geom_line(aes(time,lag(lev,4)-rev),color='blue',linetype=2)+
             geom_line(aes(time,lev-lag(rev,3)),color='red',linetype=3)+
             geom_line(aes(time,lag(lev,3)-rev),color='blue',linetype=3)+
             geom_line(aes(time,lev-lag(rev,2)),color='red',linetype=4)+
             geom_line(aes(time,lag(lev,2)-rev),color='blue',linetype=4)+
             geom_line(aes(time,lev-lag(rev,1)),color='red',linetype=5)+
             geom_line(aes(time,lag(lev,1)-rev),color='blue',linetype=5)+
             geom_line(aes(time,lev-lag(rev,6)),color='red',linetype=6)+
             geom_line(aes(time,lag(lev,6)-rev),color='blue',linetype=6)+
           geom_line(aes(time,lev-lag(rev,9)),color='red',linetype=9)+
             geom_line(aes(time,lag(lev,9)-rev),color='blue',linetype=9)
             # ylim(c(-100,200))
           ,
           sac=slider(1,nsac,step=1))







manipulate(ggplot(filter(t,time>=window,time<window+5000))+
             geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,(rev+lev)/2))+
             geom_line(aes(time,lev-rev),color='pink')+
             geom_point(aes(time,conv*-100))+
             # geom_point(aes(time,showslow*100))+
             # geom_hline(yintercept = c(208,233))+
             geom_line(aes(time,slow.prediction),color='orange'),
           # geom_line(aes(time,lag(lev,3)-rev),color='pink'),
           window=slider(1,max(z$time-5000),step=5000))
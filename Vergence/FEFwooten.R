t<- loadnewcsv2(path="C:/Users/setup/Desktop/NRTP Vergence/FEF/")

t %>% 
  group_by(neuron) %>% 
  mutate(sdf=spikedensity(rasters,200),
         showrasters=replace(rasters,rasters<1,NA),
         sdf20=lag(sdf,20),
         sdf50=lag(sdf,50),
         targ.verg=thp2-thp,
         verg.error=targ.verg-verg.angle)->
  t

t%>% 
  do(m=lm(sdf~verg.angle+verg.velocity,data=.),
     m20=lm(sdf20~verg.angle+verg.velocity,data=.),
     m50=lm(sdf50~verg.angle+verg.velocity,data=.)) ->
  tm

glance(tm,m)
glance(tm,m20)
glance(tm,m50)

tidym<-tidy(tm,m)

qplot(neuron,estimate,data=filter(tidym,term=='verg.angle'))+geom_hline(yintercept=0)
qplot(neuron,estimate,data=filter(tidym,term=='verg.velocity'))+geom_hline(yintercept=0)

tidym %>%
  select(neuron,term,estimate) %>%
  spread(term,estimate)->
  stm

qplot(verg.angle,verg.velocity,data=stm)+
  geom_abline()



manipulate({
  window_size=10000
  ggplot(filter(t,neuron==chosenCell,time>window,time<window+window_size))+
    geom_line(aes(time,sdf))+
    geom_line(aes(time,verg.angle),color='green')+
    geom_point(aes(time,showrasters+40),size=3,shape='|')+
    geom_line(aes(time,targ.verg),color='darkgreen')+
    geom_line(aes(time,verg.error),color='pink')
},
window=slider(1,200000,step=10000),
chosenCell=picker("Wooten-701","Wooten-702","Wooten-703","Wooten-704"))



tt<- filter(t,neuron=='Wooten-701')
m<- lm(sdf20~verg.angle+targ.verg,data=tt)
tt<- mutate(tt,predV=predict(m,newdata=tt))

tt<- filter(t,neuron=='Wooten-702')
m<- lm(sdf20~verg.angle+targ.verg,data=tt)
tt<- mutate(tt,predV=predict(m,newdata=tt),
            targ.verg.vel=parabolicdiff(targ.verg,50))

manipulate({
  window_size=6667
  ggplot(filter(tt,time>window,time<window+window_size))+
    # geom_line(aes(time,sdf))+
    # geom_line(aes(time,verg.angle),color='green')+
    geom_point(aes(time,showrasters+40),size=3,shape='|')+
    # geom_line(aes(time,targ.verg),color='darkgreen')+
    geom_line(aes(time,abs(targ.verg.vel)),color='darkblue')
  # geom_line(aes(time,verg.error),color='pink')+
  # geom_line(aes(time,predV),color='orange')
},
window=slider(1,max(tt$time),step=10000))

#next idea:
#mark target direction changes so that we can analyze based on cycle
#because the monkey behavior is not always good, this will lead to errors
#we could use the actual vergence velocity but then we won't get uniform groups for averaging
#maybe just split them up and then reject trials where the monkey deviates too much from the target
#this is a job for another day. I suggest using a method similar to what we did for the Knight analysis

tt %>%
  mutate(cycle=time %% 6667,
         targ.verg.acc=parabolicdiff(targ.verg.vel,7),
         c=markSaccadesDouble(targ.verg.acc,threshold1=100,threshold2=20,maxreject=1000))->
  tt

tt %>%
  mutate(cycles=abs(c)) %>% #combine saccades and non-saccades
  group_by(c) %>%
  mutate(counter=1:n())->
  tc


ggplot(filter(tt,c>0))+
  geom_point(aes(cycle,targ.verg.acc,color=time))

ggplot(tc)+
  geom_area(aes(counter,targ.verg))


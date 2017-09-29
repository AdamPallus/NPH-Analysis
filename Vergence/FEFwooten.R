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
mm<- lm(sdf20~targ.verg,data=tt)

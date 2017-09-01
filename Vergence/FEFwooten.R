t<- loadnewcsv2(path="C:/Users/setup/Desktop/NRTP Vergence/FEF/")

t %>% 
  group_by(neuron) %>% 
  mutate(sdf=spikedensity(rasters,20),
         sdf20=lag(sdf,20),
         sdf50=lag(sdf,50)) %>% 
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

qplot(verg.angle,verg.velocity,data=)
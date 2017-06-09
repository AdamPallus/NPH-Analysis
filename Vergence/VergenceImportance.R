t %>%
  # filter(monkey=='Ozette') %>%
  group_by(monkey,neuron) %>%
  mutate(time=row_number(),
         sdf20=lag(sdf,20)) %>%
  do(m=lm(sdf20~verg.velocity+verg.angle,data=.)) ->
  x

# x%>%
#   summarize(neuron=neuron[1],
#             monkey=monkey[1],
#             vvimp=calc.relimp(m,rela=T)$lmg[1],
#             vaimp=calc.relimp(m,rela=T)$lmg[2])->
#   x

x%>%
  summarize(neuron=neuron[1],
            monkey=monkey[1],
            vvimp=calc.relimp(m,rela=F)$lmg[1],
            vaimp=calc.relimp(m,rela=F)$lmg[2])->
  x
saveRDS(x,'VergenceImportancenotscaled.RDS')

# x%>%
#   mutate(R2=summary(m)$r.squared,
#      b=coef(m)[1],
#      r=coef(m)[2],
#      v=coef(m)[3],
#      vvimp=.$imp$lmg[1],
#      vaimp=.$imp$lmg[2])->
#   x

x<-arrange(x,desc(R2))

# saveRDS(x,'VergenceImportance.RDS')

head(x,20)
x<- separate(x,neuron,c('monk','cellnum'),remove=FALSE)
x$monk<-NULL
x$cellnum<-as.numeric(x$cellnum)
ggplot(filter(x,monkey %in% c('Bee','Ozette')))+
  geom_histogram(aes(vvimp,fill=monkey),position='dodge')+
  facet_wrap(~cellnum<100)

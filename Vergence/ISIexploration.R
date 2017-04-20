t %>%
  group_by(neuron) %>%
  dplyr::select(neuron, rasters) %>%
  mutate(time=row_number()) %>%
  filter(rasters==1) %>%
  mutate(isi=time-lag(time,1)) %>%
  left_join(t,.,by=c('neuron','time','rasters')) ->
  t

qplot(isi,data=filter(tisi,isi<50),geom='density')+
  facet_wrap(~neuron)

ta<- filter(t,neuron=='Kopachuck-902')

m<- lm(isi~repV,data=ta)

qplot(repV,isi,data=filter(ta,isi<50),geom='bar')

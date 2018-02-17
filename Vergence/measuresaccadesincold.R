
measureSaccadesINC<-function(t){
  if (t$monkey[1]=='DC'){
    t%>%
      mutate(blinks=markSaccades(rep,buffer=80,threshold=70),
             # dsnum=markSaccadesDouble(conj.velocity,threshold1=30,threshold2=15),
             dsnum=markSaccadesDouble(replace(conj.velocity,blinks>0,150),
                                      threshold1=30,threshold2=15,maxreject=100000),
             sdf=spikedensity(rasters,10),
             sdf10=lag(sdf,10),
             conj.vert=(repV+lepV)/2,
             conj.vert.vel=(revV+levV)/2,
             conj.hor=(rep+lep)/2,
             conj.hor.v=(rev+lev)/2)->
      t
  }else{
    t %>%
      mutate(dsnum=markSaccadesDouble(conj.velocity,threshold1=30,threshold2=15),
              sdf=spikedensity(rasters,10),
              sdf10=lag(sdf,10),
              conj.vert=(repV+lepV)/2,
              conj.vert.vel=(revV+levV)/2,
              conj.hor=(rep+lep)/2,
              conj.hor.v=(rev+lev)/2) ->
      t
  }
  t
}

t %>%
  group_by(neuron) %>%
  do(measureSaccadesINC(.))->
  t


t %>%
  filter(monkey=='DC') %>%
  group_by(neuron) %>%
  mutate(blinks=markSaccades(rep,buffer=80,threshold=70),
         # dsnum=markSaccadesDouble(conj.velocity,threshold1=30,threshold2=15),
         dsnum=markSaccadesDouble(replace(conj.velocity,blinks>0,150),
                                  threshold1=30,threshold2=15,maxreject=100000),
         sdf=spikedensity(rasters,10),
         sdf10=lag(sdf,10),
         conj.vert=(repV+lepV)/2,
         conj.vert.vel=(revV+levV)/2,
         conj.hor=(rep+lep)/2,
         conj.hor.v=(rev+lev)/2) ->
  tdc

t %>%
  filter(monkey %in% c('Kopachuck','Bee')) %>%
  group_by(neuron) %>%
  mutate(dsnum=markSaccadesDouble(conj.velocity,threshold1=30,threshold2=15),
         sdf=spikedensity(rasters,10),
         sdf10=lag(sdf,10),
         conj.vert=(repV+lepV)/2,
         conj.vert.vel=(revV+levV)/2,
         conj.hor=(rep+lep)/2,
         conj.hor.v=(rev+lev)/2) ->
  tkb

t<- rbind(select(tdc,-blinks),tkb)
tdc<- NULL
tkb<- NULL

t<-read.csv('C:/Users/setup/Desktop/NRTP Vergence/PPRF/Ozette-504-2016.09.06_1226_Vergence.csv')

t<- loadnewcsv2(path='C:/Users/setup/Desktop/NRTP Vergence/PPRF/')

t %>%
  group_by(neuron) %>%
  mutate(time=row_number(),
         showrasters=replace(rasters,rasters<1,NA),
         s=markSaccadesDouble(conj.velocity,threshold1=40,threshold2=10))->
  t

chosencell='Ozette-506'
windowsize=200
manipulate(ggplot(filter(t,neuron==chosencell,time>window,time<window+windowsize))+
             geom_point(aes(time,showrasters*10),shape='|')+
             geom_line(aes(time,rev),color='red')+
             geom_hline(yintercept=20),
           window=slider(1,max(t$time),step=windowsize))
           
ggplot(filter(t,neuron=='Ozette-506',time>149300,time<149500))+
  geom_point(aes(time,showrasters*10),shape='|')+
  geom_line(aes(time,rev),color='red')+
  geom_hline(yintercept=20)


t %>%
  filter(s>0) %>%
  group_by(neuron,s) %>%
  summarize(starttime=first(time)-50,
            stoptime=last(time)+50)->
  ts

manipulate(ggplot(filter(t,neuron==chosencell,time>starttime,time<stoptime))+
             geom_point(aes(time,showrasters*10),shape='|')+
             geom_line(aes(time,rev),color='red')+
             geom_hline(yintercept=20),
           window=slider(1,max(t$time),step=windowsize))
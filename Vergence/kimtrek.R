
k<- read.csv('kimtrek.csv')
k<-rename(k,distance.to.next.shelter=Distance.to.next.shelter.from.previous..m.)
md<- mean(k$distance.to.next.shelter)
qplot(distance.to.next.shelter,data=k,fill=State)+
  theme_minimal()+
  xlab('Distance to next shelter (miles)')+
  geom_vline(aes(xintercept = mean(distance.to.next.shelter)))+
  annotate('text',x=10,y=35,label=paste('Mean =',round(md,2)))

ggplot(k)+
  geom_boxplot(aes(State,distance.to.next.shelter,fill=State))+
  theme_minimal()

k<- mutate(k,shelter.num=row_number())

ggplot(k)+
  geom_point(aes(shelter.num,Distance.to.next..big.town))+
  geom_line(aes(shelter.num,Distance.to.next..big.town))+
  xlim(0,120)+
  geom_label(aes(y=distance,x=arrival,label=Next.town,color=state),data=ks)+
  theme_minimal()+
  ylab('Distance to next big town (miles)')+
  xlab('Shelter number')
  

k %>%
  group_by(Next.town) %>%
  summarize(arrival=first(shelter.num),
         towns=first(Next.town),
         distance=first(Distance.to.next..big.town),
         state=first(State))->
  ks

arrivals<- unique(k$arrival)
towns<- unique(k$Next.town)
starting.distance=unique(ks$starting.distance)

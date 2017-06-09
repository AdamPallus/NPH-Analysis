# t %>%
  # group_by(neuron) %>%
  # dplyr::select(neuron, rasters) %>%
  # mutate(time=row_number()) %>%
  # filter(rasters==1) %>%
  # mutate(isi=time-lag(time,1)) %>%
  # left_join(t,.,by=c('neuron','time','rasters')) ->
  # t

# qplot(isi,data=filter(tisi,isi<50),geom='density')+
#   facet_wrap(~neuron)
# 
# ta<- filter(t,neuron=='Kopachuck-902')
# 
# m<- lm(isi~repV,data=ta)
# 
# qplot(repV,isi,data=filter(ta,isi<50),geom='bar')


t %>%
  group_by(neuron) %>%
  mutate(conj.velocity=sqrt(((rev+lev)/2)^2)+sqrt(((revV+levV)/2)^2),
           s=markSaccades(conj.velocity,buffer=15,threshold=40)) %>%
  filter(s<0) %>%
  group_by(neuron,s) %>%
  summarize(misi=mean(isi,na.rm=T),
            sdisi=sd(isi,na.rm=T),
            meanFR=1/misi*1000,
            mean.conj.vertical=mean((repV+lepV)/2),
            dur=n())->
  st

st%>%
  group_by(neuron) %>%
  summarize(rISI=cor(misi,sdisi,use='complete.obs'),
            rRP=cor(meanFR,mean.conj.vertical,use='complete.obs')) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  rst

st%>%
  group_by(neuron) %>%
  filter(dur<1000) %>%
  summarize(rISI=cor(misi,sdisi,use='complete.obs'),
            rRP=cor(meanFR,mean.conj.vertical,use='complete.obs')) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  rst

qplot(rISI^2,rRP^2,data=rst,color=monkey)+geom_abline()


rst %>%
  ungroup() %>%
  rename(rISISD=r) %>%
  left_join(t,rst,by='neuron')->
  t


qplot(rRP,data=rst,bins=10)+facet_wrap(~monkey,ncol=1)

rst %>%
  group_by(monkey) %>%
  summarize(mean.r=mean(r,na.rm=T),
    ci.low=as.numeric(t.test(r)$conf.int[1]),
    ci.high=as.numeric(t.test(r)$conf.int[2]))->
  srst

st<- separate(st,neuron,c('monkey','cellnum'),remove=FALSE)
st<- left_join(st,rst,by=c('neuron','monkey','cellnum'))

ggplot(filter(st,monkey=='Kopachuck',abs(rRP)>0),aes(meanFR,mean.conj.vertical,group=neuron))+
  geom_smooth(method='lm',aes(color=rRP^2),se=FALSE)+
  scale_color_continuous(low='orange',high='black')

ggplot(st,aes(meanFR,mean.conj.vertical,group=neuron))+
  geom_smooth(method='lm',aes(color=rRP^2),se=FALSE)+
  scale_color_continuous(low='orange',high='black')+
  facet_wrap(~monkey)

qplot(misi,sdisi,data=filter(st,neuron=='Bee-101',misi<200))+stat_smooth(method='lm')

qplot(meanFR,mean.verg.angle,color=abs(mean.verg.velocity),data=filter(st,neuron=='Bee-101'))


qplot(misi,sdisi,data=filter(st,misi<200))+stat_smooth(method='lm')+facet_wrap(~neuron,ncol=4)

qplot(r,data=rst,bins=10)+facet_wrap(~monkey,ncol=1)

ggplot(rst,aes(r))+geom_histogram(aes(fill=monkey),position='dodge')+
  geom_errorbarh(aes(x=mean.r,y=6,xmin=ci.low,xmax=ci.high,color=monkey),data=srst)+
  geom_vline(aes(xintercept=mean.r,color=monkey),data=srst)


ggplot(srst,aes(x=mean.r,y=0))+geom_point()+geom_errorbarh(aes(xmin=ci.low,xmax=ci.high,color=monkey,height=0.01))

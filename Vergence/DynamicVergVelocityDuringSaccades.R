source('joinsaccades.R')

t<- readRDS('enhancemarked.RDS')
t<- readRDS('enhancemarked-NRTP.RDS')




# tp<- filter(tp,neuron=='Bee-215')
# 
# tp<- filter(tp,neuron %in% c('Bee-215','Ozette-118','Bee-110'))
# 
# tp <- dplyr::filter(t,neuron %in%
#                       c('Bee-103','Bee-104','Bee-107','Bee-108','Bee-110','Bee-112','Bee-113','Bee-202',
#                         'Bee-203','Bee-204','Bee-205','Bee-208','Bee-209','211','Bee-215','Ozette-102',
#                         'Ozette-114','Ozette-116','Ozette-117','Ozette-118','Ozette-120','Ozette-121','Ozette-122'))
# 

ozette.cells<- c('Ozette-110','Ozette-117','Ozette-118','Ozette-120','Ozette-121')
bee.cells<- c('Bee-103','Bee-108','Bee-207','Bee-211','Bee-215')

desired.cells<- ozette.cells
desired.cells<- bee.cells

tp<- dplyr::filter(t,neuron %in% desired.cells)

tp %>% group_by(neuron) %>%
  do(joinsaccades(.,buffer=20,threshold=20))->
  tp

# #the following is for plotting chunks of cells
# tp<- group_by(t,neuron)
# tp$neuron.num=group_indices(tp)
# tp %>%
#   filter(neuron.num>40,neuron.num<=60) %>%
#   group_by(neuron) %>%
#   do(joinsaccades(.,buffer=20,threshold=20)) ->
#   tp


tp %>%
  group_by(neuron) %>%
  mutate(bin.velocity=cut(verg.velocity,c(seq(-200,200,by=20)))) %>%
  # filter(!is.na(sacnum)) %>%
  group_by(neuron, sacnum) %>%
  mutate(conj.h=(lep+rep)/2,
         conj.v=(lepV+repV)/2,
         conj.h.amp=last(conj.h)-first(conj.h),
         conj.v.amp=last(conj.v)-first(conj.v),
         conj.angle=atan2(conj.v,conj.h)*180/pi,
         peak.verg.velocity=maxabs(verg.velocity),
         r.amp=sqrt(conj.h^2+conj.v^2),
         verg.amp=last(verg.angle)-first(verg.angle),
         saccade.type='conj',
         saccade.type=replace(saccade.type,verg.amp< -2 & r.amp>2,'diverging'),
         saccade.type=replace(saccade.type,verg.amp> 2& r.amp>2,'converging'),
         saccade.type=replace(saccade.type,is.na(sacnum) | r.amp<2,'no.saccade')) ->
  gp

#model attempt

addPrediction<- function(t, formula='sdflag~verg.angle+verg.velocity:enhance.type') {
  
  m<- lm(formula,data=filter(t,saccade.type != 'conj'))
  r<- summary(m)$r.squared
  t<- mutate(t,predicted.sdf=predict(m,newdata=t),
             predicted.sdf=replace(predicted.sdf,predicted.sdf<0,0),
             model.r2=r)
  
}

gp %>%
  group_by(neuron) %>%
  do(addPrediction(.,formula='sdflag~verg.angle+verg.velocity:enhance.type')) ->
  gptest

###############

# summary(aov(sdflag~verg.velocity:saccade.type,data=gp))

gs<-ggplot(aes(bin.velocity,sdflag),data=gp)+
  geom_boxplot()+
  facet_grid(neuron~saccade.type,scales='free_y')+
  stat_smooth(aes(group=1),method='lm')
  # ylim(c(0,NA))

ggsave('soa-3box.png',height=26,width=8,plot=gs)

gs<-ggplot(aes(verg.velocity,sdflag),data=filter(gptest,abs(peak.verg.velocity)<250))+
  geom_point(alpha=1/20,size=0.5)+
  geom_point(aes(verg.velocity,predicted.sdf),color='orange',alpha=1/20,size=0.5)+
  facet_grid(neuron~saccade.type,scales='free')

# coord_cartesian(xlim=c(-200,200)))

# ggsave('nrtp-2a.png',height=26,width=5,plot=gs)

# 
# ggplot(aes(log(verg.velocity+200),sdflag),data=gp)+geom_point(alpha=1/20,size=0.5)+
#   # facet_wrap(~saccade.type)+
#   facet_grid(neuron~saccade.type,scales='free_y')+
#   xlim(c(4.75,6))

zp %>%
  group_by(neuron) %>%
  do(getObservedcoef(.)) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zo

zp %>%
  group_by(neuron) %>%
  do(getObservedcoefMONO(.)) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zoMONO

ggplot(zoMONO)+
  geom_point(aes(RH_o,RV_o),color='red')+
  geom_point(aes(LH_o,LV_o),color='blue')+
  facet_wrap(~monkey)+
  coord_fixed()


mxabs<-function(a,b){
  
  ifelse(abs(a)>abs(b),a,b)
}

zoMONO %>%
  group_by(monkey,neuron) %>%
  mutate(PrefH=maxabs())
  

ggplot(zoMONO)+
  geom_point(aes(mxabs(RH_o,LH_o),mxabs(RV_o,LV_o)),color='red')+
  facet_wrap(~monkey)+
  coord_fixed()


#Using original function (not mono)
ggplot(zo %>% filter(type != 'cyclopean'))+
  geom_point(aes(horizontal_o,vertical_o,color=type))+
  facet_wrap(~monkey)


zo %>%
  filter(type !='cyclopean')  %>%
  select(-cellnum,-intercept_o,-horizontal_o) %>%
  spread(type,vertical_o) %>%
  rename(leftV=left,
         rightV=right)->
  zopV

zo %>%
  filter(type !='cyclopean')  %>%
  select(-cellnum,-intercept_o,-vertical_o) %>%
  spread(type,horizontal_o) %>%
  rename(leftH=left,
         rightH=right)->
  zopH

zop <- left_join(zopV,zopH)

ggplot(zop)+
  geom_segment(aes(x=leftH,xend=rightH,y=leftV,yend=rightV))+
  geom_point(aes(horizontal_o,vertical_o,color=type),data=zo %>% filter(type !='cyclopean'))+
  facet_wrap(~monkey)+
  coord_fixed()

#we could bootstrap this
#update: we bootstrapped it

CheckEyeImportance<-function(z){
  
  mod<- lm(meanFR ~ mean.R.H+mean.R.V+ mean.L.H+mean.L.V,data=z)
  
  dt<-bind_rows(calc.relimp(mod)$lmg)
  # dt<-as.data.frame(rbind(as.numeric(coef(mod))))
  # names(dt)<-c('intercept_o','RH_o','RV_o','LH_o','LV_o')
  dt
}

zp %>%
  group_by(neuron) %>%
  do(CheckEyeImportance(.)) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zi

ggplotly(
ggplot(zi)+
  geom_histogram(aes(mean.R.V/mean.L.V,color=neuron))+
  facet_wrap(~monkey,ncol=1)
)


ggplotly(
  zi %>%
    mutate(skewratio=(mean.R.H+mean.L.H)/(mean.R.V+mean.L.V)) %>%
    ggplot()+
    geom_histogram(aes(skewratio,color=neuron))+
    facet_wrap(~monkey,ncol=1)
)


zi %>%
  mutate(skewratio=(mean.R.H+mean.L.H)/(mean.R.V+mean.L.V),
         skewratioR=mean.R.H/mean.L.V,
         skewratioL=mean.L.H/mean.L.V,
         minskew=min(skewratioR,skewratioL),
         eyeratio=(mean.R.H+mean.R.V)/(mean.L.H+mean.L.V)) %>%
  ggplot()+
  geom_boxplot(aes(monkey,eyeratio))

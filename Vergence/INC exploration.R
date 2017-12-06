t %>%
  filter(monkey=='DC') %>%
  group_by(neuron) %>%
  mutate(blinks=markSaccades(rep,buffer=80,threshold=70),
         # dsnum=markSaccadesDouble(conj.velocity,threshold1=30,threshold2=15),
         dsnum=markSaccadesDouble(replace(conj.velocity,blinks>0,150),
                                  threshold1=30,threshold2=15),
         sdf=spikedensity(rasters,10),
         sdf10=lag(sdf,10),
         conj.vert=(repV+lepV)/2,
         conj.vert.vel=(revV+levV)/2,
         conj.hor=(rep+lep)/2,
         conj.hor.v=(rev+lev)/2) %>%
  group_by(dsnum) %>%
  mutate(sd.conj.velocity=sd(conj.velocity),
         dur=n(),
         asleep=(sd.conj.velocity>7.5 & dsnum<0) || dur>2000)->
  t


#summarize by fixation
t %>%
  group_by(neuron)%>%
  mutate(time=row_number()) %>%
  filter(dsnum<0) %>% #fixations only
  group_by(neuron,dsnum) %>%
  summarize(sd.conj.velocity=sd(conj.velocity),
            mean.conj.velocity=mean(conj.velocity),
            spread=max(conj.velocity)-min(conj.velocity),
            qrange=quantile(conj.velocity,0.975)-quantile(conj.velocity,0.025),
            dur=n(),
            starttime=first(time),
            c2eyes=cor(repV,lepV),
            meanFR=sum(rasters)/dur*1000,
            mean.V=mean(conj.vert),
            mean.H=mean(conj.hor),
            mean.R.H=mean(rep),
            mean.L.H=mean(lep),
            mean.R.V=mean(repV),
            mean.L.V=mean(lepV),
            mean.T.V=mean(tvp),
            mean.T.H=mean(thp),
            asleep=sd.conj.velocity>7.5 || dur>2000)->
  zp


measureCell<-function(x){
  x<- filter(x,dur>200,abs(mean.R.H)<50,abs(mean.L.H)<50)
  mRH<-lm(meanFR~mean.R.H,data=x)
  mLH<-lm(meanFR~mean.L.H,data=x)
  mRV<-lm(meanFR~mean.R.V,data=x)
  mLV<-lm(meanFR~mean.L.V,data=x)
  mRHV<-lm(meanFR~mean.R.H+mean.R.V,data=x)
  mLHV<-lm(meanFR~mean.L.H+mean.L.V,data=x)
  
  RH.p=anova(mRH)$'Pr(>F)'[1]
  LH.p=anova(mLH)$'Pr(>F)'[1]
  LV.p<- anova(mLV)$'Pr(>F)'[1]
  RV.p<- anova(mRV)$'Pr(>F)'[1]
  RH<-as.numeric(coef(mRHV)[2])
  RV<-as.numeric(coef(mRHV)[3]) 
  LH<-as.numeric(coef(mLHV)[2])
  LV<-as.numeric(coef(mLHV)[3])  
  
  #calculate direction preference using slopes of regression lines
  dir.pref.R<-as.numeric(atan2(RV,RH))*180/pi
  dir.pref.L<-as.numeric(atan2(LV,LH))*180/pi
  
  #calculate direction preference using relative importance of horizontal and vertical
  br=as.numeric(calc.relimp(mRHV)$lmg)
  bl=as.numeric(calc.relimp(mLHV)$lmg)
  dir.imp.R<- atan2(br[2]*sign(RV),br[1]*sign(RH))*180/pi
  dir.imp.L<- atan2(bl[2]*sign(LV),bl[1]*sign(LH))*180/pi
  
  any.sig=RV.p<0.001 | LV.p<0.001| LH.p <0.001| RH.p<0.001
  R2R<-summary(mRHV)$r.squared
  R2L<- summary(mLHV)$r.squared
  
  #Calculate RMSE
  # rmseR=sqrt(c(crossprod(mRHV$residuals))/mRHV$df.residual)
  # rmseL=sqrt(c(crossprod(mLHV$residuals))/mLHV$df.residual)
  rmseR=summary(mRHV)$sigma
  rmseL=summary(mLHV)$sigma
  
  d<-data.frame(neuron=unique(x$neuron),
                RH=RH,RV=RV,LH=LH,LV=LV,dir.pref.R=dir.pref.R,dir.pref.L=dir.pref.L,
                RH.p=RH.p,LH.p=LH.p,RV.p=RV.p,LV.p=LV.p,any.sig=any.sig,
                R2R,R2L,dir.imp.R,dir.imp.L,
                rmseR=rmseR,rmseL=rmseL)
  
  #return: slope for right and left eye, dir pref, p values for right left h v 
}

#zm<- readRDS('NPH measured 11-6-2017.RDS')

zp %>%
  group_by(neuron) %>%
  do(measureCell(.)) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zm

ggplot(zm %>% filter(any.sig,cellnum==902))+
  geom_segment(aes(y=0,yend=1,x=dir.pref.R,xend=dir.pref.R),color='red')+
  geom_segment(aes(y=0,yend=1,x=dir.pref.L,xend=dir.pref.L),color='blue')+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  facet_wrap(~monkey)+
  theme_minimal()

zm %>% 
  select(neuron,monkey,cellnum,R2R,R2L,dir.pref.R,dir.pref.L) %>% 
  mutate(r.skew=90-abs(dir.pref.R),
         l.skew=90-abs(dir.pref.L),
         m.skew=90-abs((dir.pref.R+dir.pref.L)/2))->
  skew

ggplot(skew)+
  geom_point(aes(m.skew,R2R))

ggplot(skew)+
  geom_point(aes(m.skew,R2R))+
  geom_point(aes(l.skew,R2L),color='blue')+
  geom_point(aes(r.skew,R2R),color='red')

ggplot(skew)+
  geom_point(aes(abs(m.skew),R2R))+
  stat_smooth(aes(abs(m.skew),R2R),se=FALSE)

zp<- filter(zp,dur>200,abs(mean.R.H)<50,abs(mean.L.H)<50,abs(mean.R.V)<50,abs(mean.L.V)<50,meanFR<500)

chosenCell='DC-915'

zm %>% filter(neuron==chosenCell) %>% select(neuron,R2R,R2L,dir.pref.R,dir.pref.L) %>% kable()


ggplot(zp %>% filter(neuron==chosenCell))+
  geom_point(aes(mean.R.H,meanFR),color='red')+
  geom_point(aes(mean.L.H,meanFR),color='blue')

ggplot(zp %>% filter(neuron==chosenCell))+
  geom_point(aes(mean.R.V,meanFR),color='red')+
  geom_point(aes(mean.L.V,meanFR),color='blue')

ggplot(zp %>% filter(neuron==chosenCell))+
  geom_point(aes(mean.V,meanFR),color='purple')+
  geom_point(aes(mean.H,meanFR),color='orange')+
  stat_smooth(aes(mean.V,meanFR),color='purple',method='lm')+
  stat_smooth(aes(mean.H,meanFR),color='orange',method='lm')

ggplot(zm %>% filter(neuron==chosenCell,any.sig))+
  geom_segment(aes(y=0,yend=1,x=dir.pref.R,xend=dir.pref.R),color='red')+
  geom_segment(aes(y=0,yend=1,x=dir.pref.L,xend=dir.pref.L),color='blue')+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  facet_wrap(~monkey)+
  theme_minimal()

ggplot(zm %>% filter(neuron==chosenCell))+
  geom_segment(aes(y=0,yend=1,x=dir.pref.R,xend=dir.pref.R),color='red')+
  geom_segment(aes(y=0,yend=1,x=dir.pref.L,xend=dir.pref.L),color='blue')+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  facet_wrap(~monkey)+
  theme_minimal()


zp %>% filter(neuron==chosenCell) %>% lm(meanFR~mean.R.V+mean.R.H,data=.) -> m

zp %>% filter(neuron==chosenCell) %>% mutate(pFR=predict(m,newdata=.))->
  zpp

qplot(mean.V,pFR,data=zpp)+geom_point(aes(mean.V,meanFR),color='hotpink')
qplot(mean.H,pFR,data=zpp)+geom_point(aes(mean.H,meanFR),color='hotpink')
#Analysis
"""
Based on the above plots and careful examination of the data, it seems that the most significant
difference between the normal INC cells and those found in strabismus is simply the goodness of the
position have such varied mean firing rates.
"""
#Bee-118 - oblique direction cell from a normal animal
#Bee-110 - perfect vertical (down) cell 

zp %>%
  group_by(neuron) %>%
  mutate(bin.R.V=cut(mean.R.V,seq(-25,25,by=10)))->
  zp
  
zp %>%
  # filter(neuron %in% c('Bee-110','Bee-118'),
  filter(neuron %in% c('DC-912','DC-915'),
         bin.R.V=='(-5,5]') %>%
  ggplot()+
  # geom_boxplot(aes(bin.R.V,meanFR,color=neuron))+
  geom_point(aes(mean.R.V,meanFR,color=neuron))

tt %>% 
  filter(neuron==chosenCell) %>%
  left_join(filter(zp,neuron==chosenCell),by='dsnum') ->
  tplot
  
tplot<-left_join(filter(t,neuron==chosenCell),filter(zp,neuron==chosenCell),by='dsnum')

tplot<-left_join(t,zp,by='dsnum')

tplot %>%
  group_by(dsnum) %>%
  mutate(counter=row_number(),
         dur=n()) %>%
  filter(dsnum<0)%>%
  # filter(dur<2000,dsnum<0,max(rep)<75)%>%
  ggplot()+
  geom_line(aes(counter,rep,group=dsnum))+
  geom_line(aes(counter,repV,group=dsnum))


blinks<-markSaccades(vtest$rep,buffer=80,threshold=70)
  
qplot(time,rep,data=vtest,geom='line')+
  geom_line(aes(time,lep),color='blue')+
  geom_point(aes(time,blinks*10))

vb<- replace(v,blinks>0,200)
bb<- markSaccadesDouble(vb,30,15)

qplot(time,rep,data=vtest,geom='line')+
  # geom_line(aes(time,lep),color='blue')+
  geom_point(aes(time,bb*10))

qplot(1:length(vb),vb,geom='line')

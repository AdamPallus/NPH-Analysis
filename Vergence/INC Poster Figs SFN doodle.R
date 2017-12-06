t<-readRDS('INCSTRAB-11-3-2017.RDS')
t<- readRDS('NPHdata.RDS')

t %>%
  # filter(monkey=='DC') %>%
  group_by(neuron) %>%
  mutate(#blinks=markSaccades(rep,buffer=80,threshold=70),
         dsnum=markSaccadesDouble(conj.velocity,threshold1=30,threshold2=15),
         # dsnum=markSaccadesDouble(replace(conj.velocity,blinks>0,150),
         #                                  threshold1=30,threshold2=15),
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

ggplot(filter(zm,any.sig))+
  geom_segment(aes(y=0,yend=1,x=dir.pref.R,xend=dir.pref.R))+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  facet_wrap(~monkey)+
  theme_minimal()+
  ggtitle('Direction calculated using slopes of 2-D regression')

ggplot(filter(zm,any.sig))+
  geom_segment(aes(y=0,yend=1,x=dir.imp.R,xend=dir.imp.R))+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  facet_wrap(~monkey)+
  theme_minimal()+
  ggtitle('Direction calculated using relative importance of horizontal and vertical')

ggplot(filter(zm,any.sig))+
  geom_segment(aes(y=0,yend=1,x=dir.imp.R,xend=dir.imp.R,color=cut(R2R,c(0,0.1,0.4,.6,0.7,1))))+
  # scale_color_continuous(low='orange',high='black')+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  facet_wrap(~monkey,ncol=1)+
  theme_minimal()+
  ggtitle('Direction calculated using relative importance of horizontal and vertical')


ggplot(filter(zm,any.sig))+
  geom_segment(aes(y=0,yend=R2R,x=dir.imp.R,xend=dir.imp.R))+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  facet_wrap(~monkey)+
  theme_minimal()+
  ggtitle('Direction calculated using relative importance and scaled by goodness of fit')

ggplot(filter(zm,any.sig))+
  geom_segment(aes(y=0,yend=rmseR,x=dir.imp.R,xend=dir.imp.R))+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  facet_wrap(~monkey)+
  theme_minimal()+
  ggtitle('Direction calculated using relative importance and scaled by RMSE')

ggplot(zm %>% filter(any.sig,cellnum==901))+
  geom_segment(aes(y=0,yend=1,x=dir.pref.R,xend=dir.pref.R))+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  facet_wrap(~monkey)+
  theme_minimal()

ggplot(zm %>% filter(any.sig))+
  geom_segment(aes(y=0,yend=1,x=dir.pref.R,xend=dir.pref.R),color='red')+
  geom_segment(aes(y=0,yend=1,x=dir.pref.L,xend=dir.pref.L),color='blue')+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  facet_wrap(~monkey)+
  theme_minimal()

ggplot(zm)+
  geom_segment(aes(y=0,yend=1,x=dir.pref.R,xend=dir.pref.R))+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  facet_wrap(~monkey)

ggplot(filter(zm,any.sig))+
  geom_segment(aes(y=0,yend=1,x=dir.pref.R,xend=dir.pref.R,color=RH.p<0.001 | LH.p <0.001))+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  facet_wrap(~monkey)+
  theme(legend.position = 'bottom')

nBee=nrow(filter(zm,monkey=='Bee'))
nDC=nrow(filter(zm,monkey=='DC'))
nKopa=nrow(filter(zm,monkey=='Kopachuck'))

zm %>%
  group_by(monkey) %>%
  filter(RV.p<0.001 | LV.p<0.001| LH.p <0.001| RH.p<0.001) %>%
  summarize(n=n(),
            nRVs=sum(RV.p<0.001),
            nRHs=sum(RH.p<0.001),
            nLVs=sum(LV.p<0.001),
            nLHs=sum(LH.p<0.001),
            percentHR=nRHs/n,
            percentHL=nLHs/n,
            percentVR=nRVs/n,
            percentVL=nLVs/n)->
  zms

#model direction pref for each eye

zp %>%
  group_by(neuron) %>%
  do(mod.conj=lm(meanFR~mean.V+mean.H,data=filter(.,!asleep,dur>100,meanFR>0)),
     mod.left=lm(meanFR~mean.L.V+mean.L.H,data=filter(.,!asleep,dur>100,meanFR>0)),
     mod.right=lm(meanFR~mean.R.V+mean.R.H,data=filter(.,!asleep,dur>100,meanFR>0)))->
  # do(mod=lm(meanFR~mean.T.V+mean.T.H,data=filter(.,!asleep,dur>100,meanFR>0)))->
  zm


zm %>%
  mutate(mean.L.V.imp=calc.relimp(mod.left)$lmg[1],
         mean.L.H.imp=calc.relimp(mod.left)$lmg[2],
         mean.R.V.imp=calc.relimp(mod.right)$lmg[1],
         mean.R.H.imp=calc.relimp(mod.right)$lmg[2])%>%
  select(-mod.left,-mod.right,-mod.conj)%>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zzm

zzm %>%
  mutate(dir.pref.L=atan2(mean.L.V.imp,mean.L.H.imp)*180/pi,
         dir.pref.R=atan2(mean.R.V.imp,mean.R.H.imp)*180/pi)->
  zzm


ggplot(zzm)+
  geom_segment(aes(yend=(mean.R.V.imp+mean.R.H.imp),xend=dir.pref.R,y=0,x=dir.pref.R),color='black',
               alpha=0.5,arrow=arrow(type='closed',length =unit(0.1,"inches")))+
  # geom_text(aes(dir.pref,R2+0.05,label=cellnum),alpha=1)+
  coord_polar(direction=-1,start=pi/2)+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  theme_minimal()+
  ggtitle('Direction preference and goodness of fit')+
  ylab('Goodness of Fit')+
  xlab('Preferred Direction')+
  facet_wrap(~monkey)

ggplot(zzm)+
  geom_segment(aes(yend=(mean.L.V.imp+mean.L.H.imp),xend=dir.pref.L,y=0,x=dir.pref.L),color='black',
               alpha=0.5,arrow=arrow(type='closed',length =unit(0.1,"inches")))+
  # geom_text(aes(dir.pref,R2+0.05,label=cellnum),alpha=1)+
  coord_polar(direction=-1,start=pi/2)+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  theme_minimal()+
  ggtitle('Direction preference and goodness of fit')+
  ylab('Goodness of Fit')+
  xlab('Preferred Direction')+
  facet_wrap(~monkey)


#new pref dir
zp %>%
  group_by(neuron) %>%
  mutate(dir.fix=atan2(mean.V,mean.H)*180/pi,
         dir.fix.R=atan2(mean.R.V,mean.R.H)*180/pi,
         dir.fix.L=atan2(mean.L.V,mean.L.H)*180/pi)->
  x

chosenCell='Bee-101'


ggplot(filter(x,neuron==chosenCell))+
  geom_point(aes(dir.fix,meanFR))+
  geom_point(aes(dir.fix.R,meanFR),color='red')+
  geom_point(aes(dir.fix.L,meanFR),color='blue')+
  coord_polar(start=pi/2)+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))

ggplot(x)+
  # geom_point(aes(dir.fix,meanFR))+
  geom_point(aes(dir.fix.R,meanFR),color='red')+
  geom_point(aes(dir.fix.L,meanFR),color='blue')+
  coord_polar(start=pi/2)+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  ylim(0,400)+
  facet_wrap(~neuron,ncol=4)

cells=unique(x$neuron)
# manipulate(
# ggplot(filter(x,neuron==cells[chosenCell]))+
#   # geom_point(aes(dir.fix,meanFR))+
#   geom_point(aes(dir.fix.R,meanFR),color='red')+
#   geom_point(aes(dir.fix.L,meanFR),color='blue')+
#   coord_polar(start=pi/2)+
#   ggtitle(cells[chosenCell])+
#   scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90)),
# chosenCell=slider(1,length(cells)))

manipulate(
  ggplot(filter(x,neuron==cells[chosenCell]))+
    # geom_point(aes(dir.fix,meanFR))+
    geom_point(aes(mean.R.H,meanFR),color='red')+
    geom_point(aes(mean.L.H,meanFR),color='blue')+
    stat_smooth(aes(mean.R.H,meanFR),method='lm',color='red')+
    stat_smooth(aes(mean.L.H,meanFR),method='lm',color='blue')+
    ggtitle(cells[chosenCell]),
  chosenCell=slider(1,length(cells)))

manipulate(
  ggplot(filter(x,neuron==cells[chosenCell]))+
    # geom_point(aes(dir.fix,meanFR))+
    geom_point(aes(mean.R.V,meanFR),color='red')+
    geom_point(aes(mean.L.V,meanFR),color='blue')+
    stat_smooth(aes(mean.R.V,meanFR),method='lm',color='red')+
    stat_smooth(aes(mean.L.V,meanFR),method='lm',color='blue')+
    ggtitle(cells[chosenCell]),
  chosenCell=slider(1,length(cells)))



#'5-2-2018
#'In this scipt, I'm trying to see what influence the saccade that preceeds the fixation
#'has on the activity of the INC cells. I was hypothesizing that different burst neurons
#'are responsible for generating different saccades which would mean different signals are 
#'integrated even when the eyes are in the same position during fixation. 
#'
#'Right no it doesn't seem to be working because I'm expecing that there should be a relationship 
#'between saccade direction and fixation position, but I'm not seeing it. I tried dsnum and dsnum-1
#'
#'It's possible that the data set is just random enough?  
#'
#'It's looking like I'm just not getting good data about the saccades.
#'I could either try to use the previous fixation to compare, or I could try rejecting bad saccades
#'

#'5-3-2018
#'There was just a bug where the fixations were being numbered after removing all the bad
#'fixations while the saccades were being numbered first, then having the bad ones removed.
#'What I did here to get it to work is to just renumber the saccades so they match up. Now it works!

measureFixations<-function(t) {
  
  t %>%
    # group_by(neuron)%>%
    # mutate(time=row_number()) %>%
    filter(dsnum<0) %>% #fixations only
    group_by(neuron,dsnum) %>%
    summarize(sd.conj.velocity=sd(conj.velocity),
              mean.conj.velocity=mean(conj.velocity),
              spread=max(conj.velocity)-min(conj.velocity),
              qrange=quantile(conj.velocity,0.975)-quantile(conj.velocity,0.025),
              peak.conj.velocity=maxabs(conj.velocity),
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
              mean.verg.angle=mean(lep-rep),
              sd.sdf=sd(sdf10),
              asleep=sd.conj.velocity>7.5 || dur>2000)->
    zp
}

saccadeprefix<-function(t) {
  t%>% 
    # group_by(neuron) %>%
    # mutate(time=row_number()) %>%
    filter(dsnum>0) %>%
    group_by(neuron,dsnum) %>%
    summarize(conj.vert=first(conj.vert),
              R.H.Amp=last(rep)-first(rep),
              R.V.Amp=last(repV)-first(repV),
              L.H.Amp=last(lep)-first(lep),
              L.V.Amp=last(lepV)-first(lepV),
              disjunctiveH=sign(R.H.Amp*L.H.Amp)<0,
              disjunctiveV=sign(R.V.Amp*L.V.Amp)<0,
              conj.H.Amp=(R.H.Amp+L.H.Amp)/2,
              conj.V.Amp=(R.V.Amp+L.V.Amp)/2,
              r.amp=sqrt(conj.H.Amp^2+conj.V.Amp^2),
              endtime=last(time),
              # r.angle=atan2(R.V.Amp,R.H.Amp)*180/pi) %>%
              r.angle=atan2(conj.V.Amp,conj.H.Amp)*180/pi) %>%
    
    select(neuron,dsnum,r.amp,r.angle,R.V.Amp,endtime)->
    zsp
}

t %>%
  # filter(neuron=='Bee-110') %>%
  group_by(neuron) %>%
  do(measureFixations(.)) ->
  zpp

t %>%
  # filter(neuron=='Bee-110') %>%
  group_by(neuron) %>%
  do(saccadeprefix(.))->
  zps

zps %>%
  mutate(dsnumold=dsnum,
         dsnum=(row_number()+1)*-1)->
  zps



zpp %>%
  # filter(neuron=='Bee-110') %>%
  left_join(zps,by=c('neuron','dsnum'))->
  testsac


qplot(R.V.Amp,mean.V,size=r.amp,data=testsac %>% filter(abs(R.V.Amp)<30))


qplot(R.V.Amp,meanFR,data=testsac %>% filter(meanFR<300,meanFR>1,abs(R.V.Amp)<30))+
  facet_wrap(~neuron)

qplot(R.V.Amp,meanFR,data=testsac %>% 
        filter(meanFR<300,meanFR>1,abs(R.V.Amp)<30,neuron=='Kopachuck-942'))+
  facet_wrap(~neuron)

qplot(r.angle,meanFR,data=testsac %>% 
        filter(meanFR<300,meanFR>1,abs(R.V.Amp)<30,neuron=='Kopachuck-942',r.amp>5))+
  facet_wrap(~neuron)+stat_smooth()+
  geom_vline(xintercept = c(-90,90))+
  xlab('Direction of saccade prior to fixation (deg)')+
  ylab('Mean Firing Rate During Fixation (deg/s)')

qplot(r.angle,meanFR,data=testsac %>% 
        filter(meanFR<300,meanFR>1,abs(R.V.Amp)<30,neuron=='Kopachuck-942',r.amp>5))+
  facet_wrap(~neuron)+stat_smooth(method='lm',formula = y ~ poly(x,3))+
  geom_vline(xintercept = c(-90,90))+
  xlab('Direction of saccade prior to fixation (deg)')+
  ylab('Mean Firing Rate During Fixation (deg/s)')

qplot(r.angle,meanFR,data=testsac %>% 
        filter(meanFR<300,meanFR>1,abs(R.V.Amp)<30,r.amp>5,dur>200))+
  facet_wrap(~neuron,scales='free_y')+stat_smooth()+
  geom_vline(xintercept = c(-90,90))+
  xlab('Direction of saccade prior to fixation (deg)')+
  ylab('Mean Firing Rate During Fixation (deg/s)')

mod<- lm(meanFR~poly(r.angle,3),
         data=filter(testsac,neuron=='Bee-110',meanFR>1,meanFR<250,r.amp>5,dur>200))
summary(mod)

template<-data.frame(r.angle=-180:180)

xx<-predict(mod,newdata=template)

preferred.direction=xx[xx=max(xx)]


getPreferredDirection<-function(tt){
  
  mod<- lm(meanFR~poly(r.angle,3),
           data=filter(tt,meanFR>1,meanFR<250,r.amp>5,dur>200))
  
  template<-data.frame(r.angle=-180:180)
  
  template$xx<-predict(mod,newdata=template)
  
  preferred.direction=filter(template,xx==max(xx))$r.angle
  pFR=max(template$xx)
  return(data.frame(dirpref=preferred.direction,pFR=pFR))
}

testsac %>%
  group_by(neuron) %>%
  do(getPreferredDirection(.)) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  pdirs

qplot(dirpref,data=pdirs)+facet_wrap(~monkey,ncol=1)

ggplot(pdirs)+
  geom_segment(aes(y=0,yend=pFR,x=dirpref,xend=dirpref,color=monkey))+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  theme_minimal()+
  theme(legend.position='none')+
  facet_wrap(~monkey,ncol=1)+
  xlab('Direction preference')


#Direction using fixations again...
testsac %>% 
  mutate(fixdirpref=atan2(mean.V,mean.H)*180/pi) %>%
  filter(neuron=='Bee-110',meanFR<250,meanFR>1) %>% 
  ggplot(aes(fixdirpref,meanFR))+
  geom_point()+
  stat_smooth(method='lm',formula= y~poly(x,3))


testsac %>% 
  filter(neuron=='Bee-110',meanFR<250,meanFR>1) %>% 
  mutate(fix.angle=atan2(mean.V,mean.H)*180/pi,
         fix.radius=sqrt(mean.H^2+mean.V^2)) %>%
  filter(meanFR<250,meanFR>1) %>% 
  ggplot(aes(fix.angle,meanFR,color=fix.radius))+
  geom_point()+
  stat_smooth(method='lm',formula= y~poly(x,3))+
  facet_wrap(~neuron)+
  geom_vline(xintercept = c(-90,90))

testsac %>% 
  filter(neuron=='Kopachuck-903',meanFR<250,meanFR>1) %>% 
  mutate(fix.angle=atan2(mean.V,mean.H)*180/pi,
         fix.radius=sqrt(mean.H^2+mean.V^2)) %>%
  filter(meanFR<250,meanFR>1,
         fix.radius>3) %>% 
  ggplot(aes(fix.angle,meanFR,color=fix.radius))+
  geom_point()+
  stat_smooth(method='lm',formula= y~poly(x,3))+
  facet_wrap(~neuron)+
  geom_vline(xintercept = c(-90,90))

getPreferredDirectionFix<-function(ndata){
  
  mod<- lm(meanFR~poly(fix.angle,3),
           data=filter(ndata,meanFR>1,meanFR<300,fix.radius>5,dur>200))
  
  template<-data.frame(fix.angle=-179:180)

  template$xx<-predict(mod,newdata=template)

  preferred.direction=filter(template,xx==max(xx))$fix.angle
  pFR=max(template$xx)
  return(data.frame(dirpref=preferred.direction,pFR=pFR,R2=summary(mod)$r.squared))
}

getPreferredDirectionFixSin<-function(ndata,eye='cyclopean',min_amp=5){
  
  if (eye=='cyclopean'){
    ndata %>%
      filter(meanFR>1,meanFR<300,fix.radius>min_amp,dur>200) %>%
      mutate(fix.angle=atan2(mean.V,mean.H),
             fix.radius=sqrt(mean.H^2+mean.V^2))->
      ndata
  } else if(eye=='right'){
    
    ndata %>%
      filter(meanFR>1,meanFR<300,fix.radius>min_amp,dur>200) %>%
      mutate(fix.angle=atan2(mean.R.V,mean.R.H),
             fix.radius=sqrt(mean.R.H^2+mean.R.V^2))->
      ndata
  }else if (eye=='left'){
    
    ndata %>%
      filter(meanFR>1,meanFR<300,fix.radius>min_amp,dur>200) %>%
      mutate(fix.angle=atan2(mean.L.V,mean.L.H),
             fix.radius=sqrt(mean.L.H^2+mean.L.V^2))->
      ndata
  }else{
    message('Invalid Eye Choice')
    return(NULL)
  }
  
  ndata %>%
    filter(meanFR>1,meanFR<300,fix.radius>5,dur>200) %>%
    mutate(fix.angle=atan2(mean.R.V,mean.R.H),
           fix.radius=sqrt(mean.R.H^2+mean.R.V^2))->
    ndata
  
  mod<- lm(meanFR~sin(fix.angle)+cos(fix.angle)+fix.angle,
           data=filter(ndata,meanFR>1,meanFR<300,fix.radius>5,dur>200))
  
  template<-data.frame(fix.angle=seq(-pi,pi,by = 0.01))
  
  template$xx<-predict(mod,newdata=template)
  
  preferred.direction=filter(template,xx==max(xx))$fix.angle
  pFR=max(template$xx)
  return(data.frame(dirpref=preferred.direction*180/pi,pFR=pFR,R2=summary(mod)$r.squared))
}

testsac %>%
  ungroup() %>%
  group_by(neuron) %>%
  # filter(neuron=='Bee-110') %>%
  do(getPreferredDirectionFixSin(.,eye='left',min_amp = 10)) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  dpfix

multiplot(
  ggplot(dpfix)+
    geom_segment(aes(y=0,yend=pFR,x=dirpref,xend=dirpref,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=3)+
    xlab('Direction preference')+
    ylab('Average Maximum FR')
,
  ggplot(dpfix)+
    geom_segment(aes(y=0,yend=R2,x=dirpref,xend=dirpref,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=3)+
    xlab('Direction preference')+
    ylab('Goodness of Fit')
)

testsac %>% 
  filter(meanFR<300,meanFR>1,
         fix.radius>5) %>% 
  ggplot(aes(fix.angle,meanFR,color=fix.radius))+
  geom_point()+
  stat_smooth(method='lm',formula= y~poly(x,3))+
  facet_wrap(~neuron)+
  geom_vline(xintercept = c(-90,90))


ggplot(dpfix,aes(monkey,R2))+
  geom_boxplot()+
  geom_jitter(aes(color=monkey))

ggplot(dpfix,aes(monkey,abs(dirpref)))+
  geom_boxplot()+
  geom_jitter(aes(color=monkey,size=R2))

ggplot(dpfix,aes(dirpref,R2,color=monkey))+
  geom_point()+
  facet_wrap(~monkey,ncol=1)+
  geom_vline(xintercept = c(-90,90))


testdata<-filter(testsac,neuron=='Bee-110',
                 meanFR<300,
                 meanFR>1,
                 fix.radius>5)

testdata<-mutate(testdata,fix.angle=fix.angle*pi/180)

ggplot(testdata,aes(fix.angle,meanFR))+
  geom_point()+
  stat_smooth(method='lm',formula=y~sin(x)/x)

mod<- lm(meanFR~poly(fix.angle,3),
         data=filter(testdata,
                     meanFR>1,meanFR<300,fix.radius>5,dur>200))

mod<- lm(meanFR~sin(fix.angle),
         data=filter(testdata,
                     meanFR>1,meanFR<300,fix.radius>5,dur>200))

summary(mod)

neurons=unique(testsac$neuron)
manipulate(
testsac %>% 
  filter(neuron==neurons[chosenCell],meanFR<250,meanFR>1) %>% 
  mutate(fix.angle=atan2(mean.V,mean.H),
         fix.radius=sqrt(mean.H^2+mean.V^2)) %>%
  filter(meanFR<250,meanFR>1,
         fix.radius>3) %>% 
  ggplot(aes(fix.angle,meanFR,color=fix.radius))+
  geom_point()+
  stat_smooth(method='lm',formula= y~sin(x)+cos(x)+x)+
  facet_wrap(~neuron)+
  geom_vline(xintercept = c(-pi/2,pi/2)),
chosenCell=slider(1,length(neurons)))

testsac %>% 
  # filter(neuron=='Bee-118')%>%
  filter(meanFR<250,meanFR>1) %>% 
  mutate(fix.angle=atan2(mean.R.V,mean.R.H),
         fix.radius=sqrt(mean.R.H^2+mean.R.V^2)) %>%
  filter(meanFR<250,meanFR>0,
         fix.radius>10,
         fix.radius<40) %>% 
  ggplot(aes(fix.angle,meanFR,color=fix.radius))+
  geom_point()+
  stat_smooth(method='lm',formula= y~sin(x)+cos(x)+x)+
  facet_wrap(~neuron,scales='free_y')+
  geom_vline(xintercept = c(-pi/2,pi/2))+
  xlab('Right Eye direction (radians)')

testsac %>% 
  # filter(neuron=='Bee-118')%>%
  filter(meanFR<250,meanFR>1) %>% 
  mutate(fix.angle=atan2(mean.V,mean.H),
         fix.radius=sqrt(mean.H^2+mean.V^2)) %>%
  filter(meanFR<250,meanFR>0,
         fix.radius>10,
         fix.radius<40) %>% 
  ggplot(aes(fix.angle,meanFR,color=fix.radius))+
  geom_point()+
  stat_smooth(method='lm',formula= y~sin(x)+cos(x)+x)+
  facet_wrap(~neuron,scales='free_y')+
  geom_vline(xintercept = c(-pi/2,pi/2))+
  xlab('Cyclopean Eye direction (radians)')

testsac %>% 
  # filter(neuron=='Bee-118')%>%
  filter(meanFR<250,meanFR>1) %>% 
  mutate(fix.angle=atan2(mean.L.V,mean.L.H),
         fix.radius=sqrt(mean.L.H^2+mean.L.V^2)) %>%
  filter(meanFR<250,meanFR>0,
         fix.radius>10,
         fix.radius<40) %>% 
  ggplot(aes(fix.angle,meanFR,color=fix.radius))+
  geom_point()+
  stat_smooth(method='lm',formula= y~sin(x)+cos(x)+x)+
  facet_wrap(~neuron,scales='free_y')+
  geom_vline(xintercept = c(-pi/2,pi/2))+
  xlab('Left Eye direction (radians)')


testsac %>% 
  filter(neuron=='DC-927')%>%
  filter(meanFR<250,meanFR>1) %>% 
  mutate(fix.angle.L=atan2(mean.L.V,mean.L.H),
         fix.radius.L=sqrt(mean.L.H^2+mean.L.V^2),
         fix.angle.R=atan2(mean.R.V,mean.R.H),
         fix.radius.R=sqrt(mean.R.H^2+mean.R.V^2),
         fix.angle.C=atan2(mean.V,mean.H),
         fix.radius.C=sqrt(mean.H^2+mean.V^2)) %>%
  filter(meanFR<250,meanFR>0,
         fix.radius.L>10,
         fix.radius.R<40) %>% 
  # ggplot(aes(fix.angle,meanFR,color=fix.radius))+
  ggplot()+
  geom_point(aes(fix.angle.L,meanFR),color='blue')+
  geom_point(aes(fix.angle.R,meanFR),color='red')+
  geom_point(aes(fix.angle.C,meanFR),color='black')+
  
  stat_smooth(aes(fix.angle.L,meanFR),method='lm',formula= y~sin(x)+cos(x)+x,color='blue')+
  stat_smooth(aes(fix.angle.R,meanFR),method='lm',formula= y~sin(x)+cos(x)+x,color='red')+
  stat_smooth(aes(fix.angle.C,meanFR),method='lm',formula= y~sin(x)+cos(x)+x,color='black')+
  
  # stat_smooth(method='lm',formula= y~sin(x)+cos(x)+x)+
  facet_wrap(~neuron,scales='free_y')+
  geom_vline(xintercept = c(-pi/2,pi/2))

testsac %>% 
  separate(neuron,c('monkey','cellnum'),remove=FALSE) %>%
  filter(monkey=='Kopachuck')%>%
  filter(meanFR<250,meanFR>1) %>% 
  mutate(fix.angle.L=atan2(mean.L.V,mean.L.H),
         fix.radius.L=sqrt(mean.L.H^2+mean.L.V^2),
         fix.angle.R=atan2(mean.R.V,mean.R.H),
         fix.radius.R=sqrt(mean.R.H^2+mean.R.V^2),
         fix.angle.C=atan2(mean.V,mean.H),
         fix.radius.C=sqrt(mean.H^2+mean.V^2)) %>%
  filter(meanFR<250,meanFR>0,
         fix.radius.L>10,
         fix.radius.R<40) %>% 
  # ggplot(aes(fix.angle,meanFR,color=fix.radius))+
  ggplot()+
  # geom_point(aes(fix.angle.L,meanFR),color='blue')+
  geom_point(aes(fix.angle.R,meanFR,color=fix.radius.R))+
  # geom_point(aes(fix.angle.C,meanFR),color='black')+
  
  # stat_smooth(aes(fix.angle.L,meanFR),method='lm',formula= y~sin(x)+cos(x)+x,color='blue')+
  stat_smooth(aes(fix.angle.R,meanFR),method='lm',formula= y~sin(x)+cos(x)+x,color='red')+
  # stat_smooth(aes(fix.angle.C,meanFR),method='lm',formula= y~sin(x)+cos(x)+x,color='black')+
  
  # stat_smooth(method='lm',formula= y~sin(x)+cos(x)+x)+
  facet_wrap(~neuron,ncol=8)+
  geom_vline(xintercept = c(-pi/2,pi/2))+
  xlab('Right Eye Direction (radians)')

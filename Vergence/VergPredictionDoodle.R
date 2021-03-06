source('markSaccadesDoubleDrift.R')
modelVV2<- function(t,chosenCell='Bee-113',saccadebuffer=20,saccadethreshold=30,
                    model.form='verg.velocity~sdf20+verg.angle',
                    lagsdf=31,returnmodel=FALSE){
  
  x<- ungroup(filter(t,neuron==chosenCell))
  if (!('time' %in% names(x))){
    x<- mutate(x,time=row_number())
  }
  parabolic_n<- 20
  x %>%
    mutate(verg.velocity=parabolicdiff(lep-rep,parabolic_n),
           rev=parabolicdiff(rep,parabolic_n),
           lev=parabolicdiff(lep,parabolic_n),
           revV=parabolicdiff(repV,parabolic_n),
           levV=parabolicdiff(lepV,parabolic_n),
           sdf=spikedensity(rasters,sd=25),
           sdf20=dplyr::lag(sdf,lagsdf),
           conj.velocity=sqrt(((rev+lev)/2)^2+((revV+levV)/2)^2)) ->
    x
  # x<- mutate(x,saccadic=markSaccades(conj.velocity,buffer=5,threshold=20)>0)
    x<-  mutate(x,saccadic=!is.na(markSaccadesDoubleDrift(conj.velocity)))
    
  x<- joinsaccades(x,buffer=saccadebuffer,threshold=saccadethreshold)
  x %>%
    group_by(sacnum) %>%
    mutate(verg.amp=last(verg.angle)-first(verg.angle),
           isconj=verg.amp<1,
           # saccadic=counter>saccadebuffer & counter<n()-saccadebuffer, #+20,
           # saccadic=replace(saccadic,!saccadic,NA)
           conj.h.vel=(lev+rev)/2,
           conj.v.vel=(levV+revV)/2,
           instant.dir=atan2(conj.v.vel,conj.h.vel)*180/pi
           # target.verg=thp-thp2,
           # verg.error=verg.angle-target.verg
           )->
    x
  
  xm<- group_by(x,time) %>% summarize_all(funs(first))
  # x<- mutate(x,saccadic=!is.na(sacnum))
  
  ###############
  ##Make sure to uncomment this line to make future figures!
  ##############
  #Note: For divergence cells, you want to show the model diverging movements,
  #so you can't use velocity >0. I used velocity > 0 for cell that didn't fire at all
  #when the eyes were diverging. They would fire 0 spikes whether it was -5 or -50 deg/s, 
  #so trying to train a linear model with that only made the fit worse.
  
  #Depending on the firing rate of the cell, you might be able to model other movements.
  #For the cell patos 102 (from 9.09.2013), I was using from -30 to 8 deg/s
  
  #It might be worth trying to remove the pre and post saccadic vergence to model just slowverg
  
                mod<- lm(model.form,data=filter(xm,!saccadic,verg.velocity>0))
  # mod<- lm(model.form,data=filter(x,!saccadic))
  # mod<- lm(model.form,data=filter(xm,verg.velocity>0))
  # mod<- lm(model.form,data=filter(xm,!saccadic,verg.velocity<8,verg.velocity> -30))
  
  # mod<-lm(model.form,data=xm)
  # mod<- lm(model.form,data=filter(x,!isconj))
  if (returnmodel) return(mod)
  print(tidy(mod))
  # mod2<- lm('verg.velocity~sdf20+verg.angle',data=filter(x,enhance.type=='none',sdf20>30))
  message(paste('R-squared =', summary(mod)$r.squared))
  # x<- dplyr::select(x,-sacnum,-counter)
  x<- mutate(ungroup(x),predV=predict(mod,newdata=x),
             showrasters=replace(rasters,rasters<1,NA))
  
}


# mod<- modelVV2(t,chosenCell='Bee-27',lagsdf=20,
#                model.form='verg.velocity~sdf20+verg.angle',
#                returnmodel = TRUE)
# 
# 
# mod<- modelVV2(t,chosenCell='Bee-27',lagsdf=20,
#                model.form='sdf20~verg.velocity+verg.angle',
#                returnmodel = TRUE)
# 
# x3<- modelVV2(t,chosenCell='Bee-33',lagsdf=31,
#               model.form='verg.velocity~sdf20+verg.angle',
#               saccadebuffer=10)


bufferlength=400

modelVV2(ttest,chosenCell='Patos-105',lagsdf=20,
              model.form='verg.velocity~sdf20+verg.angle',
         # model.form='sdf20~verg.velocity+verg.angle',
              saccadebuffer=bufferlength) %>%
  group_by(sacnum) %>%
  mutate(saccade.dur=n()-2*bufferlength, 
         saccade.end=saccade.dur+bufferlength,
         peak.conj.velocity=maxabs(conj.velocity),
         peak.R.H= maxabs(rev),
         peak.R.V= maxabs(revV),
         peak.L.H= maxabs(lev),
         peak.L.V= maxabs(levV),
         R.H.Amp=rep[saccade.end]-rep[bufferlength],
         L.H.Amp=lep[saccade.end]-lep[bufferlength],
         R.V.Amp=repV[saccade.end]-repV[bufferlength],
         L.V.Amp=lepV[saccade.end]-lepV[bufferlength],
         conj.H.Amp=(R.H.Amp+L.H.Amp)/2,
         conj.V.Amp=(R.V.Amp+L.V.Amp)/2,
         r.angle=atan2(R.V.Amp,R.H.Amp)*180/pi,
         r.amp=sqrt(R.H.Amp^2+R.V.Amp^2),
         vect.amp= (sqrt(R.H.Amp^2+R.V.Amp^2)+sqrt(L.H.Amp^2+L.V.Amp^2))/2,
         maxamp=max(abs(R.H.Amp),abs(R.V.Amp),abs(L.H.Amp),abs(L.V.Amp)),
         saccadic.verg.amp=verg.angle[saccade.end]-verg.angle[bufferlength],
         total.verg.amp=sum(verg.velocity)/1000,
         mean.verg.amp=mean(verg.angle[saccade.end:n()]-mean(verg.angle[1:bufferlength])),
         peak.verg.velocity= maxabs(verg.velocity),
         min.verg.trans = min(verg.velocity),
         max.verg.trans = max(verg.velocity),
         off.verg.velocity=min(abs(min.verg.trans),abs(max.verg.trans)),
         min.verg.angle=min(verg.angle),
         max.verg.angle=max(verg.angle),
         max.verg.velocity=max(verg.velocity),
         min.verg.velocity=min(verg.velocity),
         initial.verg.angle=verg.angle[bufferlength],
         predicted.verg.amp=sum(predV)/1000,
         predicted.peak.verg.velocity=maxabs(predV))->
  z


# z %>%
#   mutate(target.verg=thp-thp2,
#          verg.error=verg.angle-target.verg)->
#   z
window_size=4000
manipulate({
  d=filter(z,time>=window,time<window+window_size)
  d<- mutate(d,saccadic=replace(saccadic,!saccadic,NA))
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_line(aes(time,sdf-100),color='hotpink')+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    # geom_area(aes(time,conj.velocity),alpha=1/3)+
    # geom_line(aes(time,sdf),alpha=1/3)+
    geom_line(aes(time,rep+50),color='red')+
    geom_line(aes(time,lep+50),color='blue')+
    geom_line(aes(time,repV+50),color='purple')+
    # geom_line(aes(time,lepV*10+300),color='blue')+
    # geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(-25,NA))+
    geom_point(aes(time,saccadic*50))
    # geom_line(aes(time,target.verg*10))
    # geom_line(aes(time,(target.verg-verg.angle)*10),color='hotpink')
  },
  window_size=slider(1000,5000,step=100,initial = 4000),
  window=slider(window_size,max(z$time-window_size),step=4000))

zp<- summarize_each(z,funs(first))
zp<-filter(zp,r.amp>3,predicted.verg.amp<100)

qplot(verg.amp,predicted.verg.amp,data=zp)+geom_abline()

qplot(verg.amp,predicted.verg.amp,data=zp,color=r.angle)+geom_abline()

ggplot(filter(zp,verg.amp>0))+
  # geom_point(aes(verg.amp,predicted.peak.verg.velocity),color='orange')+
  # geom_point(aes(verg.amp,peak.verg.velocity),color='black')+
  stat_smooth(aes(verg.amp,predicted.peak.verg.velocity),color='orange',method='lm')+
  stat_smooth(aes(verg.amp,peak.verg.velocity),color='black',method='lm')+
  ylim(c(0,200))


zp$SaccadeDirection=as.factor(sign(zp$r.angle))
levels(zp$SaccadeDirection)<- c('Downard','Upward')
zp$cd=as.factor(sign(zp$verg.amp))
levels(zp$cd)<- c('Diverging','Converging')

ggplot(zp)+
  geom_boxplot(aes(SaccadeDirection,verg.amp))+
  facet_wrap(~cd)+
  ylab("Vergence Amplitude (deg)")+
  xlab("Saccade Direction")

summary(aov(verg.amp~SaccadeDirection,data=zp))
summary(aov(verg.amp~SaccadeDirection,data=filter(zp,cd=='Converging')))
summary(aov(verg.amp~SaccadeDirection,data=filter(zp,cd=='Diverging')))

qplot(verg.amp,predicted.verg.amp,data=zp,color=SaccadeDirection)+geom_abline()

summary(aov(predicted.verg.amp,verg.amp+sign(r.angle),data=filter(zp,verg.amp>0)))

qplot(r.angle,predicted.verg.amp/verg.amp,data=filter(zp,abs(verg.amp)>0.5))+
  stat_smooth()+
  geom_hline(yintercept=1)+
  ylim(0,2)

ggplot(filter(zp,abs(verg.amp)>0.5))+
  geom_point(aes(r.angle,predicted.verg.amp/verg.amp,color=verg.amp>0))+
  stat_smooth(aes(r.angle,predicted.verg.amp/verg.amp),data=filter(zp,verg.amp>0.5))+
  geom_hline(yintercept=1)+
  ylim(0,2)+
  ggtitle('Model= no saccades, converging only')

zpp<- filter(zp,abs(conj.H.Amp)<100)

ggplot(filter(zp,abs(verg.amp)>0.5,r.amp>3))+
  geom_point(aes(conj.H.Amp,predicted.verg.amp/verg.amp,color=verg.amp>0))+
  stat_smooth(aes(conj.H.Amp,predicted.verg.amp/verg.amp),data=filter(zp,verg.amp>0.5))+
  geom_hline(yintercept=1)+
  ylim(-1,2)

ggplot(filter(zp,verg.amp>0.5,r.amp>3))+
  geom_point(aes(conj.V.Amp,predicted.verg.amp/verg.amp,color=conj.H.Amp))+
  stat_smooth(aes(conj.V.Amp,predicted.verg.amp/verg.amp),data=filter(zp,verg.amp>0.5))+
  geom_hline(yintercept=1)


ggplot(filter(zp,abs(verg.amp)>0.5))+
  geom_point(aes(r.angle,predicted.verg.amp/verg.amp,color=verg.amp>0))+
  stat_smooth(aes(r.angle,predicted.verg.amp/verg.amp),data=filter(zpp,verg.amp>0.5))+
  geom_hline(yintercept=1)+
  ylim(-1,2)+
  ggtitle('Model= no saccades, converging only')



{
  window=214750
  window_size=1000
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,rep+100),color='red')+
    geom_line(aes(time,lep+100),color='blue')+
    geom_line(aes(time,repV+100),color='red',linetype=2)+
    geom_line(aes(time,lepV+100),color='blue',linetype=2)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,150))+
    geom_point(aes(time,saccadic*50))
  }

{
window=203000
window_size=1500
d=filter(x3,time>=window,time<window+window_size)
ggplot(d)+
  geom_point(aes(time,showrasters+30),shape='|')+
  # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
  # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
  # geom_line(aes(time,slowpredict),color='orange')+
  geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
  # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
  geom_line(aes(time,predV),color='orange')+
  # geom_line(aes(time,predV2),color='magenta')+
  geom_line(aes(time,verg.velocity),color='darkblue')+
  geom_area(aes(time,conj.velocity),alpha=1/3)+
  geom_line(aes(time,rep+100),color='red')+
  geom_line(aes(time,lep+100),color='blue')+
  geom_line(aes(time,repV+100),color='red',linetype=2)+
  geom_line(aes(time,lepV+100),color='blue',linetype=2)+
  geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
  # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
  ylim(c(NA,150))+
  geom_point(aes(time,saccadic*50))
}

{
  window=209300
  window_size=1000
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    geom_line(aes(time,rep+100),color='red')+
    geom_line(aes(time,lep+100),color='blue')+
    geom_line(aes(time,repV+100),color='red',linetype=2)+
    geom_line(aes(time,lepV+100),color='blue',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,150))
  }

{
  window=228500
  window_size=2000
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,rep+100),color='red')+
    geom_line(aes(time,lep+100),color='blue')+
    geom_line(aes(time,repV+100),color='red',linetype=2)+
    geom_line(aes(time,lepV+100),color='blue',linetype=2)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))
}

{
  window=233500
  window_size=1000
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))
}

{
  window=243000
  window_size=1500
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))
}

{
  window=244500
  window_size=2000
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))
}

{
  window=249250
  window_size=1000
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,rep+100),color='red')+
    geom_line(aes(time,lep+100),color='blue')+
    geom_line(aes(time,repV+100),color='red',linetype=2)+
    geom_line(aes(time,lepV+100),color='blue',linetype=2)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))
}

{
  window=256500
  window_size=1500
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))
}

{
  window=261000
  window_size=2000
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))
}

{
  window=265500
  window_size=2000
  d=filter(x3,time>=window,time<window+window_size)
  d<- mutate(d,pred.angle=cumsum(predV)/100+first(verg.angle)*10,
             pred.angle=pred.angle-pred.angle[400]+verg.angle[400]*10)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,pred.angle),color='orange',linetype=2)+
    # geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))
}

{
  window=270000
  window_size=2000
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))
}

{
  window=277500
  window_size=2000
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))
}

{
  window=284000
  window_size=2000
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))
}

{
  window=297000
  window_size=2000
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))
}

{
  window=307000
  window_size=2000
  d=filter(x3,time>=window,time<window+window_size)
  ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))
}

saveModelPlot<- function(x, window=311500,window_size=2000,savename='model',savetype='PNG'){
  d=filter(x,time>=window,time<window+window_size)
  gs<- ggplot(d)+
    geom_point(aes(time,showrasters+30),shape='|')+
    # geom_point(aes(time,showenhance*verg.velocity),color='magenta')+
    # geom_area(aes(time,sdf),color='black',fill='pink',alpha=1/10)+
    # geom_line(aes(time,slowpredict),color='orange')+
    geom_line(aes(time,verg.angle*10+0),color='darkgreen')+
    # geom_line(aes(time,predP/1000),color='darkgreen',linetype=2)+
    geom_line(aes(time,predV),color='orange')+
    # geom_line(aes(time,predV2),color='magenta')+
    geom_line(aes(time,verg.velocity),color='darkblue')+
    geom_area(aes(time,conj.velocity),alpha=1/3)+
    geom_line(aes(time,cumsum(predV)/100+first(verg.angle)*10),color='orange',linetype=2)+
    geom_line(aes(time,target.verg*10))+
    geom_line(aes(time,rep+150),color='red')+
    geom_line(aes(time,lep+150),color='blue')+
    geom_line(aes(time,repV+150),color='red',linetype=2)+
    geom_line(aes(time,lepV+150),color='blue',linetype=2)+
    # geom_line(aes(time,verg.velocity-predV),color='red',linetype=2)
    ylim(c(NA,200))+
    ylab('')+
    annotate('text',y=100,x=window+200,label='Vergence Angle',color='darkgreen')+
    annotate('text',y=80,x=window+200,label='Vergence Velocity',color='darkblue')+
    annotate('text',y=70,x=window+200,label='Model',color='orange')+
    annotate('text',y=150,x=window+200,label='Vectorial Velocity',color='darkgray')
  savename<-paste('plots/',savename,sep='')
  ggsave(paste(paste(savename,window,sep='-'),savetype,sep='.'))
}

# saveModelPlot(x3,window=311500)

periodstoplot=as.array(c(203000,209000,228500,233000,243000,244500,249000,256500,
                261000,265500,270000,277500,284000,297000,307000,311500))

apply(periodstoplot,1,saveModelPlot,x=x3)

saveModelPlot(x=x3,window=81000)

goodsacs=unique(z$sacnum[abs(z$total.verg.amp)>4&z$r.amp>4])
goodsacs=unique(filter(z,abs(verg.amp)<2,r.amp>4)$sacnum)
goodsacs=goodsacs[!is.na(goodsacs)]
manipulate({
  d=filter(z,sacnum==goodsacs[currentsac])
  d<- mutate(d,showrasters=replace(rasters,rasters<1,NA))
  tvc=(d$lep-d$rep)[bufferlength+200]-(d$lep-d$rep)[bufferlength-100]
  ggplot(d)+
    # geom_line(aes(time,((lep-rep)-(lep-rep)[1])*5+15+150),color='darkgreen',size=1)+
    # geom_line(aes(time,((thp2-thp)-(thp2-thp)[1])*5+15+150),color='darkgray',size=1)+
    geom_area(aes(time,sdf/2),alpha=1)+
    geom_line(aes(time,((lep-rep))*5+15+150),color='darkgreen',size=1)+
    geom_hline(aes(yintercept=(lep-rep)[1]*5+15+150))+
    geom_point(aes(time,saccadic*-10))+
    geom_vline(aes(xintercept=time[1]+bufferlength+saccade.dur))+
    # geom_line(aes(time,(thp-thp2)*5+15+150),color='darkgray',size=1)+
    geom_line(aes(time,(rep-rep[1])*5+150),color='red',size=1)+
    geom_line(aes(time,(lep-lep[1])*5+150),color='blue',size=1)+
    geom_line(aes(time,(((repV+lepV)/2)-((repV+lepV)/2)[1])*5+150-15),color='violet',size=1)+
    geom_line(aes(time,verg.velocity*2+100),color='blue',alpha=1)+
    geom_line(aes(time,predV*2+100),color='orange')+
    geom_point(aes(time,showrasters+5),shape='|',color='lightgrey',size=3)+
    geom_text(aes(x=time[1]+100,label=paste('Saccadic Verg Change: ',round(verg.amp,3))),y=175)+
    geom_text(aes(x=time[1]+350,label=paste('Total verg change: ',round(tvc,3))),y=175)+
    geom_text(aes(x=time[1]+350,label=paste('Difference: ',round(tvc-verg.amp,3))),y=165)+
    # annotate('segment',x=window,xend=window,y=0,yend=50)+
    # annotate('text',x=window,y=45,label='100 spk/s')+
    # annotate('segment',x=window,xend=window,y=100,yend=120)+
    # annotate('text',x=window,y=114,label='10 deg/s')+
    # annotate('segment',x=window,xend=window,y=150,yend=175)+
    # annotate('text',x=window,y=170,label='5 deg')+
    # annotate('segment',x=window,xend=window+100,y=75,yend=75)+
    # annotate('text',x=window,y=75,label='100 ms')+
    theme_minimal()+
    ylab('')
  
},
currentsac=slider(1,length(goodsacs),step=1,initial = 1))

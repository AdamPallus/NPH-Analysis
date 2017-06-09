#THis figure will show the two models used to predict firing rate.
#First, I'll show the simple model massively overestimates the firing rate during saccades
#Then I'll show that a more complex model improves the fit (will I?) 
#And demonstrate that the terms are significant, using bootstrapping 
#Next demonstrate that the terms are significantly different from each other
#And finally, show that sensitivity is reduced during saccades

modelVV<- function(t,chosenCell='Bee-211',saccadebuffer=200,saccadethreshold=30,
                    model.form='sdf20~+verg.angle+verg.velocity', #simple model
                    lagsdf=31,returnmodel=FALSE,
                   includeSaccades=FALSE){
  
  x<- ungroup(filter(t,neuron==chosenCell))
  if (!('time' %in% names(x))){
    x<- mutate(x,time=row_number())
  }
  parabolic_n<- 10
  x %>%
    mutate(verg.velocity=parabolicdiff(lep-rep,parabolic_n),
           rev=parabolicdiff(rep,parabolic_n),
           lev=parabolicdiff(lep,parabolic_n),
           revV=parabolicdiff(repV,parabolic_n),
           levV=parabolicdiff(lepV,parabolic_n),
           sdf=spikedensity(rasters,sd=10),
           sdf20=dplyr::lag(sdf,lagsdf),
           conj.velocity=sqrt(((rev+lev)/2)^2+((revV+levV)/2)^2)) ->
    x
  
  x<- joinsaccades(x,buffer=saccadebuffer,threshold=saccadethreshold)
  x %>%
    group_by(sacnum) %>%
    mutate(verg.amp=last(verg.angle)-first(verg.angle),
           isconj=verg.amp<1,
           # saccadic=counter>saccadebuffer & counter<n()-saccadebuffer+20,
           saccadic=counter>saccadebuffer & counter<n()-saccadebuffer,
           enhance.type=as.character(saccadic),
           saccadic=replace(saccadic,!saccadic,NA),
           conj.h.vel=(lev+rev)/2,
           conj.v.vel=(levV+revV)/2,
           instant.dir=atan2(conj.v.vel,conj.h.vel)*180/pi
           # target.verg=thp-thp2,
           # verg.error=verg.angle-target.verg
    )->
    x
  
  x %>%
    mutate(enhance.type=replace(enhance.type,enhance.type=='FALSE','slow'),
           enhance.type=replace(enhance.type,is.na(enhance.type),'slow'),
           # enhance.type=replace(enhance.type,enhance.type=='slow'&abs(verg.velocity)<2.5,'fix'),
           enhance.type=replace(enhance.type,enhance.type=='slow'&verg.velocity>0,'slowC'),
           enhance.type=replace(enhance.type,enhance.type=='slow'&verg.velocity<0,'slowD'),
           enhance.type=replace(enhance.type,enhance.type=='TRUE' & verg.velocity>0,'conver'),
           enhance.type=replace(enhance.type,enhance.type=='TRUE' & verg.velocity<0,'diver'),
           enhance.type=as.factor(enhance.type))->
    x
  
  xm<- group_by(x,time) %>% summarize_each(funs(first))
  # x<- mutate(x,saccadic=!is.na(sacnum))
  if (includeSaccades){
    mod<- lm(model.form,data=xm) 
  }else{
    mod<- lm(model.form,data=filter(xm,is.na(saccadic),verg.velocity>0))
  }
  # mod<- lm(model.form,data=filter(x,!saccadic))
  # mod<- lm(model.form,data=filter(x,verg.velocity>0,!saccadic))
  
  # mod<-lm(model.form,data=xm)
  # mod<- lm(model.form,data=filter(x,!isconj))
  if (returnmodel) return(mod)
  print(tidy(mod))
  # mod2<- lm('verg.velocity~sdf20+verg.angle',data=filter(x,enhance.type=='none',sdf20>30))
  message(paste('R-squared =', summary(mod)$r.squared))
  # x<- dplyr::select(x,-sacnum,-counter)
  x<- mutate(ungroup(x),predSDF=predict(mod,newdata=x),
             showrasters=replace(rasters,rasters<1,NA))
  
}

mod<-modelVV(t,chosenCell='Bee-211',lagsdf=31,
                           model.form='sdf20~+verg.angle+verg.velocity',
                           returnmodel = TRUE)
bufferlength=200

modelVV(t,chosenCell='Bee-211',lagsdf=31,
                model.form='sdf20~+verg.angle+verg.velocity',
                saccadebuffer = bufferlength) %>%
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
         peakFR=max(sdf),
         predict.peakFR=max(predSDF))->
  z

zp<- summarize_each(z,funs(first))
zp<-filter(zp,r.amp>3,abs(total.verg.amp)<30)
qplot(peakFR,predict.peakFR,data=zp)+
  geom_abline()+
  stat_smooth(method='lm')

qplot(total.verg.amp,peakFR,data=zp)+
  geom_point(aes(total.verg.amp,predict.peakFR),color='orange')


qplot(saccadic.verg.amp,peakFR,data=zp)+
  geom_point(aes(saccadic.verg.amp,predict.peakFR),color='orange')

#ComplexModel----
modelVV(t,chosenCell='Bee-211',lagsdf=31,
        model.form='sdf20~+verg.angle+verg.velocity:enhance.type',
        saccadebuffer = bufferlength,
        includeSaccades=TRUE) %>%
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
         peakFR=max(sdf),
         predict.peakFR=max(predSDF))->
  z
zp<- summarize_each(z,funs(first))
zp<-filter(zp,r.amp>3,abs(total.verg.amp)<30)
qplot(peakFR,predict.peakFR,data=zp)+
  geom_abline()+
  stat_smooth(method='lm')

qplot(total.verg.amp,peakFR,data=zp)+
  geom_point(aes(total.verg.amp,predict.peakFR),color='orange')


qplot(saccadic.verg.amp,peakFR,data=zp)+
  geom_point(aes(saccadic.verg.amp,predict.peakFR),color='orange')
#----
goodsacs=unique(z$sacnum[z$r.amp>2&z$verg.amp>2])
manipulate({
  d2<- filter(z,sacnum==goodsacs[chosenSac])  
  verg.start=max(d2$counter[d2$counter<200&abs(d2$verg.velocity)<3])
  verg.end=min(d2$counter[d2$counter>200+first(d2$saccade.dur)&abs(d2$verg.velocity)<3])
  ggplot(d2)+
    geom_point(aes(counter,showrasters+30),shape='|',size=3)+
    geom_line(aes(counter,verg.velocity),color='darkblue',size=2)+
    # geom_line(aes(counter,conj.velocity),color='magenta',size=2)+
    geom_vline(xintercept=c(200,200+first(d2$saccade.dur)),linetype=3,size=2)+
    geom_vline(xintercept = c(verg.start,verg.end),linetype=3,size=2,color='darkblue')+
    # geom_vline(xintercept = c(41,213),linetype=3,size=2)+
    geom_area(aes(counter,predSDF),fill='orange',alpha=.2)+
    geom_area(aes(counter,sdf),alpha=0.2)+
    geom_hline(yintercept = c(-3,3))+
    # ylim(c(NA,150))+
    xlab('Time (ms)')+
    ylab('Velocity (deg/s)')+
    # annotate('text',x=100,y=30,label=paste('real.amp=',round(real.amp,2)))+
    # annotate('text',x=100,y=25,label=paste('predict.amp=',round(predict.amp,2)))+
    # annotate('text',x=100,y=20,label=paste('sacnum=',d2$sacnum[1]))+
    geom_line(aes(counter,verg.angle*10),color='darkgreen')#+
    # geom_line(aes(counter,target.verg*10))
},
chosenSac=slider(1,length(goodsacs),initial=1))

#BatchRecalculate----
t %>%
  group_by(neuron) %>%
  do(tidy(modelVV(t,chosenCell=first(.$neuron),lagsdf=20,
                  model.form='sdf20~+verg.angle+verg.velocity:enhance.type',
                  saccadebuffer = bufferlength,
                  includeSaccades=TRUE,returnmodel = TRUE))) %>%
  select(neuron,term,estimate) %>%
  mutate(term=replace(term,term=='(Intercept)','b'))->
  complex

complex %>%
  mutate(term=replace(term,term=='verg.velocity:enhance.typeconver','conver'),
         term=replace(term,term=='verg.velocity:enhance.typediver','diver'),
         term=replace(term,term=='verg.velocity:enhance.typeslowC','slowC'),
         term=replace(term,term=='verg.velocity:enhance.typeslowD','slowD'))->
  complex

cx<- spread(complex,term,estimate)

#compare slow convergence to enhanced convergence
qplot(conver,slowC,data=cx)+geom_abline()+geom_vline(xintercept = 0)

qplot(conver,slowC,data=cx)+geom_abline()+geom_vline(xintercept = 0)+
  geom_point(aes(conver,slowC),color='red',size=2,data=filter(cx,neuron=='Bee-211'))




##uncomment if you need the model for bootstrapping or whatever
# t %>%
#   group_by(neuron) %>%
#   do(m=modelVV(t,chosenCell=first(.$neuron),lagsdf=20,
#                model.form='sdf20~+verg.angle+verg.velocity',
#                saccadebuffer = bufferlength,
#                includeSaccades=FALSE,returnmodel = TRUE))->
#   simple

t %>%
  group_by(neuron) %>%
  do(tidy(modelVV(t,chosenCell=first(.$neuron),lagsdf=20,
               model.form='sdf20~+verg.angle+verg.velocity',
               saccadebuffer = bufferlength,
               includeSaccades=FALSE,returnmodel = TRUE))) %>%
  select(neuron,term,estimate) %>%
  mutate(term=replace(term,term=='(Intercept)','b'))->
  simpletidy




ModelEnhancement<- function(z,d=NULL){
  # CalculateEnhancement<- function(t,chosenCell='Bee-211'){
  #I originally wrote this function to spit out the parameters for the regression
  #comparing vergence amplitude to vergence enhnacement, defined as the difference between
  #observed peak vergence velocity and predicted peak vergence velocity based on firing rate
  
  #this function can easily be modified for plotting. See "EnhancementApril2017.R"
  # z<- filter(t,neuron==chosenCell)
  bufferlength=200
  saccade.length=150
  z%>%
    # group_by(neuron) %>%
    mutate(#sdf20=lag(sdf,d), #d comes from dynamiclead2 function
      lev=parabolicdiff(lep,20),
      rev=parabolicdiff(rep,20),
      levV=parabolicdiff(lepV,20),
      revV=parabolicdiff(repV,20),
      sdf=spikedensity(rasters,20),
      verg.velocity=lev-rev,
      conj.velocity=sqrt(((rev+lev)/2)^2+((revV+levV)/2)^2))->
    z
  
  if (is.null(d)){
    # message('calculating dynamic lead...')
    d<- dynamiclead2(p=z,formula = 'verg.velocity~sdflag+verg.angle',lags=seq(0,40,by=2))
  }
  
  z %>%
    mutate(sdf20=lag(sdf,d)) %>%
    ungroup() %>%
    mutate(time=row_number()) %>%
    do(joinsaccadesuniform(.,buffer=bufferlength,threshold=20,saccade.length=saccade.length))%>%
    group_by(sacnum) %>%
    mutate(realsaccade=counter>bufferlength & counter< bufferlength + saccade.dur) %>%
    ungroup()->
    z
  
  
  mod<- lm('verg.velocity~sdf20+verg.angle',data=filter(z,!realsaccade,verg.velocity>0))
  
  z<-mutate(z, predV=predict(mod,newdata=z))
  
  z %>%
    # group_by(neuron) %>%
    mutate(issaccade=!is.na(sacnum)) %>%
    filter(issaccade,saccade.dur>20) %>%
    group_by(neuron,sacnum) %>%
    mutate(cverg=abs(verg.velocity)>3,
           cverg=replace(counter,cverg,NA)) %>%
    mutate(peakFR=max(sdf20),
           saccade.dur=first(saccade.dur), #was originally a summary
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
           r.angle=atan2(R.V.Amp,R.H.Amp)*180/pi,
           r.amp=sqrt(R.H.Amp^2+R.V.Amp^2),
           vect.amp= (sqrt(R.H.Amp^2+R.V.Amp^2)+sqrt(L.H.Amp^2+L.V.Amp^2))/2,
           maxamp=max(abs(R.H.Amp),abs(R.V.Amp),abs(L.H.Amp),abs(L.V.Amp)),
           verg.amp=verg.angle[saccade.end]-verg.angle[bufferlength],
           mean.verg.amp=mean(verg.angle[saccade.end:n()]-mean(verg.angle[1:bufferlength])),
           peak.verg.velocity= maxabs(verg.velocity),
           min.verg.trans = min(verg.velocity[bufferlength:saccade.end]),
           max.verg.trans = max(verg.velocity[bufferlength:saccade.end]),
           off.verg.velocity=min(abs(min.verg.trans),abs(max.verg.trans)),
           min.verg.angle=min(verg.angle),
           max.verg.angle=max(verg.angle),
           max.verg.velocity=max(verg.velocity),
           min.verg.velocity=min(verg.velocity),
           initial.verg.angle=verg.angle[bufferlength],
           verg.lead=bufferlength-max(cverg[100:bufferlength],na.rm=T),
           verg.lead=replace(verg.lead,is.na(verg.lead),0),
           predict.ratio=sum(predV)/sum(verg.velocity))->
    z
  z %>%
    group_by(sacnum) %>%
    mutate(predict.verg.change=cumsum(predV)/1000+first(verg.angle),
           max.predict.velocity=max(predV,na.rm=T),
           total.verg.amp=last(verg.angle)-first(verg.angle),
           total.predict.amp=last(predict.verg.change)-first(predict.verg.change))->
    z
}

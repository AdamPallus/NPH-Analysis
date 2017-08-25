preparetoBOOTmarksaccs<- function(z){
  
  z%>% #this block smooths out velocity traces
    mutate(#lev=parabolicdiff(lep,20),
           #rev=parabolicdiff(rep,20),
           #levV=parabolicdiff(lepV,20),
           #revV=parabolicdiff(repV,20),
           sdf=spikedensity(rasters,10),
           #verg.velocity=lev-rev,
           conj.velocity=sqrt(((rev+lev)/2)^2+((revV+levV)/2)^2))->
    z
  
  bufferlength=200
  saccade.length=150
  d=20
  
  z %>% #this is just one neuron's worth of data
    mutate(sdf20=lag(sdf,d),
           time=row_number()) %>%
    mutate(realsaccade=markSaccades(conj.velocity,buffer=15,threshold=20)>0)%>%
    do(joinsaccadesuniform(.,buffer=bufferlength,threshold=20,saccade.length=saccade.length))%>%
    group_by(sacnum) %>%
    # mutate(realsaccade=counter>bufferlength & counter< bufferlength + saccade.dur) %>%
    ungroup()->
    z
  
  z %>%
    mutate(issaccade=!is.na(sacnum)) %>%
    filter(issaccade,saccade.dur>20) %>%
    group_by(sacnum) %>%
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
           min.verg.trans = min(verg.velocity),
           max.verg.trans = max(verg.velocity),
           off.verg.velocity=min(abs(min.verg.trans),abs(max.verg.trans)),
           min.verg.angle=min(verg.angle),
           max.verg.angle=max(verg.angle),
           max.verg.velocity=max(verg.velocity),
           min.verg.velocity=min(verg.velocity),
           initial.verg.angle=verg.angle[bufferlength],
           verg.lead=bufferlength-max(cverg[100:bufferlength],na.rm=T),
           verg.lead=replace(verg.lead,is.na(verg.lead),0),
           total.verg.amp=last(verg.angle)-first(verg.angle))->
    z
  z<- mutate(z,verg.bins=cut(verg.amp,c(-50,-1,1,50)))
  
  levels(z$verg.bins)<- c('Diverging','Conjugate','Converging')
  
  z %>%
    group_by(sacnum) %>%
    mutate(enhance.type='slowC',
           enhance.type=replace(enhance.type,!realsaccade&verg.velocity<0,'slowD'),
           enhance.type=replace(enhance.type,realsaccade & verg.velocity>0, 'converging'),
           enhance.type=replace(enhance.type,realsaccade & verg.velocity<0, 'diverging'))->
    z
}
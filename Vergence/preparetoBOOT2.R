preparetoBOOT2<- function(z){
  saccadethreshold=30
  saccadebuffer=200
  bufferlength=saccadebuffer
  parabolic_n=10
  z %>%
    mutate(
      rev=parabolicdiff(rep,parabolic_n),
      lev=parabolicdiff(lep,parabolic_n),
      revV=parabolicdiff(repV,parabolic_n),
      levV=parabolicdiff(lepV,parabolic_n),
      verg.velocity=lev-rev,
      sdf=spikedensity(rasters,sd=10),
      sdf20=dplyr::lag(sdf,20),
      conj.velocity=sqrt(((rev+lev)/2)^2+((revV+levV)/2)^2)) ->
    z
  
  z<- joinsaccades(z,buffer=saccadebuffer,threshold=saccadethreshold)
  z %>%
    group_by(sacnum) %>%
    mutate(verg.amp=last(verg.angle)-first(verg.angle),
           isconj=verg.amp<1,
           # saccadic=counter>saccadebuffer & counter<n()-saccadebuffer+20,
           saccadic=counter>saccadebuffer & counter<n()-saccadebuffer,
           enhance.type=as.character(saccadic),
           saccadic=replace(saccadic,!saccadic,NA),
           conj.h.vel=(lev+rev)/2,
           conj.v.vel=(levV+revV)/2
           # instant.dir=atan2(conj.v.vel,conj.h.vel)*180/pi
           # target.verg=thp-thp2,
           # verg.error=verg.angle-target.verg
    )->
    z
  
  z %>%
    mutate(enhance.type=replace(enhance.type,enhance.type=='FALSE','slow'),
           enhance.type=replace(enhance.type,is.na(enhance.type),'slow'),
           # enhance.type=replace(enhance.type,enhance.type=='slow'&abs(verg.velocity)<2.5,'fix'),
           enhance.type=replace(enhance.type,enhance.type=='slow'&verg.velocity>0,'slowC'),
           enhance.type=replace(enhance.type,enhance.type=='slow'&verg.velocity<0,'slowD'),
           enhance.type=replace(enhance.type,enhance.type=='TRUE' & verg.velocity>0,'conver'),
           enhance.type=replace(enhance.type,enhance.type=='TRUE' & verg.velocity<0,'diver'),
           enhance.type=as.factor(enhance.type))->
    z
  
  z %>% 
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
           # mean.verg.amp=mean(verg.angle[saccade.end:n()]-mean(verg.angle[1:bufferlength])),
           peak.verg.velocity= maxabs(verg.velocity),
           min.verg.trans = min(verg.velocity),
           max.verg.trans = max(verg.velocity),
           off.verg.velocity=min(abs(min.verg.trans),abs(max.verg.trans)),
           min.verg.angle=min(verg.angle),
           max.verg.angle=max(verg.angle),
           max.verg.velocity=max(verg.velocity),
           min.verg.velocity=min(verg.velocity),
           initial.verg.angle=verg.angle[bufferlength],
           peakFR=max(sdf)
    )->
    z
  z<- mutate(z,verg.bins=cut(verg.amp,c(-50,-1,1,50)))
  levels(z$verg.bins)<- c('Diverging','Conjugate','Converging')
  z
}
  
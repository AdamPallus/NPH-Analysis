calculateTransient<- function(z) {
  
  require(dplyr)
  source('joinsaccadesuniform.R')
  source('Adamhelperfunctions.R')
  source('markEnhancement.R')
  
  message('Marking Vergence Movements...')
  #mark enhancements 
  zz<-markEnhancement(v=z$verg.velocity,threshold2=12)
  zz<- dplyr::select(zz,time,enhancenum)
  
  z<- left_join(z,zz,by='time')
  z<-ungroup(z)
  
  #determine whether it's convergence or divergence
  z %>%
    mutate(verg.enhance=!is.na(enhancenum),
           transient.type='none',
           verg.direction=verg.velocity>0)->
    z
  i<- z$verg.enhance & !z$verg.direction
  z$transient.type[i]<- 'divergence'
  i<- z$verg.enhance & z$verg.direction
  z$transient.type[i]<- 'convergence'
  
  message('marking saccades...')
  #mark saccades and create buffer around each saccade for analysis
  bufferlength<- 200
  saccade.length<- 150
  z%>%
    # group_by(neuron) %>%
    mutate(time=row_number()) %>%
    do(joinsaccadesuniform(.,buffer=bufferlength,threshold=20,saccade.length=saccade.length))->
    # do(joinsaccades(.,buffer=bufferlength,threshold=20))->
    z
  
  message('measuring saccades...')
  #Once saccades have been marked, we measure lots of things about each saccade. 
  #For this plot, we also remove all data that isn't part of a saccade or the buffer period
  z %>%
    # group_by(neuron) %>%
    mutate(issaccade=!is.na(sacnum)) %>%
    filter(issaccade) %>%
    group_by(sacnum) %>%
    # filter(counter>0,counter<saccade.dur) %>%
    summarize(saccade.dur=first(saccade.dur),
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
              # verg.amp= last(verg.angle)-first(verg.angle),
              verg.amp= verg.angle[n()]-verg.angle[1],
              initial.verg.angle=verg.angle[bufferlength])->
    s
  
  s$saccade.dur<-NULL
  
  z<-left_join(z,ungroup(s),by='sacnum')
  z %>% 
    group_by(sacnum) %>% 
    mutate(counter=row_number()-bufferlength)-> 
    z
  
  #mark saccades as convergent, divergent or neither
  verg.thresh<- 1.5
  z$saccade.type<- 'saccade.only'
  z$saccade.type[z$mean.verg.amp < -verg.thresh]= 'diverging'
  z$saccade.type[z$mean.verg.amp > verg.thresh]= 'converging'
  z$saccade.type<-as.factor(z$saccade.type)
  
  
  # z %>% 
  #   group_by(sacnum) %>%
  #   mutate(isdivergence.trans=transient.type=='divergence',
  #          transient.onset=first(counter[isdivergence.trans]),
  #          counter2=counter-transient.onset) ->
  #   z
  
  #find all the saccades where there is a divergence transient, mark that as the onset
  z %>% 
    group_by(sacnum) %>%
    filter(transient.type=='divergence') %>%
    summarize(transient.onset=first(counter)) %>%
    filter(transient.onset> -25, transient.onset<100) ->
    d
  
  #find the saccades where there is only a convergent transient, mark that
  z%>%
    filter(!sacnum %in% d$sacnum) %>%
    group_by(sacnum) %>%
    filter('convergence' %in% unique(transient.type)) %>%
    summarize(conv.onset=head(counter[!is.na(transient.type)],1)) ->
    p
  
  %>%
    filter(conv.onset> -25, conv.onset<100) ->
    p
  
#add these measurements to the full datafile
  z<- left_join(z,d,by='sacnum')
  z<- left_join(z,p,by='sacnum')
  
  #combine divergent and converent onsets so that we mark either the start of the divergence
  #or the start of the convergence if there is no divergence. 
  z$transient.onset[!is.na(z$conv.onset)]=z$conv.onset[!is.na(z$conv.onset)]
  
  #make counter2 align all saccades by transient onset for future removal of the template
  z<- mutate(z,counter2=counter-transient.onset)
  
}

'This is a script to make a figure demonstrating npk,
vergence lead and predicting/removing transients.
First we will mark and measure saccades. Then we will apply the transient template
and use the model to calculate the expected transient'

b<- filter(t,neuron=='Bee-211')
trans.mod<- readRDS('TransientPredictedNPKmodel.RDS')
mean.waveforms<- readRDS('BeeTransientTemplate.RDS')



bufferlength=200

mean.waveforms %>%
  mutate(norm.trans.template=trans.template-trans.template[1],
         norm.trans.template=norm.trans.template/min(norm.trans.template)*-1,
         counter=row_number()) %>%
  select(counter,norm.trans.template) ->
  mean.waveforms



b %>%
  mutate(time=row_number()) %>%
  do(joinsaccadesuniform(.,buffer=bufferlength,threshold=20,saccade.length = 150)) ->
  bb

bb%>%
  mutate(cverg=abs(verg.velocity)>3,
         cverg=replace(counter,cverg,NA)) %>%
  group_by(sacnum) %>%
  mutate(saccade.end=saccade.dur+bufferlength,
         peak.conj.velocity=maxabs(conj.velocity),
         peak.R.H= maxabs(rev),
         peak.R.V= maxabs(revV),
         peak.L.H= maxabs(lev),
         peak.L.V= maxabs(levV),
         verg.amp=verg.angle[saccade.end]-verg.angle[bufferlength],
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
         # predicted.verg.amp=sum(predV)/1000,
         r.direction='upward',
         r.direction=replace(r.direction,r.angle<0,'downward'),
         r.direction=replace(r.direction,abs(r.angle)<45,'rightward'),
         r.direction=replace(r.direction,abs(r.angle)>135,'leftward'))->
         # verg.lead=bufferlength-max(cverg[100:bufferlength],na.rm=T),
         # verg.lead=replace(verg.lead,is.na(verg.lead),0))->
  bb


bb %>%
  group_by(sacnum)

goodsacs=unique(filter(bb,r.amp>3,verg.amp>2)$sacnum)

# goodsacs=unique(filter(bb,conj.H.Amp<1,conj.V.Amp>1,verg.amp>4)$sacnum)


nsac=length(goodsacs)
manipulate({
  bb.plot<-filter(bb,sacnum==goodsacs[sac])
  
  bb.plot$predicted.npk=predict(trans.mod,newdata=bb.plot)[1]
  shift.mean.waveforms<-mutate(mean.waveforms,counter=counter+200-3)
  bb.plot<-left_join(bb.plot,shift.mean.waveforms,by='counter')
  
  bb.plot<- mutate(bb.plot,predicted.transient=norm.trans.template*predicted.npk*-1)
  bb.plot<- mutate(bb.plot,verg.velocity=parabolicdiff(verg.angle,20))
  
  verg.lead<-bb.plot$verg.velocity<2
  verg.lead<- data.frame(counter=1:length(verg.lead),thresh=verg.lead)
  
  verg.lead=220-max(filter(verg.lead,thresh,counter<220)$counter)
  
    ggplot(bb.plot)+
    geom_line(aes(counter,verg.velocity))+
    geom_line(aes(counter,predicted.transient),color='pink')+
    geom_line(aes(counter,verg.velocity-predicted.transient),color='hotpink')+
    # geom_line(aes(counter,norm.trans.template*npk*-1),color='purple')+
    # geom_line(aes(counter,scaled.transient),color='grey')+
    # geom_line(aes(counter,verg.velocity-norm.trans.template*npk*-1),color='hotpink',linetype=2)+
    # geom_line(aes(counter,verg.velocity-norm.trans.template*npk*-1-predV),color='magenta',size=1)+
    # geom_line(aes(counter,notrans.verg.velocity),color='hotpink',linetype=2)+
    # geom_line(aes(counter,predV),color='orange',size=1)+
    # geom_point(aes(counter,showrasters+30),shape='|',size=3)+
    # geom_area(aes(counter,conj.velocity),alpha=0.2)+
    # geom_line(aes(counter,rep*10+100),color='red')+
    # geom_line(aes(counter,lep*10+100),color='blue')+
    # geom_line(aes(counter,(repV+repV)*5+100),color='purple')+
    # geom_line(aes(counter,rev),color='red')+
    # geom_line(aes(counter,lev),color='blue')+
    #   geom_line(aes(counter,lev-rev),color='magenta')+
    # geom_line(aes(counter,(revV+levV)/2),color='purple')+
    ylim(c(-85,175))+
    theme_minimal()+
      annotate('text',50,50,label=verg.lead)+
      geom_vline(xintercept = 220-verg.lead)+
      geom_hline(yintercept = 2)+
      geom_segment(x=100,xend=100,y=0,yend=50)
    # annotate('text',50,50,label='transient removed',color='hotpink')+
    # annotate('text',50,45,label='predicted transient',color='purple')+
    # annotate('text',50,40,label='actual vergence velocity')+
    # annotate('text',50,35,label='predicted vergence velocity',color='orange')+
    # annotate('text',50,30,label='predicted enhancement',color='magenta')+
    # annotate('text',200,140,label='conjugate velocity')+
    # annotate('text',150,100,label=paste(bb.plot$r.direction[1],'saccade',sep=' '))+
    # annotate('text',150,95,label=paste(round(bb.plot$r.angle[1]),'deg',sep=' '))+
    # annotate('text',100,-20,label=paste('sacnum=',goodsacs[sac],sep=''))
  # geom_label(x=150,y=100,aes(label=r.direction[1]))
},
sac=slider(1,nsac,step=1)

)

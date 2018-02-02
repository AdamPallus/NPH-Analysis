#Go to VergPreditionDoodle.r FIRST
t<- loadnewcsv2(path="C:/Users/setup/Desktop/NRTP Vergence/SOASTRAB/patos/")

#Functions----
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
  
  # mod<- lm(model.form,data=filter(xm,!saccadic,verg.velocity>0))
  mod<- lm(model.form,data=filter(x,!saccadic))
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

MakeFigure<-function(d,labelloc=288000,zoom=TRUE,ylimit=c(-30,30)){
  require(gridExtra)
  # d<-mutate(d,showsaccadic=replace(saccadic,!saccadic,NA))
  
  Hpositions<- ggplot(d)+
    geom_line(aes(time,(rep)),color='red',size=1)+
    geom_line(aes(time,(lep)),color='blue',size=1)+
    theme_minimal()+xlab('')+theme(axis.text.x = element_blank())+
    ylab('Horizontal\nEye Positon (deg)')
  
  Vpositions<- ggplot(d)+
    geom_line(aes(time,(((repV+lepV)/2))),color='violet',size=1)+
    theme_minimal()+xlab('')+theme(axis.text.x = element_blank())+
    ylab('Vertical\nEye Position (deg)')
  
  VergPositions<- ggplot(d)+
    geom_line(aes(time,lep-rep),color='darkgreen',size=1)+
    theme_minimal()+xlab('')+theme(axis.text.x = element_blank())+
    ylab('Vergence Position\n(deg)')
  
  VergVelocities<-ggplot(d)+theme_minimal()+
    geom_line(aes(time,verg.velocity),color='blue',alpha=1)+
    geom_line(aes(time,predV),color='orange')+
    # geom_point(aes(time,as.numeric(showsaccadic)),alpha=0.2)+
    # ylim(c(-10,30))+
    xlab('')+theme(axis.text.x = element_blank())+
    ylab('Vergence Velocity\n(deg/s)')
    # annotate('text',x=labelloc,y=-5,label='Predicted',color='orange')
  
  if (zoom){
    # VergVelocities<-VergVelocities+ylim(c(-10,30))
    VergVelocities<-VergVelocities+ylim(ylimit)
  }
  
  sdf<- ggplot(d)+theme_minimal()+
    geom_area(aes(time,sdf),alpha=1)+
    geom_point(aes(time,showrasters+5),shape='|',color='lightgrey',size=3)+
    ylab('Firing Rate\n(spk/s)')
  xlab('Time (ms)')
  
  # multiplot(Hpositions,Vpositions,VergPositions,VergVelocities,sdf)
  # p<- list(Hpositions,Vpositions,VergPositions,VergVelocities,sdf)
  grid.arrange(arrangeGrob(Hpositions,Vpositions,VergPositions,VergVelocities,sdf,ncol=1,
                           heights=c(.5,.3,.5,1,.5)),padding=0.2)
  
  # grid.arrange(arrangeGrob(Hpositions,Vpositions,VergPositions,VergVelocities,VergVelocities+ylim(c(-10,30)),sdf,ncol=1,
  #                          heights=c(.5,.3,.5,0.5,1,.5)),padding=0.2)
}

#Plot----

window_size=10000
manipulate({
  d=filter(z,time>=window,time<window+window_size)
  d<- group_by(d,time) %>% summarize_all(funs(first))
  d<- mutate(d,saccadic=replace(saccadic,!saccadic,NA))
  MakeFigure(d,window)
},
window=slider(window_size,max(z$time-window_size),step=window_size))


#patos-105----

t%>%
  filter(neuron=='Patos-105') %>%
  mutate(blinks=markSaccades(lep,buffer=120,threshold=60),
         conj.vertical=(repV+lepV)/2) %>%
  filter(blinks<0) ->
  ttest
z<-modelVV2(ttest,chosenCell='Patos-105',lagsdf=20,
            model.form='verg.velocity~sdf20+verg.angle',
            # model.form='sdf20~verg.velocity+verg.angle',
            saccadebuffer=bufferlength)


#Patos-103----
#BLINKS!!

t <- mutate(t,conj.vertical=(repV+lepV)/2)
  

t%>%
  filter(neuron=='Patos-103') %>%
  mutate(blinks=markSaccades(lep,buffer=120,threshold=60),
         conj.vertical=(repV+lepV)/2) %>%
  filter(blinks<0) ->
  ttest

z<-modelVV2(t,chosenCell='Patos-103',lagsdf=20,
         model.form='verg.velocity~sdf20+verg.angle+conj.vertical',
         # model.form='sdf20~verg.velocity+verg.angle',
         saccadebuffer=bufferlength)
          
           

#Patos-102----
z %>%
  filter(time>=85000,
         time<87500) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(85000)

z %>%
  filter(time>=600000,
         time<610000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(600000)

z %>%
  filter(time>=600000,
         time<610000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(600000,zoom=FALSE)


MakeFigure(z) #just run the model on 600000-610000
MakeFigure(z,zoom=FALSE)

#patosspecificthing:
ttest<- filter(t,time>601250,time<608750)

smallMod<-modelVV2(ttest,chosenCell='Patos-102',lagsdf=20,
                   model.form='verg.velocity~sdf20+verg.angle+conj.vertical',
                   # model.form='verg.velocity~sdf20',
                   # model.form='sdf20~verg.velocity+verg.angle',
                   saccadebuffer=bufferlength,
                   returnmodel = TRUE) 

modelVV2(t,chosenCell='Patos-102',lagsdf=20,
         model.form='verg.velocity~sdf20+verg.angle',
         # model.form='sdf20~verg.velocity+verg.angle',
         saccadebuffer=bufferlength) %>%
  mutate(predV=predict(smallMod,newdata=.))->
  z

#Bee-33----

z %>%
  filter(time>=109700,
         time<112500) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(112000)

z %>%
  filter(time>=232000,
         time<236000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(233000,FALSE)

#Bee-15----
z %>%
  filter(time>=117500,
         time<120000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(119000)

z %>%
  filter(time>=171000,
         time<173000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(172000)

z %>%
  filter(time>=203000,
         time<205000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(204000)



#Bee-25 ----
z %>%
  filter(time>=287000,
         time<289000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(288000)

z %>%
  filter(time>=271000,
         time<273000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(271000)


z %>%
  filter(time>=294500,
         time<296000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(295000)




#Bee-215----
z %>%
  filter(time>=35000,
         time<37999) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(36000)

z %>%
  filter(time>=93000,
         time<96000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(93000)

z %>%
  filter(time>=682000,
         time<684000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(683000)

z %>%
  filter(time>=747500,
         time<750000) %>%
  group_by(time) %>%
  summarize_all(funs(first)) %>%
  MakeFigure(748000)

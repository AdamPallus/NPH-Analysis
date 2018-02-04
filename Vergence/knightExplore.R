hh %>%
  group_by(sacnum) %>%
  summarize(start_time=first(time))->
  stimes

hh %>% group_by(sacnum) %>%
  mutate(reject=as.factor(any(is.na(G[200:500]))))->
  hh

levels(hh$reject)<- c('red','black')


manipulate(
  ggplot(filter(hh,sacnum==chosenMovement))+
    geom_line(aes(counter,H),color='blue')+
    # geom_line(aes(counter,Ef),color='red')+
    geom_line(aes(counter,G),color='darkgreen')+
    geom_line(aes(counter,T,color=reject),size=2,alpha=0.5)+
    scale_color_manual(values=c('black','red'))+
    geom_line(aes(counter,Hv/10),color='blue',linetype=2)+
    # geom_line(aes(counter,EV/10),color='red')+
    geom_line(aes(counter,Gv/10),color='darkgreen',linetype=2)+
    # geom_text(x=0,y=50,aes(label=round(sd(Gv),2)))+
    geom_point(aes(counter,sign(gazeshifts)*50))+
    geom_point(aes(counter,sign(headmovement)*25),color='blue')+
    annotate('text',x=-100,y=50,label='Target Position')+
    annotate('text',x=500,y=20,label='Head Position')+
    annotate('text',x=320,y=5,label='Head Velocity')+
    annotate('text',x=270,y=40,label='Gaze Velocity')+
    annotate('text',x=250,y=70,label='Gaze Position')+
    annotate('text',x=125,y=55,label='First Gaze Shift')+
    annotate('text',x=300,y=28,label='Head Movement')+
    geom_line(aes(counter,sign(gazeshifts)*Gv/10),size=2,color='darkgreen',alpha=0.5)+
    geom_line(aes(counter,sign(headmovement)*Hv/10),color='blue',size=2,alpha=0.5),
  chosenMovement=slider(1,length(unique(hh$sacnum)),step=1))


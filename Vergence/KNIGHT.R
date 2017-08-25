#FIRST PASS----
h<- read.csv('HEADFREE.txt',sep="\t",header=FALSE)

h<- select(h,-V9)
names(h)<- c('G','GV','H','HV','E','EV','time','T')




h<-mutate(h,counter=row_number())

h<- mutate(h,
           Gf=replace(G,G==0,NA),
           Ef=replace(E,E==0,NA),
           Es=Gf-H,
           target.velocity=parabolicdiff(T,5),
           s=markSaccades(target.velocity,buffer=500,threshold=40))


window_size=5000

manipulate(ggplot(filter(h,counter>window,counter<window+window_size))+
  geom_line(aes(counter,H),color='blue')+
  geom_line(aes(counter,Ef),color='red')+
  geom_line(aes(counter,Gf),color='green')+
    geom_line(aes(counter,T),size=2,alpha=0.5),
  window=slider(window_size,max(h$counter-window_size),step=4000))

chosensac=1
manipulate(ggplot(filter(h,s==chosensac))+
             geom_line(aes(counter,H),color='blue')+
             geom_line(aes(counter,Ef),color='red')+
             geom_line(aes(counter,Gf),color='green')+
             geom_line(aes(counter,T),size=2,alpha=0.5),
           chosensac=slider(1,max(h$s)))


markTagetMovements<-function(t,buffer=200,threshold=1000,saccade.length=1000){
  
  findSaccades<-function(ev,threshold=40){
    mindur<- 1
    i<-which(abs(ev)>threshold) #find all the times when speed > threshold
    sacoff<-which(diff(i)>mindur) #minimum duration of an accepted saccade
    sacon<-c(1,sacoff+1) #first saccade
    sacoff<-c(sacoff,length(i)) #end of last saccade
    saccade.onset<-i[sacon] #get actual times
    saccade.offset<-i[sacoff] 
    
    return(data.frame(saccade.onset,saccade.offset))
  }
  
  jsac<- function(stimes){
    summary(stimes)
    #input should be an array of length 2: c(onsettime,offsettime, saccade.number,saccade.dur)
    df<- data.frame(time=stimes[[1]]:stimes[[2]])
    df$sacnum<- stimes[[4]]
    df$saccade.dur<- stimes[[3]]
    return(df)
    # return(stimes[[1]]:stimes[[2]])
  }
  
  stimes<-findSaccades(t$target.velocity,threshold)
  stimes %>%
    mutate(dur=saccade.offset-saccade.onset,
           s=row_number(),
           saccade.onset=saccade.onset-buffer,
           ###########HERE IS WHERE i MAKE SACCADES UNIFORM #######
           saccade.offset=saccade.onset+saccade.length+2*buffer)->
    stimes
  
  x<- as.data.frame(rbindlist(apply(stimes,1,jsac)))
  x %>%
    group_by(sacnum) %>%
    mutate(counter=time-first(time)) ->
    x
  left_join(t,x,by='time')
}

hh<- markTagetMovements(h,buffer=200,threshold=1000,saccade.length=500)

chosensac=1

#POSITION
manipulate(ggplot(filter(hh,sacnum==chosensac))+
             geom_line(aes(counter,H),color='blue')+
             geom_line(aes(counter,Ef),color='red')+
             geom_line(aes(counter,Gf),color='green')+
             geom_line(aes(counter,T),size=2,alpha=0.5),
           chosensac=slider(1,max(hh$sacnum,na.rm=T)))
#VELOCITY
manipulate(ggplot(filter(hh,sacnum==chosensac))+
             geom_line(aes(counter,HV),color='blue')+
             geom_line(aes(counter,EV),color='red')+
             geom_line(aes(counter,GV),color='green')+
             geom_line(aes(counter,T),size=2,alpha=0.5)+
             ylim(-400,400)
           ,
           chosensac=slider(1,max(hh$sacnum,na.rm=T)))

#BOTH
manipulate(ggplot(filter(hh,sacnum==chosensac))+
             geom_line(aes(counter,H),color='blue')+
             geom_line(aes(counter,Ef),color='red')+
             geom_line(aes(counter,Gf),color='green')+
             geom_line(aes(counter,T),size=2,alpha=0.5)+
             geom_line(aes(counter,HV/10),color='blue')+
             geom_line(aes(counter,EV/10),color='red')+
             geom_line(aes(counter,GV/10),color='green')
           ,
           chosensac=slider(1,max(hh$sacnum,na.rm=T)))

#RESTART----
h<- read.csv('filteredKnight.csv')

h %>% 
  select(Gf,Hf,T) %>%
  mutate(Gv=parabolicdiff(Gf,7),
         Hv=parabolicdiff(Hf,7),
         target.velocity=parabolicdiff(T,7),
         time=row_number(),
         gazeshifts=markSaccades(Gv,buffer=5,threshold=30),
         gazeshifts=replace(gazeshifts,gazeshifts<0,0)) %>%
  markTagetMovements(buffer=200,threshold=1000,saccade.length=500) %>%
  group_by(sacnum) %>%
  mutate(firstshift=min(gazeshifts[counter>200&gazeshifts>0],na.rm=T),
    gazeshifts=replace(gazeshifts,
                            gazeshifts!=firstshift,
                       0))->
  hh

manipulate(ggplot(filter(hh,sacnum==chosensac))+
             geom_line(aes(counter,Hf),color='blue')+
             # geom_line(aes(counter,Ef),color='red')+
             geom_line(aes(counter,Gf),color='darkgreen')+
             geom_line(aes(counter,T),size=2,alpha=0.5)+
             geom_line(aes(counter,Hv/10),color='blue',linetype=2)+
             # geom_line(aes(counter,EV/10),color='red')+
             geom_line(aes(counter,Gv/10),color='darkgreen',linetype=2)+
             # geom_text(x=0,y=50,aes(label=round(sd(Gv),2)))+
             geom_text(x=0,y=60,aes(label=firstshift[1]))+
             geom_point(aes(counter,sign(gazeshifts)*50))
           ,
           chosensac=slider(1,max(hh$sacnum,na.rm=T)))

hh %>% 
  group_by(sacnum) %>%
  summarize(gvsd=round(sd(Gv[200:500])))->
  hs

goodsacs=unique(filter(hs,gvsd<500)$sacnum)
manipulate(ggplot(filter(hh,sacnum==goodsacs[chosensac]))+
             geom_line(aes(counter,Hf),color='blue')+
             # geom_line(aes(counter,Ef),color='red')+
             geom_line(aes(counter,Gf),color='darkgreen')+
             geom_line(aes(counter,T),size=2,alpha=0.5)+
             geom_line(aes(counter,Hv/10),color='blue',linetype=2)+
             # geom_line(aes(counter,EV/10),color='red')+
             geom_line(aes(counter,Gv/10),color='darkgreen',linetype=2)+
             geom_point(aes(counter,sign(gazeshifts)*50))
           ,
           chosensac=slider(1,length(goodsacs)))

hh %>% 
  group_by(sacnum) %>%
  mutate(IEP=Gf[100]-Hf[100],
         IHP=Hf[100],
         IGP=Gf[100],
         )

ggplot(x)+
  geom_line(aes(counter,Hf),color='blue')+
  # geom_line(aes(counter,Ef),color='red')+
  geom_line(aes(counter,Gf),color='darkgreen')+
  geom_line(aes(counter,T),size=2,alpha=0.5)+
  geom_line(aes(counter,Hv/10),color='blue',linetype=2)+
  # geom_line(aes(counter,EV/10),color='red')+
  geom_line(aes(counter,Gv/10),color='darkgreen',linetype=2)+
  geom_point(aes(counter,gtest*0+50))


hh %>%
  group_by(sacnum) %>%
  mutate(firstshiftXX=min(gazeshifts[counter>200],na.rm=T))->
  hh


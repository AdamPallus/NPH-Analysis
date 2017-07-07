library(ggplot2)
library(dplyr)
library(knitr)
library(tidyr)
library(broom)
# library(grid)
library(relaimpo)
library(leaps)
library(stringr)
library(cladoRcpp)
library(boot)
source('Adamhelperfunctions.R')

s<- loadnewcsv(path="C:/Users/setup/Desktop/NRTP Vergence/INCstim/")

# s<- filter(t,neuron %in% c('Kopachuck-501','Kopachuck-502'))
bufferlength=200
s %>%
  group_by(neuron) %>%
  mutate(time=row_number(),
         sdf=spikedensity(rasters,sd=10), #rasers are stim pulses
         stimes=markSaccades(sdf,buffer=bufferlength,threshold=120))-> #use mark saccades to identify stim trains
  sx


# s<- mutate(s,time=row_number())
# s$stimes<- markSaccades(s$sdf,buffer=100,threshold=120)

sx %>%
  group_by(neuron) %>% #neuron = stim file
  filter(stimes>0) %>% #remove fixations
  mutate(showstim=replace(rasters,rasters<1,NA))%>%
  group_by(neuron,stimes) %>%
  mutate(counter=time-first(time),
         repDIFF=rep-rep[counter==bufferlength],
         lepDIFF=lep-lep[counter==bufferlength],
         repVDIFF=repV-repV[counter==bufferlength],
         lepVDIFF=lepV-lepV[counter==bufferlength])->
  sp


chosensite='Kopachuck-506'
spt<- filter(sp,neuron==chosensite)
# spt<- dplyr::select(spt,1:10)

#data wrangling to let us to facet_grid with horizontal and vertical separated 
#facet_grid(is.Vertical,stimes) where stimes = time of stimulation
spt %>%
  group_by(neuron,stimes) %>%
  gather("Component","Velocity",c(3,5,7,9))%>%
  gather("ComponentP","Position",2:5) %>%
  mutate(Component=as.factor(Component),
         is.Vertical=ComponentP %in% c('lepV','repV'),
         is.Vertical=as.factor(is.Vertical))->
  x
levels(x$is.Vertical)<- c("Horizontal",'Vertical')

# levels(x$Component)<- c("LH","LV","RH","RV")
# levels(x$ComponentV)<- c("LH","LV","RH","RV")

# ggplot(x)+
#   geom_line(aes(counter,Position,color=ComponentP))+
#   facet_grid(is.Vertical~stimes)+
#   geom_vline(xintercept = 200)

#cool trick: use export then preview in Rstudio so expand the graph without writing a file
ggplot(x)+
  geom_line(aes(counter,Position,color=ComponentP))+
  geom_point(aes(counter,showstim+10),shape='|',alpha=1/20)+
  facet_grid(stimes~is.Vertical,scales='free_y')+
  geom_vline(xintercept = 200)

#try to plot xy

spt<-mutate(spt, isstim=(counter>200 & counter<max(counter,na.rm=T)-200))

ggplot(spt)+
  geom_point(aes(rep,repV),color='red',alpha=1/20,data=filter(spt,!isstim))+
  geom_point(aes(lep,lepV),color='blue',alpha=1/20,data=filter(spt,!isstim))+
  geom_point(aes(rep,repV),color='red',alpha=1,data=filter(spt,isstim))+
  geom_point(aes(lep,lepV),color='blue',alpha=1,data=filter(spt,isstim))+
  geom_point(aes(lep,lepV),size=2,data=filter(spt,counter==1))+
  geom_point(aes(rep,repV),size=2,data=filter(spt,counter==1))+
  facet_wrap(~stimes,ncol=3,scales='free')+
  xlab('Horizontal Position (deg)')+
  ylab('Vertical Position (deg)')

ggsave('XYdemo5111.PDF',height=30,width=15)
            
# ggplot(spt)+
#   geom_line(aes(rep,repV),color='red',alpha=1/20,linetype=2,data=filter(spt,!isstim))+
#   geom_line(aes(lep,lepV),color='blue',alpha=1/20,linetype=2,data=filter(spt,!isstim))+
#   geom_line(aes(rep,repV),color='red',alpha=1,data=filter(spt,isstim))+
#   geom_line(aes(lep,lepV),color='blue',alpha=1,data=filter(spt,isstim))+
#   facet_wrap(~stimes,ncol=3,scales='free')

#----
#data saved Monday Night 4/3/2017
# spt<- readRDS('DataforXYplotsINCstim.RDS')

library(manipulate)
spt<-mutate(sp, isstim=(counter>200 & counter<max(counter,na.rm=T)-200))

stimlocs=unique(spt$neuron)

manipulate(ggplot(filter(spt,neuron==stimlocs[chosenSite]))+
  geom_point(aes(rep,repV),color='red',alpha=1/20,data=filter(spt,!isstim,neuron==stimlocs[chosenSite]))+
  geom_point(aes(lep,lepV),color='blue',alpha=1/20,data=filter(spt,!isstim,neuron==stimlocs[chosenSite]))+
  geom_point(aes(rep,repV),color='red',alpha=1,data=filter(spt,isstim,neuron==stimlocs[chosenSite]))+
  geom_point(aes(lep,lepV),color='blue',alpha=1,data=filter(spt,isstim,neuron==stimlocs[chosenSite]))+
  geom_point(aes(lep,lepV),size=2,data=filter(spt,counter==1,neuron==stimlocs[chosenSite]))+
  geom_point(aes(rep,repV),size=2,data=filter(spt,counter==1,neuron==stimlocs[chosenSite]))+
  facet_wrap(~stimes,ncol=3,scales='free')+
  xlab('Horizontal Position (deg)')+
  ylab('Vertical Position (deg)')+
  ggtitle(stimlocs[chosenSite])+
    coord_fixed()
  ,
  chosenSite=slider(1,length(stimlocs),step=1))



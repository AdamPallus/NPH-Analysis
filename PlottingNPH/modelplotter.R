
addmodelprediction<-function(t){
  
t <-  mutate(t,sdflag=lag(sdf,50),time=row_number())
m<-lm(sdflag~rep+lep,t)
mv<-lm(sdflag~rep+lep+repV+lepV,t)
t$posmodel<-predict(m,t)
t$pvmodel<-predict(mv,t)
t
}

library(dplyr)
library(broom)

path <- "~/GitHub/NPH-Analysis/practicedata/"
files <- list.files(path=path,pattern='*.csv')
tt<- data.frame()
for (i in 1:length(files)) {
  t <- read.csv(paste(path,files[i],sep=''))
  t$neuron<-gsub('.csv','',files[i])
  t<-addmodelprediction(t)
  tt<- rbind(tt,t)
}

library(ggplot2)

ggplot(filter(tt,time<50000))+
geom_area(aes(time,sdflag))+
geom_line(aes(time,pvmodel),color="green")+
geom_line(aes(time,posmodel),color="red")+facet_grid(neuron~.)

path<- "C:/Users/setup/Desktop/NRTP Vergence/"
f <- list.files(path=path,pattern='*.csv')

d<- read.csv(paste(path,f,sep=''))
buffer=20
d<-mutate(d,
          s=markSaccades((sqrt(rev^2)+sqrt(revV^2))/2,buffer=buffer,threshold=40),
          time=row_number(),
          verg.angle=rep-lep,
          verg.velocity=parabolicdiff(verg.angle,7),
          isfixation=s<0,
          s=abs(s))

# dd<-filter(d,s==29,isfixation==FALSE)
# qplot(time,rep,geom='line',data=dd)+geom_point(aes(time,rasters+5),data=filter(dd,rasters>0),shape='|',size=4)


spiketimes<- d$time[d$rasters==1]
isi<- spiketimes[-1]-spiketimes[-(length(spiketimes)-1)]
spiketimes<-spiketimes[-1]

p<- data.frame(spiketimes=spiketimes,isi=isi)
bp<- f.BPsummary(list(p),Pthresh=0.001)

bp$burst[[1]] %>%
  group_by(clusid) %>%
  summarise(start=min(start),end=max(end),
            duration=end-start,
            p=first(adjP),
            ymin=0,
            ymax=20) ->
  bursts

bp$pause[[1]] %>%
  group_by(clusid) %>%
  summarise(start=min(start),
            end=max(end),
            duration=end-start,
            p=first(adjP),
            ymin=0,
            ymax=20) ->
  pauses


duringsaccade=d$time[!d$isfixation]

mintime=20000
maxtime=30000
displaysize=1000

manipulate(
  ggplot(filter(p,spiketimes<maxtime,spiketimes>mintime))+
    geom_rect(data=filter(pauses,end<maxtime,end>mintime,end %in% duringsaccade,p<(5.0*10^(-pmin))), 
              aes(xmin=start,xmax=end,ymin=ymin,ymax=ymax),alpha=0.2)+
    geom_rect(data=filter(bursts,end<maxtime,end>mintime,duration>10,end %in% duringsaccade,p<(5.0*10^(-pmin))), 
              aes(xmin=start,xmax=end,ymin=ymin,ymax=ymax),fill='red',alpha=0.2)+
    geom_point(aes(spiketimes,10),shape='|',size=5)+
    geom_line(aes(time,verg.angle),data=filter(d,time<maxtime,time>mintime)),
  mintime=slider(0,60000),
  maxtime=slider(0,60000),
  pmin=slider(0,20,initial=3)
)


manipulate(
  ggplot(filter(p,spiketimes<maxtime,spiketimes>mintime))+
    geom_rect(data=filter(pauses,end<maxtime,end>mintime), 
              aes(xmin=start,xmax=end,ymin=ymin,ymax=ymax),alpha=0.2)+
    #     geom_rect(data=filter(bursts,end<maxtime,end>mintime,duration>10), 
    #               aes(xmin=start,xmax=end,ymin=ymin,ymax=ymax),fill='red',alpha=0.2)+
    geom_point(aes(spiketimes,10),shape='|',size=5)+
    geom_line(aes(time,verg.angle),data=filter(d,time<maxtime,time>mintime)),
  mintime=slider(0,60000),
  maxtime=slider(0,60000,initial=10000)
)

ggplot(aes(spiketimes,log(isi)),data=p)+geom_point()+
  geom_rect(data=pauses, aes(xmin=start,xmax=end,ymin=ymin,ymax=ymax),alpha=0.2)


manipulate(
  ggplot(filter(p,spiketimes<maxtime,spiketimes>mintime))+
    geom_rect(data=filter(pauses,end<maxtime,end>mintime,end %in% duringsaccade,p<(5.0*10^(-pmin))), 
              aes(xmin=start,xmax=end,ymin=ymin,ymax=ymax),alpha=0.2)+
    geom_rect(data=filter(bursts,end<maxtime,end>mintime,duration>10,end %in% duringsaccade,p<(5.0*10^(-pmin))), 
              aes(xmin=start,xmax=end,ymin=ymin,ymax=ymax),fill='red',alpha=0.2)+
    geom_point(aes(spiketimes,10),shape='|',size=5)+
    geom_line(aes(time,verg.angle),data=filter(d,time<maxtime,time>mintime)),
  mintime=slider(0,60000),
  maxtime=slider(0,60000),
  pmin=slider(0,20,initial=3)
)


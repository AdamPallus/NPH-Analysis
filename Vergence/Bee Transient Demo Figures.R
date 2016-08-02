

z<- readRDS('MeasuredDataBee-smooth.RDS')
library(manipulate)
p<- filter(z, r.amp>4,saccade.dur<100,abs(verg.amp)>3)
p<- filter(z, r.amp>4,saccade.dur<100,abs(verg.amp)<1)
p<- filter(z, r.amp>4,saccade.dur<100,verg.amp> 4)

goodsacs<- unique(p$sacnum)

nsac=length(goodsacs)
manipulate(ggplot(filter(z,sacnum==goodsacs[sac]))+
             geom_line(aes(counter,verg.velocity),color='purple')+
             geom_line(aes(counter,conj.velocity),color='brown')+
             # geom_point(aes(counter,enhancenum*0-50))+
             # geom_point(aes(counter,verg.velocity,color=transient.type),size=1,
             # data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
             geom_hline(yintercept = c(-2,2))+
             geom_line(aes(counter,verg.angle*5),color='darkgreen')+
             # geom_vline(aes(xintercept = verg.onset))+
             # geom_line(aes(counter,real.verg.velocity),color='magenta')+
             # geom_hline(yintercept=c(-12,12))+
             # geom_point(aes(counter,showrasters+100),shape='|',size=3)+
             coord_cartesian(ylim=c(-100,200)),
           sac=slider(1,nsac,step=1)
)

p<- filter(z, r.amp>4,saccade.dur<100,verg.amp> 4)
goodsacs<- unique(p$sacnum)
#for converging saccades
zz<- filter(z,sacnum %in% goodsacs[c(25,101,109,111,321,676)])

ggplot(zz)+
  geom_line(aes(counter,verg.velocity),color='purple',size=1.5)+
  geom_line(aes(counter,conj.velocity),color='brown')+
  # geom_point(aes(counter,enhancenum*0-50))+
  # geom_point(aes(counter,verg.velocity,color=transient.type),size=1,
  # data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
  # geom_hline(yintercept = c(-2,2))+
  geom_line(aes(counter,verg.angle*10),color='darkgreen')+
  # geom_vline(aes(xintercept = verg.onset))+
  # geom_line(aes(counter,real.verg.velocity),color='magenta')+
  # geom_hline(yintercept=c(-12,12))+
  # geom_point(aes(counter,showrasters+100),shape='|',size=3)+
  coord_cartesian(ylim=c(-50,200),xlim=c(-100,200))+
  facet_wrap(~sacnum)+
  theme(strip.background = element_blank(),strip.text.x=element_blank())+
  xlab('Time (ms)')+
  ylab('')


p<- filter(z, r.amp>4,saccade.dur<100,verg.amp< -4)
goodsacs<- unique(p$sacnum)
#for diverging saccades
zz<- filter(z,sacnum %in% goodsacs[c(130,154,155,156,636,638)])
ggplot(zz)+
  geom_line(aes(counter,verg.velocity),color='purple',size=1.5)+
  geom_line(aes(counter,conj.velocity),color='brown')+
  # geom_point(aes(counter,enhancenum*0-50))+
  # geom_point(aes(counter,verg.velocity,color=transient.type),size=1,
  # data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
  # geom_hline(yintercept = c(-2,2))+
  geom_line(aes(counter,verg.angle*10-100),color='darkgreen')+
  # geom_vline(aes(xintercept = verg.onset))+
  # geom_line(aes(counter,real.verg.velocity),color='magenta')+
  # geom_hline(yintercept=c(-12,12))+
  # geom_point(aes(counter,showrasters+100),shape='|',size=3)+
  coord_cartesian(ylim=c(-200,100),xlim=c(-100,200))+
  facet_wrap(~sacnum)+
  theme(strip.background = element_blank(),strip.text.x=element_blank())+
  xlab('Time (ms)')+
  ylab('')

p<- filter(z, r.amp>4,saccade.dur<100,abs(verg.amp)<1)
goodsacs<- unique(p$sacnum)
#for transients only
zz<- filter(z,sacnum %in% goodsacs[c(5,6,1957,1960,1973,2232)])
ggplot(zz)+
  geom_line(aes(counter,verg.velocity),color='purple',size=1.5)+
  geom_line(aes(counter,conj.velocity),color='brown')+
  # geom_point(aes(counter,enhancenum*0-50))+
  # geom_point(aes(counter,verg.velocity,color=transient.type),size=1,
  # data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
  # geom_hline(yintercept = c(-2,2))+
  geom_line(aes(counter,verg.angle*10-100),color='darkgreen')+
  # geom_vline(aes(xintercept = verg.onset))+
  # geom_line(aes(counter,real.verg.velocity),color='magenta')+
  # geom_hline(yintercept=c(-12,12))+
  # geom_point(aes(counter,showrasters+100),shape='|',size=3)+
  coord_cartesian(ylim=c(-100,100),xlim=c(-100,200))+
  facet_wrap(~sacnum)+
  theme(strip.background = element_blank(),strip.text.x=element_blank())+
  xlab('Time (ms)')+
  ylab('')
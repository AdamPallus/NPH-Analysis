

z<- readRDS('Measured and Smoothed Ozette.RDS')
z<- mutate(z,cv=(rev+lev)/2)
library(manipulate)
# p<- filter(z, r.amp>4,saccade.dur<100,abs(verg.amp)>3)
# p<- filter(z, r.amp>4,saccade.dur<100,abs(verg.amp)<1)
# p<- filter(z, r.amp>4,saccade.dur<100,verg.amp< -4)
# p<- filter(z, r.amp>4,saccade.dur<100,verg.amp> 4)
p<- filter(z, r.amp>4,abs(verg.amp)< 1)


goodsacs<- unique(p$sacnum)

nsac=length(goodsacs)
manipulate(ggplot(filter(z,sacnum==goodsacs[sac]))+
             # coord_cartesian(ylim=c(-350,350))+
             coord_cartesian(xlim=c(-50,200))+
             geom_line(aes(counter,verg.velocity),color='purple')+
             geom_line(aes(counter,cv),color='brown')+
             geom_line(aes(counter,rev),color='red')+
             geom_line(aes(counter,lev),color='blue')+
             # geom_point(aes(counter,enhancenum*0-50))+
             # geom_point(aes(counter,verg.velocity,color=transient.type),size=1,
             # data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
             geom_hline(yintercept = c(-2,2))+
             geom_line(aes(counter,verg.angle*5),color='darkgreen'),
             # geom_vline(aes(xintercept = verg.onset))+
             # geom_line(aes(counter,real.verg.velocity),color='magenta')+
             # geom_hline(yintercept=c(-12,12))+
             # geom_point(aes(counter,showrasters+100),shape='|',size=3)+

           sac=slider(1,nsac,step=1)
)

p<- filter(z, r.amp>4,saccade.dur<100,verg.amp> 4)
goodsacs<- unique(p$sacnum)
#for converging saccades
zz<- filter(z,sacnum %in% goodsacs[c(16,22,53,58,66,191)])

ggplot(zz)+
  geom_line(aes(counter,verg.velocity),color='purple',size=1.5)+
  # geom_line(aes(counter,conj.velocity),color='brown')+
  geom_line(aes(counter,cv),color='brown')+
  geom_line(aes(counter,rev),color='red')+
  geom_line(aes(counter,lev),color='blue')+
  # geom_point(aes(counter,enhancenum*0-50))+
  # geom_point(aes(counter,verg.velocity,color=transient.type),size=1,
  # data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
  # geom_hline(yintercept = c(-2,2))+
  geom_line(aes(counter,verg.angle*10),color='darkgreen')+
  # geom_vline(aes(xintercept = verg.onset))+
  # geom_line(aes(counter,real.verg.velocity),color='magenta')+
  # geom_hline(yintercept=c(-12,12))+
  # geom_point(aes(counter,showrasters+100),shape='|',size=3)+
  coord_cartesian(ylim=c(-300,300),xlim=c(-200,200))+
  facet_wrap(~sacnum)+
  theme(strip.background = element_blank(),strip.text.x=element_blank())+
  xlab('Time (ms)')+
  ylab('')

ggsave('Ozette Converging Transients Examples.pdf',height=10,width=15)


p<- filter(z, r.amp>4,saccade.dur<100,verg.amp< -4)
goodsacs<- unique(p$sacnum)
#for diverging saccades
zz<- filter(z,sacnum %in% goodsacs[c(9,10,19,25,29,39)])
ggplot(zz)+
  geom_line(aes(counter,verg.velocity),color='purple',size=1.5)+
  geom_line(aes(counter,cv),color='brown')+
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
ggsave('Ozette Diverging Transients Examples.pdf',height=10,width=15)


p<- filter(z, r.amp>4,saccade.dur<100,abs(verg.amp)<1)
goodsacs<- unique(p$sacnum)
#for transients only
zz<- filter(z,sacnum %in% goodsacs[c(64,65,66,428,441,620)])
ggplot(zz)+
  geom_line(aes(counter,verg.velocity),color='purple',size=1.5)+
  geom_line(aes(counter,cv),color='brown')+
  # geom_point(aes(counter,enhancenum*0-50))+
  # geom_point(aes(counter,verg.velocity,color=transient.type),size=1,
  # data=filter(z, sacnum==goodsacs[sac],enhancenum>0))+
  # geom_hline(yintercept = c(-2,2))+
  geom_line(aes(counter,verg.angle*10-50),color='darkgreen')+
  # geom_vline(aes(xintercept = verg.onset))+
  # geom_line(aes(counter,real.verg.velocity),color='magenta')+
  # geom_hline(yintercept=c(-12,12))+
  # geom_point(aes(counter,showrasters+100),shape='|',size=3)+
  coord_cartesian(ylim=c(-100,100),xlim=c(-100,200))+
  facet_wrap(~sacnum)+
  theme(strip.background = element_blank(),strip.text.x=element_blank())+
  xlab('Time (ms)')+
  ylab('')

ggsave('Ozette Transients Only Examples.pdf',height=10,width=15)

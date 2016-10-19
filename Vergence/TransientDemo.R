library(manipulate)
library(ggplot2)
zz<- readRDS('TransientDemo.RDS')
goodsacs<- unique(zz$sacnum)     
nsac=length(goodsacs)


manipulate(ggplot(filter(zz,sacnum==goodsacs[sac]))+
             # geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(time,(lev-rev)-100),color='darkblue',alpha=1)+
             geom_line(aes(time,(lev-rev)),color='green',alpha=1)+
             geom_line(aes(time,enhance.velocity-100),size=2,color='darkblue')+
             geom_line(aes(time,(lep-rep)*5-100),color='darkgreen')+
             geom_line(aes(time,lev-lag(rev,4)),color='red',linetype=2)+
             geom_line(aes(time,lag(lev,4)-rev),color='blue',linetype=2)+
             geom_line(aes(time,lev-lag(rev,3)),color='red',linetype=3)+
             geom_line(aes(time,lag(lev,3)-rev),color='blue',linetype=3)+
             geom_line(aes(time,lev-lag(rev,2)),color='red',linetype=4)+
             geom_line(aes(time,lag(lev,2)-rev),color='blue',linetype=4)+
             geom_line(aes(time,lev-lag(rev,1)),color='red',linetype=5)+
             geom_line(aes(time,lag(lev,1)-rev),color='blue',linetype=5)+
             geom_line(aes(time,lev-lag(rev,6)),color='red',linetype=6)+
             geom_line(aes(time,lag(lev,6)-rev),color='blue',linetype=6)+
             geom_line(aes(time,lev-lag(rev,9)),color='red',linetype=9)+
             geom_line(aes(time,lag(lev,9)-rev),color='blue',linetype=9)
           # ylim(c(-100,200))
           ,
           sac=slider(1,nsac,step=1))

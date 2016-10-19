

for (sac in 2:nsac){

ga<- ggplot(filter(zz,sacnum==goodsacs[sac]))+
             # geom_area(aes(time,sdf),alpha=1/10)+
             geom_line(aes(counter,(lev-rev)-100),color='darkblue',alpha=1)+
             geom_line(aes(counter,(lev-rev)),color='green',alpha=1)+
             geom_line(aes(counter,enhance.velocity-100),size=2,color='darkblue')+
             geom_line(aes(counter,(lep-rep)*5-100),color='darkgreen')+
             geom_line(aes(counter,lev-lag(rev,4)),color='red',linetype=2)+
             geom_line(aes(counter,lag(lev,4)-rev),color='blue',linetype=2)+
             geom_line(aes(counter,lev-lag(rev,3)),color='red',linetype=3)+
             geom_line(aes(counter,lag(lev,3)-rev),color='blue',linetype=3)+
             geom_line(aes(counter,lev-lag(rev,2)),color='red',linetype=4)+
             geom_line(aes(counter,lag(lev,2)-rev),color='blue',linetype=4)+
             geom_line(aes(counter,lev-lag(rev,1)),color='red',linetype=5)+
             geom_line(aes(counter,lag(lev,1)-rev),color='blue',linetype=5)+
             geom_line(aes(counter,lev-lag(rev,6)),color='red',linetype=6)+
             geom_line(aes(counter,lag(lev,6)-rev),color='blue',linetype=6)+
             geom_line(aes(counter,lev-lag(rev,9)),color='red',linetype=9)+
             geom_line(aes(counter,lag(lev,9)-rev),color='blue',linetype=9)+
             geom_point(aes(plep*10+200,100+plepV*10),color='blue',alpha=1/2)+
             geom_point(aes(prep*10+200,100+prepV*10),color='red',alpha=1/2)+
             geom_point(aes(200,100),size=3)
  
  ggsave(paste('TestDemo-',sac,'.PNG',sep=''))
  
  }
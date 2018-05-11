
#Run code from INCAnslysis.Rmd to get zm and zmc
#plotted these for Mark's grant renewal on 4-18-2018

library(plotly)


multiplot(
ggplot(zm)+
  geom_segment(aes(y=0,yend=1,x=dir.imp.R,xend=dir.imp.R,color=monkey))+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  theme_minimal()+
  theme(legend.position='none')+
  facet_wrap(~monkey,ncol=1)+
  xlab('Direction preference\nusing relative importance')+
  ylab(''),
ggplot(zm)+
  geom_segment(aes(y=0,yend=R2R,x=dir.imp.R,xend=dir.imp.R,color=monkey))+
  scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
  coord_polar(start=pi/2,direction=-1)+
  theme_minimal()+
  theme(legend.position='none')+
  facet_wrap(~monkey,ncol=1)+
  xlab('Direction preference\nusing relative importance')+
  ylab('R-squared'),
cols=2
)



multiplot(
  ggplot(zm)+
    geom_segment(aes(y=0,yend=1,x=dir.pref.R,xend=dir.pref.R,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing SLOPES')+
    ylab(''),
  ggplot(zm)+
    geom_segment(aes(y=0,yend=R2R,x=dir.pref.R,xend=dir.pref.R,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing SLOPES')+
    ylab('R-squared'),
  cols=2
)

zm %>%
  group_by(monkey) %>%
  tally()

multiplot(
  ggplot(zm)+
    geom_segment(aes(y=0,yend=1,x=dir.imp.L,xend=dir.imp.L,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing relative importance')+
    ylab(''),
  ggplot(zm)+
    geom_segment(aes(y=0,yend=R2L,x=dir.imp.L,xend=dir.imp.L,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing relative importance')+
    ylab('R-squared'),
  cols=2
)

multiplot(
  ggplot(zm)+
    geom_segment(aes(y=0,yend=1,x=dir.pref.L,xend=dir.pref.L,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing SLOPES')+
    ylab(''),
  ggplot(zm)+
    geom_segment(aes(y=0,yend=R2L,x=dir.pref.L,xend=dir.pref.L,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing SLOPES')+
    ylab('R-squared'),
  cols=2
)


multiplot(
  ggplot(zmc)+
    geom_segment(aes(y=0,yend=1,x=dir.pref.slope,xend=dir.pref.slope,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing SLOPES')+
    ylab(''),
  ggplot(zmc)+
    geom_segment(aes(y=0,yend=R2,x=dir.pref.slope,xend=dir.pref.slope,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing SLOPES')+
    ylab('R-squared'),
  cols=2
)


multiplot(
  ggplot(zm)+
    geom_segment(aes(y=0,yend=1,x=dir.pref.imp,xend=dir.pref.imp,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing Importance')+
    ylab(''),
  ggplot(zm)+
    geom_segment(aes(y=0,yend=R2,x=dir.pref.imp,xend=dir.pref.imp,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing Importance')+
    ylab('R-squared'),
  cols=2
)



multiplot(
  ggplot(zm)+
    geom_segment(aes(y=0,yend=1,x=dir.goodslopes.L,xend=dir.goodslopes.L,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing relative importance')+
    ylab(''),
  ggplot(zm)+
    geom_segment(aes(y=0,yend=R2L,x=dir.goodslopes.L,xend=dir.goodslopes.L,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing relative importance')+
    ylab('R-squared'),
  cols=2
)

multiplot(
  ggplot(zm)+
    geom_segment(aes(y=0,yend=1,x=dir.goodslopes.R,xend=dir.goodslopes.R,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing relative importance')+
    ylab(''),
  ggplot(zm)+
    geom_segment(aes(y=0,yend=R2R,x=dir.goodslopes.R,xend=dir.goodslopes.R,color=monkey))+
    scale_x_continuous(limits=c(-180,180),breaks=c(0,90,180,-90))+
    coord_polar(start=pi/2,direction=-1)+
    theme_minimal()+
    theme(legend.position='none')+
    facet_wrap(~monkey,ncol=1)+
    xlab('Direction preference\nusing relative importance')+
    ylab('R-squared'),
  cols=2
)


zm %>%
  mutate(skewness=dir.goodslopes.L %% 90,
         skewness=min(skewness,90-skewness))->
  zm

ggplotly(
qplot(skewness,fill=monkey,color=neuron,data=zm)
)

ggplotly(
  qplot(skewness,R2L,fill=monkey,color=neuron,data=zm)
)

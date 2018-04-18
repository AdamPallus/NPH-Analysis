multiplot(plotlist=compare2eyes(zp,zm,'DC-943',color1='purple',color2='orange'),cols=2)

zp %>%
  filter(meanFR>3) %>%
  group_by(neuron) %>%
  do(measureCell(.)) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  zm

neurons<- unique(zp$neuron)
manipulate(
  multiplot(plotlist=compare2eyes(zp,zm,neurons[chosenCell],
                                 color1='red',
                                 color2='blue'),
           cols=2),
  chosenCell=slider(1,length(neurons),step=1))


zp %>%
  filter(meanFR>3) %>%
  group_by(neuron) %>%
  do(tidy(lm(meanFR ~ mean.V,data=.))) %>%
  select(neuron,term,estimate) %>%
  mutate(term=replace(term,term=='(Intercept)','intercept')) %>%
  spread(term,estimate) %>%
  mutate(yint= -intercept/mean.V) %>%
  separate(neuron,c('monkey','cellnum'),remove=FALSE)->
  thresholdanalysis


#These are the cells that aren't always active when the monkey is looking at the screen
#We can now test the hypothesis that there is a difference in the thresholds
filter(thresholdanalysis,abs(yint)<100) %>%
  qplot(data=.,yint,fill=monkey)


filter(thresholdanalysis,abs(yint)<100) %>%
  ggplot()+geom_density(aes(abs(yint),color=monkey),alpha=0.1)

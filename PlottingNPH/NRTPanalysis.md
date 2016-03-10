# NRTP Vergence
Adam  
February 16, 2016  
#Introduction
This is an analysis of the activity of individual neurons in the NRTP of monkeys while they make combined saccade vergence movements. 

#Data Processing
* Load .csv files from the appropriate directory. These .csv files were creating using matlab.
* Calclate the lead time for each neuron.
+ shift data from cell and eye coils to align based on lead time
* Identify saccades.


```
## Warning: package 'knitr' was built under R version 3.2.3
```











![](NRTPanalysis_files/figure-docx/verg.fixations-1.png)

We plot the firing rate of each cell during saccades, including 100ms before and after the movement. Convergent saccades have an amplitude change of +3, divergent, -3 and version saccades are the rest. 



![](NRTPanalysis_files/figure-docx/verg.change-1.png)

![](NRTPanalysis_files/figure-docx/peak.verg.velocity-1.png)

##Model Fitting: Peak transient velocity or amplitude of vergence angle change?
In this next section we attempt to distinguish between two factors that are correlated with the peak firing rates of the vergence-burster cells: the amplitude of the vergence angle change and the peak velocity of the vergence transient. It may be possible to distinguish these because there are vergence transients even during saccades without a significant change in vergence angle. The above plot demonstrates this. In some cells, the blue points are below the red points, and above the green ones. This means that for a given peak transient velocity, the cell will fire more if the vergence angle changes more. 

### Model based on the positive transient


![](NRTPanalysis_files/figure-docx/plot.positive.trans.fit-1.png)

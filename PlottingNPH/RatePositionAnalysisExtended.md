# NPH rate position analysis
Adam  
October 12, 2015  







First I will plot the average firing rate of the neuron while the eyes are in various positions. I've restricted my analysis to periods when the eyes are not in motion using a simple eye velocity threshold. I require both the vertical and horizontal eye position to be less than one. This allows for pre-movement burst activity to potentially interfere with the static analysis. 






![](RatePositionAnalysisExtended_files/figure-html/gridplot-1.png) 

Next, let's show the rate position curves for horizontal and vertical individually.



![](RatePositionAnalysisExtended_files/figure-html/RightEye-1.png) 

![](RatePositionAnalysisExtended_files/figure-html/LeftEye-1.png) 

Next, I will create a table of the linear regression coefficients for the formula $$F_r=b+k_hE_h + k_hE_v$$, where $E_h$ and $E_v$ are the horizontal and vertical eye positions during periods where the eye velocity is less than 5. 


animal   cellnum    r.h.slope    r.v.slope   r.h.p   r.v.p     r.angle   l.h.slope    l.v.slope      l.h.p   l.v.p     l.angle
-------  --------  ----------  -----------  ------  ------  ----------  ----------  -----------  ---------  ------  ----------
Bee      20         0.5581900   -0.3434334       0       0   -31.60243   0.5458206   -0.3193649   0.000000       0   -30.33232
Bee      909        0.0896918    1.1360874       0       0    85.48598   0.0997385    1.2067677   0.000000       0    85.27528
Bee      910        0.0996061   -2.5718739       0       0   -87.78210   0.1249027   -2.5352268   0.000000       0   -87.17950
Patos    11         0.0488771   -0.1747124       0       0   -74.37065   0.0108067   -0.2017336   0.001687       0   -86.93364

Now, let's plot the vectors of the preferred position for each cell.

![](RatePositionAnalysisExtended_files/figure-html/DirectionPlot-1.png) 


```
## $title
## [1] "Only Signficant Slopes"
## 
## attr(,"class")
## [1] "labels"
```

The follwing plot shows the average of the absolute value of the slopes for each animal.

![](RatePositionAnalysisExtended_files/figure-html/AverageSlopes-1.png) 

In the next analyses, we will evaluate the average firing rate during each period of fixation. First, we identify saccades using a simple velocity threshold and mark them using a buffer of 15ms. The remaining periods are considered fixations.



![](RatePositionAnalysisExtended_files/figure-html/plotfixations-1.png) 

![](RatePositionAnalysisExtended_files/figure-html/plotregressions-1.png) ![](RatePositionAnalysisExtended_files/figure-html/plotregressions-2.png) 



  

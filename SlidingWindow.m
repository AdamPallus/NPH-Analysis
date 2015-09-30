function [goodstarttimes, goodendtimes] = SlidingWindow(eyevelocity, winlength, threshold, abovethreshold, eyeaccel, accelthreshold, usecomplexdetection)
% SlidingWindow
%   This code implements a sliding window with minimal use of loops.
%   Syntax: [output] = SlidingWindow(a, winlength, pointtotake) where a is
%   the vector to search, winlength is the size of the sliding window and
%   pointtotake is the actual data point saved for analysis. As the window
%   slides, the output accumulates the values at index
%   'pointtotake'. 

% eyevelocity = This should contain the variable name for the velocity vector you
% want to use. 
%
% winlength: This is the size of the sliding window. 
%
% pointtotake = 2; % Must be equal to or less than winlength
%
% threshold = This is the eye velocity threshold;
%
% abovethreshold = 0; % Set this to one if you want the program to search for points above threshold.
%
% eyeaccel = This was added on 10/26/12 to allow saccade detection based on
% both velocity and acceleration criteria. If you want to use this, it
% should be the absolute value of acceleration. 
%
% accelthreshold: This is the threshold for eyeaccel. 

% Complex detection ends a saccade if either of two conditions are met:
% 1. Eye velocity dips below 50 or...
% 2. Eye velocity dips below 'threshold' AND the absolute value of 
% acceleration dips below 'threshold2'. This more complex saccade offset
% detection seems to do a better job of eliminating postsaccadic drifts.
% For example, this allows the program to detect cases when the
% postsaccadic drift is so fast that it reaccelerates the eye before the
% velocity ever dips below threshold. When this occurs, there will be a few
% ms when the acceleration will be very low because the eye velocity is
% reversing. Particularly for QT, this can occur even when the eye velocity
% never dips below 100 deg/s. The 'complex detection' algorithm doesn't
% get fooled by these cases. 

% usecomplexdetection = 0; 

if abovethreshold == 1
%     puppy = find(dog < threshold); % Make sure this line searches for data points on the unwanted side of the threshold
    badperiods = find(eyevelocity < threshold & eyeaccel < accelthreshold); % Make sure this line searches for data points on the unwanted side of the threshold
    if usecomplexdetection == 1;
        complexbadperiods = find(eyevelocity < 50);
        badperiods = union(badperiods, complexbadperiods);
    end;
else    %searching for fixations
    
%     puppy = find(dog > threshold); % Make sure this line searches for data points on the unwanted side of the threshold
      badperiods = find(eyevelocity > threshold | eyeaccel > accelthreshold); % Make sure this line searches for data points on the unwanted side of the threshold
end;
%find bad periods with at least *winlength* points between them
%this corresponds with a good period of at least that long
% timeofgoodperiods = find(diff(badperiods) >= winlength); 
goodstarttimes = badperiods(diff(badperiods) >= winlength)+1;
goodendtimes=zeros(size(goodstarttimes));
for a = 1:length(goodstarttimes)
    goodendtimes(a)=badperiods(find(badperiods>goodstarttimes(a),1));
end;

% dogstartslength = length(goodstarttimes)
% dogendslength = length(dogends)
goodendtimes = goodendtimes';
end


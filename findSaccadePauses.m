
function [good_pauses,pause_during_saccade,pauses_during_saccade] = ...
    findSaccadePauses(sacOnset,sacOffset,start_pause,end_pause)
%Syntax: [good_pauses,pause_during_saccade,pauses_during_saccade] =
%    findSaccadePauses(sacOnset,sacOffset,start_pause,end_pause)
nsaccades=length(sacOnset);
pause_times=NaN(end_pause(end),1);
pause_during_saccade=NaN(nsaccades,1);

for i = 1:length(start_pause)
    pause_times(start_pause(i):end_pause(i))=i;
end

for i = 1:length(sacOnset)
    if sacOnset(i) < length(pause_times)
        thisSaccade=pause_times(sacOnset(i):sacOffset(i));
%         thisSaccade=thisSaccade(thisSaccade>0);
        pause_during_saccade(i)=max(thisSaccade);
        pauses_during_saccade{i}=unique(thisSaccade(thisSaccade>0));
    else
        pause_during_saccade(i)=NaN;
        pauses_during_saccade{i}=ones(0,1);
    end
end

good_pauses=unique(pause_during_saccade(pause_during_saccade>0));


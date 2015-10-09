function b=spikesort(~,~)

h=evalin('base','h');
data=evalin('base','data');

threshold=str2double(h.thresh.String);
if threshold < 1 || threshold >10
    display('Invalid Threshold. Using default of 3.3');
    threshold=3.3;
end
    
addpath(genpath('C:\Users\setup\Documents\GitHub\UltraMegaSort')) 

Fs=50000; %sampling rate of waveform recorder
spikes = ss_default_params(Fs,'thresh',2.2);
spikes = ss_detect(data,spikes);
spikes = ss_align(spikes);
spikes = ss_kmeans(spikes);
spikes = ss_energy(spikes);
spikes = ss_aggregate(spikes);

% main tool
xxx=figure;
splitmerge_tool(spikes)
close(xxx)

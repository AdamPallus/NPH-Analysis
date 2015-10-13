function choosecluster(~,~)
h=evalin('base','h');
try
    spikes=evalin('base','spikes');
catch
    errordlg('Save Spikes from Sort GUI','Saves Spikes First')
    return
end
clusters=unique(spikes.assigns);
clear xx
for i =1:length(clusters)
    xx{i}=num2str(clusters(i));
end

[choice cancel]=listdlg('ListString',xx,...
    'PromptString','Choose a Cluster','selectionmode','single');
if cancel ~=0
    spiketimes=spikes.spiketimes(spikes.assigns==str2double(xx{choice}));
    spiketimes=spiketimes*spikes.params.Fs;
else
    display('Cancelled')
end

doplot= questdlg('Analyze Choice','Plot?');

if strcmp(doplot,'Yes')
    b=evalin('base','b');
    a=figure;
    plot(b.Unit.values)
    hold on
    plot(spiketimes,-0.07,'^r')
    title(['Cluster: ',xx{choice},' #spikes: ',num2str(length(spiketimes))])
    waitfor(a)
end
keep= questdlg('Keep Choice','Keep?');

if strcmp(keep,'Yes')
    assignin('base','spiketimes',spiketimes)
    h.savecsv.Enable='on';
end




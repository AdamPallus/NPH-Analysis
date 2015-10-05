%The purpose of this function is to take the data dumped from spike2 and
%convert it into a .csv file that is usable by R or Matlab. It will allow
%you to choose a .mat file with raw data from spike2, sort spikes and
%re-save the .mat file containing the sorted spikes. It has buttons to
%allow you to analyze multiple clusters and save as multiple .csv files.

function NPHGUI()
% path='C:/Users/setup/Desktop/Nucleus Prepositus Hypoglossi/';
% [filename, filepath]=uigetfile({[path,'*.mat']},'Select File to Analyze',...
%     'multiselect','off');
% if filename==0
%     return
% end
% b=load([filepath filename]);
b=[];
b.f=figure;
b.clusters=uicontrol('style','list','units','normalized',...
    'string', {'Gaze Shift Trials','Pursuit Trials'},...
    'position',[.1,.5,.4,.1]);

b.sort=uicontrol(b.f,'string','Sort Spikes','units','normalized',...
    'position',[0.3 0.2 0.2 0.1],...
    'callback',{@sortspikes b});

% if ~isfield(b,'spiketimes')
%     b.spiketimes=sortspikes({b.Unit.values}); 
% end

b.plotwf=uicontrol(b.f,'string','See Waveform','units','normalized',...
    'position',[0.5 0.2 0.2 0.1],...
    'callback',{@plotwf b});



function plotwf(~,~,b)
figure
plot(b.Unit.values)
if isfield(b,'spiketimes')
    hold on
    plot(b.spiketimes,-0.07,'^r')
end
    



function tableout=NPHAnalysis()
[filename, filepath]=uigetfile('.mat','Select Data File');
b=load([filepath filename]);
if ~isfield(b,'spiketimes')
    display('need to sort spikes on this file')
    q=questdlg('Sort Spikes Now?','Needs Spiketimes');
    if strcmp(q,'Yes')
        b.spiketimes=sortspikes({b.Unit.values});
        try
            save([filepath, filename(1:end-4), '-sorted.mat'],'-struct','b')
        catch
            display('Save Failed')
        end
        %Consider adding something here that re-saves the .mat file with
        %spiketimes attached
    else
        return
    end
    
end
%Create data table for export to R

%create spike density function
sdf=makesdf(b,20);
rep=b.H_Eye.values;%horizontal right eye position
rev=parabolicdiff(smooth(rep,15),5);%horizontal right eye velocity
repV=b.V_Eye.values;%vertical right eye position
revV=parabolicdiff(smooth(repV,15),5);%vertical right eye velocity

lep=b.H_Eye2.values;%horizontal right eye position
lev=parabolicdiff(smooth(lep,15),5);%horizontal right eye velocity
lepV=b.V_Eye2.values;%vertical right eye position
levV=parabolicdiff(smooth(lepV,15),5);%vertical right eye velocity

% sdf=[sdf zeros[1 length(rep)-length(sdf)]] %pad sdf
t=table(sdf,rep,rev,repV,revV,...
    lep,lev,lepV,levV,...
    'variablenames',{'sdf','rep','rev','repV','revV'...
    'lep','lev','lepV','levV'});
if nargout>0
    tableout=t;
end
[filename, filepath]=uiputfile('*.csv','Save Table',filename(1:end-4));
display([filepath filename])
writetable(t,[filepath filename])

function spiketimes=sortspikes(data)
        Fs=50000; %sampling rate of waveform recorder
        spikes = ss_default_params(Fs,'thresh',3.3);
        spikes = ss_detect(data,spikes);
        spikes = ss_align(spikes);
        spikes = ss_kmeans(spikes);
        spikes = ss_energy(spikes);
        spikes = ss_aggregate(spikes);
        % main tool
        try
            splitmerge_infunc(spikes);
        end
        waitfor(gcf)
%         waitforbuttonpress
        clusters=unique(spikes.assigns);
        for i =1:length(clusters)
            xx{i}=num2str(clusters(i));
        end

        [choice, cancel]=listdlg('ListString',xx,...
            'PromptString','Choose a Cluster','selectionmode','single');
        if cancel ~=0
            spiketimes=spikes.spiketimes(spikes.assigns==str2double(xx{choice}));
            spiketimes=spiketimes*spikes.params.Fs;
        else
            display('Cancelled')
        end
        doplot= questdlg('Analyze Choice','Plot?');

        if strcmp(doplot,'Yes')
            try
                figure
                plot(data{1})
                hold on
                plot(spiketimes,-0.07,'^r')
                title(['Cluster: ',xx{choice},' #spikes: ',num2str(length(spiketimes))])
            catch
                display('failed plot')
            end
        end

function sdf=makesdf(b, stdsize)
    rasters=zeros([1,length(b.H_Eye.values)]);
    rasters(floor(b.spiketimes/50))=1;
    gaus=fspecial('gaussian',[1 stdsize*10],stdsize)*1000;
    sdf=conv(rasters,gaus,'same')';

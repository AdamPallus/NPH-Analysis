function savecsv(~,~)
b=evalin('base','b');
try
b.spiketimes= evalin('base','spiketimes');
end
savespikes=questdlg('Save Spiketimes?','Save?');
savelocation=questdlg('Save .csv to same folder?','Save?');

if strcmp(savespikes,'Yes')
    b.spikes=evalin('base','spikes');
    save([b.filepath, b.filename(1:end-4), '-sorted.mat'],'-struct','b')
end

%create spike density function
[sdf, rasters]=makesdf(b,20);
rep=b.H_Eye.values;%horizontal right eye position
lep=b.H_Eye2.values;%horizontal right eye position
repV=b.V_Eye.values;%vertical right eye position
lepV=b.V_Eye2.values;%vertical right eye position

thp=b.H_Targ.values; %horizontal target position
tvp=b.V_Targ.values; %vertical target position

trimlength=min(length(rep),length(lep));

if length(rep) ~= length(lep)
    
    rep=rep(1:trimlength);
    repV=repV(1:trimlength);
    thp=thp(1:trimlength);
    tvp=tvp(1:trimlength);
    lep=lep(1:trimlength);
    lepV=lepV(1:trimlength);
end

if length(sdf)> trimlength
    sdf=sdf(1:trimlength);
end

if length(tvp)>trimlength
    thp=thp(1:trimlength);
    tvp=tvp(1:trimlength);
end

if length(rasters) > trimlength
    rasters=rasters(1:trimlength);
end

    
rev=parabolicdiff(smooth(rep,15),5);%horizontal right eye velocity
revV=parabolicdiff(smooth(repV,15),5);%vertical right eye velocity
lev=parabolicdiff(smooth(lep,15),5);%horizontal right eye velocity
levV=parabolicdiff(smooth(lepV,15),5);%vertical right eye velocity

%sdf=[sdf zeros[1 length(rep)-length(sdf)]] %pad sdf

% t=table(sdf,rep,rev,repV,revV,...
%     lep,lev,lepV,levV,...
%     'variablenames',{'sdf','rep','rev','repV','revV'...
%     'lep','lev','lepV','levV'});

% t=table(sdf,rep,rev,repV,revV,...
%     lep,lev,lepV,levV,thp,tvp,...
%     'variablenames',{'sdf','rep','rev','repV','revV'...
%     'lep','lev','lepV','levV','thp','tvp'});

t=table(rasters,rep,rev,repV,revV,...
    lep,lev,lepV,levV,thp,tvp,...
    'variablenames',{'rasters','rep','rev','repV','revV'...
    'lep','lev','lepV','levV','thp','tvp'});


defaultname=[b.filepath, b.filename(1:end-4), '.csv'];
if strcmp(savelocation,'Yes')
    writetable(t,defaultname)
else
    [filename, filepath]=uiputfile('*.csv','Save Table','~/data');
    display([filepath filename])
    % assignin('base','t',t)
    writetable(t,[filepath filename])
end

function [sdf, rasters]=makesdf(b, stdsize)
rasters=zeros([1,length(b.H_Eye.values)]);
rasters(floor(b.spiketimes/50))=1;
gaus=fspecial('gaussian',[1 stdsize*10],stdsize)*1000;
sdf=conv(rasters,gaus,'same')';
rasters=rasters';

function savecsv(~,~)
b=evalin('base','b');
b.spiketimes= evalin('base','spiketimes');

savespikes=questdlg('Save Spiketimes?','Save?');
if savespikes
    b.spikes=evalin('base','spikes');
    save([b.filepath, b.filename(1:end-4), '-sorted.mat'],'-struct','b')
end

%create spike density function
sdf=makesdf(b,20);
rep=b.H_Eye.values;%horizontal right eye position
lep=b.H_Eye2.values;%horizontal right eye position
repV=b.V_Eye.values;%vertical right eye position
lepV=b.V_Eye2.values;%vertical right eye position

thp=b.H_Targ.values; %horizontal target position
tvp=b.V_Targ.values; %vertical target position

if length(rep)>length(lep)
    rep=rep(1:length(lep));
    repV=repV(1:length(lep));
elseif length(lep)<length(rep)
    lep=lep(1:length(rep));
    lepV=lepV(1:length(rep));
end
if length(sdf)>length(lep)
    sdf=sdf(1:length(lep));
end
    
rev=parabolicdiff(smooth(rep,15),5);%horizontal right eye velocity
revV=parabolicdiff(smooth(repV,15),5);%vertical right eye velocity
lev=parabolicdiff(smooth(lep,15),5);%horizontal right eye velocity
levV=parabolicdiff(smooth(lepV,15),5);%vertical right eye velocity

% sdf=[sdf zeros[1 length(rep)-length(sdf)]] %pad sdf
t=table(sdf,rep,rev,repV,revV,...
    lep,lev,lepV,levV,thp,tvp,...
    'variablenames',{'sdf','rep','rev','repV','revV'...
    'lep','lev','lepV','levV','thp','tvp'});

[filename, filepath]=uiputfile('*.csv','Save Table','~/data');
display([filepath filename])
writetable(t,[filepath filename])

function sdf=makesdf(b, stdsize)
rasters=zeros([1,length(b.H_Eye.values)]);
rasters(floor(b.spiketimes/50))=1;
gaus=fspecial('gaussian',[1 stdsize*10],stdsize)*1000;
sdf=conv(rasters,gaus,'same')';

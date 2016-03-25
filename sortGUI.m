% [filename, filepath]=uigetfile('.mat','Select Data File','C:\Users\setup\Desktop\Nucleus Prepositus Hypoglossi');
[filename, filepath]=uigetfile('.mat','Select Data File','C:\Users\setup\Desktop\NRTP Vergence');
% [filename, filepath]=uigetfile('.mat','Select Data File','C:\Users\setup\Desktop\SOA from Mark');

if filename == 0
    return
end

b=load([filepath filename]);
b.filepath=filepath;
b.filename=filename;
data={b.Unit.values};


f=figure;
f.Position=[680 767 197 331];
handles.t=uitabgroup(f);

handles.a=uitab(handles.t,'title','Analyze');
handles.s=uitab(handles.t,'title','Save');

handles.thresh=uicontrol(handles.a,'style','edit','String','3.3','units','normalized',...
    'position',[0.17 0.75,0.6,0.1]);
handles.thresht=uicontrol(handles.a,'style','text','String','Spike Threshold',...
    'units','normalized',...
    'position',[0.22,0.85,0.5,0.06]);
handles.sortspikes=uicontrol(handles.a,'string','Sort Spikes','units','normalized',...
    'position',[0.08 0.2 0.8 0.3],...
    'callback',{@spikesort});

handles.choosecluster=uicontrol(handles.s,'string','Choose Cluster','units','normalized',...
    'position',[0.08 0.6 0.8 0.3],...
    'callback',{@choosecluster});

handles.savecsv=uicontrol(handles.s,'string','Save as CSV','units','normalized',...
    'position',[0.08 0.2 0.8 0.3],...
    'callback',{@savecsv});
handles.savecsv.Enable='off';
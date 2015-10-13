[filename, filepath]=uigetfile('.mat','Select Data File','C:\Users\setup\Desktop\Nucleus Prepositus Hypoglossi');
if filename == 0
    return
end

b=load([filepath filename]);
b.filepath=filepath;
b.filename=filename;
data={b.Unit.values};


f=figure;
f.Position=[680 767 197 331];
h.t=uitabgroup(f);

h.a=uitab(h.t,'title','Analyze');
h.s=uitab(h.t,'title','Save');

h.thresh=uicontrol(h.a,'style','edit','String','3.3','units','normalized',...
    'position',[0.17 0.75,0.6,0.1]);
h.thresht=uicontrol(h.a,'style','text','String','Spike Threshold',...
    'units','normalized',...
    'position',[0.22,0.85,0.5,0.06]);
h.sortspikes=uicontrol(h.a,'string','Sort Spikes','units','normalized',...
    'position',[0.08 0.2 0.8 0.3],...
    'callback',{@spikesort});

h.choosecluster=uicontrol(h.s,'string','Choose Cluster','units','normalized',...
    'position',[0.08 0.6 0.8 0.3],...
    'callback',{@choosecluster});

h.savecsv=uicontrol(h.s,'string','Save as CSV','units','normalized',...
    'position',[0.08 0.2 0.8 0.3],...
    'callback',{@savecsv});
h.savecsv.Enable='off';
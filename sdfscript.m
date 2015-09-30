% s=smooth(spiketimes,15);
rasters=zeros([1,ceil(length(b.Unit.values)/5000)]);
rasters(floor(s/50))=1;
% rasters=rasters(1:100000);
stdsize=20;
samplerate=1;
gaus=fspecial('gaussian',[1 stdsize*10],stdsize*samplerate);
sdf=conv(rasters,gaus,'same');
figure
plot(sdf)
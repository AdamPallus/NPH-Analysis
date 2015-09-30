
smoothamount=5;
isi=spiketimes(2:end)-spiketimes(1:end-1);
spikerate=1./isi*50000;
h(1)=subplot(2,1,1);
plot(b.H_Eye.values,'r')
hold on
plot(b.H_Eye2.values,'b')
h(2)=subplot(2,1,2);
plot(spiketimes(2:end)/50,smooth(spikerate,smoothamount))
linkaxes(h,'x');
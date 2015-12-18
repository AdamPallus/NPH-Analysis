function [sdf]=rasters2sdf(rasters, stdsize)
gaus=fspecial('gaussian',[1 stdsize*10],stdsize)*1000;
sdf=conv(rasters,gaus,'same')';
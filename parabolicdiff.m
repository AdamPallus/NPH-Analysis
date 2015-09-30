function vels=parabolicdiff(pos,n)
%This is an n-point parabolic differentiator used to convert position
%information into velocity, or velocity into acceleration
    if nargin<2
        n=9;%default value
    end
    q = sum(2*((1:n).^2));
    vels=zeros(size(pos));

    c=-conv(pos,[-n:-1 1:n],'valid'); 
    vels(1:n)=ones(n,1)*c(1);
    vels(end-n+1:end)=ones(n,1)*c(end);
    vels(n:end-n)=c;
    vels=vels/q*1000;

end
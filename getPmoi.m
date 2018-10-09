function [Ixx,Iyy,Izz] = getPmoi(x,y)
% Polar moment of Inertia (second moment of area about perpendicular axis)
% https://en.wikipedia.org/wiki/Second_moment_of_area
% http://www.infogoaround.org/JBook/CentroidInertia.pdf
% x,y are centered cartesian coordinates at centroid (first point not
% repeated)

% Izz = Ixx + Izz

x(end+1) = x(1);
y(end+1) = y(1);


nLineSegs = length(x)-1;
ixxSum = 0;
iyySum = 0;
for i=1:nLineSegs
    ixxSegment = (y(i)^2 + y(i)*y(i+1) + y(i+1)^2)*(x(i)*y(i+1) - x(i+1)*y(i));
    ixxSum = ixxSum + ixxSegment;

    iyySegment = (x(i)^2 + x(i)*x(i+1) + x(i+1)^2)*(x(i)*y(i+1) - x(i+1)*y(i));
    iyySum = iyySum + iyySegment;
end
Ixx = abs((1/12)*ixxSum);
Iyy = abs((1/12)*iyySum);

Izz = abs(Ixx + Iyy);



end





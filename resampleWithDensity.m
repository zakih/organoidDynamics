function [xNew, yNew,densityMinCheck] = resampleWithDensity(x,y,density)
% Given a contour's (x,y) coordinates, resample to have density = points/length
% densityMinCheck: this is the density based on minimum distance between
% original (x,y)



% original point density
% density = length(x)/perimeterList(imageNum)



% Arc length and perimeter
nPoints = length(x);
distList = zeros(nPoints,1);
for i=1:nPoints-1
    distList(i) = sqrt( (x(i+1) - x(i) )^2 + (y(i+1)-y(i))^2 );
end
distList(end) = sqrt( (x(1) - x(end) )^2 + (y(1)-y(end))^2 );
perimeter = sum(distList);

arcLength = zeros(nPoints+1,1);
for i=2:nPoints+1
    arcLength(i) = arcLength(i-1) + distList(i-1);
end
%distList = [0 ;distList];


%  Number of points per unit length based on minimum distance between
%  points
densityMinCheck = ceil(1/min(distList(1:end-1)));
pointDist = 1/density;

% xNew = zeros(ceil(perimeter*density),1);
% yNew = zeros(ceil(perimeter*density),1);
xNew = x(1);
yNew = y(1);
lengthFromPreviousSegment = 0;
nStar = 1;
for i = 1:length(x)-1
    % Direction from i to i+1:
    u1 = x(i+1)-x(i);
    u2 = y(i+1)-y(i);
    uMag = sqrt(u1^2+u2^2);
    u1 = u1/uMag;
    u2 = u2/uMag;
    
    % Calculate number of points to add to this line segment:
    effectiveLength = uMag + lengthFromPreviousSegment;
    nPoints2Add = floor(effectiveLength*density);
    
    % Add points (x,y)_(i+1) = (x,y)_i + pointDist*(u1,u2)
    % Add first point which may be closer to (x,y)_i than pointDist
    xStar = x(i)+ (pointDist-lengthFromPreviousSegment)*u1;
    yStar = y(i)+ (pointDist-lengthFromPreviousSegment)*u2;
    xNew(end+1) = xStar;
    yNew(end+1) = yStar;
    nPoints2Add = nPoints2Add -1;
    nStar = nStar + 1;
    
    for n = 1:nPoints2Add
        xStar = xNew(nStar)+ (pointDist)*u1;
        yStar = yNew(nStar)+ (pointDist)*u2;
        xNew(end+1) = xStar;
        yNew(end+1) = yStar;
        nStar = nStar + 1;
    end
    % left over distance
    lengthFromPreviousSegment = mod(effectiveLength,pointDist);
end




end

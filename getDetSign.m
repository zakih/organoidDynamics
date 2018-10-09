function  detSign  = getDetSign( x,y )
% Input (x,y) coordinate series (without last pair being equal to first)
% Compute orientation matrix of each vertex triplet and store determinant
% sign (detSign)
% https://en.wikipedia.org/wiki/Curve_orientation8 

roundingThreshold = 15;
detSign = zeros(length(x),1);

% First vertex: A: n , B: 1, C = 2 
xA = x(end);
yA = y(end);
xB = x(1);
yB = y(1);
xC = x(2);
yC = y(2);
detSign(1) = sign(round((xB - xA)*(yC-yA) - (xC-xA)*(yB-yA),roundingThreshold));
% Last vertex: A: n - 1, B: n, C = 1
xA = x(end-1);
yA = y(end-1);
xB = x(end);
yB = y(end);
xC = x(1);
yC = y(1);
detSign(end) = sign(round((xB - xA)*(yC-yA) - (xC-xA)*(yB-yA),roundingThreshold));
% Iterate from vertex # 2 to vertex # n-1
for i = 2:length(detSign)-1
    % Start at point 3 being C
    xA = x(i-1);
    yA = y(i-1);
    xB = x(i);
    yB = y(i);
    xC = x(i+1);
    yC = y(i+1);
    detLocal = sign(round((xB - xA)*(yC-yA) - (xC-xA)*(yB-yA),roundingThreshold));
    detSign(i) = sign(detLocal);

end

end


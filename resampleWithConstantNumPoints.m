function [xNew, yNew,densityResultant] = resampleWithConstantNumPoints(x,y,numPointsReqd)
%% Given a contour's (x,y) coordinates, resample to have numPointsReqd at
% variable densityResultant = numPointsReqd/arcLength
% densityMinCheck: this is the density based on minimum distance between
% original (x,y)



%% original point density
% density = length(x)/perimeterList(imageNum)



% Arc length and perimeter
nPoints = length(x);
distList = zeros(nPoints,1);
for i=1:nPoints-1
    distList(i) = sqrt( (x(i+1) - x(i) )^2 + (y(i+1)-y(i))^2 );
end
distList(end) = sqrt( (x(1) - x(end) )^2 + (y(1)-y(end))^2 );
perimeter = sum(distList);

% arcLength = zeros(nPoints+1,1);
% for i=2:nPoints+1
%     arcLength(i) = arcLength(i-1) + distList(i-1);
% end
%distList = [0 ;distList];


%  Number of points per unit length based on minimum distance between
%  points
% densityMinCheck = ceil(1/min(distList(1:end-1)));
% Required distance between points
% pointDist = 1/densit y; % for constant density
pointDist = perimeter/numPointsReqd; % for constant number of points
densityResultant = numPointsReqd/perimeter;

xNew = zeros(numPointsReqd,1);
yNew = zeros(numPointsReqd,1);
xNew(1) = x(1);
yNew(1) = y(1);


algo = 2;


%%  Algo # 2

if algo == 2
    lengthFromPreviousSegment = 0; %lp
    nStar = 2;
    for i = 1:length(x)-1
        % Calculate d(i,i+1):
        % Direction from i to i+1:
        u1 = x(i+1)-x(i);
        u2 = y(i+1)-y(i);
        uMag = sqrt(u1^2+u2^2);
        u1 = u1/uMag;
        u2 = u2/uMag;
        
        % Calculate effective length r:
        effectiveLength = lengthFromPreviousSegment + uMag;
        
        if effectiveLength >=pointDist
            % Add points
            % Calculate number of points to add to this line segment:
            nPoints2Add = floor(effectiveLength*densityResultant);
            
            % Add first point which may be closer to (x,y)_i than pointDist
            xStar = x(i)+ (pointDist-lengthFromPreviousSegment)*u1;
            yStar = y(i)+ (pointDist-lengthFromPreviousSegment)*u2;
            xNew(nStar) = xStar;
            yNew(nStar) = yStar;
            nStar = nStar + 1;
            nPoints2Add = nPoints2Add -1;
            
            x_c = xStar;
            y_c = yStar;
            for n = 1:nPoints2Add
                xStar = x_c + pointDist*u1;
                yStar = y_c + pointDist*u2;
                xNew(nStar) = xStar;
                yNew(nStar) = yStar;
                nStar = nStar + 1;
                x_c = xStar;
                y_c = yStar;
            end
            % Update lp:
            lengthFromPreviousSegment = mod(effectiveLength,pointDist);        
                    
        else
            % Add d(i,i+1) to lp:
            lengthFromPreviousSegment = lengthFromPreviousSegment + uMag;
        end
        
    end
   
    % Remove extra points 
    if length(xNew)>numPointsReqd
       xNew(numPointsReqd+1:end) = [];
       yNew(numPointsReqd+1:end) = [];
    end
   
end
%%  Algo # 1

if algo == 1
    lengthFromPreviousSegment = 0;
    nStar = 2;
    
    for i = 1:length(x)-1
        % Direction from i to i+1:
        u1 = x(i+1)-x(i);
        u2 = y(i+1)-y(i);
        uMag = sqrt(u1^2+u2^2);
        u1 = u1/uMag;
        u2 = u2/uMag;
        
        % Calculate number of points to add to this line segment:
        effectiveLength = uMag + lengthFromPreviousSegment;
        nPoints2Add = floor(effectiveLength*densityResultant);
        
        % Add points (x,y)_(i+1) = (x,y)_i + pointDist*(u1,u2)
        % Add first point which may be closer to (x,y)_i than pointDist
        if nStar<=numPointsReqd
            xStar = x(i)+ (pointDist-lengthFromPreviousSegment)*u1;
            yStar = y(i)+ (pointDist-lengthFromPreviousSegment)*u2;
            xNew(nStar) = xStar;
            yNew(nStar) = yStar;
            nPoints2Add = nPoints2Add -1;
            nStar = nStar + 1;
        end
        
        
        for n = 1:nPoints2Add
            if nStar<= numPointsReqd
                xStar = xNew(nStar-1)+ (pointDist)*u1;
                yStar = yNew(nStar-1)+ (pointDist)*u2;
                xNew(nStar) = xStar;
                yNew(nStar) = yStar;
                nStar = nStar + 1;
            end
        end
        
        % left over distance
        lengthFromPreviousSegment = mod(effectiveLength,pointDist);
    end
    
    
    if nStar == numPointsReqd
        % Direction from i to i+1:
        u1 = x(1)-x(end);
        u2 = y(1)-y(end);
        uMag = sqrt(u1^2+u2^2);
        u1 = u1/uMag;
        u2 = u2/uMag;
        
        
        % Add points (x,y)_(i+1) = (x,y)_i + pointDist*(u1,u2)
        % Add first point which may be closer to (x,y)_i than pointDist
        xStar = x(end)+ (pointDist-lengthFromPreviousSegment)*u1;
        yStar = y(end)+ (pointDist-lengthFromPreviousSegment)*u2;
        xNew(nStar) = xStar;
        yNew(nStar) = yStar;
    end
end

%%

end

function [Area, xbar, ybar] = geom(x,y)
% area and centroid for clockwise oriented curves
% x,y points must loop completely so last and first point are equal


% % Sample shape
% x =[
%     49.0000
%     55.0000
%     59.0000
%     71.0000
%     80.0000
%     90.0000
%     112.0000
%     129.0000
%     148.0000
%     151.0000
%     164.0000
%     161.0000
%     161.0000
%     157.0000
%     151.0000
%     138.0000
%     128.0000
%     113.0000
%     92.0000
%     74.0000
%     62.0000
%     54.0000
%     47.0000
%     49.0000];
% y =[
%     101.0000
%     115.0000
%     134.0000
%     146.0000
%     154.0000
%     161.0000
%     164.0000
%     161.0000
%     163.0000
%     150.0000
%     138.0000
%     125.0000
%     113.0000
%     99.0000
%     78.0000
%     74.0000
%     62.0000
%     54.0000
%     52.0000
%     61.0000
%     68.0000
%     74.0000
%     87.0000
%     96.0000];

% Triangle
% x = [0 1 0 0];
% y = [0 0 1 0];
% 
% offset = 1;
% x = [0 4 0 0]+offset;
% y = [0 0 2 0]+offset;

%plot(x,y)

% Circle
% t = 0:0.01:2*pi;
% r =  30;
% x = r*cos(t);
% y = r*sin(t);
% x(length(x)+1) = x(1);
% y(length(y)+1) = y(1);
% area = pi*(r^2);


%% Area Line Integrals (Non-Parameterized integrals) 
% A = \int x dy
nLineSegs = length(x)-1;
contInt = 0;
for i=1:nLineSegs
    
    if x(i+1) ~= x(i) && y(i+1) ~= y(i)
        % case 1
        % A = \int x dy
        m =  (y(i+1)-y(i))/(x(i+1)-x(i));
        A_c = (m/2)*(x(i+1)^2-x(i)^2);
        contInt = contInt + A_c;
    elseif x(i+1) == x(i)
        % case 2
        % x is constant 
        xC = x(i);
        A_c = xC*(y(i+1)-y(i));
        contInt = contInt + A_c;
    else
        % case 3
        % y is constant
        yC = y(i);
        %A_c = yC*(x(i)-x(i+1));
        A_c = 0; % since dy = 0
        contInt = contInt + A_c;
    end

end
Area = contInt;

%% x centroid line integral
% xbar = \int x^2/2 dy
nLineSegs = length(x)-1;
contInt = 0;
for i=1:nLineSegs
    
    if x(i+1) ~= x(i) && y(i+1) ~= y(i)
        % case 1
        % A = \int x dy
        m =  (y(i+1)-y(i))/(x(i+1)-x(i));
        x_c = (m/6)*(x(i+1)^3-x(i)^3);
        contInt = contInt + x_c ;
    elseif x(i+1) == x(i)
        % case 2
        % x is constant
        xC = x(i);
        x_c = (xC^2/2)*(y(i+1)-y(i));
        contInt = contInt + x_c ;
    else
        % case 3
        % y is constant
        yC = y(i);
        x_c = 0; % since dy = 0
        contInt = contInt + x_c ;
    end

end
xbar = contInt/Area;

%% y centroid line integral
% ybar = - \int y^2/2 dx
nLineSegs = length(x)-1;
contInt = 0;
for i=1:nLineSegs
    
    if x(i+1) ~= x(i) && y(i+1) ~= y(i)
        % case 1
        m =  (y(i+1)-y(i))/(x(i+1)-x(i));
        y_c = (-1/(6*m))*(y(i+1)^3-y(i)^3);
        contInt = contInt + y_c ;
    elseif x(i+1) == x(i)
        % case 2
        % x is constant
        xC = x(i);
        y_c = 0; % since dx = 0;
        contInt = contInt + y_c ;
    else
        % case 3
        % y is constant 
        yC = y(i);
        y_c = (-(yC^2)/2)*(x(i+1)-x(i));
        contInt = contInt + y_c ;
    end

end
ybar = contInt/Area;

% %%
% plot(x,y,'b-','lineWidth',2)
% hold on
% plot(x,y,'ko')
% hold on
% plot(xbar,ybar,'ko')
% hold on
% plot(xbar,ybar,'k+')

end

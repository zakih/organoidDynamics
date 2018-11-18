%% Load organoid data information 
% raw data saved as txt ' '-spearated .txt files
% Use to study data in Data\BoundaryCoordinates_basal &
% Data\BoundaryCoordinates_fgf2


clc
clear all
close all


filename = 'AKF_16_08_31_TGFbInhTL_'; % List of all image folders which have image series in them


oldFolder = cd('Data/TimeSeriesData/BoundaryCoordinates_basal');
allOrganoids = dir( [ '**/*' filename '*']);
allOrganoids = {allOrganoids.name}.';

numOrganoids = size(allOrganoids,1);
for i = 1:numOrganoids
    allOrganoids{i} = allOrganoids{i}(1:end-4);
end
cd(oldFolder);
basalOrganoids = cell(size(allOrganoids,1),2);
basalOrganoids(:,1) = allOrganoids;
basalOrganoids(:,2) = {'basal'};
numBasalOrganoids = numOrganoids;



oldFolder = cd('Data/TimeSeriesData/BoundaryCooridinates_fgf2');
allOrganoids = dir( [ '**/*' filename '*']);
allOrganoids = {allOrganoids.name}.';

numOrganoids = size(allOrganoids,1);
for i = 1:numOrganoids
    allOrganoids{i} = allOrganoids{i}(1:end-4);
end
cd(oldFolder);
fgf2Organoids = cell(size(allOrganoids,1),2);
fgf2Organoids(:,1) = allOrganoids;
fgf2Organoids(:,2) = {'fgf2'};
numFgf2Organoids = numOrganoids;


allOrganoids = vertcat(basalOrganoids,fgf2Organoids);
numOrganoids = size(allOrganoids,1);
clearvars i filename oldFolder



fileLog = {};  % organoid/image name, organoid type, folder, file name (time series data point)
dataDirectoryBasal = 'Data/TimeSeriesData/BoundaryCoordinates_basal/';
dataDirectoryFGF2 =  'Data/TimeSeriesData/BoundaryCooridinates_fgf2/';
for i = 1:numOrganoids
    % Switch to subdirectory of organoid i's folder
    organoidName  = allOrganoids{i,1};
    type = allOrganoids{i,2};
    if strcmp(type,'basal')==1
        currentOrganoidDirectory = strcat(dataDirectoryBasal,organoidName,'.tif');
        oldFolder = cd(currentOrganoidDirectory);
        timeSeriesi = dir('**/*.txt');
        cd(oldFolder)
        numDataPoints = size(timeSeriesi,1);
        tempCell = cell(numDataPoints,4);
        tempCell(:,1) = {organoidName};
        tempCell(:,2) = {type};
        tempCell(:,3) = {timeSeriesi.folder};
        tempCell(:,4) = {timeSeriesi.name}.';
        fileLog = vertcat(fileLog,tempCell);
    else
        currentOrganoidDirectory = strcat(dataDirectoryFGF2,organoidName,'.tif');
        oldFolder = cd(currentOrganoidDirectory);
        timeSeriesi = dir('**/*.txt');
        cd(oldFolder)
        numDataPoints = size(timeSeriesi,1);
        tempCell = cell(numDataPoints,4);
        tempCell(:,1) = {organoidName};
        tempCell(:,2) = {type};
        tempCell(:,3) = {timeSeriesi.folder};
        tempCell(:,4) = {timeSeriesi.name}.';
        fileLog = vertcat(fileLog,tempCell);
    end
end



fileLog(:,5) = fileLog(:,4);
for i = 1:size(fileLog,1)
    fileLog{i,5} = str2double(fileLog{i,5}(1:end-4));
end

[~, ii] = sortrows([fileLog(:,1) fileLog(:,5)],[1 2]);
fileLog = fileLog(ii,:);


clearvars ii timeSeriesi organoidName type tempCell i numTotalFilesMax dataDirectoryBasal dataDirectoryFGF2 currentOrganoidDirectory oldFolder numDataPoints




%% Load preprocessed data
load('zAllOrganoids.mat')
% Load signal processing data (from signalProcessing.m)
load('DTFT_X_shift_Organoids.mat')
load('DTFT_freq_Organoids.mat')


%% List of features extracted


nFiles  = length(fileLog);
numOutputs = 20;
allOrganoidFiles = cell(nFiles,numOutputs);
organoidNameList = allOrganoids(:,1);



% columns:
% 1.  organoid/image name
% 2.  treatment type
% 3.  folder
% 4.  file name
% 5.  time point
% 6.  perimeter
% 7.  area
% 8.  numModes
% 9.  ampMean90
% 10.  ampStd90
% 11.  aaHullRatio
% 12.  convex
% 13. concave
% 14. collinear
% 15. fractionAllCurvatureChanges
% 16. fractionConvConcChanges
% 17. Ixx
% 18. Iyy
% 19. Izz
% 20. formFactor




for file = 1:nFiles  % Loop over this
    %% Load a given file's x & y coordinate series
    disp(['File number: ' num2str(file) ]);
    organoidName = fileLog{file,1};
    organoidNum = find(strcmp(organoidNameList, organoidName));
    disp(['Organoid number: ' num2str(organoidNum) ]);
    type = fileLog{file,2};
    ii = find(contains(fileLog(:,1),organoidName));
    numTimePoints = max([fileLog{ii,5}]);
    
    filename = strcat(fileLog{file,3},'/',fileLog{file,4});
    timePoint = fileLog{file,5}+1;

     % Load preprocessed(x,y) pairs as complex numbers: z = x + iy
    z = zAll{timePoint,organoidNum}';
    x = real(z);
    y = imag(z);
        

    
    
    
    %% Arc length and perimeter
    nPoints = length(x);
    distList = zeros(nPoints,1);
    for i=1:nPoints-1
        distList(i) = sqrt( (x(i+1) - x(i) )^2 + (y(i+1)-y(i))^2 );
    end
    distList(end) = sqrt( (x(1) - x(end) )^2 + (y(1)-y(end))^2 );
    perimeter = sum(distList);
    allOrganoidFiles{file,6} = perimeter;
    
    arcLength = zeros(nPoints+1,1);
    for i=2:nPoints+1
        arcLength(i) = arcLength(i-1) + distList(i-1);
    end
    %distList = [0 ;distList];
    
    
    
    %% Calculate centroid from (x,y)
    % NEED TO FIX CENTROID METHOD
    x(end+1) = x(1);
    y(end+1) = y(1);
    [area, xbar, ybar] = geom(x,y);
    allOrganoidFiles{file,7} = area;
    
    % Plot in original coordinate system
    % figure()
    % plot(x,y,'k-','lineWidth',2)
    % axis square
    % xlabel('x')
    % ylabel('y')
    % hold on
    % plot(xbar,ybar,'k+','MarkerSize',10)
    % hold on
    % plot(xbar,ybar,'ko','MarkerSize',10)
    
    
    
    % Recenter coordinates according to centroid, change sign if necessary
    xCartesian = x;
    yCartesian = y; % to get correct orientation
    
    xCart_center = xCartesian - xbar;
    yCart_center = yCartesian - (1*ybar);
    
    %Isolated image
    % figure()
    % set(gcf,'position',[100,100,700,700])
    % plot(xCart_center,yCart_center,'k-','lineWidth',2)
    % hold on
    % plot(xCart_center,yCart_center,'ro')
    % % Label point #'s:
    % % for i= 1:length(xCart_center)
    % % text(xCart_center(i),yCart_center(i),num2str(i))
    % % end
    %
    % %lim = 50;
    % %axis([-lim,lim,-lim,lim])
    % axis square
    % xlabel('X')
    % ylabel('Y')
    % hold on
    % plot(0,0,'k+','MarkerSize',10)
    % hold on
    % plot(0,0,'ko','MarkerSize',10)
    
    
 
    
    
    
    %% Fourier analysis statistics
   
    Nsample  = length(z);
    Xshifted = XAll{timePoint,organoidNum}';

    %         fs = 6; % samples per cycle (unit length along contour)
    %         df = fs/N;
    %         freq = (0:df:(fs-df)) - (fs-mod(N,2)*df)/2;
    %         freq= freq';

    % # modes needed to get 90% of energy
    energyMax = 0.9;
    xOriginal = ifftshift(Xshifted);
    XmagOriginal = abs(xOriginal);
    XmagOriginalSorted = -1*sort(-XmagOriginal);
    
    totalEnergy = sum(XmagOriginalSorted(2:end));
    amplitudeCDF = zeros(length(XmagOriginalSorted)-1,1);
    for i = 1:length(amplitudeCDF)
        amplitudeCDF(i) = sum(XmagOriginalSorted(2:i+1));
    end
    [~, numMode90] = min( abs(amplitudeCDF-energyMax*totalEnergy));
%     numMode90=numMode90+1;
    ampMean90 = mean(XmagOriginalSorted(2:numMode90));
    ampStd90 = std(XmagOriginalSorted(2:numMode90));
    
    
    % Store # modes needed to get 90% of energy
    allOrganoidFiles{file,8} = numMode90;
    allOrganoidFiles{file,9} = ampMean90;
    allOrganoidFiles{file,10} = ampStd90;
    

    
    
    
    %% Local concavity
    % Classify points as convex/concave
    % https://en.wikipedia.org/wiki/Curve_orientation8
    
    
    % 1. Contour orientation
    % Convex Hull
    xLoopHull = xCart_center;
    yLoopHull = yCart_center;
    % Returns indices of points on convex hull, in counter-clockwise order
    k = convhull(xLoopHull,yLoopHull);
    % Recover hull indices in actual order of contour
    kOriginalOrder = k;
    kOriginalOrder(2:end-1) = sort(kOriginalOrder(2:end-1));
    k = kOriginalOrder;
    % Generate convex hull points
    xLoopHull = xLoopHull(k);
    yLoopHull = yLoopHull(k);
    [areaHull, ~, ~] = geom(xLoopHull,yLoopHull);
    aaHullRatio = area/areaHull;
    allOrganoidFiles{file,11} = aaHullRatio;
    % record orinetation (-1: clockwise, +1: counter-clockwise)
    detSign = getDetSign(xLoopHull,yLoopHull);
    contourOrientation = 1;
    if any(detSign<0)
        contourOrientation = -1;
    end
    

    
    % 2. local determinant
    xLoop = xCart_center;
    yLoop = yCart_center;
    detSign = getDetSign(xLoop,yLoop);
    % Percent convexity
    if contourOrientation == 1
        % Counter-clockwise contour
        concave = find(detSign < 0 );
        collinear = find(detSign == 0);
        convex = find(detSign > 0);
    else
        % Clockwise contour
        convex = find(detSign < 0 );
        collinear = find(detSign == 0);
        concave = find(detSign > 0);
    end
    percentConvex = length(convex)/length(xLoop);
    percentConcave = length(concave)/length(xLoop);
    percentCollinear = length(collinear)/length(xLoop);
    
    allOrganoidFiles{file,12} = percentConvex;
    allOrganoidFiles{file,13} = percentConcave;
    allOrganoidFiles{file,14} = percentCollinear;
    
    
    
    %%  Changes in convexity along contour
    detSignCoded = zeros(length(detSign),1);
    % 1: convex
    % 2: collinear
    % 3: concave
    for i=1:length(detSign)
        if contourOrientation == 1
            % Counter-clockwise contour
            if detSign(i) < 0
                % concave
                detSignCoded(i) = 3;
            elseif detSign(i) == 0
                %collinear
                detSignCoded(i) = 2;
            else
                % convex
                detSignCoded(i) = 1;
            end
        else
            % Clockwise contour
            if detSign(i) < 0
                % convex
                detSignCoded(i) = 1;
            elseif detSign(i) == 0
                %collinear
                detSignCoded(i) = 2;
            else
                % concave
                detSignCoded(i) = 3;
            end
        end
    end
    signChange = zeros(3,3);
    % Local curvature transition matrix:
    %           convex | collinear | concave
    % convex    | S_11 |    S_12   |  S_13  |
    % collinear | S_21 |    S_22   |  S_23  |
    % concave   | S_31 |    S_32   |  S_33  |
    for i=2:length(detSignCoded)
        if detSignCoded(i-1) == 1
            if detSignCoded(i) == 1
                signChange(1,1) = signChange(1,1)+1;
            elseif detSignCoded(i) ==2
                signChange(1,2) = signChange(1,2)+1;
            else
                signChange(1,3) = signChange(1,3)+1;
            end
        elseif detSignCoded(i-1) ==2
            if detSignCoded(i) == 1
                signChange(2,1) = signChange(2,1)+1;
            elseif detSignCoded(i) ==2
                signChange(2,2) = signChange(2,2)+1;
            else
                signChange(2,3) = signChange(2,3)+1;
            end
        else
            if detSignCoded(i) == 1
                signChange(3,1) = signChange(3,1)+1;
            elseif detSignCoded(i) ==2
                signChange(3,2) = signChange(3,2)+1;
            else
                signChange(3,3) = signChange(3,3)+1;
            end
        end
    end
    
    % Fraction of all local curvature changes (not along diagonal of S)
    fractionAllCurvatureChanges = 1- sum(trace(signChange))/sum(signChange(:));
    allOrganoidFiles{file,15} = fractionAllCurvatureChanges;
    % Fraction of convexity-concavity changes:
    fractionConvConcChanges = (signChange(1,1) + signChange(1,3) + signChange(3,1) + signChange(3,3))/sum(signChange(:));
    allOrganoidFiles{file,16} = fractionConvConcChanges;
    
    
    %% Second moment of Area
    
    % Function takes *centered* (x,y) *WITHOUT* first point repeated in loop form
    [Ixx,Iyy,Izz] = getPmoi(xCart_center,yCart_center);
    allOrganoidFiles{file,17} = Ixx;
    allOrganoidFiles{file,18} = Iyy;
    allOrganoidFiles{file,19} = Izz;
    
    % Form factor
    allOrganoidFiles{file,20} = perimeter^2/(area*4*pi);
end



%% Save Output
allOrganoidFiles(:,1:5) = fileLog;
allData = cell2table(allOrganoidFiles);
writetable(allData,'allOrganoidFeatures.csv','Delimiter',',','QuoteStrings',true)

%%
save('allOrganoidFiles.mat','allOrganoidFiles')




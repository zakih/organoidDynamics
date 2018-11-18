% Scale, Smooth, resample all contours
% Scale by scalingFactor below
% Smooth 90-degree edges in original data
% Resample at equal distances along contour
% Save all new contours as .mat file

clc
clear all
close all

% Preprocessing options:
scalingFactor = 1; % No scaling
smooth = 1; % 1: smooth, 0: no smooth

nOrganoidsTotal = 39;


%%
% Load saved image (x,y) data saved as txt ' '-spearated .txt files
% Use to study data in Data\BoundaryCoordinates_basal &
% Data\BoundaryCoordinates_fgf2
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



numTotalFilesMax = 10487;
fileLog = cell(numTotalFilesMax,4); % organoid/image name, organoid type, folder, file name (time series data point)
fileLog = {};
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




%%

zAll = cell(263,nOrganoidsTotal);
densityHistory = zeros(263,nOrganoidsTotal);


for o = 1:nOrganoidsTotal
    organoidNum = o; % i =1,...,nOrganoidsTotal
    disp(['Organoid number: ' num2str(o) ]);
    organoidName = allOrganoids{organoidNum,1};
    imageIndices = find(contains(fileLog(:,1),organoidName)); % index of all organoid i's images
    numTimePoints = length(imageIndices);
    type = fileLog{imageIndices(1),2};
%     perimeterList = zeros(numTimePoints,1);
%     areaList = zeros(numTimePoints,1);
    xyCell = cell(numTimePoints,2); % Store all x in first column, y in second column of cell
    
    for im = 1:numTimePoints
        i = imageIndices(im);
        filename = strcat(fileLog{i,3},'/',fileLog{i,4});
        delimiter = ' ';
        formatSpec = '%f%f%[^\n\r]';
        fileID = fopen(filename,'r');
        dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter,  'ReturnOnError', false);
        fclose(fileID);
        
        x = dataArray{:, 1};
        y = dataArray{:, 2};
        
        
        scaling = scalingFactor; % micrometers/px
        x = x*scaling;
        y = y*scaling;
        
        if smooth == 1
            % Interpolate between original (x,y) points to smooth 90 edges
            % Otherwise convexity analysis will be wrong due to all 90 degree edges
            % Smooth (x,y) data to get correct perimeter and convexity analysis
            xSmooth = zeros(length(x),1);
            ySmooth = zeros(length(y),1);
            for i=1:length(xSmooth)-1
                xSmooth(i) = (x(i)+x(i+1))/2;
                ySmooth(i) = (y(i)+y(i+1))/2;
            end
            xSmooth(length(xSmooth)) = (x(length(x))+x(1))/2;
            ySmooth(length(ySmooth)) = (y(length(y))+y(1))/2;
            x=xSmooth;
            y=ySmooth;
        end
        
        
        % Arc length and perimeter
        nPoints = length(x);
        distList = zeros(nPoints,1);
        for i=1:nPoints-1
            distList(i) = sqrt( (x(i+1) - x(i) )^2 + (y(i+1)-y(i))^2 );
        end
        distList(end) = sqrt( (x(1) - x(end) )^2 + (y(1)-y(end))^2 );
%         perimeter = sum(distList);
%         perimeterList(im) = perimeter;
        
        arcLength = zeros(nPoints+1,1);
        for i=2:nPoints+1
            arcLength(i) = arcLength(i-1) + distList(i-1);
        end
        %distList = [0 ;distList];
        
        
        
        % Calculate centroid from (x,y)
        checkX = x(1)==x(end);
        checkY = y(1)==y(end);
        if checkX == 0 || checkY ==0
            x(end+1) = x(1);
            y(end+1) = y(1);
        end
        [~, xbar, ybar] = geom(x,y);
%         areaList(im) = Area;
        
        % Recenter coordinates according to centroid, change sign if necessary
        xCartesian = x;
        yCartesian = y; % to get correct orientation
        
        xCart_center = xCartesian - xbar;
        yCart_center = yCartesian - (1*ybar);
        
        xyCell(im,1) = {xCart_center};
        xyCell(im,2) = {yCart_center};
    end
    
    
    
    %% Create evenly spaced points along curve
    
    xyCellNew = cell(numTimePoints,2); % Store all x in first column, y in second column of cell
    for im = 1:numTimePoints
        
        x = xyCell{im,1};
        y = xyCell{im,2};
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
        distList = [0 ;distList];
        
        
        %  Number of points per unit length based on minimum distance between
        %  points
%         density = ceil(1/min(distList(1:end-1)));
        density = ceil(1/min(distList(distList>0)));
        densityHistory(im,o) = density;
        [xNew, yNew,densityMinCheck] = resampleWithDensity(x,y,density);
        
        
        % Store scaled,smoothed,resampled contour coordinates in complex
        % form
        zAll(im,o) = {xNew+1i*yNew};
        
    end
    
    

    
end






%% save organoid data
save('zAllOrganoids.mat','zAll')


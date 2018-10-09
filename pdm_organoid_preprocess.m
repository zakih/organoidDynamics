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


% Organoid names
nFiles  = length(fileLog);
numOutputs = 19;
organoidNameList = allOrganoids(:,1);



%% Test & Plot organoid 'o' at time 't'
% 
% close all
% organoidNum = 1;
% timePoint = 1;
% % 8,254 had too many points
% organoidName = allOrganoids{organoidNum,1};
% type = allOrganoids{organoidNum,2};
% 
% 
% z = zAll{timePoint,organoidNum}';
% x = real(z);
% y = imag(z);
% x(end+1) = x(1);
% y(end+1) = y(1);
% 
% figure()
% plot(z,'k--','linewidth',2)
% hold on
% plot(x,y,'r-')
% str = {organoidName,['Type: ' type]};
% title(str,'interpreter','none')
% 
% % Resample at constant # of points
% numPointsReqd = 5000;
% [xNew, yNew,densityResultant] = resampleWithConstantNumPoints(x,y,numPointsReqd);
% 
% figure()
% plot(z,'r-','linewidth',2)
% hold on
% plot(xNew,yNew,'b.')
% str = {organoidName,['Type: ' type]};
% title(str,'interpreter','none')
% axis square 
% for i = 1:length(xNew)
%    text(xNew(i),yNew(i),num2str(i)) 
% end
% % % Check arc length 
% % nPoints = length(xNew);
% % distList = zeros(nPoints,1);
% % for i=1:nPoints-1
% %     distList(i) = sqrt( (xNew(i+1) - xNew(i) )^2 + (yNew(i+1)-yNew(i))^2 );
% % end
% % distList(end) = sqrt( (xNew(1) - xNew(end) )^2 + (yNew(1)-yNew(end))^2 );

%% Make Q-matrix of contours: numTimePoints x numOrganoids array of contour vectors 'q'

numTimePoints = 261;
numOrganoids = 40;

numPointsArray = zeros(numTimePoints,numOrganoids);
numPointsReqd = 5000;
qArray = cell(numTimePoints,numOrganoids);


for organoidNum = 1:numOrganoids
    for timePoint = 1:numTimePoints
        
        z = zAll{timePoint,organoidNum}';
        x = real(z);
        y = imag(z);
        % Repeat first point for resampling algorithm to work
        x(end+1) = x(1);
        y(end+1) = y(1);
        
        %% Resample at constant # of points
        
        % current number of points in contour
        numPointsArray(timePoint,organoidNum) = length(x);
        
        % Resample at constant # of points
        [xNew, yNew,densityResultant] = resampleWithConstantNumPoints(x,y,numPointsReqd);
        
        %% Calculate centroid from (x,y)
        xNewTemp = [xNew;xNew(1)];
        yNewTemp = [yNew;yNew(1)];
        [area, xbar, ybar] = geom(xNewTemp,yNewTemp);
        
        % Recenter coordinates according to centroid, change sign if necessary
        xCart_center = xNew - xbar;
        yCart_center = yNew - ybar;
        
        
        %% Store in Q-amatrix
        q_t_o = [xCart_center;yCart_center];
        qArray{timePoint,organoidNum} = q_t_o;
        
    end
end



%% Q-cell to Q-matrix

lengthQsample = 2*numPointsReqd;
qMatrix = zeros(numTimePoints,numOrganoids,lengthQsample);
for organoidNum = 1:numOrganoids
    for timePoint = 1:numTimePoints
        qMatrix(timePoint,organoidNum,:) = qArray{timePoint,organoidNum};
    end
end

%% Save Q-matrix

save('qArray.mat','qArray')
save('qMatrix.mat','qMatrix')
%%
figure()
heatmap(numPointsArray)








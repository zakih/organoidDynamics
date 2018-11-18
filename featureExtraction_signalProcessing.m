%% Load organoid data information 
clc
clear all
close all

% Load saved image (x,y) data saved as txt ' '-spearated .txt files
% Use to study data in Data\BoundaryCoordinates_basal &
% Data\BoundaryCoordinates_fgf2

nOrganoidsTotal = 39;
 

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




%% Loop over all data: FFT X[n]=DTFT(z)

% Load z = x + iy for all data
load('zAllOrganoids.mat')
XAll = cell(263,nOrganoidsTotal);
fAll = cell(263,nOrganoidsTotal);
for o = 1:nOrganoidsTotal
    organoidNum = o; % i =1,...,nOrganoidsTotal
    disp(['Organoid number: ' num2str(o) ]);
    organoidName = allOrganoids{organoidNum,1};
    imageIndices = find(contains(fileLog(:,1),organoidName)); % index of all organoid i's images
    numTimePoints = length(imageIndices);
    for im = 1:numTimePoints
        
        % Load (x,y) pairs as complex numbers: z = x + iy
        z = zAll{im,o};
        Nsample  = length(z);
        X = fft(z,Nsample);
        
        % frequencies by number
        freq =(-Nsample/2:Nsample/2-1)/Nsample; %DFT Sample points
%         amplitudes = abs(fftshift(X)); % should we divide by Nsample?
        xShift = fftshift(X);
        
        XAll(im,o) = {xShift};
        fAll(im,o) = {freq};
    end
end

save('DTFT_X_shift_Organoids.mat','XAll')
save('DTFT_freq_Organoids.mat','fAll')















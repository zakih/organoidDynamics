close all
clear all
clc



%% 1. Load data

load('qMatrix.mat')
mkdir('pdm_data')
savdir = 'D:\Dropbox\Research\Projects\JH_Cell_Shape\Paper1\JHCell_final_reproduction\pdm_data';

for organoidNum = 1:40
    disp(organoidNum)
    % Single organoid sample (multiple time points)
    qMatrixOrganoid = qMatrix(:,organoidNum,:);
    qMatrixOrganoid = squeeze(qMatrixOrganoid)';
    % remove repeated first point
    qMatrixOrganoid = [qMatrixOrganoid(1:5000,:);qMatrixOrganoid(5001:10000,:)];
    numTimePoints = 261;

    
    
    %% 2. Mean shape
    M = numTimePoints; % number of samples
    qbar = (1/M) * sum(qMatrixOrganoid,2);
    
    %% 3. Compute covariance matrix, S
    
    numPointsPerContour = 5000;
    sCovMat = zeros(numPointsPerContour*2,numPointsPerContour*2);
    for i = 1:numTimePoints
        q_i = qMatrixOrganoid(:,i);
        sCovMat = sCovMat + (q_i - qbar)*(q_i-qbar)';
    end
    sCovMat = sCovMat/(numTimePoints-1);
    
    %% 4. Eigen decomposition
    
    numLargestEigs = 10;
    [V, D] = eigs(sCovMat,numLargestEigs);
    eigenValues = diag(D);
    varExplained = eigenValues/sum(eigenValues);
    
    %% 5. Matrix of top 't' eigenvectors
    t= 5;
    U = V(:,1:t);
    
    % Save modes and mean shape
    fileName = ['U_' num2str(organoidNum) '.mat'];
    save(fullfile(savdir,fileName),'U');
    fileName = ['qbar_' num2str(organoidNum) '.mat'];
    save(fullfile(savdir,fileName),'qbar');
    fileName = ['eigenVals_' num2str(organoidNum) '.mat'];
    save(fullfile(savdir,fileName),'eigenValues');
    
    
end




%% Eigendecomposition statistics

varExplained_1stMode = zeros(40,1);
varExplained_3Modes = zeros(40,1);
auc_5Modes = zeros(40,1);
for organoidNum = 1:40
    
    fileName = ['eigenVals_' num2str(organoidNum) '.mat'];
    load(fileName)
    varExplained = eigenValues/sum(eigenValues);
    
    % Variance explained by 1st mode
    varExplained_1stMode(organoidNum) = varExplained(1);
    % Cumulative variance in first 3 modes
    varExplained_3Modes(organoidNum) = sum(varExplained(1:3));
    % Area under cumulative variance curve upto 3 modes
    cumVar = cumsum(varExplained);
    auc_5Modes(organoidNum) = trapz(cumVar(1:5));
    
end
dlmwrite('varExplained_1stMode.csv',varExplained_1stMode)
dlmwrite('auc_5Modes.csv',auc_5Modes)
dlmwrite('varExplained_3Modes.csv',varExplained_3Modes)




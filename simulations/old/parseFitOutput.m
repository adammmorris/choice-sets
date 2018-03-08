%% parseFitOutput
% Parses the output of the fitting process.

%% Inputs
% savePath: path to folder Params_SubjX estimates
% dataPath: path to .mat file w/ actualParams (optional)

%% Outputs
% paramEstimates: estimated params for each subject (with negLL in first
% column)
% goodSubjects: tells which subjects we got parameter estimates for
% corrs: correlations between actual & estimated params (if dataPath was
% inputted)

function [paramEstimates, goodSubjects, corrs] = parseFitOutput(savePath, whichSubjects, whichParams, numExtraVars, dataPath, realData)

numSubjects = length(whichSubjects);
numFreeParams = length(whichParams);
paramEstimates = zeros(numSubjects, numFreeParams + numExtraVars);
goodSubjects = false(numSubjects,1);

for whichSubj_ind = 1:numSubjects    
    whichSubj = whichSubjects(whichSubj_ind);
    name = [savePath num2str(whichSubj) '.txt'];
    if exist(name, 'file')
        paramEstimates(whichSubj_ind, :) = csvread(name);
        goodSubjects(whichSubj_ind) = true;
    end
end

%% Get correlations
corrs = zeros(numFreeParams, 1);
if ~realData
    load(dataPath);
    for i = 1:numFreeParams
        temporary = corrcoef(actualParams(goodSubjects, whichParams(i)), paramEstimates(goodSubjects, i + numExtraVars));
        corrs(i) = temporary(2,1);
    end
end
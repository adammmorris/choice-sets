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

function [paramEstimates, goodSubjects, corrs] = parseFitOutput(savePath, numSubjects, numStarts, whichParams, numExtraVars, dataPath)

numFreeParams = length(whichParams);
paramEstimates = zeros(numSubjects, numFreeParams + numExtraVars);
goodSubjects = false(numSubjects,1);

for whichSubj = 1:numSubjects    
    if allStarts
        name = [savePath num2str(whichSubj) '.txt'];
        if exist(name, 'file')
            paramEstimates(whichSubj, :) = csvread(name);
            goodSubjects(whichSubj) = true;
        end
    else
        paramEstimates_subj = zeros(numStarts, numFreeParams + numExtraVars);

        for whichStart = 1:numStarts
            taskid = sub2ind([numStarts, numSubjects], whichStart, whichSubj);

            name = [savePath num2str(taskid) '.txt'];
            if exist(name, 'file')
                paramEstimates_subj(whichStart, :) = csvread(name);
                goodSubjects(whichSubj) = true;
            end
        end

        [~, bestStart] = min(paramEstimates_subj(:, 1));
        paramEstimates(whichSubj, :) = paramEstimates_subj(bestStart, :);
    end
end

%% Get correlations
if exist('dataPath', 'var')
    load(dataPath);
    corrs = zeros(numFreeParams, 1);
    for i = 1:numFreeParams
        temporary = corrcoef(actualParams(goodSubjects, whichParams(i)), paramEstimates(goodSubjects, i + numExtraVars));
        corrs(i) = temporary(2,1);
    end
end
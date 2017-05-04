%% fitResults
% For a given dataset, get all the model fitting results, and compare
% models.

envName = 'wg_v3';
simsName = 'real2';
realData = true;

whichEnv = ['env/' envName '.mat'];
main = ['fitting/' envName '/' simsName];
datapath = [main '/sims.mat'];

load(whichEnv);
load(datapath);

numWords = envInfo{1};
numTrials = envInfo{2};

numSubjects = length(subjMarkers);
numChoices = zeros(numSubjects, 1);
LLs_chance = zeros(numSubjects, 1);
for subj = 1:numSubjects
    if subj < length(subjMarkers)
        index = subjMarkers(subj):(subjMarkers(subj + 1) - 1);
    else
        index = subjMarkers(subj):length(choice);
    end
    
    numChoices(subj) = length(index);
    LLs_chance(subj) = log(1 / sum(recalled(subj, :))) * length(index);
end
%numChoices = repmat(numTrials, numSubjects, 1);
%LLs_chance = log((1 / numWords)) * numChoices;

modelNames_all = {'mixture-mf-mb', 'mixture-mf', 'mixture-mb', 'random', ...
    'cs-mf-mb', 'cs-mf', 'cs-mb', 'cs-rand'};
modelParams_all = {[1 -1 0 -1 -1], [1 -1 0 1 0], [1 -1 0 0 1], [1 0 0 0 0], ...
    [-1 -1 -1 -1 -1], [-1 -1 -1 1 0], [-1 -1 -1 0 1], [-1 0 -1 0 0]};

whichParams_all = cell(length(modelParams_all), 1);
for j = 1:length(modelParams_all)
    whichParams_all{j} = find(modelParams_all{j} == -1);
end
%whichParams_all = {1:3, 1:2, 1:2, 1:2, 1, 2:4, 2:3, [2 4], 2, []};

whichModels = 1:length(modelNames_all);

modelNames = modelNames_all(whichModels);
whichParams = whichParams_all(whichModels);

numModels = length(modelNames);
paramEstimates = cell(numModels, 1);
negLLs = zeros(numSubjects, numModels);
goodSubjects = true(numSubjects, 1);
corrs = cell(numModels);

numExtraVars = 3; % [post LL LME]

for i = 1:numModels
    savePath = [main '/fit_' modelNames{i} '/'];
    [paramEstimates{i}, goodSubjects_cur, corrs{i}] = ...
        parseFitOutput(savePath, numSubjects, whichParams{i}, numExtraVars, datapath, realData);
    goodSubjects = goodSubjects & goodSubjects_cur; % drop any subjects we didn't get
end

for i = 1:numModels
    paramEstimates{i}(~goodSubjects, :) = [];
    paramEstimates{i}(paramEstimates{i} == -Inf) = -realmax;
    paramEstimates{i}(paramEstimates{i} == Inf) = realmax;
end

[params, details] = generateParamsCell(paramEstimates{:});

%% Model comparison
compareModels_bayes(params, details, 5, numChoices, LLs_chance(goodSubjects));
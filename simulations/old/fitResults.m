%% fitResults
% For a given dataset, get all the model fitting results, and compare
% models.

load('env/env_30_15_5_2.mat');

numWords = envInfo{1};
numTrials = envInfo{2};

numSubjects = 200;
numChoices = repmat(numTrials, numSubjects, 1);
LLs_chance = log((1 / numWords)) * numChoices;

main = 'fitting/env_30_15_5_2/sims_mixture';
savePaths = {[main '/fit_preferred/params/'], ...
    [main '/fit_randcs/params/'], ...
    [main '/fit_mixture/params/'], ...
    [main '/fit_puremb/params/'], ...
    [main '/fit_puremf/params/'], ...
    [main '/fit_random/params/']};
numFreeParams = {1, 0, 2, 1, 1, 0};

numModels = size(savePaths, 2);
paramEstimates = cell(numModels, 1);
negLLs = zeros(numSubjects, numModels);
goodSubjects = true(numSubjects, 1);
corrs = cell(numModels);

datapath = [main '/sims.mat'];
numExtraVars = 3; % [post LL det(hessian)]

for i = 1:numModels
    [paramEstimates{i}, goodSubjects_cur, corrs{i}] = parseFitOutput(savePaths{i}, numSubjects, numFreeParams{i}, numExtraVars, datapath);
    goodSubjects = goodSubjects & goodSubjects_cur; % drop any subjects we didn't get
end

for i = 1:numModels
    paramEstimates{i}(~goodSubjects, :) = [];
    paramEstimates{i}(paramEstimates{i} == -Inf) = -realmax;
end

[params, details] = generateParamsCell(paramEstimates{:});

%% Model comparison
compareModels_bayes(params, details, 1, numChoices, LLs_chance(goodSubjects));
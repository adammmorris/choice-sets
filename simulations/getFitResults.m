%% fitResults
% For a given dataset, get all the model fitting results, and compare
% models.

datapath = 'fitting/value/v3_2/output.mat';
simspath = 'fitting/value/v3_2/sims.mat';
load(datapath);
load(simspath);

subjlist = 1:length(subjMarkers);

numSubjects = length(subjlist);
numChoices = zeros(numSubjects, 1);
LLs_chance = zeros(numSubjects, 1);
for subj_ind = 1:length(subjlist)
    subj = subjlist(subj_ind);
    if subj < length(subjMarkers)
        index = subjMarkers(subj):(subjMarkers(subj + 1) - 1);
    else
        index = subjMarkers(subj):length(choice);
    end
    
    numChoices(subj_ind) = length(index);
    LLs_chance(subj_ind) = log(1 / sum(recalled(subj, :))) * length(index);
end

modelNames_all = {'mixture-mf-mb', 'mixture-mf', 'mixture-mb', 'random', ...
    'cs-mf-mb', 'cs-mf', 'cs-mb', 'cs-rand', ...
    'cs-amf-mb', 'cs-amf', 'mixture-amf-mb', 'mixture-amf', ...
    'cs-rmf-mb', 'cs-free', 'cs-mf-mb-poss', 'cs-mb-poss'};
modelParams_all = {[1 -1 0 -1 -1 0 0], [1 -1 0 1 0 0 0], [1 -1 0 0 1 0 0], [1 0 0 0 0 0 0], ...
    [-1 -1 -1 -1 -1 0 0], [-1 -1 -1 1 0 0 0], [-1 -1 -1 0 1 0 0], [-1 0 -1 0 0 0 0], ...
    [-1 -1 -1 -1 -1 0 1], [-1 -1 -1 1 0 0 1], [1 -1 0 -1 -1 0 1], [1 -1 0 1 0 0 1], ...
    [-1 -1 -1 -1 -1 0 2], [-1 1 -1 -1 -1], [-1 -1 -1 -1 -1 -1 0], [-1 -1 -1 0 -1 -1]};

whichParams_all = cell(length(modelParams_all), 1);
for j = 1:length(modelParams_all)
    whichParams_all{j} = find(modelParams_all{j} == -1);
end

whichModels = [1 5 6];

modelNames = modelNames_all(whichModels);
whichParams = whichParams_all(whichModels);
numModels = length(modelNames);

details = zeros(numSubjects, numModels, 4);
for i = 1:numModels
    model = whichModels(i);
    details(:, i, 1) = results{i}(:, 1);
    details(:, i, 2) = results{i}(:, 2);
    details(:, i, 3) = results{i}(:, 3);
    details(:, i, 4) = results{i}(:, 4);
end

%% Model comparison
compareModels_bayes(optParams, details, 2, LLs_chance);
%hist(optParams{5}(:,4))
%sprintf('%.2d,', paramEstimates{5}(:,7))
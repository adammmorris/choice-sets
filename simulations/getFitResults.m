%% fitResults
% For a given dataset, get all the model fitting results, and compare
% models.

datapath = 'fitting/value/v3_2/output_test.mat';
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

whichModels = 1:6;
numModels = length(whichModels);

details = zeros(numSubjects, numModels, 4);
for i = 1:numModels
    model = whichModels(i);
    details(:, i, 1) = results{model}(:, 1);
    details(:, i, 2) = results{model}(:, 2);
    details(:, i, 3) = results{model}(:, 3);
    details(:, i, 4) = results{model}(:, 4);
end

%% Model comparison
compareModels_bayes(optParams(whichModels), details, 2, LLs_chance);
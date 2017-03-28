clearvars

%% Parameters
numAgents = 300;
envName = 'wg_v2';
whichEnv = ['env/' envName '.mat'];
copyOver = false;

load(whichEnv);
nWords = envInfo{1};
exposures_tr = envInfo{5};
rewards_tr = envInfo{7};

% Set up their parameters
actualParams = zeros(numAgents, 5); % [nToEval temp w_MB]

for thisSubj = 1:numAgents
    nToEval = randi([2 5]);
    temp = gamrnd(4.82, .88);
    weights = rand(1,3); % w_NE w_MF w_MB
    weights = weights / sum(weights);
    
    actualParams(thisSubj,:) = [nToEval temp weights];
end

modelNames_all = {'cs-expose-mf', 'cs-expose', 'cs-mf', 'cs-rand', ...
    'mixture-all', 'mixture-expose-mb', 'mixture-mf-mb', 'mixture-mb'};
modelParams_all = {
    [actualParams(:, 1:3), 1 - actualParams(:, 3), zeros(numAgents, 1)], ...
    [actualParams(:, 1:2) ones(numAgents, 1) zeros(numAgents, 2)], ...
    [actualParams(:, 1:2) zeros(numAgents, 1) ones(numAgents, 1) zeros(numAgents, 1)], ...
    [actualParams(:, 1) zeros(numAgents, 4)], ...
    [ones(numAgents,1) actualParams(:, 2:5)], ...
    [ones(numAgents,1), actualParams(:, 2:3), zeros(numAgents, 1), 1 - actualParams(:, 3)], ...
    [ones(numAgents,1) actualParams(:, 2) zeros(numAgents, 1) actualParams(:, 4) 1-actualParams(:,4)], ...
    [ones(numAgents,1) actualParams(:, 2) zeros(numAgents, 2) ones(numAgents, 1)]};

whichModels = [3 4 7];

modelNames = modelNames_all(whichModels);
modelParams = modelParams_all(whichModels);

for m = 1:length(modelNames)
    [results, results_long] = runModel(envInfo, modelParams{m}, true);
    choice = results(:, 3); subjMarkers = getSubjMarkers(results(:,1));
    save(['results/' envName '/' modelNames{m} '.mat'], 'choice', 'exposures_tr', 'rewards_tr', 'subjMarkers', 'actualParams');

    if copyOver
        save(['fitting/' envName '/sims_' modelNames{m} '/sims.mat'], 'choice', 'exposures_tr', 'rewards_tr', 'subjMarkers', 'actualParams');
    end
    
    df = array2table(results_long, 'VariableNames', {'Subj', 'Trial', 'OptionID', 'Choice', 'numExposures', 'MFval', 'MBval', 'nPrevChosen'});
    writetable(df, ['results/' envName '/' modelNames{m} '.csv']);
end
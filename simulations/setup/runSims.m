%clearvars

%% Parameters
numAgents = 120;

load(whichEnv);
nWords = envInfo{1};
rewards_tr = envInfo{3};

% Set up their parameters
% actualParams = zeros(numAgents, 5); % [nToEval temp w_MB]
% 
% for thisSubj = 1:numAgents
%     nToEval = randi([2 4]);
%     temp = gamrnd(4.5, 1);
%     temp2 = gamrnd(4.5, 1);
%     w_MF = rand();
%     %w_MF = 1;
%     
%     actualParams(thisSubj,:) = [nToEval temp temp2 w_MF 1-w_MF];
% end
% 

modelNames_all = {'mixture-mf-mb', 'cs-mf-mb'};
modelParams_all = {
    [], ...
    actualParams
};

whichModels = 2;

modelNames = modelNames_all(whichModels);
modelParams = modelParams_all(whichModels);

for m = 1:length(modelNames)
    [results, results_long] = runModel(envInfo, modelParams{m});
    choice = results(:, 3); subjMarkers = getSubjMarkers(results(:,1));
    save(['results/' envName '/' modelNames{m} '.mat'], 'choice', 'rewards_tr', 'subjMarkers', 'actualParams');
end
%clearvars

%% Parameters
numAgents = 120;
envName = 'wg_v8';
whichEnv = ['env/' envName '.mat'];
copyOver = false;

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

actualParams = cur_params;

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

    if copyOver
        save(['fitting/' envName '/sims_' modelNames{m} '/sims.mat'], 'choice', 'rewards_tr', 'subjMarkers', 'actualParams');
    end
    
    df = array2table(results_long, 'VariableNames', {'Subj', 'Trial', 'OptionID', 'Choice', 'MFval', 'MBval', 'Choice2'});
    writetable(df, ['results/' envName '/' modelNames{m} '.csv']);
    
    df2 = array2table(results, 'VariableNames', {'subject', 'trial', 'choice_real_ind', 's1_value', 'rank_value'});
    writetable(df2, ['results/' envName '/' modelNames{m} '-s2.csv']);
end
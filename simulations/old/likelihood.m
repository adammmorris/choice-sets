%% Inputs
% envInfo: a struct with 4 elements: numWords, numTrials, rewards_te.
% choices: numTrials x 1 vector. DROP ANYONE WHO DIDN'T COMPLETE ALL
%   TRIALS.
% rewards_tr: the training rewards for each word for this subject
% likeArray: a nTrials x nBetaSteps matrix that gives you the pre-computed
%   probability of, on each trial, choosing the word that the subject chose 
%   under a choice-set model (with a particular beta)
% betaDiscrete: [max_beta, nBetaSteps]
% freeParams: [nToEval beta w_MB]
% fixedParams: a 3-length vector, telling you which elements of 'freeParams' to
% use (if fixedParams(i) == -1) or to ignore (and use the value of fixedParams(i)).

%% Outputs
% likelihood: the log likelihood (NOT negative)

function [likelihood] = likelihood(envInfo, choices, rewards_tr, likeArray, betaInfo, freeParams, fixedParams)

%% Load env info
numTrials = envInfo{2};
rewards_te = envInfo{3};

if numel(choices) ~= numTrials
    error('Subject did not complete all trials.');
end

%% Set params
params = zeros(length(fixedParams), 1);
params(fixedParams == -1) = freeParams;
params(fixedParams ~= -1) = fixedParams(fixedParams ~= -1);
nToEval = params(1);
beta = params(2);
w_MB = params(3);

%% Calculate log likelihood
likelihood = 0;

% If a single stage-model..
if nToEval == 1
    for trial = 1:numTrials
        probs = exp(beta * (rewards_te(trial, :) * w_MB + rewards_tr * (1 - w_MB))) / ...
            sum(exp(beta * (rewards_te(trial, :) * w_MB + rewards_tr * (1 - w_MB))));
        likelihood = likelihood + log(probs(choices(trial)));
    end
else % If a choice-set model..
    % Find the closest beta
    betas = linspace(0, betaInfo(1), betaInfo(2));
    [~, betaInd] = min(abs(betas - beta));
    
    for trial = 1:numTrials
        likelihood = likelihood + log(likeArray(trial, betaInd));
    end
end
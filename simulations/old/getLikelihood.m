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

function [likelihood] = getLikelihood(envInfo, choices, rewards_tr, freeParams, fixedParams, whichTrials)

%% Load env info
numWords = envInfo{1};
rewards_te = envInfo{3};

if nargin >= 6
    numTrials = length(whichTrials);
else
    numTrials = envInfo{2};
    whichTrials = 1:numTrials;
end

% Rescale rewards
max_re = max([max(rewards_tr) max(max(envInfo{3}))]);
rewards_tr = rewards_tr / max_re;
rewards_te = rewards_te / max_re;

%% Set params
params = zeros(length(fixedParams), 1);
params(fixedParams == -1) = freeParams;
params(fixedParams ~= -1) = fixedParams(fixedParams ~= -1);
nToEval = params(1);
beta = params(2);
w_MB = params(3);

%% Calculate log likelihood
likelihood = 0;

if nToEval == 1 % any non-choice-set
    for trial_ind = 1:numTrials
        trial = whichTrials(trial_ind);
        probs = exp(beta * (rewards_te(trial, :) * w_MB + rewards_tr * (1 - w_MB))) / ...
            sum(exp(beta * (rewards_te(trial, :) * w_MB + rewards_tr * (1 - w_MB))));
        likelihood = likelihood + log(probs(choices(trial)));
    end
elseif beta == 0 % randcs
    for trial_ind = 1:numTrials
        trial = whichTrials(trial_ind);
        
        word = choices(trial);
        lq = sum(rewards_te(trial, :) < rewards_te(trial, word)); % # of MB values < q
        tq = sum(rewards_te(trial, :) == rewards_te(trial, word)) - 1; % # of MB values == q
        
        prob = 0;
        for numTies = 0:(nToEval - 1)
            prob = prob + binCoef(lq, nToEval - numTies - 1) * binCoef(tq, numTies) / ...
                ((numTies + 1) * binCoef(numWords, nToEval));
        end
        
        likelihood = likelihood + log(prob);
    end
else
    binCoef_table = zeros(numWords, numWords);
    for i = 1:numWords+1
        for j = 1:numWords+1
            binCoef_table(i, j) = binCoef(i - 1, j - 1);
        end
    end
    
    probs_allsets = zeros(binCoef(numWords, nToEval), 1);
    
    for trial_ind = 1:numTrials
        trial = whichTrials(trial_ind);
        
        word = choices(trial);
        lowest_val = min(rewards_te(trial, :));
        
        weights = exp(beta * (rewards_te(trial, :) * w_MB + rewards_tr * (1 - w_MB))) / ...
            sum(exp(beta * (rewards_te(trial, :) * w_MB + rewards_tr * (1 - w_MB))));
        
        if rewards_te(trial, word) ~= lowest_val || sum(rewards_te(trial, :) == lowest_val) > 1
            all_binomial_inds = zeros(binCoef(numWords, nToEval), 1);
            
            for numTies = 0:(nToEval-1)
                sets = getSets(word, numTies, nToEval, rewards_te(trial, :));
                
                for set_ind = 1:size(sets, 1)
                    set = sets(set_ind, :);
                    set_binomial_ind = getBinomialIndex(set, binCoef_table);
                    
                    if probs_allsets(set_binomial_ind + 1) == 0
                        pset = getPowerSet(set);
                        temp_prob = zeros(2 ^ nToEval, 1);
                        
                        
                        d = sum(exp(beta * rewards_tr(setdiff_fast(1:numWords, set)))); % get complement weight sum
                        
                        for subset_ind = 1:length(pset) % loop through power set
                            subset = pset{subset_ind};
                            
                            % store each iteration
                            temp_prob(subset_ind) = (-1) ^ length(subset) / ...
                                (1 + sum(exp(beta * rewards_tr(subset))) / d);
                        end
                        
                        probs_allsets(set_binomial_ind + 1) = sum(temp_prob);
                    end
                    
                    all_binomial_inds(set_binomial_ind + 1) = 1 / (numTies + 1);
                end
            end
            
            prob = sum(all_binomial_inds .* probs_allsets);
            if prob < 0 && prob > -1e-10, prob = eps; end
        else
            prob = -realmax;
        end
        
        likelihood = likelihood + log(prob);
    end
end
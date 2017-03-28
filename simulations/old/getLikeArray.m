%% getLikeArray
% Pre-computes the likelihood of a choice given our preferred model (for a
% bunch of discretized betas).

%% Inputs
% results: numTrials x 1 vector with choices on each trial. Assumes we've
%   dropped anyone who didn't complete all trials.
% nToEval: size of choice set
% betaInfo: [maxBeta nBetaSteps]
% rewards_tr: this subject's training rewards
% rewards_te: test rewards for each trial (numWords x numTrials)
% sim: simulation or exact

%% Outputs
% likeArray: numTrials x nBetaSteps. Stores the probability of the
% subject's choice on every trial, for a given beta.

function [likeArray] = getLikeArray(results, nToEval, betaInfo, rewards_tr, rewards_te, sim, nSamples)

% Set up betas
maxBeta = betaInfo(1);
nBetaSteps = betaInfo(2);
betas = linspace(0, maxBeta, nBetaSteps);

% Set up other variables
[numTrials, numWords] = size(rewards_te);
likeArray = zeros(numTrials, betaInfo(2));

if ~sim
    % Pre-compute table of binary coefficients (index 1 is for 0, index i is
    % for i - 1 - so always add one to index)
    binCoef_table = zeros(numWords, numWords);
    for i = 1:numWords+1
        for j = 1:numWords+1
            binCoef_table(i, j) = binCoef(i - 1, j - 1);
        end
    end
    
    % Initialize array to store ongoing probability calculations
    probs_allsets = zeros(binCoef(numWords, nToEval), nBetaSteps);
    
    for trial = 1:numTrials
        % Get the word they actually chose for this trial.
        word = results(trial);
        lowest_val = min(rewards_te(trial, :));
        
        % If this word is not the lowest (or if there are ties for lowest)..
        if rewards_te(trial, word) ~= lowest_val || sum(rewards_te(trial, :) == lowest_val) > 1
            
            % for a set s with P(s), stores what you need to multiply
            % P(s) by in the final sum.
            % (e.g. if there's no ties in this set, multiply by 1;
            % if there's 3 ties, multiply by 1/4.)
            all_binomial_inds = zeros(binCoef(numWords, nToEval), 1);
            
            
            for numTies = 0:(nToEval-1)
                sets = getSets(word, numTies, nToEval, rewards_te(trial, :));
                
                % Loop through all the sets with this # of ties (and others worse)
                for set_ind = 1:size(sets, 1)
                    set = sets(set_ind, :);
                    set_binomial_ind = getBinomialIndex(set, binCoef_table);
                    
                    % If we haven't calculated the probability of this set yet,
                    % do it for all betas.
                    % (We only need to check the first index, b/c we do them all at
                    % once.)
                    if probs_allsets(set_binomial_ind + 1, 1) == 0
                        pset = getPowerSet(set);
                        temp_prob = zeros(2 ^ nToEval, 1);
                        
                        for beta_ind = 1:nBetaSteps
                            beta = betas(beta_ind);
                            
                            d = sum(exp(beta * rewards_tr(setdiff_fast(1:numWords, set)))); % get complement weight sum
                            
                            for subset_ind = 1:length(pset) % loop through power set
                                subset = pset{subset_ind};
                                
                                % store each iteration
                                temp_prob(subset_ind) = (-1) ^ length(subset) / ...
                                    (1 + sum(exp(beta * rewards_tr(subset))) / d);
                            end
                            
                            probs_allsets(set_binomial_ind + 1, beta_ind) = sum(temp_prob);
                        end
                    end
                    
                    all_binomial_inds(set_binomial_ind + 1) = 1 / (numTies + 1);
                end
            end
            
            for beta_ind = 1:nBetaSteps
                likeArray(trial, beta_ind) = sum(all_binomial_inds .* probs_allsets(:, beta_ind));
            end
        else
            likeArray(trial, :) = 0; % If this word is the lowest, can't be chosen.
        end
    end
    
    likeArray(likeArray < 0 & likeArray > -1e-10) = eps; % if there's a floating point error..
else
    for trial = 1:numTrials
        word = results(trial);
        rand_table = exprnd(1, [numWords, nBetaSteps, nSamples]);
        
        parfor beta_ind = 1:nBetaSteps
            nThisChoice = 0;
            beta = betas(beta_ind);
            temp = exp(beta * rewards_tr)';
            
            for i = 1:nSamples
                [~, options] = sort(rand_table(:, beta_ind, i) ./ temp);
                toEval = options(1:5);
                [~, choice_ind] = max(rewards_te(1, toEval));
                if toEval(choice_ind) == word, nThisChoice = nThisChoice + 1; end
            end
            
            likeArray(trial, beta_ind) = nThisChoice / nSamples + eps;
        end
    end
end
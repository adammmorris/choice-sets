%% Inputs
% envInfo: a struct with 4 elements: numWords, numTrials, rewards_te.
% choices: numTrials x 1 vector.
% rewards_tr: the training rewards for each word for this subject
% likeArray: a nTrials x nBetaSteps matrix that gives you the pre-computed
%   probability of, on each trial, choosing the word that the subject chose
%   under a choice-set model (with a particular beta)
% betaDiscrete: [max_beta, nBetaSteps]
% freeParams: [nToEval beta w_NE w_MF w_MB]
% fixedParams: a 3-length vector, telling you which elements of 'freeParams' to
% use (if fixedParams(i) == -1) or to ignore (and use the value of fixedParams(i)).

%% Outputs
% likelihood: the log likelihood (NOT negative)

function [LL] = likelihood(choices, rewards_s1, rewards_s2, recalled, freeParams, fixedParams)

%% Load env info
numWords = 12;

numTrials = length(choices);
if size(rewards_s1,1) == 1
    rewards_s1 = repmat(rewards_s1, numTrials, 1);
end

if size(recalled,1) == 1
    recalled = repmat(recalled, numTrials, 1);
end

poss = ones(numWords, 1) * -1;

%% Set params
params = zeros(length(fixedParams), 1);
params(fixedParams == -1) = freeParams;
params(fixedParams ~= -1) = fixedParams(fixedParams ~= -1);
nToEval = params(1);
beta = params(2);
epsilon = params(3); % if doSoftmax, this is beta2
%epsilon = beta;
w_MF = params(4);
w_MB = 1 - w_MF;
%w_MB = params(5);
w_poss = params(6);
negMF = params(7);

doSoftmax = true;

%wSum = w_MF + w_MB + w_poss;
%if beta > 0 && abs(wSum - 1) > .01
%    error('Weights do not sum to 1.');
%end

% for negative MF
if negMF == 1
    rewards_s1 = max(max(rewards_s1)) - rewards_s1;
elseif negMF == 2
    rewards_s1 = rewards_s1(randperm(length(rewards_s1)));
end

%% Calculate log likelihood
likelihood = zeros(numTrials, 1);

if nToEval == 1 % any non-choice-set
    for trial = 1:numTrials
        maxRe_s1 = max(rewards_s1(trial, :));
        maxRe_s2 = max(rewards_s2(trial, :));
        
        availWords = find(recalled(trial,:));
        numAvailWords = length(availWords);
        word = find(availWords == choices(trial));
        
        probs_num = exp(beta * ...
            (rewards_s2(trial, availWords) * w_MB / maxRe_s2 + ...
            rewards_s1(trial, availWords) * w_MF / maxRe_s1));
        probs = probs_num / sum(probs_num);
        
        %probs = (1 - epsilon) * probs + epsilon * (1 / numAvailWords);
        
        likelihood(trial) = log(probs(word));
    end
elseif beta == 0 % randcs  
    for trial = 1:numTrials
        availWords = find(recalled(trial,:));
        numAvailWords = length(availWords);
        word = find(availWords == choices(trial));
        maxRe_s1 = max(rewards_s1(trial, :));
        maxRe_s2 = max(rewards_s2(trial, :));
        
        prob = 0;
        if doSoftmax
            sets = [];
            possible_others = combnk_fast(setdiff_fast(1:numAvailWords, word), nToEval - 1);
            
            for other_ind = 1:size(possible_others, 1)
                sets(end+1, :) = [word possible_others(other_ind, :)];
            end
            
            for set_ind = 1:size(sets, 1)
                cur_set_prob = 1 / binCoef(numAvailWords, nToEval);
                choice_prob_num = exp(epsilon * rewards_s2(trial, availWords(sets(set_ind, :))) / maxRe_s2);
                choice_prob = choice_prob_num / sum(choice_prob_num);
                
                prob = prob + choice_prob(1) * cur_set_prob; % word they chose is always 1st
            end
        else
            lq = sum(rewards_s2(trial, availWords) < rewards_s2(trial, availWords(word))); % # of MB values < q
            tq = sum(rewards_s2(trial, availWords) == rewards_s2(trial, availWords(word))) - 1; % # of MB values == q
            
            sets_prob = 0;
            for numTies = 0:(nToEval - 1)
                cur_sets_prob = binCoef(lq, nToEval - numTies - 1) * binCoef(tq, numTies) / binCoef(numAvailWords, nToEval);
                sets_prob = sets_prob + cur_sets_prob;
                prob = prob + ((1 - epsilon) * (1 / (numTies + 1)) + epsilon * (1 / numAvailWords)) * cur_sets_prob;
            end
            
            prob = prob + epsilon * (1 / numAvailWords) * (1 - sets_prob);
        end
        
        likelihood(trial) = log(prob);
    end
else
    for trial = 1:numTrials
        %disp(['trial ' num2str(trial)]);
        maxRe_s1 = max(rewards_s1(trial, :));
        maxRe_s2 = max(rewards_s2(trial, :));
        availWords = find(recalled(trial,:));
        numAvailWords = length(availWords);
        
        word = find(availWords == choices(trial));
        
%         if poss(availWords) > -1
%             weights_num = exp(beta * ...
%                 (rewards_s2(trial, availWords) * w_MB / maxRe_s2 + ...
%                 rewards_s1(availWords) * w_MF / maxRe_s1 + ...
%                 poss(availWords) * w_poss));
%         else
%             % reweight
%             softmax_weights_new = [w_MB w_MF];
%             softmax_weights_new = softmax_weights_new / sum(softmax_weights_new);
%             weights_num = exp(beta * ...
%                 (rewards_s2(trial, availWords) * softmax_weights_new(1) / maxRe_s2 + ...
%                 rewards_s1(availWords) * softmax_weights_new(2) / maxRe_s1));
%         end
        weights_num = exp(beta * ...
                 (rewards_s2(trial, availWords) * w_MB / maxRe_s2 + ...
                 rewards_s1(trial, availWords) * w_MF / maxRe_s1));
        weights = weights_num / sum(weights_num);
        
        prob = 0;
        %disp(sprintf('%d %d', whichSubj, trial));
        if doSoftmax
            sets = [];
            possible_others = combnk_fast(setdiff_fast(1:numAvailWords, word), nToEval - 1);
            
            for other_ind = 1:size(possible_others, 1)
                sets(end+1, :) = [word possible_others(other_ind, :)];
            end
            
            for set_ind = 1:size(sets, 1)
                set = sets(set_ind, :);
                
                pset = getPowerSet_reduced(set);
                temp_prob = zeros(length(pset), 1);
                
                d = sum(weights(setdiff_fast(1:numAvailWords, set))); % get complement weight sum
                
                for subset_ind = 1:length(pset) % loop through power set
                    subset = pset{subset_ind};
                    
                    % store each iteration
                    temp_prob(subset_ind) = (-1) ^ numel(subset) / (1 + sum(weights(subset)) / d);
                end
                
                cur_set_prob = sum(temp_prob) + 1 + d * ((-1)^numel(set));
                choice_prob_num = exp(epsilon * rewards_s2(trial, availWords(set)) / maxRe_s2);
                choice_prob = choice_prob_num / sum(choice_prob_num);
                
                prob = prob + choice_prob(1) * cur_set_prob;
            end
        else
            set_prob = 0;
            
            for numTies = 0:(nToEval-1)
                sets = getSets(word, numTies, nToEval, rewards_s2(trial, availWords));
                
                for set_ind = 1:size(sets, 1)
                    set = sets(set_ind, :);
                    
                    pset = getPowerSet_reduced(set);
                    temp_prob = zeros(length(pset), 1);
                    
                    d = sum(weights(setdiff_fast(1:numAvailWords, set))); % get complement weight sum
                    
                    for subset_ind = 1:length(pset) % loop through power set
                        subset = pset{subset_ind};
                        
                        % store each iteration
                        temp_prob(subset_ind) = (-1) ^ numel(subset) / (1 + sum(weights(subset)) / d);
                    end
                    
                    cur_set_prob = sum(temp_prob) + 1 + d * ((-1)^numel(set));
                    set_prob = set_prob + cur_set_prob;
                    prob = prob + ((1 - epsilon) * (1 / (numTies + 1)) + epsilon * (1 / numAvailWords)) * cur_set_prob;
                end
            end
            
            prob = prob + epsilon * (1 / numAvailWords) * (1 - set_prob);
        end
        
        likelihood(trial) = log(prob);
    end
end

LL = sum(likelihood);
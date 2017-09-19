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

function LL = getLikelihood(envInfo, choices, rewards_te, recalled, whichSubj, freeParams, fixedParams, nSamples)

%% Load env info
numWords = envInfo{1};
rewards_tr = envInfo{3}(whichSubj, :);
maxRe_tr = envInfo{4};

recalled = recalled(whichSubj, :);
numTrials = length(choices);

%% Set params
params = zeros(length(fixedParams), 1);
params(fixedParams == -1) = freeParams;
params(fixedParams ~= -1) = fixedParams(fixedParams ~= -1);
nToEval = params(1);
beta = params(2);
epsilon = params(3); % if doSoftmax, this is beta2
w_MF = params(4);
w_MB = params(5);

doSoftmax = true;

wSum = w_MF + w_MB;
if beta > 0 && abs(wSum - 1) > .01
    error('Weights do not sum to 1.');
end

%% Calculate log likelihood
likelihood = zeros(numTrials, 1);

if nToEval == 1 % any non-choice-set
    for trial = 1:numTrials
        maxRe_te = max(rewards_te(trial, :));
        
        availWords = find(recalled);
        numAvailWords = length(availWords);
        word = find(availWords == choices(trial));
        
        probs_num = exp(beta * ...
                (rewards_te(trial, availWords) * w_MB / maxRe_te + ...
                rewards_tr(availWords) * w_MF / maxRe_tr));
        probs = probs_num / sum(probs_num);
        
        probs = (1 - epsilon) * probs + epsilon * (1 / numAvailWords);
        
        likelihood(trial) = log(probs(word));
    end
elseif beta == 0 % randcs
    availWords = find(recalled);
    numAvailWords = length(availWords);
    
    for trial = 1:numTrials
        word = find(availWords == choices(trial));
        maxRe_te = max(rewards_te(trial, :));
        
        prob = 0;
        if doSoftmax
            sets = [];
            possible_others = combnk_fast(setdiff_fast(1:numAvailWords, word), nToEval - 1);
            
            for other_ind = 1:size(possible_others, 1)
                sets(end+1, :) = [word possible_others(other_ind, :)];
            end
            
            for set_ind = 1:size(sets, 1)
                cur_set_prob = 1 / binCoef(numAvailWords, nToEval);
                choice_prob_num = exp(epsilon * rewards_te(trial, availWords(sets(set_ind, :))) / maxRe_te);
                choice_prob = choice_prob_num / sum(choice_prob_num);
                
                prob = prob + choice_prob(1) * cur_set_prob; % word they chose is always 1st
            end
        else
            lq = sum(rewards_te(trial, availWords) < rewards_te(trial, availWords(word))); % # of MB values < q
            tq = sum(rewards_te(trial, availWords) == rewards_te(trial, availWords(word))) - 1; % # of MB values == q
        
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
    if nSamples == 0 % do exact calculation
        for trial = 1:numTrials
            maxRe_te = max(rewards_te(trial, :));
            availWords = find(recalled);
            numAvailWords = length(availWords);
            
            word = find(availWords == choices(trial));
            
            weights_num = exp(beta * ...
                (rewards_te(trial, availWords) * w_MB / maxRe_te + ...
                rewards_tr(availWords) * w_MF / maxRe_tr));
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
                    choice_prob_num = exp(epsilon * rewards_te(trial, availWords(set)) / maxRe_te);
                    choice_prob = choice_prob_num / sum(choice_prob_num);
                    
                    prob = prob + choice_prob(1) * cur_set_prob;
                end
            else
                set_prob = 0;

                for numTies = 0:(nToEval-1)
                    sets = getSets(word, numTies, nToEval, rewards_te(trial, availWords));

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
    else % sample
        for trial = 1:numTrials
            word = choices(trial);
            rand_table = exprnd(1, [numWords, nSamples]);
            
            nThisChoice = 0;
            temp = exp(beta * rewards_tr)';
            
            parfor i = 1:nSamples
                [~, options] = sort(rand_table(:, i) ./ temp);
                toEval = options(1:5);
                [~, choice_ind] = max(rewards_te(trial, toEval));
                if toEval(choice_ind) == word, nThisChoice = nThisChoice + 1; end
            end
            
            prob = nThisChoice / nSamples;
            if prob == 0, prob = eps; end
            likelihood(trial) = log(prob);
        end
    end
end

LL = sum(likelihood);
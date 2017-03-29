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
exposures_tr = envInfo{5}(whichSubj, :);
maxExposures_tr = envInfo{6};
rewards_tr = envInfo{7}(whichSubj, :);
maxRe_tr = envInfo{8};
chosen_tr = envInfo{9}(whichSubj, :);
maxChosen_tr = maxExposures_tr;

recalled = recalled(whichSubj, :);
numTrials = length(choices);

%% Set params
params = zeros(length(fixedParams), 1);
params(fixedParams == -1) = freeParams;
params(fixedParams ~= -1) = fixedParams(fixedParams ~= -1);
nToEval = params(1);
beta = params(2);
w_NE = params(3);
w_MF = params(4); % num exposures
w_MB = params(5);
w_AV = params(6);
w_CH = params(7);
epsilon = .05;

wSum = w_NE + w_MF + w_MB + w_AV + w_CH;
%disp(sprintf('%d %d %d', w_NE, w_MF, w_MB));
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
                exposures_tr(availWords) * w_NE / maxExposures_tr + ...
                rewards_tr(availWords) * w_MF / maxRe_tr + ...
                abs(rewards_tr(availWords) - 5) * w_AV / (maxRe_tr / 2) + ...
                chosen_tr(availWords) * w_CH / maxChosen_tr));
        probs = probs_num / sum(probs_num);
        
        probs = (1 - epsilon) * probs + epsilon * (1 / numAvailWords);
        
        likelihood(trial) = log(probs(word));
    end
elseif beta == 0 % randcs
    availWords = find(recalled);
    numAvailWords = length(availWords);
    
    for trial = 1:numTrials
        %word = choices(trial);
        word = find(availWords == choices(trial));
        
        lq = sum(rewards_te(trial, availWords) < rewards_te(trial, availWords(word))); % # of MB values < q
        tq = sum(rewards_te(trial, availWords) == rewards_te(trial, availWords(word))) - 1; % # of MB values == q
        
        prob = 0;
        sets_prob = 0;
        for numTies = 0:(nToEval - 1)
            cur_sets_prob = binCoef(lq, nToEval - numTies - 1) * binCoef(tq, numTies) / binCoef(numAvailWords, nToEval);
            sets_prob = sets_prob + cur_sets_prob;
            prob = prob + ((1 - epsilon) * (1 / (numTies + 1)) + epsilon * (1 / numAvailWords)) * cur_sets_prob;
        end
        
        prob = prob + epsilon * (1 / numAvailWords) * (1 - sets_prob);
        
        % get how many are tied for lowest
        %numLowest = sum(rewards_te(trial, availWords) == min(rewards_te(trial, availWords)));
        % these are only impossible if they can't all fit into one choice
        % set together
        %numLowest = numLowest * (numLowest < nToEval);
        
        %if prob == 0, prob = eps; end
        %prob = (prob + epsilon) / (1 + epsilon * length(availWords));
        
        likelihood(trial) = log(prob);
    end
else
    if nSamples == 0 % do exact calculation
        for trial = 1:numTrials
            maxRe_te = max(rewards_te(trial, :));
            availWords = find(recalled);
            numAvailWords = length(availWords);
            
            %word = choices(trial);
            word = find(availWords == choices(trial));
            
            weights_num = exp(beta * ...
                (rewards_te(trial, availWords) * w_MB / maxRe_te + ...
                exposures_tr(availWords) * w_NE / maxExposures_tr + ...
                rewards_tr(availWords) * w_MF / maxRe_tr + ...
                abs(rewards_tr(availWords) - 5) * w_AV / (maxRe_tr / 2) + ...
                chosen_tr(availWords) * w_CH / maxChosen_tr));
            weights = weights_num / sum(weights_num);
            
            prob = 0;
            set_prob = 0;
            %disp(sprintf('%d %d', whichSubj, trial));
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
            
            %if prob < 0 && prob > -1e-10, prob = eps; end % roundoff error
            %if prob == 0, prob = eps; end % impossible choice
            
            % get how many are tied for lowest
            %numLowest = sum(rewards_te(trial, availWords) == min(rewards_te(trial, availWords)));
            % these are only impossible if they can't all fit into one choice
            % set together
            %numLowest = numLowest * (numLowest < nToEval);

            prob = prob + epsilon * (1 / numAvailWords) * (1 - set_prob);
            
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
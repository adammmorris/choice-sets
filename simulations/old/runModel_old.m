function [results, results_long] = runModel(envInfo, params)
%% Get parameters
numWords = envInfo{1};
numTrials = envInfo{2};
rewards_tr = envInfo{4};
maxRe_tr = envInfo{5};
rewards_te = envInfo{6};
maxRe_te = envInfo{7};

numAgents = size(params, 1);

%% Run model
results = zeros(numAgents * numTrials, 5);
results_ind = 1;
results_long = zeros(numAgents * numTrials * numWords, 6);
results_long_ind = 1;

for agent = 1:numAgents
    nToEval = params(agent, 1);
    beta = params(agent, 2);
    beta2 = params(agent, 3);
    w_MF = params(agent, 4);
    w_MB = params(agent, 5);

    wSum = w_MF + w_MB;
    if beta > 0 && abs(wSum - 1) > .01
        error('Weights do not sum to 1.');
    end
    
    %chosen = false(numWords, 1);
    
    for trial = 1:numTrials        
        availWords = 1:numWords;

        probs_num = exp(beta * ...
                (rewards_te(trial, availWords) * w_MB / maxRe_te + ...
                rewards_tr(agent, availWords) * w_MF / maxRe_tr));
        probs = probs_num / sum(probs_num);
        
        if nToEval == 1
            choice = availWords(fastrandsample(probs, 1));
        else
            % Get one-pass WRS with Efraimidis & Spirakis's method
            [~, options] = sort(exprnd(1, [1, length(availWords)]) ./ probs);
            toEval = options(1:nToEval);
            
            %[~, choice_ind] = max(rewards_te(trial, availWords(toEval)));
            probs2_num = exp(beta2 * rewards_te(trial, availWords(toEval)) / maxRe_te);
            probs2 = probs2_num / sum(probs2_num);
            choice_ind = fastrandsample(probs2, 1);
            
            choice = availWords(toEval(choice_ind));
        end
        
        %chosen(1:end) = false; % they just can't repeat words twice in a row
        %chosen(choice) = true;

        results(results_ind, :) = [agent trial choice ...
            rewards_tr(agent, choice) rewards_te(trial, choice)];
        results_ind = results_ind + 1;

        rewards_te_rank = tiedrank(rewards_te(trial, :));
        for j = 1:numWords
            results_long(results_long_ind, :) = [agent trial j (choice == j) ...
                rewards_tr(agent, j) > 5 rewards_te_rank(j)];
            results_long_ind = results_long_ind + 1;
        end
    end
end
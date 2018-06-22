function [results, params, hessian] = fit_fixed(dataPath, fixedParams, priorPDFs, numStarts, nFnEvals)

%% Set up
load(dataPath);
numSubjects = length(subjMarkers);

% Params
% K, beta1, beta2, w_MF, w_MB, w_poss, negMF, sep
K_PARAM_IND = false(length(fixedParams), 1);
K_PARAM_IND(1) = true;

freeParams = fixedParams == -1;
freeParams_noK = freeParams;
freeParams_noK(K_PARAM_IND) = false;
nFreeParams = sum(freeParams);
nContFreeParams = sum(freeParams_noK);

bounds = [2 0 0 0 0 0 0; 5 10 10 1 1 1 1];

WEIGHT_INDS = false(length(fixedParams), 1);
%WEIGHT_INDS = [false false false true true true false];

A_all = zeros(1, length(fixedParams));
A_all(WEIGHT_INDS) = 1;
A = A_all(freeParams_noK);
b = 1;

if ~any(A)
    A = [];
    b = [];
end

options = optimoptions(@fmincon, 'Display', 'off', 'UseParallel', false, 'MaxFunEvals', nFnEvals);

results = zeros(1, 4); % post ll bic lme
params = zeros(1, nFreeParams);

% Calculate starts for continuous parameters
starts = zeros(numStarts, nContFreeParams);
bounds_fp = bounds(:, freeParams_noK);
for i = 1:nContFreeParams
    ub = bounds_fp(2,i);
    lb = bounds_fp(1,i);
    starts(:,i) = rand(numStarts, 1) * (ub-lb) + lb;
end

weight_params = WEIGHT_INDS(freeParams_noK);
for j = 1:numStarts
    starts(j, weight_params) = starts(j, weight_params) / sum(starts(j, weight_params));
end

choice_subj = choice;
rewards_s1_subj = rewards_s1;
rewards_s2_subj = rewards_s2;
recalled_subj = recalled;

%% Start!
if nFreeParams > 0 % Are there free parameters?
    if fixedParams(K_PARAM_IND) == -1 % Are we optimizing over the discrete nToEval parameter?
        krange = bounds(1,K_PARAM_IND):bounds(2,K_PARAM_IND);
        nDiscrete = length(krange);
        logposts = zeros(nDiscrete, 1); % p(data | cont_params) * p(cont_params) * p(discrete_param)
        hessian = cell(nDiscrete, 1);
        optParams_all = zeros(nDiscrete, nContFreeParams);
        
        for nToEval_ind = 1:nDiscrete
            if nContFreeParams > 0 % If there's other continuous parameters to optimize over..
                f = @(params) -posterior(choice_subj, rewards_s1_subj, rewards_s2_subj, recalled_subj, [krange(nToEval_ind) params], fixedParams, priorPDFs);
                logposts_starts = zeros(numStarts, 1);
                params_starts = zeros(numStarts, nContFreeParams);
                
                parfor thisStart = 1:numStarts
                    [params_starts(thisStart, :), logposts_starts(thisStart), ~, ~, ~, ~] = ...
                        fmincon(f, starts(thisStart, :), [], [], A, b, ...
                        bounds(1, freeParams_noK), bounds(2, freeParams_noK), [], options);
                end
                
                [~, bestStart] = min(logposts_starts);
                logposts(nToEval_ind) = -logposts_starts(bestStart);
                optParams_all(nToEval_ind, :) = params_starts(bestStart, :);
                
                hessian{nToEval_ind} = NumHessian(f, optParams_all(nToEval_ind, :));
            else % If there's not..
                logposts(nToEval_ind) = posterior(choice(index), rewards_s1_subj, rewards_s2_subj, recalled_subj, krange(nToEval_ind), fixedParams, priorPDFs);
                hessian{nToEval_ind} = 1;
            end
        end
        
        lme = log((2*pi)^(nContFreeParams / 2) * sum(exp(logposts) .* (cellfun(@det, hessian) .^ (-1/2))));
        [post, optParams_ind] = max(logposts);
        optParams = [krange(optParams_ind) optParams_all(optParams_ind, :)];
    else % If not optimizing over discrete param, just optimize over continuous & do Laplace approximation.
        f = @(params) -posterior(choice_subj, rewards_s1_subj, rewards_s2_subj, recalled_subj, params, fixedParams, priorPDFs);
        logposts_starts = zeros(numStarts, 1);
        params_starts = zeros(numStarts, nFreeParams);
        
        parfor thisStart = 1:numStarts
            [params_starts(thisStart, :), logposts_starts(thisStart), ~, ~, ~, ~] = ...
                fmincon(f, starts(thisStart, :), [], [], A, b, ...
                bounds(1, freeParams), bounds(2, freeParams), [], options);
        end
        
        [~, bestStart] = min(logposts_starts);
        post = -logposts_starts(bestStart);
        optParams = params_starts(bestStart, :);
        
        hessian = NumHessian(f, optParams);
        lme = post + .5 * (nFreeParams * log(2*pi) - log(det(hessian)));
    end
    
    ll = likelihood(choice_subj, rewards_s1_subj, rewards_s2_subj, recalled_subj, optParams, fixedParams);
    bic = nFreeParams * (log(length(choice_subj)) - log(2*pi)) - 2 * ll;
    if isnan(lme) || ~isreal(lme) || isinf(lme) % resort to BIC
        lme = -0.5 * bic;
    end
    
    results(1, :) = [post ll bic lme];
    params(1, :) = optParams;
else % If there's no free parameters, just evaluate it at the one place.
    [post, ll] = posterior(choice_subj, rewards_s1_subj, rewards_s2_subj, recalled_subj, [], fixedParams, priorPDFs);
    bic = -2 * ll;
    lme = ll;
    hessian = 0;
    
    results(1, :) = [post ll bic lme];
end
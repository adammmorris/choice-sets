%% Inputs
% dataPath: path to a file containing
%   (a) 'choice', a (numRounds * numSubjects) x 1 vector,
%   which gives (for each subject & round) [choice],
%   (b) 'subjMarkers', a numSubjects-length vector which gives the starting row #
%   in results for each subject, and
%   (c) 'rewards_tr', a numSubjects x nWords, which gives each subject's training
%   rewards.
%   Should already have practice rounds filtered out, and should have
%   dropped anyone who didn't complete all trials.
% whichEnv: path to environment file with 'envInfo' struct.
% savePath: folder to save results in (should end with /!)
% numStarts: how many starts to do
% taskid: a # between 1 and (numStarts * numSubjects)
% fixedParams: which params to fix, and to what values. There are 3
% possible parameters: [nToEval, beta, w_NE, w_MF, w_MB, w_AV].
%   If you want to let a parameter be free, set to -1.
%   Otherwise, set to the value you want to fix it at.

function fitModel(dataPath, whichEnv, savePath, fixedParams, priorPDFs, taskid, numStarts, nFnEvals, nSamples, on_cluster)
if ~exist('on_cluster', 'var')
    on_cluster = true; % is this being run on the cluster?
end

%% Load data & env
load(dataPath);
load(whichEnv);
envInfo{3} = rewards_tr;
envInfo{5} = poss;

%% Set up
numSubjects = length(subjMarkers);

% if (fixedParams(2) > 0)
%     error('beta cannot be fixed higher than 0.');
% end

if (taskid < 1 || taskid > numSubjects)
    error('taskid must be between 1 and numSubjects');
end

whichSubj = taskid;

% Params
K_PARAM_IND = [true false false false false false false];

freeParams = fixedParams == -1;
freeParams_noK = freeParams;
freeParams_noK(K_PARAM_IND) = false;
nFreeParams = sum(freeParams);
nContFreeParams = sum(freeParams_noK);

% CHANGE THESE BACK
bounds = [2 0 0 0 0 0 0; 4 5 5 1 1 1 1];
%bounds = [2 0 0 -10 0; 4 10 10 10 10];
WEIGHT_INDS = [false false false true true true false];
%WEIGHT_INDS = [false false false false false];

A_all = zeros(1, length(fixedParams));
A_all(WEIGHT_INDS) = 1;
A = A_all(freeParams_noK);
b = 1;

if ~any(A)
    A = [];
    b = [];
end

% Index
if whichSubj < length(subjMarkers)
    index = subjMarkers(whichSubj):(subjMarkers(whichSubj + 1) - 1);
else
    index = subjMarkers(whichSubj):length(choice);
end

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

if on_cluster
    %distcomp.feature( 'LocalUseMpiexec', false );
    %parpool('local', str2num(getenv('SLURM_CPUS_PER_TASK')));
    %parpool('local', 8);
end

%% Start!
options = optimoptions(@fmincon, 'Display', 'off', 'UseParallel', false, 'MaxFunEvals', nFnEvals);
options_unc = optimoptions(@fminunc, 'Display', 'Off', 'Algorithm', 'quasi-newton', 'MaxFunEvals', 0);

if nFreeParams > 0 % Are there free parameters?
    if fixedParams(K_PARAM_IND) == -1 % Are we optimizing over the discrete nToEval parameter?
        krange = bounds(1,K_PARAM_IND):bounds(2,K_PARAM_IND);
        nDiscrete = length(krange);
        logposts = zeros(nDiscrete, 1); % p(data | cont_params) * p(cont_params) * p(discrete_param)
        hessians = cell(nDiscrete, 1);
        optParams_all = zeros(nDiscrete, nContFreeParams);
        
        for nToEval_ind = 1:nDiscrete
            if nContFreeParams > 0 % If there's other continuous parameters to optimize over..
                f = @(params) -getPosterior(envInfo, choice(index), rewards_te_trial(index, :), recalled, whichSubj, [krange(nToEval_ind) params], fixedParams, priorPDFs, nSamples);
                logposts_starts = zeros(numStarts, 1);
                params_starts = zeros(numStarts, nContFreeParams);
                     
                for thisStart = 1:numStarts
                    [params_starts(thisStart, :), logposts_starts(thisStart), ~, ~, ~, ~] = ...
                        fmincon(f, starts(thisStart, :), [], [], A, b, ...
                        bounds(1, freeParams_noK), bounds(2, freeParams_noK), [], options);
                end
                
                [~, bestStart] = min(logposts_starts);
                logposts(nToEval_ind) = -logposts_starts(bestStart);
                optParams_all(nToEval_ind, :) = params_starts(bestStart, :);
                
                numWeights = sum(freeParams(WEIGHT_INDS));
                if numWeights > 1
                    [~, ~, ~, ~, ~, hessians{nToEval_ind}] = fminunc(@(params) f([params, 1 - sum(params(end:-1:(end-numWeights+2)))]), ...
                        optParams_all(nToEval_ind, 1:(end-1)), options_unc);
                else
                    [~, ~, ~, ~, ~, hessians{nToEval_ind}] = fminunc(f, optParams_all(nToEval_ind, :), options_unc);
                end
            else % If there's not..
                logposts(nToEval_ind) = getPosterior(envInfo, choice(index), rewards_te_trial(index, :), recalled, whichSubj, krange(nToEval_ind), fixedParams, priorPDFs, nSamples);
                hessians{nToEval_ind} = 1;
            end
        end
        
        lme = log((2*pi)^(nContFreeParams / 2) * sum(exp(logposts) .* (cellfun(@det, hessians) .^ (-1/2))));
        [post, optParams_ind] = max(logposts);
        optParams = [krange(optParams_ind) optParams_all(optParams_ind, :)];
    else % If not optimizing over discrete param, just optimize over continuous & do Laplace approximation.
        f = @(params) -getPosterior(envInfo, choice(index), rewards_te_trial(index, :), recalled, whichSubj, params, fixedParams, priorPDFs, nSamples);
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
        
        [~, ~, ~, ~, ~, hessian] = fminunc(f, optParams, options_unc);
        lme = nFreeParams / 2 * log(2*pi) + post - .5 * log(det(hessian));
    end
    
    % CHANGE THIS BACK
    ll = getLikelihood(envInfo, choice(index), rewards_te_trial(index, :), recalled, whichSubj, optParams, fixedParams, nSamples);
    if isnan(lme) || ~isreal(lme) || isinf(lme) % resort to BIC
        lme = -0.5 * (nFreeParams * (log(length(index)) - log(2*pi)) - 2 * ll);
    end
        
    csvwrite([savePath num2str(taskid) '.txt'], [post, ll, lme, optParams]);
else % If there's no free parameters, just evaluate it at the one place.
    [post, ll] = getPosterior(envInfo, choice(index), rewards_te_trial(index, :), recalled, whichSubj, [], fixedParams, priorPDFs, nSamples);
    
    csvwrite([savePath num2str(taskid) '.txt'], [post, ll, post]);
end

if on_cluster
    delete(gcp);
end
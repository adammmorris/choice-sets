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
% possible parameters: [nToEval, beta, w_MB].
%   If you want to let a parameter be free, set to -1.
%   Otherwise, set to the value you want to fix it at.

function fitModel(dataPath, whichEnv, savePath, numStarts_total, taskid, allStarts, fixedParams, priorPDFs, nFnEvals, nSamples, cluster)
if ~exist('cluster', 'var')
    cluster = true; % is this being run on the cluster?
end

%% Load data & env
load(dataPath);
load(whichEnv);

%% Set up
numSubjects = length(subjMarkers);

if (fixedParams(2) > 0)
    error('beta cannot be fixed higher than 0.');
end

if allStarts
    if (taskid < 1 || taskid > numSubjects)
        error('if allStarts == true, taskid must be between 1 and numSubjects');
    end
    
    whichSubj = taskid;
    numStarts = numStarts_total;
else
    if (taskid < 1 || taskid > (numStarts_total * numSubjects))
        error('if allStarts == false, taskid must be between 1 and (numStarts * numSubjects)');
    end

    [~, whichSubj] = ind2sub([numStarts_total, numSubjects * numStarts_total], taskid);
    numStarts = 1;
end

% Params
freeParams = fixedParams == -1;
numFreeParams = sum(freeParams);
bounds = [2 0 0; 5 10 1];

% Index
if whichSubj < length(subjMarkers)
    index = subjMarkers(whichSubj):(subjMarkers(whichSubj + 1) - 1);
else
    index = subjMarkers(whichSubj):length(choice);
end

% Calculate starts
starts = zeros(numStarts_total, numFreeParams);
bounds_fp = bounds(:, freeParams);
for i = 1:numFreeParams
    ub = bounds_fp(2,i);
    lb = bounds_fp(1,i);
    starts(:,i) = rand(numStarts_total, 1) * (ub-lb) + lb;
end

if cluster
    parpool('local', str2num(getenv('SLURM_CPUS_PER_TASK')));
else
   p = gcp('nocreate');
   if isempty(p)
       parpool('local', 3); % on my PC
   end
end

%% Start!
if numFreeParams > 0 % Are there free parameters?
    if fixedParams(1) == -1 % Are we optimizing over the discrete nToEval parameter?
        krange = bounds(1,1):bounds(2,1);
        nDiscrete = length(krange);
        logposts = zeros(nDiscrete, 1); % p(data | cont_params) * p(cont_params) * p(discrete_param)
        hessians = zeros(nDiscrete, 1);
        nContParams = nFreeParams - 1;
        optParams_all = zeros(nDiscrete, nContParams);
        
        for nToEval_ind = 1:nDiscrete
            if nFreeParams > 1 % If there's other continuous parameters to optimize over..
                for thisStart = 1:numStarts
                f = @(params) -getPosterior(envInfo, choice(index), whichSubj, [krange(nToEval_ind) params], fixedParams, priorPDFs, nSamples);
                options = optimoptions(@fmincon, 'Display', 'off', 'UseParallel', false, 'MaxFunEvals', nFnEvals);

                [optParams_all(nToEval_ind, :), logposts(nToEval_ind), ~, ~, ~, ~] = ...
                    fmincon(f, starts(thisStart, :), [], [], [], [], ...
                    bounds(1, freeParams), bounds(2, freeParams), [], options);
                logposts(nToEval_ind) = -logposts(nToEval_ind);

                [~, ~, ~, ~, ~, hessians(nToEval_ind)] = fminunc(f, optParams_all(nToEval_ind, :), optimoptions(@fminunc, 'Display', 'Off', 'Algorithm', 'quasi-newton', 'MaxFunEvals', 2));
            else % If there's not..
                logposts(nToEval_ind) = getPosterior(envInfo, choice(index), whichSubj, krange(nToEval_ind), fixedParams, priorPDFs, nSamples);
                hessians(nToEval_ind) = 1;
            end
        end
        
        lme = log((2*pi)^(nContParams / 2) * sum(exp(logposts) .* (det(hessians) ^ (-1/2))));
        [post, optParams_ind] = max(logposts);
        optParams = [krange(optParams_ind) optParams_all(optParams_ind, :)]';
    else % If not, just optimize over continuous & do Laplace approximation.
        f = @(params) -getPosterior(envInfo, choice(index), whichSubj, params, fixedParams, priorPDFs, nSamples);
        options = optimoptions(@fmincon, 'Display', 'off', 'UseParallel', false, 'MaxFunEvals', nFnEvals);

        [optParams, post, ~, ~, ~, ~] = ...
            fmincon(f, starts(thisStart, :), [], [], [], [], ...
            bounds(1, freeParams), bounds(2, freeParams), [], options);
        post = -post;
        
        [~, ~, ~, ~, ~, hessian] = fminunc(f, optParams, optimoptions(@fminunc, 'Display', 'Off', 'Algorithm', 'quasi-newton', 'MaxFunEvals', 2));
        lme = nFreeParams / 2 * log(2*pi) + post - .5 * log(det(hessian));
    end
    
    csvwrite([savePath num2str(taskid) '.txt'], ...
        [post, ...
        getLikelihood(envInfo, choice(index), whichSubj, optParams, fixedParams, nSamples), ...
        lme, ...
        optParams]);
else % If there's no free parameters, just evaluate it at the one place.
    [post, ll] = getPosterior(envInfo, choice(index), whichSubj, [], fixedParams, priorPDFs, nSamples);
    
    csvwrite([savePath num2str(taskid) '.txt'], ...
        [post, ...
        ll, ...
        post]);
end

delete(gcp);
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
%   Currently, nToEval must be fixed, and beta can't be fixed to
%   anything but zero (if nToEval > 1).

function fitModel_cont_unc(dataPath, whichEnv, savePath, numStarts, taskid, fixedParams, priorPDFs, nFnEvals)
%% Load data & env
load(dataPath);
load(whichEnv);

%% Set up
numSubjects = length(subjMarkers);

if (taskid < 1 || taskid > (numStarts * numSubjects))
    error('taskid must be between 1 and (numStarts * numSubjects)');
end
if (fixedParams(1) == -1)
    error('nToEval must be fixed.');
end
if (fixedParams(2) > 0)
    error('beta cannot be fixed higher than 0.');
end

[thisStart, whichSubj] = ind2sub([numStarts, numSubjects * numStarts], taskid);

% Params
freeParams = fixedParams == -1;
numFreeParams = sum(freeParams);
bounds = [1 0 0; 10 2 1];

% Index
if whichSubj < length(subjMarkers)
    index = subjMarkers(whichSubj):(subjMarkers(whichSubj + 1) - 1);
else
    index = subjMarkers(whichSubj):length(choice);
end

% Calculate starts
starts = zeros(numStarts, numFreeParams);
for i = 1:numFreeParams
    bounds_fp = bounds(:, freeParams);
    starts(:,i) = linspace(bounds_fp(1,i),bounds_fp(2,i),numStarts);
end

%% Start!
% Loop through starts
if numFreeParams > 0
    f = @(params) -model_post_cont(envInfo, choice(index), rewards_tr(whichSubj, :), bounds(2, freeParams) .* sin(params), fixedParams, priorPDFs);
    options = optimoptions(@fminunc, 'Display', 'off', 'MaxFunEvals', nFnEvals);

    [optParams, post, ~, ~, ~, hessian] = ...
        fminunc(f, starts(thisStart, :), options);

    optParams = bounds(2, freeParams) .* sin(optParams);
    
    %[~, ~, ~, ~, ~, hessian] = fminunc(f, optParams, optimoptions(@fminunc, 'MaxFunEvals', 300));
    
    csvwrite([savePath num2str(taskid) '.txt'], ...
        [-post, ...
        likelihood_cont(envInfo, choice(index), rewards_tr(whichSubj, :), optParams, fixedParams), ...
        det(hessian), ...
        optParams]);
else
    csvwrite([savePath num2str(taskid) '.txt'], ...
        [model_post_cont(envInfo, choice(index), rewards_tr(whichSubj, :), [], fixedParams, priorPDFs), ...
        likelihood_cont(envInfo, choice(index), rewards_tr(whichSubj, :), [], fixedParams), ...
        1]);
end

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
% fixedParams: which params to fix, and to what values. There are 3
% possible parameters: [nToEval, beta, w_MB].
%   If you want to let a parameter be free, set to -1.
%   Otherwise, set to the value you want to fix it at.
%   Currently, nToEval must be fixed, and beta can't be fixed to
%   anything but zero (if nToEval > 1).

function fitModel_bayes(dataPath, whichEnv, savePath, numStarts, whichSubj, fixedParams, betaInfo, priorPDFs)
%% Load data & env
load(dataPath);
load(whichEnv);

%% Set up
numSubjects = length(subjMarkers);

% whichSubj is from 1 to numSubjects
if (whichSubj < 1 || whichSubj > numSubjects)
    error('whichSubj must be between 1 and numSubjects');
end
if (fixedParams(1) == -1)
    error('nToEval must be fixed.');
end
if (fixedParams(2) > 0)
    error('beta cannot be fixed higher than 0.');
end

% Params
freeParams = fixedParams == -1;
numFreeParams = sum(freeParams);
bounds = [1 0 0; 10 betaInfo(1) 1];

% Index
if whichSubj < length(subjMarkers)
    index = subjMarkers(whichSubj):(subjMarkers(whichSubj + 1) - 1);
else
    index = subjMarkers(whichSubj):length(choice);
end

% If choice set, compute likeArray
if fixedParams(1) > 1
    if fixedParams(2) == 0
        betaInfo = [0 1];
    end
    
    likeArray = getLikeArray(choice(index), fixedParams(1), betaInfo, rewards_tr(whichSubj, :), envInfo{3}, 0);
else
    likeArray = [];
end

% Calculate starts
starts = zeros(numStarts, numFreeParams);
for i = 1:numFreeParams
    bounds_fp = bounds(:, freeParams);
    starts(:,i) = linspace(bounds_fp(1,i),bounds_fp(2,i),numStarts);
end

optParams = zeros(numStarts, numFreeParams);
post = zeros(numStarts, 1);
hessians = cell(numStarts, 1);

%% Start!
% Loop through starts
if numFreeParams > 0
    f = @(params) -model_post(envInfo, choice(index), rewards_tr(whichSubj, :), likeArray, betaInfo, params, fixedParams, priorPDFs);
    options = optimset('Display', 'off');

    for thisStart = 1:numStarts
        if fixedParams(1) == 1
            [optParams(thisStart, :), post(thisStart), ~, ~, ~, ~, hessians{thisStart}] = ...
                fmincon(f, starts(thisStart, :), [], [], [], [], ...
                bounds(1, freeParams), bounds(2, freeParams), [], options);
        else
            optParams(thisStart, :) = starts(thisStart, :);
            post(thisStart) = f(starts(thisStart, :));
            hessians{thisStart} = 1;
        end
    end
    
    [~, bestStart] = min(post);
    csvwrite([savePath 'Params_Subj' num2str(whichSubj) '.txt'], ...
        [-post(bestStart), ...
        likelihood(envInfo, choice(index), rewards_tr(whichSubj, :), likeArray, betaInfo, optParams(bestStart, :), fixedParams), ...
        det(hessians{bestStart}), ...
        optParams(bestStart, :)]);
else
    csvwrite([savePath 'Params_Subj' num2str(whichSubj) '.txt'], ...
        [model_post(envInfo, choice(index), rewards_tr(whichSubj, :), likeArray, betaInfo, [], fixedParams, priorPDFs), ...
        likelihood(envInfo, choice(index), rewards_tr(whichSubj, :), likeArray, betaInfo, [], fixedParams), ...
        1]);
end

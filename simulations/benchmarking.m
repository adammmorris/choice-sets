% results = [1 7 1 27 5 15 3 4 2 10 29 21 13 15 3];
% rewards_tr = [round(normrnd(5, 2, [1 15])) ...
%         normrnd(-5, 2, [1 15])];
% nToEval = 5;
% betaInfo = [1 2];
% load('env/env_30_15_5_2.mat'); rewards_te = envInfo{3};
% 
% %temp = getLikeArray(results, nToEval, betaInfo, rewards_tr, rewards_te, 0);
% temp = likelihood_cont(envInfo, results, rewards_tr, .5, [5 -1 0]);

priorPDFs = {@(x) log(1/4), @(x) log(gampdf(x, 4.82, .88)), @(x) log(betapdf(x, 2, 10)), ...
    @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1))};
fitModel('fitting/wg_v3/real2/sims.mat', 'env/wg_v3.mat', ...
     '', [-1 0 -1 0 0], priorPDFs, 3, 6, 100, 0, false);

% index = subjMarkers(1):(subjMarkers(2)-1);
% curchoice = choice(index);
% % mins = min(rewards_te,[],2);
% % curchoice == mins
% load('env/env_30_15_5_2.mat');
% rewards_te = envInfo{3};
% betaInfo = [.10526 2];
% 
% likeArray = getLikeArray(curchoice, 5, betaInfo, rewards_tr(1, :), rewards_te);
% sum(log(likeArray))
% -model_post(envInfo, choice(index), rewards_tr(1, :), likeArray, betaInfo, .10526, [5 -1 0], priorPDFs)

%a = getLikeArray(curchoice(1), 5, [1, 10], rewards_tr(1, :), rewards_te(1,:), 1, 100000);
clearvars

%% Basics
nAgents = 500;
nWords = 14;
nTrials = 8; % num of test trials

%% Values

%rewards_te = round(28*betarnd(2,3,[nTrials, nWords]));

% rewards_te = [[3, 1, 2, 1, 5, 4, 4, 0, 3, 2, 2, 2, 2, 3], ...
% 		[1, 4, 0, 4, 4, 0, 3, 2, 6, 3, 3, 3, 2, 2], ...
% 		[3, 2, 4, 4, 0, 4, 1, 5, 1, 2, 4, 2, 4, 1], ...
% 		[2, 3, 3, 4, 3, 2, 3, 4, 5, 2, 4, 4, 3, 2], ...
% 		[23, 12, 23, 14, 12, 25, 12, 12, 15, 26, 9, 24, 2, 8], ...
% 		[4, 19, 0, 14, 14, 12, 14, 14, 4, 17, 19, 0, 4, 0], ...
% 		[5, 12, 1, 8, 8, 13, 18, 5, 6, 0, 18, 1, 13, 6], ...
% 		[5, 2, 13, 7, 11, 7, 1, 22, 12, 3, 17, 5, 14, 15], ...
% 		[2, 1, 2, 4, 2, 3, 1, 0, 2, 0, 8, 2, 2, 2], ...
% 		[6, 5, 5, 9, 7, 8, 5, 3, 9, 5, 5, 8, 6, 4], ...
% 		[ 6,  3,  5,  3,  7,  6,  6,  1, 11,  8,  5,  7,  4,  5]];
% multipliers = [10, 10, 10, 10, 5, 5, 5, 5, 10, 10, 10, 10];

%rewards_tr = 1+round(betarnd(1.2,1.2, [maxAgents, nWords])*6);

% possibleRewards = [0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10];
% possibleExposures = [4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8];
% rewards_tr = zeros(nAgents, nWords);
% exposures_tr = zeros(nAgents, nWords);
% for j = 1:nAgents
%     rewards_tr(j, :) = possibleRewards(randperm(nWords));
%     exposures_tr(j, :) = possibleExposures(randperm(nWords));
% end
% maxRe_tr = max(possibleRewards);
% maxExposure_tr = max(possibleExposures);

r = 0;
variance = 4;
mean = [15 15];
sigma = [variance r * variance; r * variance variance];
[mvnrnd(mean, sigma, 10000)

%% Maxes

maxRe_te = max(max(rewards_te));
maxRe_tr = max(max(rewards_tr));
maxExposure_tr = 0;

%% Save

envInfo = {nWords, nTrials, rewards_te, maxRe_te, exposures_tr, maxExposure_tr, rewards_tr, maxRe_tr};
save('env/cor0.mat', 'envInfo');
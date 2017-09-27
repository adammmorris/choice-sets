clearvars

%% Basics
nAgents = 1000;
nWords = 14;
nTrials = 8; % num of test trials

%% Values
possibleRewards_tr = [0, 0, 0, 0, 0, 0, 0, 10, 10, 10, 10, 10, 10, 10];
rewards_tr = zeros(nAgents, nWords);
for j = 1:nAgents
    rewards_tr(j, :) = possibleRewards_tr(randperm(nWords));
end
maxRe_tr = max(possibleRewards_tr);

possibleRewards_te = 1:14;
rewards_te = zeros(nTrials, nWords);
for j = 1:nTrials
    rewards_te(j, :) = possibleRewards_te(randperm(nWords));
end
maxRe_te = max(possibleRewards_te);

%% Save

envInfo = {nWords, nTrials, rewards_tr, maxRe_tr, rewards_te, maxRe_te};
save('env/wg_v6.mat', 'envInfo');
%% getTrialLikelihoods
% Record the trial-by-trial likelihoods from different models, using
% optimal params.

addpath 'utilities';
datapath = 'fitting/value/v2/output.mat';
simspath = 'fitting/value/v2/sims.mat';
load(datapath);
load(simspath);
numSubjects = length(subjMarkers);

modelNames_all = {'mixture-mf-mb', 'mixture-mf', 'mixture-mb', 'random', ...
    'cs-mf-mb', 'cs-mf', 'cs-mb', 'cs-rand', ...
    'cs-amf-mb', 'cs-amf', 'mixture-amf-mb', 'mixture-amf', ...
    'cs-rmf-mb'};
modelParams_all = {[1 -1 0 -1 -1 0 0], [1 -1 0 1 0 0 0], [1 -1 0 0 1 0 0], [1 0 0 0 0 0 0], ...
    [-1 -1 -1 -1 -1 0 0], [-1 -1 -1 1 0 0 0], [-1 -1 -1 0 1 0 0], [-1 0 -1 0 0 0 0], ...
    [-1 -1 -1 -1 -1 0 1], [-1 -1 -1 1 0 0 1], [1 -1 0 -1 -1 0 1], [1 -1 0 1 0 0 1], ...
    [-1 -1 -1 -1 -1 0 2]};

whichModels = [1 5];

indices = cell(numSubjects,1);
for subj = 1:numSubjects
    if subj < length(subjMarkers)
        indices{subj} = subjMarkers(subj):(subjMarkers(subj + 1) - 1);
    else
        indices{subj} = subjMarkers(subj):length(choice);
    end
end

trial_results = zeros(length(choice), length(whichModels));
trial_results_all = zeros(length(choice), 14, length(whichModels));
factors = zeros(length(choice), 6);
factors_all = zeros(length(choice), 14, 2);
for i = 1:length(whichModels)
    model = whichModels(i);
    optParams_cur = optParams{model};
    
    for subj = 1:numSubjects
        index = indices{subj};
        numTrials = length(index);
        for trial = 1:numTrials
            curTrial = index(trial);
            curChoice = choice(curTrial);
            trial_results(curTrial, i) = getLikelihood(curChoice,rewards_s1(subj,:),rewards_s2(curTrial,:),recalled(subj,:),...
                optParams_cur(subj,:),modelParams_all{model},0);
            
            % get potential predictors
            s1 = rewards_s1(subj, curChoice) / max(rewards_s1(subj, :));
            s2 = rewards_s2(curTrial, curChoice) / max(rewards_s2(curTrial, :));
            [~, best_choice] = max(rewards_s2(curTrial, :));
            s1_op = rewards_s1(subj, best_choice) / max(rewards_s1(subj, :));
            factors(curTrial, :) = [s1 s2 s1*s2 s1_op ...
                exp(trial_results(curTrial, i)) exp(getLikelihood_opt(curChoice,rewards_s1(subj,:),rewards_s2(curTrial,:),recalled(subj,:),...
                optParams_cur(subj,:),modelParams_all{model},0))];
            
            for word = 1:14
                if recalled(subj,word)
                    trial_results_all(curTrial, word, i) = getLikelihood(word,rewards_s1(subj,:),rewards_s2(curTrial,:),recalled(subj,:),...
                        optParams_cur(subj,:),modelParams_all{model},0);
                else
                    trial_results_all(curTrial, word, i) = NaN;
                end
                
                s1 = rewards_s1(subj, word) / max(rewards_s1(subj, :));
                s2 = rewards_s2(curTrial, word) / max(rewards_s2(curTrial, :));
                factors_all(curTrial, word, 1) = s1;
                factors_all(curTrial, word, 2) = s2;
            end
            
            % get potential predictors
            s1 = rewards_s1(subj, curChoice) / max(rewards_s1(subj, :));
            s2 = rewards_s2(curTrial, curChoice) / max(rewards_s2(curTrial, :));
            [~, best_choice] = max(rewards_s2(curTrial, :));
            s1_op = rewards_s1(subj, best_choice) / max(rewards_s1(subj, :));
            factors(curTrial, :) = [s1 s2 s1*s2 s1_op ...
                exp(trial_results(curTrial, i)) exp(getLikelihood_opt(curChoice,rewards_s1(subj,:),rewards_s2(curTrial,:),recalled(subj,:),...
                optParams_cur(subj,:),modelParams_all{model},0))];
        end
    end
end

%% Test for stuff
trial_diff = trial_results(:,2) - trial_results(:,1);
hist(trial_diff)

trial_diff_subj = zeros(numSubjects, 1);
pref_subj = zeros(numSubjects, 1);
for subj = 1:numSubjects
    trial_diff_subj(subj) = mean(trial_diff(indices{subj}));
    pref_subj(subj) = mean(trial_diff(indices{subj}) > 0);
end

hist(pref_subj)
ind_best = [];
for i = 1:numSubjects
    if pref_subj(i) > .7
        ind_best = [ind_best indices{i}];
    end
end

% tests: s1 chosen, s2 chosen, interaction, s1 optimal, p(chosen),
% p(optimal)
ind_to_use = 1:length(trial_diff);
%ind_to_use = ind_best;
scatter(factors(ind_to_use,1) + normrnd(0, .01, length(ind_to_use), 1), trial_diff(ind_to_use))
scatter(factors(ind_to_use,2), trial_diff(ind_to_use))
scatter(factors(:,3), trial_diff)
scatter(factors(:,4), trial_diff)
scatter(factors(:,5), trial_diff)
scatter(factors(:,6), trial_diff)

[b, ~, stats] = glmfit(factors, trial_diff);
b
stats.p

% Fiery's plot
scatter(sum(recalled,2), trial_diff_subj);
[r, p] = corr(sum(recalled,2), trial_diff_subj)

% Surface plots
unique_s1 = unique(factors(:,1));
unique_s2 = unique(factors(:,2));
probs = zeros(length(unique_s1), length(unique_s2), length(whichModels));
probs_diff = zeros(length(unique_s1), length(unique_s2));
probs_all = zeros(length(unique_s1), length(unique_s2), length(whichModels));
for i = 1:length(whichModels)
    for j = 1:length(unique_s1)
        s1 = unique_s1(j);
        for k = 1:length(unique_s2)
            s2 = unique_s2(k);
            probs(j,k,i) = mean(exp(trial_results(factors(:,1) == s1 & factors(:,2) == s2, i)));
            probs_diff(j,k) = mean(exp(trial_diff(factors(:,1) == s1 & factors(:,2) == s2)));
            
            trial_results_all_cur = trial_results_all(:,:,i);
            trial_results_all_cur = trial_results_all_cur(:);
            s1_all = factors_all(:,:,1);
            s1_all = s1_all(:);
            s2_all = factors_all(:,:,2);
            s2_all = s2_all(:);
            probs_all(j,k,i) = nanmean(exp(trial_results_all_cur(s1_all == s1 & s2_all == s2)));
        end
    end
end

mesh(unique(factors(:,1)), unique(factors(:,2)), probs(:,:,1)')
xlabel('S1 val')
ylabel('S2 val')
zlabel('Average prob(choice)')
title('Negative-MF CS model')


mesh(unique(factors(:,1)), unique(factors(:,2)), probs_diff')
xlabel('S1 val')
ylabel('S2 val')
zlabel('Average prob(choice)')
title('CS minus NegMF CS')

mesh(unique(factors(:,1)), unique(factors(:,2)), probs_all(:,:,1)')
xlabel('S1 val')
ylabel('S2 val')
zlabel('Average prob. of choosing')
title('Mixture')

mesh(unique(factors(:,1)), unique(factors(:,2)), (probs_all(:,:,2) - probs_all(:,:,1))')
xlabel('S1 val')
ylabel('S2 val')
zlabel('Average prob. of choosing')
title('Difference between CS / Mixture')
clearvars

datapath = '../data/cs_wg_v10/real2/';
savepath = 'fitting/wg_v10/real2/';

f = fopen([datapath 'choices.csv']);
choice_csv = textscan(f, '%f %f %s %*[^\n]', 'Delimiter', ',');
fclose(f);

rewards_tr = csvread([datapath 'rewards_tr.csv'], 1, 0);
recalled = csvread([datapath 'recalled.csv'], 1, 0);
poss = csvread([datapath 'poss.csv'], 1, 0);

subj = choice_csv{1};
subjMarkers = getSubjMarkers(subj);
choice = choice_csv{2};
rewards_te_trial_str = choice_csv{3};

rewards_te_trial = zeros(length(rewards_te_trial_str), 14);
for j = 1:length(rewards_te_trial_str)
    rewards_te_trial(j, :) = str2num(rewards_te_trial_str{j}(2:(end-1)));
end

save([savepath 'sims.mat'], 'choice', 'rewards_te_trial', 'subjMarkers', 'rewards_tr', 'recalled', 'poss');
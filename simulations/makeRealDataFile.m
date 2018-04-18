clearvars

datapath = '../data/value/v2/real1/';
savepath = 'fitting/value/v2/';

f = fopen([datapath 'choices.csv']);
choice_csv = textscan(f, '%f %f %s %*[^\n]', 'Delimiter', ',');
fclose(f);

rewards_s1 = csvread([datapath 'rewards_s1.csv'], 1, 0);
recalled = csvread([datapath 'recalled.csv'], 1, 0);
%poss = csvread([datapath 'poss.csv'], 1, 0);

subj = choice_csv{1};
subjMarkers = getSubjMarkers(subj);
choice = choice_csv{2};
rewards_s2_str = choice_csv{3};

rewards_s2 = zeros(length(rewards_s2_str), 14);
for j = 1:length(rewards_s2_str)
    rewards_s2(j, :) = str2num(rewards_s2_str{j}(2:(end-1)));
end

save([savepath 'sims_45.mat'], 'choice', 'rewards_s2', 'subjMarkers', 'rewards_s1', 'recalled');
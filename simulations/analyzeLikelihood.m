envName = '30.15.unif.0.10';
simsName = 'sims_randcs';

load(['env/' envName '.mat']);
numWords = envInfo{1};
numTrials = envInfo{2};
rewards_te = envInfo{3};

main = ['fitting/' envName '/' simsName];
load([main '/sims_unifK.mat']);
numSubj = length(subjMarkers);

modelNames = {'fit_randcs_freeK', 'fit_puremb'}; % preferred should be first
params = {[-1 0 0], [1 -1 1]};
whichParams = {1, 2};
optParams_all = {parseFitOutput([main '/' modelNames{1} '/'], numSubj, 10, whichParams{1}, 3), ...
    parseFitOutput([main '/' modelNames{2} '/'], numSubj, 10, whichParams{2}, 3)};

diffs = zeros(numSubj, numTrials);
results = zeros(numSubj * numTrials, 11); % subj trial diff MFval MBval nToEval beta w_MB
maxDiff = zeros(numSubj, 2);

result_counter = 1;

for subj = 1:numSubj
    likes = zeros(numTrials, 2);
    
    if subj < length(subjMarkers)
        index = subjMarkers(subj):(subjMarkers(subj + 1) - 1);
    else
        index = subjMarkers(subj):length(choice);
    end
    
    for model = 1:length(modelNames)
        optParams = optParams_all{model}(subj, 4:end);
        
        for trial = 1:numTrials
            likes(trial, model) = getLikelihood(envInfo, choice(index), rewards_tr(subj, :), optParams, params{model}, trial);
        end
    end
    
    diffs(subj, :) = likes(:, 1) - likes(:, 2);
    for trial = 1:numTrials
        curchoice = choice(index(trial));
        
        if rewards_tr(subj, curchoice) > median(rewards_tr(subj, :)), mftop = 1;
        elseif rewards_tr(subj, curchoice) == median(rewards_tr(subj, :)), mftop = randi(2)-1;
        else mftop = 0;
        end
            
        if rewards_te(trial, curchoice) > median(rewards_te(trial, :)), mbtop = 1;
        elseif rewards_te(trial, curchoice) == median(rewards_te(trial, :)), mbtop = randi(2)-1;
        else mbtop = 0;
        end
        
        mfrank = tiedrank(rewards_tr(subj, :));
        mbrank = tiedrank(rewards_te(trial, :));
        
        results(result_counter, :) = [subj trial diffs(subj, trial) ...
            rewards_tr(subj, curchoice) rewards_te(trial, curchoice) mftop mbtop mfrank(curchoice) mbrank(curchoice) ...
            optParams_all{1}(subj, 4) optParams_all{2}(subj, 4)];
        result_counter = result_counter + 1;
    end
end

[maxDiff(:, 1), maxDiff(:, 2)] = max(diffs, [], 2);

df = array2table(results, 'VariableNames', {'Subj', 'Trial', 'Diff', ...
    'MFval', 'MBval', 'MFtop', 'MBtop', 'MFrank', 'MBrank' ...
    'nToEval', 'beta'});
writetable(df, 'analyzeLikelihood.csv');
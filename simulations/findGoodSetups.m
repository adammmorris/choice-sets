%% Searches through possible s1/s2 combinations, looking for biggest model sensitivity

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

numWords = 14;
maxValue = 50;
numMenus = 500;

probs_test = zeros(numWords, length(whichModels), numMenus);
probs_test_diff = zeros(numWords, numMenus);
menu_s1s = zeros(numWords, numMenus);
menu_s2s = zeros(numWords, numMenus);

parfor menu = 1:numMenus
    s1s_test = fastrandsample(ones(1, maxValue) / maxValue, numWords) / maxValue;
    s2s_test = fastrandsample(ones(1, maxValue) / maxValue, numWords) / maxValue;
    
    for i = 1:length(whichModels)
        model = whichModels(i);
        for word = 1:numWords
            probs_test_temp = zeros(numSubjects, 1);
            for subj = 1:numSubjects
                probs_test_temp(subj) = exp(getLikelihood(word, s1s_test, s2s_test, ...
                    ones(1, numWords), optParams{model}(subj, :), modelParams_all{model}, 0));
            end
            probs_test(word,i,menu) = mean(probs_test_temp);
        end
    end
end

for menu = 1:numMenus
    probs_test_diff(:,menu) = probs_test(:, 2,menu) - probs_test(:, 1,menu);
end

% best avg difference?
hist(max(probs_test_diff))

% unique_s1 = unique(s1s_test);
% unique_s2 = unique(s2s_test);
% probs_test_graph = zeros(length(unique_s1), length(unique_s2), length(whichModels));
% probs_test_diff_graph = zeros(length(unique_s1), length(unique_s2));
% for i = 1:length(whichModels)
%     for j = 1:length(unique_s1)
%         s1 = unique_s1(j);
%         for k = 1:length(unique_s2)
%             s2 = unique_s2(k);
%             probs_test_graph(j,k,i) = nanmean(exp(probs_test(s1s_test == s1 & s2s_test == s2, i)));
%             probs_test_diff_graph(j,k) = nanmean(exp(probs_test(s1s_test == s1 & s2s_test == s2, 2)) - exp(probs_test(s1s_test == s1 & s2s_test == s2, 1)));
%         end
%     end
% end

%surf(unique_s1, unique_s2, probs_test_diff_graph')
% scatter3(s1s_test, s2s_test, probs_test_diff)
% xlabel('S1 val')
% ylabel('S2 val')
% zlabel('Average prob(choice)')
% title('CS minus mixture model')
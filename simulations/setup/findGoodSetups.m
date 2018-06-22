%% Searches through possible s1/s2 combinations, looking for biggest model sensitivity

addpath '..';
addpath '../utilities';
%datapath = '../fitting/value/v2/output.mat';
%simspath = '../fitting/value/v2/sims.mat';
%load(datapath);
%load(simspath);
%numSubjects = length(subjMarkers);
numSubjects = 100;

modelNames_all = {'mixture-mf-mb', 'mixture-mf', 'mixture-mb', 'random', ...
    'cs-mf-mb', 'cs-mf', 'cs-mb', 'cs-rand', ...
    'cs-amf-mb', 'cs-amf', 'mixture-amf-mb', 'mixture-amf', ...
    'cs-rmf-mb'};
modelParams_all = {[1 -1 0 -1 -1 0 0], [1 -1 0 1 0 0 0], [1 -1 0 0 1 0 0], [1 0 0 0 0 0 0], ...
    [-1 -1 -1 -1 -1 0 0], [-1 -1 -1 1 0 0 0], [-1 -1 -1 0 1 0 0], [-1 0 -1 0 0 0 0], ...
    [-1 -1 -1 -1 -1 0 1], [-1 -1 -1 1 0 0 1], [1 -1 0 -1 -1 0 1], [1 -1 0 1 0 0 1], ...
    [-1 -1 -1 -1 -1 0 2]};

whichModels = [1 6];

numWords = 12;
numMenus = 20;

probs_test = zeros(numWords, length(whichModels), numMenus);
probs_test_diff = zeros(numWords, numMenus);
menu_s1s = zeros(numWords, numMenus);
menu_s2s = zeros(numWords, numMenus);

for menu = 1:numMenus
    disp(menu);
    s1s_test = betarnd(rand() * 5, rand() * 5,1,numWords);
    s2s_test = betarnd(rand() * 5, rand() * 5,1,numWords);
    %s1s_test = [10, 5, zeros(1, 12)];
    %s2s_test = [0, 5, 10, zeros(1,11)];
    
    for i = 1:length(whichModels)
        model = whichModels(i);
        for word = 1:numWords
            probs_test_temp = zeros(numSubjects, 1);
            parfor subj = 1:numSubjects
                probs_test_temp(subj) = exp(likelihood(word, s1s_test, s2s_test, ...
                    ones(1, numWords), [3 betarnd(4.5, 1) betarnd(4.5, 1)], modelParams_all{model}));
            end
            probs_test(word,i,menu) = mean(probs_test_temp);
        end
    end
    
    menu_s1s(:, menu) = s1s_test;
    menu_s2s(:, menu) = s2s_test;
end

for menu = 1:numMenus
    probs_test_diff(:,menu) = probs_test(:, 2,menu) - probs_test(:, 1,menu);
end

save('menu_output.mat', 'probs_test', 'probs_test_diff', 'menu_s1s', 'menu_s2s');

% best avg difference?
hist(max(abs(probs_test_diff)))
scatter3(menu_s1s(:), menu_s2s(:), probs_test_diff(:))
xlabel('S1 val')
ylabel('S2 val')
zlabel('Average prob(choice)')
title('CS minus mixture model')

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
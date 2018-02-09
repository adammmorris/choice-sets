%% fitResults
% For a given dataset, get all the model fitting results, and compare
% models.

envName = 'wg_v10';
simsName = 'real2';
realData = true;

whichEnv = ['env/' envName '.mat'];
main = ['fitting/' envName '/' simsName];
datapath = [main '/sims.mat'];

load(whichEnv);
load(datapath);

numWords = envInfo{1};
numTrials = envInfo{2};

% pressure
%subjlist = [5   6   8   9  11  12  16  18  19  20  24  25  29  32  34  36  37  38  39  42  43  44  46  50  51  55  56  58  61  64  66  67  69  74 77  78  79  82  84  86  87  88  89  91  93  96 100 103 107 116 117 119 120 121 122 123 124 125 126 127 129 130 131 132 136 137 138 141 147 150 156 157 160 163 165 166 167 168 171 172 173 174 176 177 183 186 188 191 192 195 196 198 199];
% delay
%subjlist = [1   2   3   4   7  10  13  14  15  17  21  22  23  26  27  28  30  31  33  35  40  41  45  47  48  49  52  53  54  57  59  60  62  63 65  68  70  71  72  73  75  76  80  81  83  85  90  92  94  95  97  98  99 101 102 104 105 106 108 109 110 111 112 113 114 115 118 128 133 134 135 139 140 142 143 144 145 146 148 149 151 152 153 154 155 158 159 161 162 164 169 170 175 178 179 180 181 182 184 185 187 189 190 193 194 197 200 201 202];
% said nothing was impossible
subjlist = find(sum(poss == 1,2) == 14);
%subjlist = 1:length(subjMarkers);


%numSubjects = length(subjMarkers);
numSubjects = length(subjlist);
numChoices = zeros(numSubjects, 1);
LLs_chance = zeros(numSubjects, 1);
for subj_ind = 1:length(subjlist)
    subj = subjlist(subj_ind);
    if subj < length(subjMarkers)
        index = subjMarkers(subj):(subjMarkers(subj + 1) - 1);
    else
        index = subjMarkers(subj):length(choice);
    end
    
    numChoices(subj_ind) = length(index);
    LLs_chance(subj_ind) = log(1 / sum(recalled(subj, :))) * length(index);
end
%numChoices = repmat(numTrials, numSubjects, 1);
%LLs_chance = log((1 / numWords)) * numChoices;

modelNames_all = {'mixture-mf-mb', 'mixture-mf', 'mixture-mb', 'random', ...
    'cs-mf-mb', 'cs-mf', 'cs-mb', 'cs-rand', ...
    'cs-amf-mb', 'cs-amf', 'mixture-amf-mb', 'mixture-amf', ...
    'cs-free', 'cs-mf-mb-poss', 'cs-mb-poss'};
modelParams_all = {[1 -1 0 -1 -1 0], [1 -1 0 1 0], [1 -1 0 0 1], [1 0 0 0 0], ...
    [-1 -1 -1 -1 -1 0 0], [-1 -1 -1 1 0], [-1 -1 -1 0 1 0], [-1 0 -1 0 0 0], ...
    [-1 -1 -1 -1 -1 0 1], [-1 -1 -1 1 0], [1 -1 0 -1 -1], [1 -1 0 1 0], ...
    [-1 1 -1 -1 -1], [-1 -1 -1 -1 -1 -1 0], [-1 -1 -1 0 -1 -1]};

whichParams_all = cell(length(modelParams_all), 1);
for j = 1:length(modelParams_all)
    whichParams_all{j} = find(modelParams_all{j} == -1);
end
%whichParams_all = {1:3, 1:2, 1:2, 1:2, 1, 2:4, 2:3, [2 4], 2, []};

whichModels = [5 9 14];

modelNames = modelNames_all(whichModels);
whichParams = whichParams_all(whichModels);

numModels = length(modelNames);
paramEstimates = cell(numModels, 1);
negLLs = zeros(numSubjects, numModels);
goodSubjects = true(numSubjects, 1);
corrs = cell(numModels);

numExtraVars = 3; % [post LL LME]

for i = 1:numModels
    savePath = [main '/fit_' modelNames{i} '/'];
    [paramEstimates{i}, goodSubjects_cur, corrs{i}] = ...
        parseFitOutput(savePath, subjlist, whichParams{i}, numExtraVars, datapath, realData);
    goodSubjects = goodSubjects & goodSubjects_cur; % drop any subjects we didn't get
end

for i = 1:numModels
    paramEstimates{i}(~goodSubjects, :) = [];
    paramEstimates{i}(paramEstimates{i} == -Inf) = -realmax;
    paramEstimates{i}(paramEstimates{i} == Inf) = realmax;
end

[params, details] = generateParamsCell(paramEstimates{:});

%% Model comparison
compareModels_bayes(params, details, 1, numChoices, LLs_chance(goodSubjects));

%% Find good subj
sprintf('%.2d,', paramEstimates{5}(:,7))
hist(paramEstimates{5}(:,7))
%histcounts(paramEstimates{5}(:,7),11)
%[h, p] = ttest(paramEstimates{13}(:, 6))
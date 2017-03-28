envName = 'wg_v3';
whichEnv = ['env/' envName '.mat'];
simsName = 'real2';

numStarts = 24;
numSubjects = 114;
numFnEvals = 100;

priorPDFs = {@(x) log(1/4), @(x) log(gampdf(x, 4.82, .88)), ...
    @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1)), ...
    @(x) log(unifpdf(x, 0, 1))};

main = ['fitting/' envName '/' simsName];
modelNames_all = {'cs-rand', 'mixture-all', 'mixture-expose-mb', 'mixture-mf-mb', 'mixture-mb-av', 'mixture-mb-chosen', 'mixture-mb', 'mixture-mf', 'random',...
    'cs-mf-mb', 'cs-mf', 'cs-mb'};
modelParams_all = {[-1 0 0 0 0 0 0], [1 -1 -1 -1 -1 0 0], ...
    [1 -1 -1 0 -1 0 0], [1 -1 0 -1 -1 0 0], [1 -1 0 0 -1 -1 0], [1 -1 0 0 -1 0 -1], ...
    [1 -1 0 0 1 0 0], [1 -1 0 1 0 0 0], [1 0 0 0 0 0 0], ...
    [-1 -1 0 -1 -1 0 0], [-1 -1 0 1 0 0 0], [-1 -1 0 0 1 0 0]};

%whichModels = [1 4 7 8 9];
%whichModels = 1;
whichModels = [10 11 12];

modelNames = modelNames_all(whichModels);
modelParams = modelParams_all(whichModels);
numModels = length(whichModels);

for model = 1:numModels
    params = modelParams{model};
    name = modelNames{model};
    for i = 1:numSubjects
        fitModel([main '/sims.mat'], whichEnv, [main '/fit_' name '/'], ...
            params, priorPDFs, i, numStarts, numFnEvals, 0, false);
    end
end
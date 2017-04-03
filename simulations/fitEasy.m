envName = 'wg_v3';
whichEnv = ['env/' envName '.mat'];
simsName = 'real2';

numStarts = 6;
numSubjects = 114;
numFnEvals = 100;

priorPDFs = {@(x) log(1/2), @(x) log(gampdf(x, 4.82, .88)), @(x) log(gampdf(x, 4.82, .88)), ... %@(x) log(betapdf(x, 2, 10)), ...
    @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1))};

main = ['fitting/' envName '/' simsName];
modelNames_all = {'mixture-mf-mb', 'mixture-mb', 'mixture-mf', 'random', ...
    'cs-rand', 'cs-mf-mb', 'cs-mf', 'cs-mb', 'cs-mf-mb-noK', 'cs-mf-noK', 'cs-mb-noK', 'cs-rand-noK'};
modelParams_all = {[1 -1 .1 -1 -1], [1 -1 .1 0 1], [1 -1 .1 1 0], [1 0 .1 0 0], ...
    [-1 0 -1 0 0], [-1 -1 -1 -1 -1], [-1 -1 -1 1 0], [-1 -1 -1 0 1], [2 -1 .1 -1 -1], [2 -1 .1 1 0], [2 -1 .1 0 1], [2 0 .1 0 0]};

whichModels = 6:8;

modelNames = modelNames_all(whichModels);
modelParams = modelParams_all(whichModels);
numModels = length(whichModels);

for model = 1:numModels
    disp(['model ' num2str(model)]);
    params = modelParams{model};
    name = modelNames{model};
    parfor i = 1:numSubjects
        disp(['subject ' num2str(i)]);
        fitModel([main '/sims.mat'], whichEnv, [main '/fit_' name '/'], ...
            params, priorPDFs, i, numStarts, numFnEvals, 0, false);
    end
end
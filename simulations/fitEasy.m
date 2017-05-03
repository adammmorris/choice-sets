envName = 'wg_v3';
whichEnv = ['env/' envName '.mat'];
simsName = 'real2';

numStarts = 12;
numSubjects = 120;
numFnEvals = 200;

priorPDFs = {@(x) log(1/3), @(x) log(gampdf(x, 4.5, 1)), @(x) log(gampdf(x, 4.5, 1)), ...
    @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1))};

main = ['fitting/' envName '/' simsName];
modelNames_all = {'mixture-mf-mb', 'mixture-mf', 'mixture-mb', 'random', ...
    'cs-mf-mb', 'cs-mf', 'cs-mb', 'cs-rand'};
modelParams_all = {[1 -1 0 -1 -1], [1 -1 0 1 0], [1 -1 0 0 1], [1 0 0 0 0], ...
    [-1 -1 -1 -1 -1], [-1 -1 -1 1 0], [-1 -1 -1 0 1], [-1 0 -1 0 0]};

whichModels = 5:8;

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
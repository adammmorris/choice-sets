addpath 'utilities';
numStarts = 5;
numFnEvals = 200;

datapath = 'fitting/value/v4/';

priorPDFs = {@(x) 1/4, @(x) unifpdf(x, 0, 10), @(x) unifpdf(x, 0, 10), ...
    @(x) unifpdf(x, 0, 1), @(x) unifpdf(x, 0, 1), @(x) unifpdf(x, 0, 1), @(x) 1/2};

modelNames_all = {'mixture-mf-mb', 'mixture-mf', 'mixture-mb', 'random', ...
    'cs-mf-mb', 'cs-mf', 'cs-mb', 'cs-rand', ...
    'cs-amf-mb', 'cs-amf', 'mixture-amf-mb', 'mixture-amf', ...
    'cs-rmf-mb'};
modelParams_all = {[1 -1 0 -1 0 0 0], [1 -1 0 1 0 0 0], [1 -1 0 0 1 0 0], [1 0 0 0 0 0 0], ...
    [-1 -1 -1 -1 0 0 0], [-1 -1 -1 1 0 0 0], [-1 -1 -1 0 1 0 0], [-1 0 -1 0 0 0 0], ...
    [-1 -1 -1 -1 0 0 1], [-1 -1 -1 1 0 0 1], [-1 -1 0 -1 0 0 1], [1 -1 0 1 0 0 1], ...
    [-1 -1 -1 -1 0 0 2]};

whichModels = [1];

modelNames = modelNames_all(whichModels);
modelParams = modelParams_all(whichModels);
numModels = length(whichModels);

results = cell(numModels, 1);
optParams = cell(numModels, 1);
hessians = cell(numModels, 1);

for model = 1:numModels
    disp(['model ' num2str(model)]);
    params = modelParams{model};
    name = modelNames{model};
    
    [results{model}, optParams{model}, hessians{model}] = fit_fixed([datapath '/sims.mat'], params, priorPDFs, numStarts, numFnEvals);
end

save([datapath '/output_fixed.mat'], 'results', 'optParams', 'hessians');
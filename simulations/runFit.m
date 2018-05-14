addpath 'utilities';
numStarts = 10;
numFnEvals = 200;

datapath = 'fitting/value/v3_2/';

modelNames_all = {'mixture-mf-mb', 'mixture-mf', 'mixture-mb', 'random', ...
    'cs-mf-mb', 'cs-mf', 'cs-mb', 'cs-rand', ...
    'cs-amf-mb', 'cs-amf', 'mixture-amf-mb', 'mixture-amf', ...
    'cs-rmf-mb', 'mixture-mf-mb-sep', 'cs-mf-mb-sep', 'cs-mf-sep'};
modelParams_all = {[1 -1 0 -1 0 0 0 0], [1 -1 0 1 0 0 0 0], [1 -1 0 0 1 0 0 0], [1 0 0 0 0 0 0 0], ...
    [3 -1 -1 -1 0 0 0 0], [3 -1 -1 1 0 0 0 0], [-1 -1 -1 0 1 0 0 0], [-1 0 -1 0 0 0 0 0], ...
    [-1 -1 -1 -1 -1 0 1 0], [-1 -1 -1 1 0 0 1 0], [1 -1 0 -1 -1 0 1 0], [1 -1 0 1 0 0 1 0], ...
    [-1 -1 -1 -1 -1 0 2 0], [1 1 0 -1 -1 0 0 1], [3 1 -1 -1 -1 0 0 1], [3 1 -1 -1 0 0 0 1]};

whichModels = [1 5 6 14 15 16];

modelNames = modelNames_all(whichModels);
modelParams = modelParams_all(whichModels);
numModels = length(whichModels);

results = cell(numModels, 1);
optParams = cell(numModels, 1);

for model = 1:numModels
    disp(['model ' num2str(model)]);
    params = modelParams{model};
    name = modelNames{model};
    
    if params(end) == 1 % sep
        priorPDFs = {@(x) 1/3, @(x) unifpdf(x, 0, 10), @(x) unifpdf(x, 0, 10), ...
            @(x) unifpdf(x, 0, 10), @(x) unifpdf(x, 0, 10), @(x) unifpdf(x, 0, 1), @(x) 1/2};
    else
        priorPDFs = {@(x) 1/3, @(x) unifpdf(x, 0, 10), @(x) unifpdf(x, 0, 10), ...
            @(x) unifpdf(x, 0, 1), @(x) unifpdf(x, 0, 1), @(x) unifpdf(x, 0, 1), @(x) 1/2};
    end
    
    [results{model}, optParams{model}] = fit([datapath '/sims.mat'], params, priorPDFs, numStarts, numFnEvals);
end

save([datapath '/output_test.mat'], 'results', 'optParams');
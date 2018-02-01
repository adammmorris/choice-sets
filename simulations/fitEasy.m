envNames = {'wg_v10'};
numSubjects_all = [205];
simsNames = {'real2'};

for i = 1:length(envNames)
    envName = envNames{i};
    whichEnv = ['env/' envName '.mat'];
    simsName = simsNames{i};
    
    numSubjects = numSubjects_all(i);
    numStarts = 10;
    numFnEvals = 200;
    
    % CHANGE THIS BACK
     priorPDFs = {@(x) log(1/3), @(x) log(gampdf(x, 4.5, 1)), @(x) log(gampdf(x, 4.5, 1)), ...
         @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1)), @(x) log(unifpdf(x, 0, 1))};
%    priorPDFs = {@(x) log(1/3), @(x) log(gampdf(x, 4.5, 1)), @(x) log(gampdf(x, 4.5, 1)), ...
%        @(x) log(unifpdf(x, -10, 10)), @(x) log(gampdf(x, 4.5, 1))};
    
    main = ['fitting/' envName '/' simsName];
    modelNames_all = {'mixture-mf-mb', 'mixture-mf', 'mixture-mb', 'random', ...
        'cs-mf-mb', 'cs-mf', 'cs-mb', 'cs-rand', ...
        'cs-amf-mb', 'cs-amf', 'mixture-amf-mb', 'mixture-amf', ...
        'cs-free', 'cs-mf-mb-poss', 'cs-mb-poss'};
    modelParams_all = {[1 -1 0 -1 -1], [1 -1 0 1 0], [1 -1 0 0 1], [1 0 0 0 0], ...
        [-1 -1 -1 -1 -1], [-1 -1 -1 1 0], [-1 -1 -1 0 1], [-1 0 -1 0 0], ...
        [-1 -1 -1 -1 -1], [-1 -1 -1 1 0], [1 -1 0 -1 -1], [1 -1 0 1 0], ...
        [-1 1 -1 -1 -1], [-1 -1 -1 -1 -1 -1], [-1 -1 -1 0 -1 -1]};
    
    whichModels = 14:15;
    
    modelNames = modelNames_all(whichModels);
    modelParams = modelParams_all(whichModels);
    numModels = length(whichModels);
    
    for model = 1:numModels
        disp(['model ' num2str(model)]);
        params = modelParams{model};
        name = modelNames{model};
        parfor subj = 1:numSubjects
            disp(['subject ' num2str(subj)]);
            fitModel([main '/sims.mat'], whichEnv, [main '/fit_' name '/'], ...
                params, priorPDFs, subj, numStarts, numFnEvals, 0, false);
        end
    end
end
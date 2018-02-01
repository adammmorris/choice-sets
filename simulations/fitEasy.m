envNames = {'wg_v10'};
subjlist = [5   6   8   9  11  19  20  24  25  30  37  39  41  46  53  57  58  66  68  69  71  80  86  88  89  90  91  93 109 118 119 122 124 125 128 129 131 133 134 138 139 140 143 149 159 162 167 168 173 176 178 179 180 189 191 194 195 198 199 202];
numSubjects_all = length(subjlist);
simsNames = {'real2'};

for i = 1:length(envNames)
    envName = envNames{i};
    whichEnv = ['env/' envName '.mat'];
    simsName = simsNames{i};
    
    numSubjects = numSubjects_all(i);
    numStarts = 5;
    numFnEvals = 150;
    
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
            subj_real = subjlist(subj);
            disp(['subject ' num2str(subj_real)]);
            fitModel([main '/sims.mat'], whichEnv, [main '/fit_' name '/'], ...
                params, priorPDFs, subj_real, numStarts, numFnEvals, 0, false);
        end
    end
end
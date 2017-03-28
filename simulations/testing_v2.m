numSubjects = 200;
numTrials = 15;
numWords = 30;
nToEval = 5;

cs = false;
beta = .1;

results_long = zeros(numSubjects * numTrials * numWords, 6);
results_long_ind = 1;

v2_all = randi([0 10], [numWords, numTrials]);
for subj = 1:numSubjects
    v1 = randi([-5 5], [numWords, 1]);
    
    for trial = 1:numTrials
        v2 = v2_all(:, trial);
        
        if ~cs
            probs = exp(beta*(v1+v2))/sum(exp(beta*(v1+v2)));
            choice = fastrandsample(probs, 1);
        else
            probs = exp(beta*v1)/sum(exp(beta*v1));
            [~, options] = sort(exprnd(1, [numWords, 1]) ./ probs);
            toEval = options(1:nToEval);
            [~, choice_ind] = max(v2(toEval));
            choice = toEval(choice_ind);
        end
        
        for word = 1:numWords
            results_long(results_long_ind,:) = [subj trial word (choice == word) v1(word) v2(word)];
            results_long_ind = results_long_ind + 1;
        end
    end
end

df = array2table(results_long, 'VariableNames', {'Subj', 'Trial', 'OptionID', 'Choice', 'MFval', 'MBval'});
writetable(df, 'results/testing.csv');
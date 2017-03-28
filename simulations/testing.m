results = [];

numSubjects = 200;
numTrials = 15;
numWords = 30;
nToEval = 5;

cs = false;
beta = .1;

v2_all = sort(randi([-5 5], [numWords, numTrials]));
for subj = 1:numSubjects
    v1 = randi([-5 5], [numWords, 1]);
    v3 = randi([-5 5], [numWords, 1]);
    
    for trial = 1:numTrials
        v2 = v2_all(:, trial);
        if ~cs
            probs = exp(beta*(v1+v2+v3))/sum(exp(beta*(v1+v2+v3)));
            choice = randsample(numWords,1,true,probs);
        else
            probs = exp(beta*v1)/sum(exp(beta*v1));
            [~, options] = sort(exprnd(1, [numWords, 1]) ./ probs);
            toEval = options(1:nToEval);
            [~, choice_ind] = max(v2(toEval));
            choice = toEval(choice_ind);
        end
        
        if v1(choice) > median(v1), mftop = 1;
        elseif v1(choice) == median(v1), mftop = randi(2)-1;
        else mftop = 0;
        end

        if v2(choice) > median(v2), mbtop = 1;
        elseif v2(choice) == median(v2), mbtop = randi(2)-1;
        else mbtop = 0;
        end

        v1_rank = tiedrank(v1);
        v2_rank = tiedrank(v2);
        
        results(end+1,:) = [subj trial choice v1(choice) v2(choice) ...
            mftop mbtop v1_rank(choice) v2_rank(choice) sum(v2_rank == v2_rank(choice))];
    end
end

df = array2table(results, 'VariableNames', ...
    {'Subj', 'Trial', 'Choice', 'MFval', 'MBval', ...
    'MF_top', 'MB_top', 'MFrank', 'MBrank', 'MB_numOfRank'});
writetable(df, 'results/testing_cs.csv');

df = array2table([(1:numWords)' v2_rank], 'VariableNames', {'Choice', 'MBrank'});
writetable(df, 'results/MBrank.csv');
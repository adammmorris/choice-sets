numWords = 12;
%mf = [0 0 0 0 0 0 0 1 1 1 1 1 1 1]; % [1 2.5 2.5 4]
mf = [zeros(1,numWords/2) ones(1,numWords/2)];
mb = [1:numWords/2 1:numWords/2];
%mb = [zeros(1,numWords/2) ones(1,numWords/2)];
%mb = linspace(0,1,numWords);
%mb = mb(randperm(length(mb)));

%mb = [     9     7    10    12     2     6     1     5    11     8     4    14    13     3];
%mb = [    11    12     2    13    14     9     8     4     6     3     1     7    10     5];

numb = 10;
ratios = zeros(numb, 1);
brange = linspace(1,10,numb);

for b = 1:length(brange)
    probs = zeros(numWords,1);
    for i = 1:numWords
        probs(i) = exp(getLikelihood({numWords, 0, mf, max(mf)}, i, mb, true(1, numWords), 1, [], [2, brange(b), brange(b), 1, 0], 0));
    end
    
    probs1 = mean(probs(mf == 0 & mb <= 3));
    probs2 = mean(probs(mf == 0 & mb > 3));
    probs3 = mean(probs(mf == 1 & mb <= 3));
    probs4 = mean(probs(mf == 1 & mb > 3));
%     probs1 = mean(probs(mf == 0 & mb == 0));
%     probs2 = mean(probs(mf == 0 & mb == 1));
%     probs3 = mean(probs(mf == 1 & mb == 0));
%     probs4 = mean(probs(mf == 1 & mb == 1));
    ratios(b) = (probs4 / probs3) / (probs2 / probs1);
end

plot(ratios)

%% mixture
numWords = 10;
mf = [zeros(1,numWords/2) ones(1,numWords/2)];
mb = [1:numWords/2 1:numWords/2];

numb = 10;
ratios = zeros(numb, 1);
brange = linspace(1,10,numb);

for b = 1:length(brange)
    probs = zeros(numWords,1);
    for i = 1:numWords
        probs(i) = exp(getLikelihood({4, 0, mf, max(mf)}, i, mb, true(1, numWords), 1, [], [1, brange(b), 0, .5, .5], 0));
    end
    
    probs1 = mean(probs(mf == 0 & mb == 1));
    probs2 = mean(probs(mf == 0 & mb == 5));
    probs3 = mean(probs(mf == 1 & mb == 1));
    probs4 = mean(probs(mf == 1 & mb == 5));
%     probs1 = mean(probs(mf == 0 & mb == 0));
%     probs2 = mean(probs(mf == 0 & mb == 1));
%     probs3 = mean(probs(mf == 1 & mb == 0));
%     probs4 = mean(probs(mf == 1 & mb == 1));
    ratios(b) = (probs4 / probs3) / (probs2 / probs1);
end

plot(ratios)

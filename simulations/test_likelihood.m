wordprobs = zeros(14,1);
for k = 1:14
    wordprobs(k) = exp(getLikelihood(envInfo, k, rewards_te_trial(1,:), true(3,14), whichSubj, [14 5 3 .5 .5], fixedParams, nSamples));
end

wordprobs
sum(wordprobs)
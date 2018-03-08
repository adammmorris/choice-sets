function [logp, logl] = getPosterior(choices, rewards_s1, rewards_s2, recalled, whichSubj, freeParams, fixedParams, priorPDFs, nSamples)
logl = getLikelihood(choices, rewards_s1, rewards_s2, recalled, whichSubj, freeParams, fixedParams, nSamples);
logp = logl;

whichParams = find(fixedParams == -1);
for k = 1:length(whichParams)
    logp = logp + log(priorPDFs{whichParams(k)}(freeParams(k)));
end

if logp == -Inf, logp = -realmax; end
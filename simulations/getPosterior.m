function [logp, logl] = getPosterior(envInfo, choices, rewards_te_trial, recalled, whichSubj, freeParams, fixedParams, priorPDFs, nSamples)
% CHANGE THIS BACK
logl = getLikelihood(envInfo, choices, rewards_te_trial, recalled, whichSubj, freeParams, fixedParams, nSamples);
logp = logl;

whichParams = find(fixedParams == -1);
for k = 1:length(whichParams)
    logp = logp + priorPDFs{whichParams(k)}(freeParams(k));
end

if logp == -Inf, logp = -realmax; end
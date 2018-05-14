function [logp, logl] = posterior(choices, rewards_s1, rewards_s2, recalled, freeParams, fixedParams, priorPDFs)
logl = likelihood(choices, rewards_s1, rewards_s2, recalled, freeParams, fixedParams);
logp = logl;

whichParams = find(fixedParams == -1);
for k = 1:length(whichParams)
    logp = logp + log(priorPDFs{whichParams(k)}(freeParams(k)));
end

if logp == -Inf, logp = -realmax; end
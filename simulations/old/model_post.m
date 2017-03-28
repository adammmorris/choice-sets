function [logp] = model_post(envInfo, choices, rewards_tr, likeArray, betaInfo, freeParams, fixedParams, priorPDFs)
logp = likelihood(envInfo, choices, rewards_tr, likeArray, betaInfo, freeParams, fixedParams);

whichParams = find(fixedParams == -1);
for k = 1:length(whichParams)
    logp = logp + priorPDFs{whichParams(k)}(freeParams(k));
end

if logp == -Inf, logp = -realmax; end
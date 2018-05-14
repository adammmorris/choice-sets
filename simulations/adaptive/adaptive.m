nAgents = 10000;
nWords = 50;

r = .75;
nEnvs = length(r);
variance = 2;
mu = [0 0];

mfearnings = zeros(nEnvs, nAgents);
mbearnings = zeros(nEnvs, nAgents);
csearnings = zeros(nEnvs, nAgents);

parfor env = 1:nEnvs
    cur_r = r(env);
    sigma = [variance cur_r * variance; cur_r * variance variance];

    for agent = 1:nAgents
        temp = mvnrnd(mu, sigma, nWords);
        %s1re = temp(:, 1);
        %s2re = temp(:, 2);
        s1re = exp(temp(:, 1));
        s2re = exp(temp(:, 2));
        
        beta = gamrnd(4.5, 1) * 10;
        %nToEval = fastrandsample([1/4 1/4 1/4 1/4], 1) + 1;
        nToEval = 10;
        
        % run mb
%         prob_num = exp(beta * (s2re + min(s2re)) / max(s2re));
%         prob = prob_num / sum(prob_num);
%         choice = fastrandsample(prob', 1);
        mbearnings(env, agent) = max(s2re);
       
        % run mf
        prob_num = exp(beta * (s1re + min(s1re)) / max(s1re));
        prob = prob_num / sum(prob_num);
        choice = fastrandsample(prob', 1);
        mfearnings(env, agent) = s2re(choice);
        
        % run cs
        availWords = 1:nWords;
        [~, options] = sort(exprnd(1, [1, length(availWords)]) ./ prob');
        toEval = options(1:nToEval);
        [~, choice_ind] = max(s2re(availWords(toEval)));
        choice = availWords(toEval(choice_ind));
        csearnings(env, agent) = s2re(choice);
    end
end

%% make graph
b = bar([mean(mfearnings, 2)'; mean(csearnings, 2)'; mean(mbearnings, 2)']');
set(gca, 'FontSize', 24, 'xticklabel', {'0', '.25', '.5', '.75', '1'}, 'yticklabel', {});
%colorbar('Ticks', [1, 2, 3, 4, 5], 'TickLabels', {'r = 0', '.25', '.5', '.75', 'r = 1'});
h = legend('No planning', 'Choice set', 'Optimal planning', 'Location', 'northeastoutside');
set(h, 'FontSize', 24);
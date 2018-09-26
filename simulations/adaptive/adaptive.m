nAgents = 10000;
nWords = 1000;

r = [0 .25 .5 .75 1];
%nEnvs = length(r);
variance = 2;
mu = [0 0];
nToEval = [2 5 10 20 40 80];
nEnvs = length(nToEval);

mfearnings = zeros(nEnvs, nAgents);
mbearnings = zeros(nEnvs, nAgents);
csearnings = zeros(nEnvs, nAgents);
rcsearnings = zeros(nEnvs, nAgents);

for env = 1:nEnvs
    %cur_r = r(env);
    cur_r = .75;
    
    sigma = [variance cur_r * variance; cur_r * variance variance];

    cur_nToEval = nToEval(env);
    
    for agent = 1:nAgents
        temp = mvnrnd(mu, sigma, nWords);
        %s1re = temp(:, 1);
        %s2re = temp(:, 2);
        s1re = exp(temp(:, 1));
        s2re = exp(temp(:, 2));
        
        beta = gamrnd(4.5, 1) * 10;
        %nToEval = fastrandsample([1/4 1/4 1/4 1/4], 1) + 1;
        
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
        toEval = options(1:cur_nToEval);
        [~, choice_ind] = max(s2re(availWords(toEval)));
        choice = availWords(toEval(choice_ind));
        csearnings(env, agent) = s2re(choice);
        
        % random cs
        availWords = 1:nWords;
        toEval = fastrandsample(repmat(1/nWords, 1, nWords), cur_nToEval);
        [~, choice_ind] = max(s2re(availWords(toEval)));
        choice = availWords(toEval(choice_ind));
        rcsearnings(env, agent) = s2re(choice);
    end
end

%% make graph
%b = bar([mean(mfearnings, 2)'; mean(rcsearnings, 2)'; mean(csearnings, 2)']');
b = bar([mean(rcsearnings, 2)'; mean(csearnings, 2)']');

% if doing it by correlation...
%set(gca, 'FontSize', 24, 'xticklabel', {'0', '.25', '.5', '.75', '1'}, 'yticklabel', {});

% if doing it by set size...
set(gca, 'FontSize', 40, 'xticklabel', {'2', '5', '10', '20', '40', '80'}, 'yticklabel', {});

%colorbar('Ticks', [1, 2, 3, 4, 5], 'TickLabels', {'r = 0', '.25', '.5', '.75', 'r = 1'});

h = legend('Non-value-guided', 'Value-guided', 'Location', 'northeastoutside');
set(h, 'FontSize', 40);
xlabel('Consideration set size (K)');
ylabel('Expected value (EV) of choice');

ltop_point = max(mean(mbearnings,2));
ltop = refline([0 ltop_point]);
ltop.Color = 'r';
ltop.LineStyle = '--';

lbot_point = max(mean(mfearnings,2));
lbot = refline([0 lbot_point]);
lbot.Color = 'm';
lbot.LineStyle = '--';

text(.2,ltop_point - 5,'\it{Maximum EV} ', 'FontSize', 32, 'Color', 'r');
text(.2,lbot_point + 8,{'\it{No} ', '\it{cons.}', '\it{set }'}, 'FontSize', 32, 'Color', 'm');
nopt = 12;
k = 6;
nsubj = 1000;
betas = linspace(.1, 1, 5);
vals = 1:nopt;
list = zeros(nsubj, k, length(betas));

for beta_ind = 1:length(betas)
    beta = betas(beta_ind);
    for i = 1:nsubj
        avail = vals;
        
        % sample
        for j = 1:k
            probs = exp(beta * avail) / sum(exp(beta * avail));
            choice = fastrandsample(probs, 1);
            list(i, j, beta_ind) = avail(choice);
            avail(choice) = [];
        end
    end
end

%% plot
h = plot(squeeze(mean(list,1)));
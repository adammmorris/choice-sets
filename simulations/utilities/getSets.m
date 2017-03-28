%% getSets
% Returns all choice sets of size nToEval where exactly numTies words
% are tied with 'word', and the rest are worse (according to rewards_te)

function [sets] = getSets(word, numTies, nToEval, rewards_te)
sets = [];
if word <= length(rewards_te)
    val = rewards_te(word);
    others = find(rewards_te < val);
    
    if numTies == 0
        possible_others = combnk_fast(others, nToEval - 1);
        for other_ind = 1:size(possible_others, 1)
            sets(end+1, :) = [word possible_others(other_ind, :)];
        end
    else
        ties = find(rewards_te == val);
        ties = ties(ties ~= word);
        
        if numel(ties) >= numTies && numTies < nToEval
            possible_ties = combnk_fast(ties, numTies);
            possible_others = combnk_fast(others, nToEval - numTies - 1);
            for tie_ind = 1:size(possible_ties, 1)
                if nToEval == 2 % exactly 1 tie in a set of 2
                    sets(end+1, :) = [word possible_ties(tie_ind, :)];
                else % otherwise..
                    for other_ind = 1:size(possible_others, 1)
                        sets(end+1, :) = [word possible_ties(tie_ind, :) possible_others(other_ind, :)];
                    end
                end
            end
        end
    end
end
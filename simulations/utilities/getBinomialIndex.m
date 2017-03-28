function [ind] = getBinomialIndex(set, binCoef_table)
ind = 0;
set = sort(set, 'descend') - 1;
l = numel(set);
for i = 1:l
   ind = ind + binCoef_table(set(i) + 1, l - i + 2); % an extra +1 to each input (quirk of binCoef_table indexing)
end
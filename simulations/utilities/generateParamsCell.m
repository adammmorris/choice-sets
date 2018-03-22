% Each inputted matrix should be [post LL det(hessian) param1 param2 ...]

function [params, details] = generateParamsCell(matrix1, matrix2, varargin)
numModels = nargin;
numSubjects = size(matrix1, 1);

params = cell(numModels,1);
params{1} = matrix1(:,4:end);
params{2} = matrix2(:,4:end);

details = zeros(numSubjects, numModels, 3);
for k = 1:3
	details(:, 1, k) = matrix1(:, k);
	details(:, 2, k) = matrix2(:, k);
end

for i=3:numModels
    params{i} = varargin{i-2}(:, 4:end);
    for k = 1:3
		details(:, i, k) = varargin{i-2}(:, k);
	end
end
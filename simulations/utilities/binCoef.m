function c = binCoef(nd,kd)
c = round(prod(((nd-kd+1):nd) ./ (1:kd)));
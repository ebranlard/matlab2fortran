function [ vartype, varshape,varprop ] = fgetVarTypeFromName( varname )
% default type
vartype='';
varshape='';
varprop='';
% attempt a type conversion based on first character, this is my own convention, comment if needed
if varname(1)=='i' || varname(1)=='n'
    vartype='integer';
elseif varname(1)=='I'
    vartype='integer';
    varshape=':';
elseif varname(1)=='b'
    vartype='logical';
elseif varname(1)=='s'
    vartype='character(len=*)';
elseif length(varname)>=3 & isequal(varname(1:3),'cpt')
    vartype='integer';
elseif varname(1)=='v'
    vartype='real*8';
    varshape=':';
else
    vartype=''; % this is postponed to the writting
end

end


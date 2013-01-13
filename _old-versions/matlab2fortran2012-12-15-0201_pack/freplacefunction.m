function [ sf, end_stack ] = freplacefunction( s,end_stack )

% freplacefunction('function [ sf, end_stack ] = ffunctionreplace( s,end_stack )',[])


ieq=strfind(s,'=');
iob=strfind(s,'[');
icb=strfind(s,']');
iop=strfind(s,'(');
icp=strfind(s,')');
% output arguments
if isempty(ieq)
    c_argout=[];
    s_argout=[];
    isf=9;
else
    isf=ieq+1;
    if isempty(iob)
        c_argout{1}=strtrim(s(9:(ieq-1)));
        s_argout=strtrim(s(9:(ieq-1)));
    else
        s_argout=s((iob+1):(icb-1));
        if ~isempty(strfind(s_argout,',')) % argout separated by commas
            [a b c d e f c_argout]=regexp(s_argout,',');
        else %argout separated by spaces
            [a b c d e f c_argout]=regexp(s_argout,'[\ ]*');
        end
    end 
end

% function name
fname=s(isf:(iop-1));
% arguments
s_argin=s((iop+1):(icp-1));
[a b c d e f c_argin]=regexp(s_argin,',');

% concatenation, don't care about inout for now...
if ~isempty(s_argout) && ~isempty(s_argin)
    s_args = [s_argin ' , ' s_argout];
elseif isempty(s_argout) && ~isempty(s_argin)
    s_args= s_argin;
elseif isempty(s_argout) && isempty(s_argin)
    s_args= s_argout;
else
    s_args='';
end

% Output
sf=['!> \n'];
sf=[sf 'subroutine ' fname '(' s_args  ')\n'];
sf=[sf 'implicit none \n'];
for iin=1:length(c_argin)
    sf=[sf 'real*8, intent(in) :: ' c_argin{iin}    '       !> \n'];
end
for iout=1:length(c_argout)
    sf=[sf 'real*8, intent(out) :: ' c_argout{iout} '       !> \n'];
end

% stacking
end_stack(2:(length(end_stack)+1))=end_stack(1:length(end_stack));
end_stack{1}=['subroutine ' fname];

end


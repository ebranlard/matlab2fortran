function [ sf, end_stack ] = freplaceif( s,end_stack )

% freplaceif('if (ksdjf)',[])
% freplaceif('if i==1 ; ! why not?',[])
% freplaceif('else if i==1 ; ! why not?',[])


Ielse=strfind(s,'else');
Iif=strfind(s,'if');

% splitting the string in substrings
s_cond=strtrim(s((Iif(1)+2):end)); % the conditional string
s_comm='';
Iterm=strfind(s_cond,';'); %is there a termination..
Icomm=strfind(s_cond,'!'); %is there a comment


if ~isempty(Icomm) || ~isempty(Iterm)
    imin=min([Iterm Icomm]);
    s_comm=s_cond(imin:end);
    s_cond=strtrim(s_cond(1:(imin-1)));
end

%Adding parenthesis if needed
[bMatched po pc]=ffindmatching(s_cond,1,'(',')',1);
if ~(pc==length(s_cond) && po==1 && bMatched)
    s_cond=['(' strtrim(s_cond) ')'];
% if (s_cond(1)==' ' && s_cond(2) ~='(' ) || (s_cond(1)~=' ' && s_cond(1)~='(' ) %of course could be smarter
end

sf=['if ' s_cond ' then ' s_comm '\n'];
if ~isempty(Ielse) && Ielse(1)==1
    % else if 
    sf=['else ' sf];
else
    % if not an elseif then we stack
    % stacking
    end_stack=fstack_push(end_stack,'if');
end
end % function


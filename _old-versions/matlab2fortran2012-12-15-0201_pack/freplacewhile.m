function [ sf, end_stack ] = freplacewhile( s,end_stack )

% freplacewhile('while (i<I)!cool',[])
% freplacefor('while i>0;!why not?',[])


Icol=strfind(s,':');  
Ieq=strfind(s,'=');
ncol=length(Icol);
s_cond=strtrim(s(6:end));
s_comm='';

Iterm=strfind(s_cond,';'); %is there a termination..
Icomm=strfind(s_cond,'!'); %is there a comment
if ~isempty(Icomm) || ~isempty(Iterm)
    imin=min([Iterm Icomm]);
    s_comm=s_cond(imin:end);
    s_cond=s_cond(1:(imin-1));
end
if (s_cond(1)==' ' && s_cond(2) ~='(' ) || (s_cond(1)~=' ' && s_cond(1)~='(' ) %of course could be smarter
    s_cond=['(' strtrim(s_cond) ')'];
end
sf=['do while ' s_cond ' ' s_comm];
sf=[sf ' \n'];
end_stack(2:(length(end_stack)+1))=end_stack(1:length(end_stack));
end_stack{1}='do'; % enddo isneeded not end while

end


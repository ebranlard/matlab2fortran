function [ sf, end_stack ] = freplacefor( s,end_stack )

% freplacefor('for i=I',[])
% freplacefor('for i=1:0.2:10  ; ! why not?',[])


Icol=strfind(s,':');  
Ieq=strfind(s,'=');
ncol=length(Icol);
loopvar=strtrim(s(4:(Ieq(1)-1)));
sf=['do ' loopvar '='];
sloopextent=s((Ieq(1)+1):end);
[~,~,~,~,~,~,split]=regexp(sloopextent,':');
switch ncol
    case 0
        sf=[sf s((Ieq(1)+1):end)];
    case 1
        sf=[sf strtrim(split{1}) ',' strtrim(split{2})];
    case 2
        sf=[sf strtrim(split{1}) ',' strtrim(split{3}) ',' strtrim(split{2})];
end
sf=[sf ' \n'];
end_stack(2:(length(end_stack)+1))=end_stack(1:length(end_stack));
end_stack{1}='do';

end


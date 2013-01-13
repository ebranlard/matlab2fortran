function [ sf, end_stack ] = freplacefor( s,end_stack )

% freplacefor('for i=I',[])
% freplacefor('for i=1:0.2:10  ; ! why not?',[])
% freplacefor('for iarg=1:length(c_all_args(:,1)',[])

Icol=strfind(s,':');  
Ieq=strfind(s,'=');
ncol=length(Icol);
loopvar=strtrim(s(4:(Ieq(1)-1)));
sf=['do ' loopvar '='];
sloopextent=s((Ieq(1)+1):end);
[~,~,~,~,~,~,split]=regexp(sloopextent,':');

% nasty handling of colum that are used for index ranges..
splitb=[];
for is=1:length(split)
    ssplit=split{is};
    no=length(strfind(ssplit,'('));
    nc=length(strfind(ssplit,')'));
    if no>nc 
        if (is+1)<=length(split)
            splitb{end+1}=[split{is},':' split{is+1}];
        end
    elseif nc>no
        % should have been accounted by previous step
    else
        splitb{end+1}=split{is};
    end
end

split=splitb;
ncol=length(split)-1;
switch ncol
    case 0
        sf=[sf s((Ieq(1)+1):end)];
    case 1
        sf=[sf strtrim(split{1}) ',' strtrim(split{2})];
    case 2
        sf=[sf strtrim(split{1}) ',' strtrim(split{3}) ',' strtrim(split{2})];
end
sf=[sf ' \n'];
% stacking
end_stack=fstack_push(end_stack,'do');

end


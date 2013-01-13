function [b po pc]=ffindmatching(s,pstart,co,cc,bforward);
% string, start position, character open, character close
%
% [b po pc]=ffindmatching('1(3(5)7()df) d',2,'(',')',1 )
%
b=0;
po=0;
pc=0;


if ~bforward
    error('not done')
end
found=0;
nopen=0;
Iopen=[];
i=pstart;
while ~found && i<=length(s)
    if s(i)==co
        nopen=nopen+1;
        Iopen=[Iopen i];
    end
    if s(i)==cc
        if nopen>=1 % this is a choice, if a closing cgaracter is found, before an opened one, we don't care...
            nopen=nopen-1;
            if nopen==0
                found=1;
                pc=i;
                po=Iopen(1);
                break
            end
        end
    end
    i=i+1;
end

b=found;

end

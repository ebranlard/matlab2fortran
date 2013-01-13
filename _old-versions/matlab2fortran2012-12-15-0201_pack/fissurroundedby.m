function [ b, p1,p2 ] = fissurroundedby( s,p,c1,c2 )
% looks if character s(p) is surrounded by the characters c1 and c2 
% returns a boolean , and the postion of these charactesrs
% [b p1 p2]=fissurroundedby('function [a b c]=issur([,])',10,'[',']')
% [b p1 p2]=fissurroundedby('function [a b c]=issur([,])',11,'[',']')
% [b p1 p2]=fissurroundedby('function [a b c]=issur([,])',10,'[',']')
% [b p1 p2]=fissurroundedby('function [''a b c'']=issur([,])',10,'''','''')
% [b p1 p2]=fissurroundedby('function [''a b c'']=issur([,])',12,'''','''')
% [b p1 p2]=fissurroundedby('(),()',4,'(',')')
p1=0;
p2=0;
%% stupid whiles
% look backward
notfound=1;
i=p-1;
while i>=1 && notfound
    if c1~=c2 
        % then if c2 is encountered we should break
        if s(i)==c2
                break
        end
    end
    if s(i)==c1
        notfound=0;
        p1=i;
    end
    i=i-1;
end

% look forward
notfound=1;
i=p+1;
while i<=length(s) && notfound
    if c1~=c2 
        % then if c1 is encountered we should break
        if s(i)==c1
            break
        end
    end
    if s(i)==c2
        notfound=0;
        p2=i;
    end
    i=i+1;
end


if p1==0 || p2==0
    b=0; % not surrounded
else
    b=1;
end
end

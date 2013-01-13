function [ sf, end_stack ] = freplaceend( s,end_stack )
% freplaceend('end !(ksdjf)',[])
% freplaceend('end  ; ! why not?',[])



if isempty(end_stack)
    warning('Too much ends.. Some stuff have not been accounted for?');
    end_stack{1}='';
end
sf=['end ' end_stack{1} ' '  s(4:end) '\n']; % could add a security comment..

% shortening the stack
end_stack=end_stack(2:length(end_stack));

end


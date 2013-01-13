function [ sf, end_stack ] = freplaceend( s,end_stack )
% freplaceend('end !(ksdjf)',[])
% freplaceend('end  ; ! why not?',[])

if isempty(end_stack)
    warning('Too much ends in the file.. Some stuff have not been accounted for?');
    end_stack{1}='';
end

[end_stack, elmt] = fstack_pop(end_stack);
sf=['end ' elmt ' '  s(4:end) '\n']; % could add a security comment..


end %function


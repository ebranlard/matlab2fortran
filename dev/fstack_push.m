function [ stack ] = fstack_push( stack, addme )
if isempty(stack)
    stack=[];
end
% add an element at the tail of variable stack
stack{end+1}=addme;

end %function


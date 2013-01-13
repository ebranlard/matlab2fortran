function [ stack , element] = fstack_pop_element( stack, n )
% remove an element at given location
element=stack(n);
stack=stack(setdiff(1:length(stack),n));
end %function


function [ stack ,element ] = fstack_pop( stack )
% remove the head
element=stack{end};
% shortening the stack
stack=stack(1:(length(stack)-1));
end %function


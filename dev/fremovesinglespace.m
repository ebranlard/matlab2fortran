function [ s ] = fremovesinglespace( s )
% remove multiple spaces
s=regexprep(s,'[\ ]+','');

end %function


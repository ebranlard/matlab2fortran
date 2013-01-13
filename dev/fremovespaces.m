function [ s ] = fremovespaces( s )
% remove multiple spaces
s=regexprep(s,'[\ ]+',' ');

end


function [ sf ] = fgetDeclaration( v )
if isempty(v.type)
    vartype='real*8';
else
    vartype=v.type;
end
if ~isempty(v.shape)
    varshape=['dimension(' v.shape ')'];
    if ~isempty(v.prop)
        varshape=[ varshape ', '];
    end
else
    varshape='';
end
if ~isempty(v.prop)
    varprop=[v.prop];
else
    varprop='';
end
if ~isempty(v.name)
    varname=v.name;
else
    varname='TODO';
end
if ~isempty(varprop)|| ~isempty(varshape)
    vartype=[ vartype ', '];
end

% varcomment=[v.comment '     !> '];
varcomment=[' !> ' v.comment(1:min(20,length(v.comment)))];


sf=sprintf('%s%s%s :: %s %s \n',vartype,varshape,varprop,varname,varcomment);

end


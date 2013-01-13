function [ decl ] = fmergeDeclarations( v1, v2 )
v1.name=strtrim(v1.name);
v2.name=strtrim(v2.name);
if ~isequal(v1.name ,v2.name)
    error('Can''t merge if name different');
end
decl.name=v1.name;




%% comment
decl.comment=[v1.comment v2.comment];

%% dimension
b1=isempty(regexp(v1.shape,'[0-9]+'));
b2=isempty(regexp(v2.shape,'[0-9]+'));
% give priority to numbers
if ~b1 && ~b2
    n1=length(strfind(v1.shape,':'));
    n2=length(strfind(v2.shape,':'));
    s_shape=':';
    for is=2:max(n1,n2)
        s_shape=[s_shape,',:'];
    end
elseif b1
    s_shape=v2.shape;
    if ~isempty(v1.shape)
        decl.comment=[decl.comment '!m2f: check dim(' v1.shape ')'];
    end
else
    s_shape=v1.shape;
    if ~isempty(v2.shape)
        decl.comment=[decl.comment '!m2f: check dim(' v2.shape ')'];
    end
end 
decl.shape=s_shape;

%% prop
if isempty(v1.prop)
    decl.prop=v2.prop;
elseif isempty(v2.prop)
    decl.prop=v1.prop;
else 
    % none of them are empty;
    s_prop=[v1.prop,',' v2.prop];
    [~,~,~,~,~,~,c]=regexp(s_prop,',');
    c=strtrim(c);
    c=sort(c);
    % cannot be allocatable and intent
    if ~isempty(strfind(s_prop,'allocatable')) && ~isempty(s_prop,'intent')
        % remove allocatable prop
        I=cellfun(@(x)~isequal(x,'allocatable'),c); 
        c=c(I);
    end
    s_shape=c{1}
    for ic=2:length(c)
        s_prop=[', ' c{ic}];
    end
end
%% type. NOTE: a more advanced merging could be thought
if ~isempty(strfind([v1.type v2.type],'matlabcell'))
    decl.type='matlabcell';
else
    vartype = fgetVarTypeFromName( v1.name );
    if isempty(v1.type)
        decl.type=v2.type;
    elseif isempty(v2.type)
        decl.type=v1.type;
    elseif isequal(v1.type, vartype)
        decl.type=v2.type;
    elseif isequal(v2.type, vartype)
        decl.type=v1.type;
    else
        decl.type=''; % don't add information you don't have
    end
end



end


function [ s , decl_stack ] = fassignement( s , decl_stack )

% extract the LHS
Ieq=strfind(s,'=');
if isempty(Ieq)
    return
end
sLHS=s(1:Ieq(1)); % !!! includes the equal for now!
% extract the RHS and comment
Ic=strfind(s,'!');
if ~isempty(Ic) % could be smarter 
    bwithin=0;
    is=Ieq(1)+1;
    while is<=length(s)
        if s(is)==''''
            if ~bwithin
                bwithin=1;
            else
                bwithin=0;
            end
        end
        if s(is)=='!' && ~bwithin
            break 
        end
        is=is+1;
    end
    sRHS=s((Ieq(1)+1):(is-1));
    sCOM=s(is:end);
else
    sRHS=s((Ieq(1)+1):end);
    sCOM='';
end
% if ~isempty(Ic) % could be smarter 
%     % make sure the comment is not surrounded by apostroph
%     [b p1 p2]=fissurroundedby(s,Ic(1),'''','''');
%     if b 
% 
% else
% end

% Initialization of variable properties
v.comment='';
v.type='';
v.shape='';
v.name='TODO';
v.prop='';

vb.comment='';
vb.type='';
vb.shape='';
vb.name='TODO';
vb.prop='';

% hack for matrices assignemnt: in the form A(1,:)=[a a] , then the shape can be determined in one dimension
bFirstDimMayBeDetermined=0;
bSecondDimMayBeDetermined=0;


%% Finding variable name and properties if possible in LHS
% test simple case pure variable
% only simple assignments are allowed x1_var =.. , I.element =
% It is chosen to keep the dot in the variable name..
Isimple=regexp(sLHS,'[a-z.A-Z0-9_ ]+=');
if ~isempty(Isimple) && Isimple(1)==1
    % that's the simple case
    vb.name=strtrim(sLHS(1:(end-1)));
    [vb.type, vb.shape, vb.prop] =fgetVarTypeFromName(vb.name);
else
    % if an assignment is of the form a(:,1) we dont cons
    % is there is some : and (, try to find the dimension
    Ipar=strfind(sLHS,'(');
    Icurl=strfind(sLHS,'{');
    if ~isempty(Icurl) % has to be first
        vb.name=strtrim(sLHS(1:(Icurl(1)-1)));
        vb.type='matlabcell';
    elseif ~isempty(Ipar)
        vb.name=strtrim(sLHS(1:(Ipar(1)-1)));
        [vb.type, ~, vb.prop] =fgetVarTypeFromName(vb.name);
        ncommas=length(strfind(sLHS,','))+1;
        vb.shape=':';
        for is=2:ncommas;
            vb.shape=[vb.shape,',:'];
        end
        % let's a simple hack for matrices
        bFirstDimMayBeDetermined=~isempty(strfind(fremovesinglespace(sLHS),':,'));
        bSecondDimMayBeDetermined=~isempty(strfind(fremovesinglespace(sLHS),',:'));
    else
        %TODO
    end
end

%% merging info
v.name=vb.name;
v=fmergeDeclarations(v,vb);


%% Dealing with squared brackets in RHS
if ~isempty(strfind(sRHS,'['));
    [ sRHS , vb.type, ncol,nlines] = freplacesbracket( sRHS );
    if min(ncol,nlines)==1
        vb.shape=sprintf('%d',max(ncol,nlines));
        % hack 
        if bFirstDimMayBeDetermined
            vb.shape=sprintf('%d,:',max(ncol,nlines));
        elseif bSecondDimMayBeDetermined
            vb.shape=sprintf(':,%d',max(ncol,nlines));
        end
    elseif ncol*nlines~=0
        % TODO more than matrices...
        vb.shape=sprintf('%d,%d',ncol,nlines);
    end
    % TODO possibility to handle some types here
    %     elseif ~isempty(regexp(sLHS,'[0-9]+'))
    %         v.type=
end
v.name=vb.name;
v=fmergeDeclarations(v,vb);



%% Dealing with transpose as apostroph
if mod(length(strfind(sRHS,'''')),2)==1  % clearly this will fail if there is an even number of transpose on the line..
    s=['!m2f: ' sLHS sRHS sCOM '\n'];
    sRHSb=regexprep(sRHS,'([a-zA-Z\(\)\_0-9:]*)''','transpose($1)');
    s=[s sLHS sRHSb sCOM];
else
    % default
    s=[sLHS sRHS sCOM];
end
decl_stack=fstack_push(decl_stack,v);

% if isequal(v.name,'TODO')
%     disp(s);
% end

end %function


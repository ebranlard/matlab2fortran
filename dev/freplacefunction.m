function [ sf, end_stack,decl_stack ] = freplacefunction( s,end_stack)
decl_stack=[];
% freplacefunction('function [ sf, end_stack ] = ffunctionreplace( s,end_stack )',[])


ieq=strfind(s,'=');
iob=strfind(s,'[');
icb=strfind(s,']');
iop=strfind(s,'(');
icp=strfind(s,')');
% output arguments
if isempty(ieq)
    c_argout=[];
    s_argout=[];
    isf=9;
else
    isf=ieq+1;
    if isempty(iob)
        c_argout{1}=strtrim(s(9:(ieq-1)));
        s_argout=strtrim(s(9:(ieq-1)));
    else
        s_argout=s((iob+1):(icb-1));
        s_argout=strtrim(s_argout);
        s_argout=fremovespaces(s_argout); % remove double spaces
        if ~isempty(strfind(s_argout,',')) % argout separated by commas
            [a b c d e f c_argout]=regexp(s_argout,',');
        else %argout separated by spaces
            [a b c d e f c_argout]=regexp(s_argout,'[\ ]*');
        end
    end 
end

% function name
fname=s(isf:(iop-1));
% arguments
s_argin=s((iop+1):(icp-1));
[a b c d e f c_argin]=regexp(s_argin,',');
if length(c_argin)==1 && length(strtrim(c_argin{1}))==0 
    c_argin=[];
end
if length(c_argout)==1 && length(strtrim(c_argout{1}))==0 
    c_argout=[];
end
%% Let's see who is inout
c_all_args=cell(0,2);
for iin=1:length(c_argin)
    found=0;
    for iout=1:length(c_argout)
        if isequal(strtrim(c_argin{iin}),strtrim(c_argout{iout}))
            found=iout;
        end
    end
    if found==0
        c_all_args{iin,1}=c_argin{iin};
        c_all_args{iin,2}='in';
    else
        c_all_args{iin,1}=c_argin{iin};
        c_all_args{iin,2}='inout';
        % deletion of output
        c_argout=c_argout(setdiff(1:length(c_argout),found));
    end
end
if length(c_argout)>0
    I=length(c_argin)+(1:length(c_argout));
    c_all_args(I,1)=c_argout;
    c_all_args(I,2)={'out'};
end
s_args='';
if ~isempty(c_all_args)
    s_args=c_all_args{1,1};
    for iarg=2:length(c_all_args(:,1))
        s_args=[s_args ', ' c_all_args{iarg,1}];
    end
end
% 
% 
%% Output
sf=['!> \n'];
sf=[sf 'subroutine ' fname '(' s_args  ')\n'];
sf=[sf 'implicit none \n'];
% sf=[sf '! Use somemodule \n'];
if ~isempty(c_all_args)
%     sf=[sf '! Input/output arguments \n'];
    for iarg=1:length(c_all_args(:,1))
        v.name=strtrim(c_all_args{iarg,1});
        [ v.type, v.shape,v.prop ] = fgetVarTypeFromName( v.name );
        if ~isempty(v.prop)
            v.prop=[v.prop ','];
        end
        v.comment='';
        v.prop=[v.prop 'intent(' c_all_args{iarg,2} ')'];
%         [ sfdecl ] = fgetDeclaration( v );
%         sf=[sf sfdecl];% ', intent(' c_all_args{iarg,2} ') :: '     '       !> \n'];
        decl_stack=fstack_push(decl_stack,v);
    end
end
% sf=[sf '! Variables declaration \n']; 
sf=[sf '!M2F-HERE-XXX \n']; 
sf=[sf '!  \n'];



% stacking
end_stack=fstack_push(end_stack,['subroutine ' fname]);

end


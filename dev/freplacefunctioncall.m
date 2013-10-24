function [ sf,decl_stack ] = freplacefunctioncall( s, decl_stack )
% this function assume the form: [ ]= kdsfkdsfj()
%
%
% freplacefunctioncall('[a , b]=ffunctionreplace( s,end_stack )  !',[])


ieq=strfind(s,'=');
iob=strfind(s,'[');
icb=strfind(s,']');

if ~isempty(ieq)
    [~,iop,icp]=ffindmatching(s,ieq(1),'(',')',1);
    % output arguments
    isf=ieq+1;
    s_argout=strtrim(s((iob+1):(icb-1)));
    s_argout=strrep(s_argout,', ',',');
    s_argout=strrep(s_argout,' ,',',');
    s_argout=fremovespaces(s_argout);
    s_argout=strrep(s_argout,' ',',');
    % [~, ~, ~, ~, ~, ~, c_argin]=regexp(s_argin,',');
    [~, ~, ~, ~, ~, ~, c_argout]=regexp(s_argout,',');

    % function name
    fname=strtrim(s(isf:(iop-1)));
    % arguments
    s_argin=s((iop+1):(icp-1));

    % concatenation, don't care about inout for now...
    if ~isempty(s_argout) && ~isempty(s_argin)
        s_args = [s_argin ' , ' s_argout];
    elseif isempty(s_argout) && ~isempty(s_argin)
        s_args= s_argin;
    elseif isempty(s_argout) && isempty(s_argin)
        s_args= s_argout;
    else
        s_args='';
    end

    scomm=s((icp+1):end);

    % To tring
    sf=['call ' fname '( ' s_args  ') ' scomm '\n'];

    % Declarations
    for iout = 1:length(c_argout)
        v.name=strtrim(c_argout{iout});
        [v.type, v.shape, v.prop] =fgetVarTypeFromName(v.name);
        v.comment='';
        decl_stack=fstack_push(decl_stack,v);
    end

end

end %function


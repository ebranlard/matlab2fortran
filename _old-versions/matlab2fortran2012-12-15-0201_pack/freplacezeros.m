function [ sf] = freplacezeros( s)

% freplacezeros('aaa zeros(1,10) ; ones(size(1)) !sdkjskf')


% I=regexp(s,'(zeros\(|ones\(|linspace\()');
I=regexp(s,'(zeros\(|ones\()');
if s(I(1))=='z'
    value=0;
elseif s(I(1))=='o'
    value=1;
else 
    value=-1; %linspace
end

[b po pc]=ffindmatching(s,I(1),'(',')',1);
s_size=s((po+1):(pc-1));

% linspace specific stuff
if value==-1
    s_linsp=s((po+1):(pc-1));
    [~,~,~,~,~,~,split]=regexp(s_linsp,',');
    s_start  = split{1} ; 
    s_end    = split{2} ; 
    s_length = split{3} ; 

    s_size=s_length;
end


Ieq=strfind(s,'=');


% default value
% if value>=0
sf= ['allocate(TODO(' s_size ')) \n'];
sf= sprintf('%sTODO = %d.0D \n',sf,value);
% else
% a = 1.5 ! Initial value
% y = (/((i*a),i=1,100)/) ! m2f: kind of linspace
%     sf= [s '\n']; %we can leave the linspace, and make a function that does that...
%     sf= sprintf('%sTODO = (/( (i*a),i=1,length(,cc  (%s) \n',sf,value);
% end
sf= [sf '!m2f: ' s '\n'];
if ~isempty(Ieq)
    % default 
    sf= ['!m2f: ' s '\n'];
    % ok, there is an assignment somewhere
    pre=s((Ieq(1)+1):(I(1)-1));
    post=s((pc+1):end);

    % Let's see if it's a simple assigment: var= operation zeros(
    Is=regexp(s,'[a-zA-Z0-9_ ]+=[() *^.+0-9]*(zeros\(|ones\()');
    if ~isempty(Is) && Is(1)==1
        % Cool
        varname=s(1:(Ieq(1)-1));
    else
        Is2=regexp(s,'[a-zA-Z0-9_ ]+=');
        if ~isempty(Is2) && Is2(1)==1
            varname=s(1:(Ieq(1)-1));
        else
            varname='TODO';
        end
    end
    sf= [sf 'real*8, allocatable, dimension(' s_size ') :: ' varname '       ! m2f:check >! \n'];
    sf= [sf 'if (allocated(' varname ')) deallocate(' varname ')            !m2f: check if wanted\n'];
    sf= [sf 'allocate(' varname '(' s_size '))\n'];
    sf= sprintf('%s%s = %d.0D \n',sf,varname,value);
    sf= [sf varname ' =' pre varname post '\n'];
end
end

function [ sf ] = freplaceprintf( s )
sf=s;
I=regexp(s,'[sf]+printf\(');
if ~isempty(I) 
    ip=I(1);% NOTE:only deals with one printf..
    if s(ip)=='s'
        scmd='sprintf';
    elseif s(ip)=='f'
        scmd='fprintf';
    else
        scmd='printf';
    end 
    % find parenthesis extent
    [~,p1,p2]=ffindmatching(s,ip+3,'(',')',1);
    s=strrep(s,'\','\\');
    s=strrep(s,'%','%%');
    
    s_print=s((p1+1):(p2-1)); % not safe and will keep the %
    s_assign=s(1:(ip-1)); 
    s_post=s((p2+1):end); 
%     s_print=regexprep(s_print,'%%[sdf.0-9]*',''', XX ,'''); %TODO improve me
    sf=['!m2f: ' s '\n '] ;
    switch(scmd)
        case 'printf'
%             sf=[sf 'write(*,*) ' s_print '\n !m2f: ' s_post '\n'] ;
            sf=[sf 'write(*,*) '' '' \n'] ;
        case 'sprintf'
%             sf=[sf 'write(s,*) ' s_print '\n !m2f: ' s_post '\n'] ;
            sf=[sf 'write(s,*) '' ''  \n'] ;
        case 'fprintf'
%             sf=[sf 'write(unitnumber,*) ' s_print '\n !m2f: ' s_post '\n'] ;
            sf=[sf 'write(unitnumber,*) '' '' \n'] ;
    end

end



end


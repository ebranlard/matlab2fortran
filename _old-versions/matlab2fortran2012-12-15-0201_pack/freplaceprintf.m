function [ sf ] = freplaceprintf( s )
sf=s;
I=regexp(s,'printf\(');
if ~isempty(I) 
    i=I(1);%only deals with one printf...
    if i==1
        scmd='printf';
    elseif s(i-1)=='s'
        scmd='sprintf';
    elseif s(i-1)=='f'
        scmd='fprintf';
    else
        scmd='printf';
    end
    % find parenthesis extent
    [~,p1,p2]=ffindmatching(s,i+3,'(',')',1);
    s=strrep(s,'\','\\');
    s=strrep(s,'%','%%');
    
    s_print=s((p1+1):(p2-1)); % not safe and will keep the %...
    s_assign=s(1:(i-1)); 
    s_post=s((p2+1):end); 
    s_print=regexprep(s_print,'%%[sdf.0-9]*',''', XX ,''');
    switch(scmd)
        case 'printf'
            sf=['!m2f: ' s '\n '] ;
            sf=[sf 'write(*,*)' s_print '\n !m2f: ' s_post '\n'] ;
        case 'sprintf'
            sf=['!m2f: ' s '\n '] ;
            sf=[sf 'write(s,*) ' s_print '\n !m2f: ' s_post '\n'] ;
        case 'fprintf'
            sf=['!m2f: ' s '\n '] ;
            sf=[sf 'write(unitnumber,*) ' s_print '\n !m2f: ' s_post '\n'] ;
    end

end



end


function [  ] = matlab2fortran( varargin )
%matlab2fortran(matlab to fortran) : converting matlab code to fortran

% Author:Emmanuel Branlard
% Date  : December 2012
% Version: 0.12.12beta

% License: None. Thank you for sharing your improvements to me by email.

% Revision: 


%matlab2fortran(matlab to fortran) is a code that performs an automatic conversion of one or multiple matlab files to fortran format.The script translate some of the main features of matlab to fortran. matlab2fortran performs a quick and dirty conversion based on a line-by-line basis (but still supports conditional loops and subroutine). The script allows multiple matlab command per line if these commands are separated by ";", but not by ",".

%matlab2fortran does not intend to produce ready-to-compile code, but performs several necessary conversions. The generated code keeps the structure, variables names and comments of the original code. 

%The script matlab2fortran (matlab to fortran) is a single script file written in matlab and does not require a particular installation other than maybe adding this script to your current folder or matlab path. The script has been tested on linux and windows.

%Main Features of matlab2fortran:
%- conversion of nested structure: do, if, switch (select), while
%- conversion of function to subroutine with recognition of output and input arguments
%- perform subroutine list of arguments with intent(in) and intent(out)
%- recognize simple function call and convert them to subroutine calls (syntax [y] = f(x) );
%- performs allocation when converting matlab function zeros, ones (linspace possible)
%- splitting of lines that contain multiple matlab command separated by ;
%- comments
%- small support for sprintf and fprintf
%- misc conversions such as logical operators
%- few intrinsic functions conversion

%TODOs:
%- better printf handling
%- intent inout arguments
%- better handling of function calls and nested function calls..

% example 1:
% matlab2fortran('tests/test1.m');
% matlab2fortran('tests/test1.m','tests/test2.m');

% example 2: all files in current directory (does not work with subfolders with dir..)
% FileList=dir('*.m');
% matlab2fortran(FileList.name);

% Params
bDebug=0;% show input in stdout
bPipe=0; % pipe output to stdout

if nargin==0
    error('Empty file list')
end

%% dealing with multiple input files "recursively..."
if length(varargin)>1
    for ifile = 1:length(varargin)
        f=varargin{ifile};
        matlab2fortran(f);
    end
else
    file=varargin{1};
    fidm=fopen(file);
    if fidm==-1
        error(['Unable to open input file: ' file]);
    end


    filef=regexprep(file,'\.m$','.f90');
    fprintf('Converting to file: %s\n',filef);
    %     warning off
    if ~bDebug
        fidf=fopen(filef,'w');
        if fidf==-1
            error(['Unable to open ouput file: ' file]);
        end
    else
        fidf=1; % standard output 
    end

    fconvertlinebyline(fidm,fidf,bDebug,bPipe)
    fclose(fidm);
    if ~bDebug 
        fclose(fidf);
    end
end
end %function

function []=fconvertlinebyline(fidm,fidf,bDebug,bPipe)
sLine = fgets(fidm);
end_stack=[];
pref_in='';
pref_out='';
if bDebug 
    pref_in='In :';
    pref_out='Out:';
end
while sLine ~= -1
    % remove useless spaces 
    sLine=strtrim(sLine);
    sLine=fremovespaces(sLine);

    if bDebug 
        fprintf('%s%s \n',pref_in,sLine);
    end

    % Simple case: start by a comment, replace it
    if isempty(sLine)
        sf=[sLine ' \n'];
        fprintf(fidf,[pref_out,sf]);
        if bPipe ; fprintf(1,[pref_out,sf]); end
%     elseif sLine(1)=='%'
%         sLine(1)='!';
%         sf=[sLine ' \n'];
%         disp(sf)
%         fprintf(fidf,[pref_out,sf]);
%         if bPipe ; fprintf(1,[pref_out,sf]); end
     else
        % the problematic printf case is dealt then
        if ~isempty(regexp(sLine,'printf\(')) 
            [ sf ] = freplaceprintf( sLine );
            fprintf(fidf,[pref_out,sf]);
            if bPipe ; fprintf(1,[pref_out,sf]); end
        else
            % comments replacement
            sLine=strrep(sLine,'%','!');
            % splitting matlab lines if commands on the same line
            [ sout ] = fsplitmatlablines( sLine,[] );
            for isplit=1:length(sout)
                s=sout{isplit};
                % ---------------------------------
                % The Main function
                % ---------------------------------
                [sf end_stack]=fconvertline(s,end_stack);
                % ---------------------------------
                sf=fremovespaces(sf);
                fprintf(fidf,[pref_out,sf]);
                if bPipe ; fprintf(1,[pref_out,sf]); end
            end
        end
    end
    sLine = fgets(fidm);
end

% if at the end we still have some end, output them
for ie=1:length(end_stack)
    fprintf(fidf,'end %s \n',end_stack{ie});
end

end

function [sf end_stack]=fconvertline(s,end_stack)
% function replace
I=strfind(s,'function');
if ~isempty(I) && I(1)==1
    [ sf end_stack] = freplacefunction( s , end_stack );
    return;
end

%% Simple regexprep or rep
% logical
s=regexprep(s,'~=','/=');
s=regexprep(s,'~(.*)','.not.($1)');
s=strrep(s,'&&','.and.');
s=strrep(s,'||','.or.');
s=strrep(s,'break','exit');
s=strrep(s,'true','.true.');
s=strrep(s,'false','.false.');

% intrisic
s=strrep(s,'strtrim(','trim(');
s=strrep(s,'ceil(','ceiling(');
s=strrep(s,'round(','int(');
s=strrep(s,'floor(','floor(');
s=strrep(s,'mod(','modulo('); %mod exists but return negative values
% array stuff
s=regexprep(s,'size\(([^,]*)\)','shape($1)'); %replace size by reshape when no comma inside, otherwise, size is good
s=strrep(s,'length(','size('); % or count...

% math
s=strrep(s,'dot','dot_product'); %replace size by reshape when no comma inside, otherwise, size is good
s=strrep(s,'^','**'); 

% strings
s=strrep(s,'\','\\'); 

%% Things that are easily recognizable by their first position on the line
% case
s=regexprep(s,'^case([^!;]*)','case ($1) !');
s=regexprep(s,'^otherwise*)','case default ');
sold=s;
s=regexprep(sold,'^switch([^!;]*)','select case ($1) !');
if length(s)~=length(sold) % we did replace something
    end_stack(2:(length(end_stack)+1))=end_stack(1:length(end_stack));
    end_stack{1}='select';
end

% for replace
I=strfind(s,'for');
if ~isempty(I) && I(1)==1
    [ sf end_stack] = freplacefor( s , end_stack );
    return;
end
% while replace
I=strfind(s,'while');
if ~isempty(I) && I(1)==1
    [ sf end_stack] = freplacewhile( s , end_stack );
    return;
end

% if replace
I=regexp(s,'if\ |if\(|elseif|else\ if');
if ~isempty(I) &&  min(I)==1
    [ sf end_stack] = freplaceif( s , end_stack );
    return;
end

% end replace
I=strfind(s,'end');
if ~isempty(I) && I(1)==1  
    %let's check that this is not a variable
    if length(s)==3 || s(4)==' ' || s(4)==';' ||s(4)=='!'
        [ sf end_stack] = freplaceend( s , end_stack );
        return;
    end
end

% function calls
I=strfind(s,'[');
if ~isempty(I) && I(1)==1 
    [ sf] = freplacefunctioncall( s );
    return;
end

% brackets
s=strrep(s,'[','(/');
s=strrep(s,']','/)');
% multiline
s=strrep(s,'...',''); %char(8) not enough..


%% More tricky things that should require syntax parsing and recursion but that we'll not do!..
% zeros and ones
I=regexp(s,'(zeros\(|ones\(|linspace\()');
if ~isempty(I)
    [ s] = freplacezeros( s );
end



% default return
sf=[s '\n'];
end

function [b po pc]=ffindmatching(s,pstart,co,cc,bforward);
% string, start position, character open, character close
%
% [b po pc]=ffindmatching('1(3(5)7()df) d',2,'(',')',1 )
%
b=0;
po=0;
pc=0;


if ~bforward
    error('not done')
end
found=0;
nopen=0;
Iopen=[];
i=pstart;
while ~found && i<=length(s)
    if s(i)==co
        nopen=nopen+1;
        Iopen=[Iopen i];
    end
    if s(i)==cc
        if nopen>=1 % this is a choice, if a closing cgaracter is found, before an opened one, we don't care...
            nopen=nopen-1;
            if nopen==0
                found=1;
                pc=i;
                po=Iopen(1);
                break
            end
        end
    end
    i=i+1;
end

b=found;

end
function [ b, p1,p2 ] = fissurroundedby( s,p,c1,c2 )
% looks if character s(p) is surrounded by the characters c1 and c2 
% returns a boolean , and the postion of these charactesrs
% [b p1 p2]=fissurroundedby('function [a b c]=issur([,])',10,'[',']')
% [b p1 p2]=fissurroundedby('function [a b c]=issur([,])',11,'[',']')
% [b p1 p2]=fissurroundedby('function [a b c]=issur([,])',10,'[',']')
% [b p1 p2]=fissurroundedby('function [''a b c'']=issur([,])',10,'''','''')
% [b p1 p2]=fissurroundedby('function [''a b c'']=issur([,])',12,'''','''')
% [b p1 p2]=fissurroundedby('(),()',4,'(',')')
p1=0;
p2=0;
%% stupid whiles
% look backward
notfound=1;
i=p-1;
while i>=1 && notfound
    if c1~=c2 
        % then if c2 is encountered we should break
        if s(i)==c2
                break
        end
    end
    if s(i)==c1
        notfound=0;
        p1=i;
    end
    i=i-1;
end

% look forward
notfound=1;
i=p+1;
while i<=length(s) && notfound
    if c1~=c2 
        % then if c1 is encountered we should break
        if s(i)==c1
            break
        end
    end
    if s(i)==c2
        notfound=0;
        p2=i;
    end
    i=i+1;
end


if p1==0 || p2==0
    b=0; % not surrounded
else
    b=1;
end
end
function [ s ] = fremovespaces( s )
% remove multiple spaces
s=regexprep(s,'[\ ]+',' ');

end

function [ sf, end_stack ] = freplaceend( s,end_stack )
% freplaceend('end !(ksdjf)',[])
% freplaceend('end  ; ! why not?',[])



if isempty(end_stack)
    warning('Too much ends.. Some stuff have not been accounted for?');
    end_stack{1}='';
end
sf=['end ' end_stack{1} ' '  s(4:end) '\n']; % could add a security comment..

% shortening the stack
end_stack=end_stack(2:length(end_stack));

end

function [ sf, end_stack ] = freplacefor( s,end_stack )

% freplacefor('for i=I',[])
% freplacefor('for i=1:0.2:10  ; ! why not?',[])


Icol=strfind(s,':');  
Ieq=strfind(s,'=');
ncol=length(Icol);
loopvar=strtrim(s(4:(Ieq(1)-1)));
sf=['do ' loopvar '='];
sloopextent=s((Ieq(1)+1):end);
[~,~,~,~,~,~,split]=regexp(sloopextent,':');
switch ncol
    case 0
        sf=[sf s((Ieq(1)+1):end)];
    case 1
        sf=[sf strtrim(split{1}) ',' strtrim(split{2})];
    case 2
        sf=[sf strtrim(split{1}) ',' strtrim(split{3}) ',' strtrim(split{2})];
end
sf=[sf ' \n'];
end_stack(2:(length(end_stack)+1))=end_stack(1:length(end_stack));
end_stack{1}='do';

end

function [ sf ] = freplacefunctioncall( s )
% this function assume the form: [ ]= kdsfkdsfj()
%
%
% freplacefunctioncall('[a , b]=ffunctionreplace( s,end_stack )  !',[])


ieq=strfind(s,'=');
iob=strfind(s,'[');
icb=strfind(s,']');

[b iop icp]=ffindmatching(s,ieq,'(',')',1);
% output arguments
isf=ieq+1;
s_argout=strtrim(s((iob+1):(icb-1)));
s_argout=strrep(s_argout,', ',',');
s_argout=strrep(s_argout,' ,',',');
s_argout=fremovespaces(s_argout);
s_argout=strrep(s_argout,' ',',');
% [a b c d e f c_argout]=regexp(s_argout,',');

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

% and output
sf=['call ' fname '( ' s_args  ') ' scomm '\n'];


end

function [ sf, end_stack ] = freplacefunction( s,end_stack )

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

% Output
sf=['!> \n'];
sf=[sf 'subroutine ' fname '(' s_args  ')\n'];
sf=[sf 'implicit none \n'];
for iin=1:length(c_argin)
    sf=[sf 'real*8, intent(in) :: ' c_argin{iin}    '       !> \n'];
end
for iout=1:length(c_argout)
    sf=[sf 'real*8, intent(out) :: ' c_argout{iout} '       !> \n'];
end

% stacking
end_stack(2:(length(end_stack)+1))=end_stack(1:length(end_stack));
end_stack{1}=['subroutine ' fname];

end

function [ sf, end_stack ] = freplaceif( s,end_stack )

% freplaceif('if (ksdjf)',[])
% freplaceif('if i==1 ; ! why not?',[])
% freplaceif('else if i==1 ; ! why not?',[])


Ielse=strfind(s,'else');
Iif=strfind(s,'if');

s_cond=strtrim(s((Iif(1)+2):end)); % the conditional string
s_comm='';
Iterm=strfind(s_cond,';'); %is there a termination..
Icomm=strfind(s_cond,'!'); %is there a comment


if ~isempty(Icomm) || ~isempty(Iterm)
    imin=min([Iterm Icomm]);
    s_comm=s_cond(imin:end);
    s_cond=s_cond(1:(imin-1));
end

if (s_cond(1)==' ' && s_cond(2) ~='(' ) || (s_cond(1)~=' ' && s_cond(1)~='(' ) %of course could be smarter
    s_cond=['(' strtrim(s_cond) ')'];
end

sf=['if ' s_cond ' then ' s_comm '\n'];
if ~isempty(Ielse) && Ielse(1)==1
    % else if 
    sf=['else ' sf];
else
    % if not an elseif then we stack
    end_stack(2:(length(end_stack)+1))=end_stack(1:length(end_stack));
    end_stack{1}='if';
end
end

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

function [ sf, end_stack ] = freplacewhile( s,end_stack )

% freplacewhile('while (i<I)!cool',[])
% freplacefor('while i>0;!why not?',[])


Icol=strfind(s,':');  
Ieq=strfind(s,'=');
ncol=length(Icol);
s_cond=strtrim(s(6:end));
s_comm='';

Iterm=strfind(s_cond,';'); %is there a termination..
Icomm=strfind(s_cond,'!'); %is there a comment
if ~isempty(Icomm) || ~isempty(Iterm)
    imin=min([Iterm Icomm]);
    s_comm=s_cond(imin:end);
    s_cond=s_cond(1:(imin-1));
end
if (s_cond(1)==' ' && s_cond(2) ~='(' ) || (s_cond(1)~=' ' && s_cond(1)~='(' ) %of course could be smarter
    s_cond=['(' strtrim(s_cond) ')'];
end
sf=['do while ' s_cond ' ' s_comm];
sf=[sf ' \n'];
end_stack(2:(length(end_stack)+1))=end_stack(1:length(end_stack));
end_stack{1}='do'; % enddo isneeded not end while

end

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
function [ sout ] = fsplitmatlablines( s,sout )
% recursive function that splits s into
% s=fsplitmatlablines('',[])
% s=fsplitmatlablines('a',[])
% s=fsplitmatlablines('a;',[])
% s=fsplitmatlablines('a ;',[])
% s=fsplitmatlablines('a ; bhjhj',[])
% s=fsplitmatlablines('a ; bhjhj ; sdifsidfj ;',[])
% s=fsplitmatlablines('a ; bhjhj '';'' sdifsidfj ;',[])


% ifound=strfind(s,';')
ifound=regexp(s,'[;]'); % the case with , requires more care and is dropped for now...
% ifound=regexp(s,'[;,]');
if ~isempty(ifound)
    notmeaningful=1;
    itry=1;
    while notmeaningful && itry<=length(ifound)
        if s(ifound(itry))==';' 
            [b1,p1,p3]=fissurroundedby(s,ifound(itry),'''','''');
            [b2,p1,p3]=fissurroundedby(s,ifound(itry),'[',']');
            b=b1+b2>0;
            % dealing with the case where what follows is a comment... this is nasty but well
            if ~b
                ii=ifound(itry);
                foundacomment=0;
                while ii<=length(s) && ~foundacomment;
                    if s(ii)=='!'
                        foundacomment=1;
                        break
                    end
                    if s(ii)==' ' || s(ii)==';'
                        ii=ii+1;
                    else
                        break
                    end
                end
                if foundacomment
                    s(ifound(itry))=' '; % removing the ';'
                    break
                end
            end
        else
            [b1]=fissurroundedby(s,ifound(itry),'[',']');
            [b2]=fissurroundedby(s,ifound(itry),'(',')');
            [b3]=fissurroundedby(s,ifound(itry),'''','''');
            b=b1+b2+b3>0;
        end
        if ~b
            notmeaningful=0;
            isplit=ifound(itry);
        end
        itry=itry+1;
    end
    if ~notmeaningful
        sout{end+1}=strtrim(s(1:(isplit-1)));
        if itry<length(s);
            send=strtrim(s((isplit+1):length(s)));
            if ~isempty(send)
                sout=fsplitmatlablines(send,sout);
            end
        end
    else
        sout{end+1}=s;
    end

else
    sout{end+1}=s;
end
end


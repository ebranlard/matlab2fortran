% DOCUMENTATION : matlab2fortran
% ---------------------------------------------------
% Brief description:
% matlab2fortran(matlab to fortran) : converting matlab code to fortran
% Usage: matlab2fortran(filename);
% 
% 
% Author:Emmanuel Branlard
% Date  : December 2012
% Version: 12.12a
% 
% License: None. Thank you for sharing your improvements to me by email.
% 
% Revision: 
%           15/12/12 : added declaration of variables at beginning of script or subroutines
%                      added handling of intent(inout), 
%                      corrected small bug for do loops and number of ":" since they can be in length(a(:,1)) 
%                      
% 
% 
% 
% matlab2fortran(matlab to fortran) is a code that performs an automatic conversion of one or multiple matlab files to fortran format.The script translate some of the main features of matlab to fortran. matlab2fortran performs a quick and dirty conversion based on a line-by-line basis (but still supports conditional loops and subroutine). The script allows multiple matlab command per line if these commands are separated by ";", but not by ",".
% 
% matlab2fortran does not intend to produce ready-to-compile code, but performs several necessary conversions. The generated code keeps the structure, variables names and comments of the original code. 
% 
% The script matlab2fortran (matlab to fortran) is a single script file written in matlab and does not require a particular installation other than maybe adding this script to your current folder or matlab path. The script has been tested on linux and windows.
% 
% Main Features of matlab2fortran:
% - conversion of nested structure: do, if, switch (select), while
% - conversion of function to subroutine with recognition of output and input arguments
% - perform subroutine list of arguments with intent(in) and intent(out)
% - make a declaration list for simple variable , sort them by Name or Type 
% - determines type based on variable name (function fgetVarTypeFromVarName)
% - recognize simple function call and convert them to subroutine calls (syntax [y] = f(x) );
% - performs allocation when converting matlab function zeros, ones (linspace possible)
% - splitting of lines that contain multiple matlab command separated by ;
% - comments
% - small support for sprintf and fprintf
% - misc conversions such as logical operators
% - few intrinsic functions conversion
% 
% TODOs:
% - better printf handling
% - Handling of [] and string concanenation
% - better handling of function calls and nested function calls..
% 
% 
% 
% Examples of calls:
% ----------------------------------------------------
% example 1:
% matlab2fortran('tests/test1.m');
% matlab2fortran('tests/test1.m','tests/test2.m');
% 
% example 2: all files in current directory (does not work with subfolders with dir..)
% FileList=dir('*.m');
% matlab2fortran(FileList.name);
function [  ] = matlab2fortran( varargin )

%% Parameters
bDebug=0;% show input in stdout
bPipe=0; % pipe output to stdout as well as in the file
bSortDeclarationbyNames=1; %
bSortDeclarationbyTypes=0; %

%%
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
    %% One file call
    file=varargin{1};
    fidm=fopen(file,'r');
    if fidm==-1
        error(['Unable to open input file: ' file]);
    end


    filef=regexprep(file,'\.m$','.f90');
    fileftmp=regexprep(file,'\.m$','_tmp.f90');
    fprintf('Converting to file: %s\n',filef);
    %     warning off
    if ~bDebug
        fidftmp=fopen(fileftmp,'w');
        if fidftmp==-1
            error(['Unable to open temporary output file: ' fileftmp]);
        end
        fidf=fopen(filef,'w');
        if fidf==-1
            error(['Unable to open output file: ' filef]);
        end
    else
        fidftmp=1; % standard output 
        fidf=1; % standard output 
    end
    %% Conversion line by line and ouput to file
    if ~bDebug
        warning off ; %WATCH OUT
    end
    [Declarations]=fconvertlinebyline(fidm,fidftmp,bDebug,bPipe);
    warning on
    fclose(fidm);
    if ~bDebug 
        fclose(fidftmp);
    end


    %% Sorting Declarations for each subroutine and main program
    Declarations=fFormatDeclarations(Declarations,bSortDeclarationbyNames,bSortDeclarationbyTypes,bDebug);


    %% Reading temp file, outputting to final file with declarations , only possible without Debug
    if ~bDebug
        fidftmp=fopen(fileftmp,'r');
        if fidftmp==-1
            error(['Unable to re open temporary output file: ' fileftmp]);
        end
        fWriteDeclarationsAtCorrectLocation(fidftmp,fidf,Declarations)
        fclose(fidftmp);
        delete(fileftmp);
        fclose(fidf);
    end % end output of declaration if not debug mode
end % end switch between one or two files

end %function



function [Declarations]=fconvertlinebyline(fidm,fidf,bDebug,bPipe)
sLine = fgets(fidm);
end_stack=[];
current_unit=1;
Declarations{current_unit}=[]; % contains all variables declaration
decl_stack=[];  % contains variable declaration for current subroutine or main..
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

    % Simple case: emptyline
    if isempty(sLine)
        sf=[sLine '\n'];
        fprintf(fidf,[pref_out,sf]);
        if bPipe ; fprintf(1,[pref_out,sf]); end
    % Simple case: start with a comment, still needs small handling
    elseif sLine(1)=='%'
%         kbd
        sLine(1)='!';
         sLine=strrep(sLine,'%','!');
%          sLine=strrep(sLine,'%','!');
        sLine=strrep(sLine,'\n','NewLine'); 
        sLine=strrep(sLine,'\','\\'); 
        sf=[sLine ' \n'];
%         sf=strrep(sf,'%','%%');
        fprintf(fidf,[pref_out,sf]);
        if bPipe ; fprintf(1,[pref_out,sf]); end
    else
        % the problematic printf case is dealt then
        if ~isempty(regexp(sLine,'[fs]+printf\(')) 
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
                [sf end_stack decl_stack unit_switch]=fconvertline(s,end_stack,decl_stack);
                if unit_switch~=0 
                    % storing the stack of declaration of previous unit
                    if bDebug
                        disp('Entering new unit')
                    end
                    Declarations{current_unit}=decl_stack;
                    current_unit=current_unit+unit_switch;
                    if unit_switch==1 
                        Declarations{current_unit}=[];
                        decl_stack=[];
                    end
                end
                % ---------------------------------
                sf=fremovespaces(sf);
                fprintf(fidf,[pref_out,sf]);
                if bPipe ; fprintf(1,[pref_out,sf]); end
            end
        end
    end
    sLine = fgets(fidm);
end

Declarations{current_unit}=decl_stack;

% if at the end we still have some end, output them
for ie=1:length(end_stack)
    fprintf(fidf,'end %s \n',end_stack{ie});
end

end %function


function [sf end_stack decl_stack unit_switch]=fconvertline(s,end_stack,decl_stack)
unit_switch=0;
% function replace
I=strfind(s,'function');
if ~isempty(I) && I(1)==1
    [ sf end_stack ] = freplacefunction( s , end_stack );
    unit_switch=1; %we are entering a new unit
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
s=strrep(s,'round(','anint('); % or just int?
s=strrep(s,'floor(','floor(');
s=strrep(s,'mod(','modulo('); %mod exists but return negative values
% array stuff
s=regexprep(s,'size\(([^,]*)\)','shape($1)'); %replace size by reshape when no comma inside, otherwise, size is good
s=strrep(s,'length(','size('); % or count...

% math
s=strrep(s,'dot','dot_product'); %replace size by reshape when no comma inside, otherwise, size is good
s=strrep(s,'^','**'); 

% strings
% s=strrep(s,'&','\\'); 
s=strrep(s,'\','\\'); 

%% Things that are easily recognizable by their first position on the line
% case
s=regexprep(s,'^case([^!;]*)','case ($1) !');
s=regexprep(s,'^otherwise*','case default ');
sold=s;
s=regexprep(sold,'^switch([^!;]*)','select case ($1) !');
if length(s)~=length(sold) % we did replace something
    % stacking
    end_stack=fstack_push(end_stack,'select');
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
I=regexp(s,'elseif|else if|if\ |if\(');  % watch out for the order here
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
% TODO with better string handling
% s=strrep(s,'[','(/');
% s=strrep(s,']','/)');
% multiline
% s=strrep(s,'...','\&');        %char(8) not enough..  %HACK : the escape sequence is not recognize and hence it merges with the next line...
s=regexprep(s,'(?<!!.*)\.\.\.','&'); % this line requires some explanation below
% If uses a negative lookbehind "(?<!)" It looks if there is no !.* before (ie, no comment)
% If this is not a commen then we replace the ... by &. This regexp will not work with octave, but it doesnt matter since it's a comment.
% Octabe will replace by & all the time
% If one want, & can be replace by \& so that matlab will not recognize this bad escpae character and hence will merge the current line with the next.. but that's a HACK


%% More tricky things that should require syntax parsing and recursion but that we'll not do!..
% zeros and ones
I=regexp(s,'(zeros\(|ones\(|linspace\()');
if ~isempty(I)
    [ s , decl_stack] = freplacezeros( s ,decl_stack);
else
    % stacking assignements so that they can be used for declarations
    [ s , decl_stack] = fassignement( s ,decl_stack);
end

% default return
sf=[s '\n'];
end %function






function fWriteDeclarationsAtCorrectLocation(fidftmp,fidf,Declarations)
current_unit=1;
sread = fgets(fidftmp);
while sread ~= -1
    if current_unit==1
        % wait for the first non comment empty to output the declaration
        if ~isempty(Declarations{1})  
            if ~isempty(sread) && sread(1)~='!'
                fprintf(fidf,'! Variable declarations \n');
                fwrite_Declarations(fidf,Declarations{current_unit});
                fprintf(fidf,'! \n');
                Declarations{1}=[];
            end
        end
    end
    I=strfind(sread,'subroutine');
    if ~isempty(I) && I(1)==1 
        %         if current_unit==1 && ~isempty(Declarations{current_unit})
        %             % case of first unit, i.e. main program 
        %             % % we need to output the declaration here
        %             fwrite_Declarations(fidf,Declarations{current_unit});
        %         end
        current_unit=current_unit+1;    %
    end
    I=strfind(sread,'!M2F-HERE-XXX');
    if ~isempty(I) && I(1)==1 
        fwrite_Declarations(fidf,Declarations{current_unit});
        sread='';
    end
    sread=strrep(sread,'\','\\'); % required 
    fprintf(fidf,sread);
    sread = fgets(fidftmp);
end %while reading file tmp
end %function fWriteDeclarationsAtCorrectLocation


function Declarations=fFormatDeclarations(Declarations,bSortDeclarationbyNames,bSortDeclarationbyTypes,bDebug)
for iud=1:length(Declarations)
    if bDebug
        fprintf(['-------------Declaration for unit ' num2str(iud) '\n']);
    end
    %             fprintf(2,Declarations{iud}{id});
    Decl=Declarations{iud};
    if length(Decl)>0 
        VarNames = cellfun(@(x)x.name, Decl, 'UniformOutput',false);
        % unique
        [~, Isort] = unique(VarNames); % NOTE: case ins-sensitive so that the user sees the possible clash since fortran is case insensitive
        [~, Isort2] = unique(lower(VarNames));
        if length(Isort)~=length(Isort2)
            warning('Be careful, there are variables that have the same characters but different cases.');
        end
        Decl=Decl(Isort);
        VarNames = cellfun(@(x)x.name, Decl, 'UniformOutput',false);
        VarTypes = cellfun(@(x)x.name, Decl, 'UniformOutput',false);

        % sorting
        if bSortDeclarationbyNames
            [~, Isort] = sort(lower(VarNames));  % NOTE: the lower here is a matter of choice
        elseif bSortDeclarationbyTypes
            [~, Isort] = sort(lower(VarTypes));
        else
            Isort=1:length(VarNames);
        end
        Decl=Decl(Isort);

        % Printing
        if bDebug
            fwrite_Declarations(1,Decl);
        end
    end
    Declarations{iud}=Decl;
end
end %function format Declarations


function []=fwrite_Declarations(fidout,Decl)
for id=1:length(Decl)
    [ sf ] = fgetDeclaration(Decl{id} );
    fprintf(fidout,sf);
end
end % function

function [ s , decl_stack ] = fassignement( s , decl_stack )
% only simple assignments are allowed x1_var =... , I.element =
% It is chosen to keep the dot in the variable name...
% if an assignment is of the form a(:,1) we dont cons
%

Ieq=strfind(s,'=');
if ~isempty(Ieq)
    Is=regexp(s,'[a-z.A-Z0-9_ ]+=');
    if ~isempty(Is) && Is(1)==1
        v.name=strtrim(s(1:(Ieq(1)-1)));
    else
        v.name='TODO';
    end
    [v.type, v.shape, v.prop] =fgetVarTypeFromName(v.name);
    v.comment='';
    decl_stack=fstack_push(decl_stack,v);
end


end %function

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
    varprop=[v.prop ', '];
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

varcomment=[v.comment '     !> '];


sf=sprintf('%s%s%s :: %s %s \n',vartype,varshape,varprop,varname,varcomment);

end

function [ vartype, varshape,varprop ] = fgetVarTypeFromName( varname )
% default type
vartype='real*8';
varshape='';
varprop='';
% attempt a type conversion based on first character, this is my own convention, comment if needed
if varname(1)=='i' || varname(1)=='n'
    vartype='integer';
elseif varname(1)=='I'
    vartype='integer';
    varshape=':';
elseif varname(1)=='b'
    vartype='logical';
elseif varname(1)=='s'
    vartype='character(len=:)';
elseif varname(1)=='v'
    vartype='real*8';
    varshape=':';
else
    vartype='real*8';
end

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
function [ s ] = fremovesinglespace( s )
% remove multiple spaces
s=regexprep(s,'[\ ]+','');

end %function

function [ s ] = fremovespaces( s )
% remove multiple spaces
s=regexprep(s,'[\ ]+',' ');

end

function [ sf, end_stack ] = freplaceend( s,end_stack )
% freplaceend('end !(ksdjf)',[])
% freplaceend('end  ; ! why not?',[])

if isempty(end_stack)
    warning('Too much ends in the file.. Some stuff have not been accounted for?');
    end_stack{1}='';
end

[end_stack, elmt] = fstack_pop(end_stack);
sf=['end ' elmt ' '  s(4:end) '\n']; % could add a security comment..


end %function

function [ sf, end_stack ] = freplacefor( s,end_stack )

% freplacefor('for i=I',[])
% freplacefor('for i=1:0.2:10  ; ! why not?',[])
% freplacefor('for iarg=1:length(c_all_args(:,1)',[])

Icol=strfind(s,':');  
Ieq=strfind(s,'=');
ncol=length(Icol);
loopvar=strtrim(s(4:(Ieq(1)-1)));
sf=['do ' loopvar '='];
sloopextent=s((Ieq(1)+1):end);
[~,~,~,~,~,~,split]=regexp(sloopextent,':');

% nasty handling of colum that are used for index ranges..
splitb=[];
for is=1:length(split)
    ssplit=split{is};
    no=length(strfind(ssplit,'('));
    nc=length(strfind(ssplit,')'));
    if no>nc 
        if (is+1)<=length(split)
            splitb{end+1}=[split{is},':' split{is+1}];
        end
    elseif nc>no
        % should have been accounted by previous step
    else
        splitb{end+1}=split{is};
    end
end

split=splitb;
ncol=length(split)-1;
switch ncol
    case 0
        sf=[sf s((Ieq(1)+1):end)];
    case 1
        sf=[sf strtrim(split{1}) ',' strtrim(split{2})];
    case 2
        sf=[sf strtrim(split{1}) ',' strtrim(split{3}) ',' strtrim(split{2})];
end
sf=[sf ' \n'];
% stacking
end_stack=fstack_push(end_stack,'do');

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
c_all_args=[];
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
sf=[sf '! Use somemodule \n'];
if ~isempty(c_all_args)
    sf=[sf '! Input/output arguments \n'];
    for iarg=1:length(c_all_args(:,1))
        v.name=c_all_args{iarg,1};
        [ v.type, v.shape,v.prop ] = fgetVarTypeFromName( v.name );
        if ~isempty(v.prop)
            v.prop=[v.prop ','];
        end
        v.comment='';
        v.prop=[v.prop 'intent(' c_all_args{iarg,2} ')'];
        [ sfdecl ] = fgetDeclaration( v );
        sf=[sf sfdecl];% ', intent(' c_all_args{iarg,2} ') :: '     '       !> \n'];
    end
end
sf=[sf '! Variables declaration \n']; 
sf=[sf '!M2F-HERE-XXX \n']; 
sf=[sf '!  \n'];



% stacking
end_stack=fstack_push(end_stack,['subroutine ' fname]);

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
    % stacking
    end_stack=fstack_push(end_stack,'if');
end
end % function

function [ sf ] = freplaceprintf( s )
sf=s;
I=regexp(s,'[sf]+printf\(');
if ~isempty(I) 
    ip=I(1);%only deals with one printf...
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
    
    s_print=s((p1+1):(p2-1)); % not safe and will keep the %...
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
% stacking
end_stack=fstack_push(end_stack,'do');

end

function [ sf,decl_stack] = freplacezeros( s, decl_stack)

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
sf= sprintf('%sTODO = %d.0D0 \n',sf,value);
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
    Is=regexp(s,'[a-z.A-Z0-9_ ]+=[() *^.+0-9]*(zeros\(|ones\()');
    if ~isempty(Is) && Is(1)==1
        % Cool
        varname=s(1:(Ieq(1)-1));
    else
        Is2=regexp(s,'[a-z.A-Z0-9_ ]+=');
        if ~isempty(Is2) && Is2(1)==1
            varname=s(1:(Ieq(1)-1));
        else
            varname='TODO';
        end
    end
    n_comma=length(strfind(s_size,','));
    s_shape=':';
    for is=2:n_comma+1
        s_shape=[s_shape,',:'];
    end

    v.type='real*8'; 
    v.name=strtrim(varname);
    v.shape=s_shape;
    v.prop='';
    v.comment=['! m2f:check dimension(' s_size ')'] ;

    decl_stack=fstack_push(decl_stack,v);

    sf= [sf 'if (allocated(' varname ')) deallocate(' varname ')            !m2f: check if wanted\n'];
    sf= [sf 'allocate(' varname '(' s_size '))\n'];
    sf= sprintf('%s%s = %d.D0 \n',sf,varname,value);
    sf= [sf varname ' =' pre varname post '\n'];
end
end %function
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

function [ stack , element] = fstack_pop_element( stack, n )
% remove an element at given location
element=stack(n);
stack=stack(setdiff(1:length(stack),n));
end %function

function [ stack ,element ] = fstack_pop( stack )
% remove the head
element=stack{end};
% shortening the stack
stack=stack(1:(length(stack)-1));
end %function

function [ stack ] = fstack_push( stack, addme )
% add an element at the tail of variable stack
stack{end+1}=addme;

end %function


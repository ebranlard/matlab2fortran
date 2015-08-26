% # DOCUMENTATION : matlab2fortran
% 
% ## Brief description:
% 
% * matlab2fortran(matlab-to-fortran) : converting matlab code to fortran
% * Usage: matlab2fortran(filename);
% * Author:Emmanuel Branlard (contributors are welcome)
% * Creation Date  : December 2012
% * Last revision  : 2015-08-26
% * Version: 1.0-29-g83df194 
% * Web-Sites: 
%     - http://github.com/elmanuelito/matlab2fortran
%     - http://emmanuel.branlard.free.fr/work/programming/
% * License: None. Thank you for sharing your improvements to me by email.
% 
% 
% ## DESCRIPTION
% 
% matlab2fortran(matlab-to-fortran) is a code that performs an automatic conversion of one or multiple matlab files to fortran format. The script translate some of the main features of matlab to fortran. matlab2fortran performs a quick and dirty conversion on a line-by-line basis (but still supports conditional loops, subroutine). The script allows multiple matlab command per line if these commands are separated by ";", but not by ",".
% 
% matlab2fortran does not intend to produce ready-to-compile code, but performs several necessary conversions. The generated code keeps the structure, variables names and comments of the original code. 
% 
% 
% ## INSTALLATION AND REQUIREMENTS
% 
% The script is located in the release directory of the repository.
% The script matlab2fortran (matlab-to-fortran) is a single script file written in matlab. It does not require a particular installation other than maybe adding this script to your current folder or matlab path. 
% 
% 
% The script has been tested on linux and windows.
% The script has been tested on matlab and octave. 
% (Octave generates some warnings. Ouputs are the same expect for some replacement of "...")
% 
% 
% 
% ## REVISIONS
% 
% * 14/12/12 : first release
% 
% * 15/12/12 : added declaration of variables at beginning of script or subroutines
%          added handling of intent(inout), 
%          corrected small bug for do loops and number of ":" since they can be in length(a(:,1)) 
% 
% * 18/12/12 :
%         - Get the function arguments do the decl_stack
%         - Handling of [] for array constructor and string concanenation
%         - support for transpose ' 
%         - When allocation for zeros, and ones, don't add the line x=x if no operation if present
%         - when removing duplicates from the stack, don't loose data
%         - Solved Bug for parenthesis in case like: if (a) && b which needs to give ((a) .and. b)
%                      
% * 16/10/13 : From now on, revisions will only be listed vith git commits
% 
% 
% 
% ## FEATURES AND TODOs
% 
% ### Main Features of matlab2fortran:
% 
% - conversion of nested structure: do, if, switch (select), while
% - conversion of function to subroutine with recognition of output and input arguments, (output arguments are put at the end)
% - perform subroutine list of arguments with intent(in), intent(out), intent(inout)
% - make a declaration list for simple variable , sort them by Name or Type 
% - determines type based on variable name (function fgetVarTypeFromVarName) and some syntax
% - does its best to determine array size with things like :, [], zeros
% - recognize simple function call and convert them to subroutine calls (syntax [y] = f(x) );
% - performs allocation when converting matlab function zeros, ones (linspace possible)
% - splitting of lines that contain multiple matlab command separated by ;
% - comments
% - small support for sprintf and fprintf
% - small support for string concatenation
% - small support for transpose when written as '
% - misc conversions such as logical operators
% - few intrinsic functions conversion
% - replaces end by size(XXX,XXX)
% - Provides Doxygen compatible comments for subroutine and arguments
% 
% ### TODOs:
% - easy: replace also & and | after && and ||
% - better printf handling
% - better handling of function calls and nested function calls..
% - inline ifs?
% - isequal
% 
% Requests:
% - In “do while” construct, it does not put the logical expression in brackets.
% - It does not convert “pi” to a real parameter having the value of 3.1415…
% - It does not detect some of Matlab’s intrinsic functions (e.g., mean, std, nnz). It considers them as variables.
% - It does not convert some intrinsic functions such as “fopen”.
% - It considers the variables in array bounds subscript (e.g., t in M(s,t)) real, while they are integers.
% 
% 
% ### Features that are not intented to be supported
% 
% - Ready-to-compile output and perfection...
% - Parsing of line and tree-like syntax detection
% - Full type detection
% - Type detection based on Matlab workspace output
% - Detecting all variables
% 
% 
% ## RUN INSTRUCTIONS and PARAMETERS
% 
% The script should run in few seconds for a file a 1000 lines (say 2s). If it takes longer, activate the Debug flag bDebug=1; to see where the script is stuck...
% Several parameters are found at the beginning of the script.
% 
% 
% ## EXAMPLES
% 
% example 1: one file
% matlab2fortran('tests/test1.m');
% 
% example 2: list of files
% matlab2fortran('tests/test1.m','tests/test2.m');
% 
% example 3: all files in current directory (does not work with subfolders with dir..)
% FileList=dir('*.m');
% matlab2fortran(FileList.name);
% 
% 
% 
% 
% 
function [  ] = matlab2fortran( varargin )

%% Parameters
global DOUBLE_KIND DOUBLE_KIND_SUFFIX;

bDebug=0;% show input in stdout
bPipe=0; % pipe output to stdout as well as in the file
bSortDeclarationbyNames=1; %
bSortDeclarationbyTypes=0; %

DOUBLE_KIND='MK'; DOUBLE_KIND_SUFFIX='_MK';
% DOUBLE_KIND='8'; DOUBLE_KIND_SUFFIX='D0';

%%
if nargin==0
    error('Empty file list')
end

%% dealing with multiple input files "recursively"..
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
    Declarations=fSortDeclarations(Declarations,bSortDeclarationbyNames,bSortDeclarationbyTypes,bDebug);


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
                [sf end_stack decl_stack unit_switch decl_stack_new]=fconvertline(s,end_stack,decl_stack);
                if unit_switch~=0 
                    % storing the stack of declaration of previous unit
                    if bDebug
                        disp('Entering new unit')
                    end
                    Declarations{current_unit}=decl_stack;
                    current_unit=current_unit+unit_switch;
                    if unit_switch==1 
                        Declarations{current_unit}=[];
                        decl_stack=decl_stack_new;
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


function [sf end_stack decl_stack unit_switch decl_stack_new]=fconvertline(s,end_stack,decl_stack)
unit_switch=0;
decl_stack_new=[];
% function replace
I=strfind(s,'function');
if ~isempty(I) && I(1)==1
    [ sf end_stack decl_stack_new] = freplacefunction( s , end_stack );
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
s=strrep(s,'norm(','norm2(');
s=strrep(s,'ceil(','ceiling(');
s=strrep(s,'round(','anint('); % or just int?
s=strrep(s,'floor(','floor(');
s=strrep(s,'mod(','modulo('); %mod exists but return negative values
% array stuff
s=regexprep(s,'size\(([^,]*)\)','shape($1)'); %replace size by reshape when no comma inside, otherwise, size is good
s=strrep(s,'length(','size('); % or count maybe..

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
I=regexp(s,'for[( ]');
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
if ~isempty(I)
    if I(1)==1  
        %let's check that this is not a variable
        if length(s)==3 || s(4)==' ' || s(4)==';' ||s(4)=='!'
            [ sf end_stack] = freplaceend( s , end_stack );
            return;
        end
    else
        % Dealing with end in the middle
        s=regexprep(s,'([^a-zA-Z0-9_]+)end([^a-zA-Z0-9_]+)','$1size(XXX,XXX)$2');
    end
end

% function calls
I=strfind(s,'[');
if ~isempty(I) && I(1)==1 
    [ sf, decl_stack] = freplacefunctioncall( s, decl_stack );

    return;
end

% multiline
% s=strrep(s,'...','\&');        %char(8) not enough..  %HACK : the escape sequence is not recognize and hence it merges with the next line...
s=regexprep(s,'(?<!!.*)\.\.\.','&'); % this line requires some explanation below
% If uses a negative lookbehind "(?<!)" It looks if there is no !.* before (ie, no comment)
% If this is not a commen then we replace the ... by &. This regexp will not work with octave, but it doesnt matter since it's a comment.
% Octave will replace by & all the time
% If one want, & can be replace by \& so that matlab will not recognize this bad escpae character and hence will merge the current line with the next.. but that's a HACK


%% More tricky things that should require syntax parsing and recursion but that we'll not do!..
% zeros and ones
% I=regexp(s,'(zeros\(|ones\(|linspace\()');
I=regexp(s,'(zeros\(|ones\()');
if ~isempty(I)
    [ s , decl_stack] = freplacezeros( s ,decl_stack);
else
    % stacking assignements so that they can be used for declarations
    [ s , decl_stack] = fassignement( s ,decl_stack); % calls freplacesbracket
end


%% brackets
% I=regexp(s,'(zeros\(|ones\(|linspace\()');
% if ~isempty(I) && mod(length(I),2)==0
% else
% TODO with better string handling
% s=strrep(s,'[','(/');
% s=strrep(s,']','/)');


% default return
sf=[s '\n'];
end %function






function fWriteDeclarationsAtCorrectLocation(fidftmp,fidf,Declarations)
% kbd
current_unit=1;
sread = fgets(fidftmp);
while sread ~= -1
    if current_unit==1
        % wait for the first non comment empty to output the declaration
        if ~isempty(Declarations{1,1})  
            if ~isempty(sread) && sread(1)~='!'
                fprintf(fidf,'! Variable declarations \n');
                fwrite_Declarations(fidf,Declarations{current_unit,1});
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
        if ~isempty(Declarations{current_unit,2})  
            fprintf(fidf,'! Arguments declarations \n');
            fwrite_Declarations(fidf,Declarations{current_unit,2});
        end
        if ~isempty(Declarations{current_unit,1})  
            fprintf(fidf,'! Variable declarations \n');
            fwrite_Declarations(fidf,Declarations{current_unit,1});
        end
        sread='';
    end
    sread=strrep(sread,'\','\\'); % required 
    fprintf(fidf,sread);
    sread = fgets(fidftmp);
end %while reading file tmp
end %function fWriteDeclarationsAtCorrectLocation


function DeclarationsOut=fSortDeclarations(Declarations,bSortDeclarationbyNames,bSortDeclarationbyTypes,bDebug)
for iud=1:length(Declarations)
    if bDebug
        fprintf(['-------------Declaration for unit ' num2str(iud) '\n']);
    end
    %             fprintf(2,Declarations{iud}{id});
    Decl=Declarations{iud};
    DeclArguments=[];
    if length(Decl)>0 
        VarNames = cellfun(@(x)x.name, Decl, 'UniformOutput',false);
        % sort by name first for convenience
        [~, Isort] = sort(lower(VarNames));  % NOTE: the lower here is a matter of choice
        Decl=Decl(Isort);

        %% Removing duplicates declaration entry (now by merging)
        iD=1;
        while iD+1<=length(Decl)
            if ~isequal(Decl{iD}.name,Decl{iD+1}.name)
                % we just increment
                iD=iD+1;
            else
                % we merge
                D=fmergeDeclarations(Decl{iD},Decl{iD+1});
                Decl{iD}=D;
                Decl=fstack_pop_element(Decl,iD+1);
            end
        end
        %% Check for same variable different case
        [~, Isort] = unique(VarNames); % NOTE: case ins-sensitive so that the user sees the possible clash since fortran is case insensitive
        [~, Isort2] = unique(lower(VarNames));
        if length(Isort)~=length(Isort2)
            warning('Be careful, there are variables that have the same characters but different cases.');
        end

        %% Dealing with intent first
        VarProp = cellfun(@(x)x.prop, Decl, 'UniformOutput',false);
        Pintent=strfind(VarProp,'intent');
        Iintent = cellfun(@(x)~isempty(x), Pintent, 'UniformOutput',true);
        DeclArguments=Decl(Iintent);
        Decl=Decl(~logical(Iintent));
        %         Decl=Decl(Isort);
        VarNamesA = cellfun(@(x)x.name, DeclArguments, 'UniformOutput',false);
        VarTypesA = cellfun(@(x)x.name, DeclArguments, 'UniformOutput',false);
        VarNames = cellfun(@(x)x.name, Decl, 'UniformOutput',false);
        VarTypes = cellfun(@(x)x.name, Decl, 'UniformOutput',false);

        % sorting
        if bSortDeclarationbyNames
            [~, IsortA] = sort(lower(VarNamesA));  % NOTE: the lower here is a matter of choice
            [~, Isort] = sort(lower(VarNames));  % NOTE: the lower here is a matter of choice
        elseif bSortDeclarationbyTypes
            [~, IsortA] = sort(lower(VarTypesA));
            [~, Isort] = sort(lower(VarTypes));
        else
            IsortA=1:length(VarNamesA);
            Isort=1:length(VarNames);
        end
        DeclArguments=DeclArguments(IsortA);
        Decl=Decl(Isort);

        % Printing
        if bDebug
            fwrite_Declarations(1,Decl);
        end
    end
    DeclarationsOut{iud,1}=Decl;
    DeclarationsOut{iud,2}=DeclArguments;
end
end %function format Declarations


function []=fwrite_Declarations(fidout,Decl)
for id=1:length(Decl)
    [ sf ] = fgetDeclaration(Decl{id} );
    fprintf(fidout,sf);
end
end % function

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
found=false;
nopen=0;
Iopen=[];
i=pstart;
while ~found && i<=length(s)
    if s(i)==co
        nopen=nopen+1;
        Iopen=[Iopen i];
    end
    if s(i)==cc
        if nopen>=1 %NOTE: this is a choice, if a closing character is found, before an opened one, we don't care..
            nopen=nopen-1;
            if nopen==0
                found=true;
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
global DOUBLE_KIND
if isempty(v.type)
    vartype=sprintf('real(%s)',DOUBLE_KIND);
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
varcomment=[' !< ' v.comment(1:min(20,length(v.comment)))];


sf=sprintf('%s%s%s :: %s %s \n',vartype,varshape,varprop,varname,varcomment);

end

function [ vartype, varshape,varprop ] = fgetVarTypeFromName( varname )
global DOUBLE_KIND;
% default type
vartype='';
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
    vartype='character(len=*)';
elseif length(varname)>=3 & isequal(varname(1:3),'cpt')
    vartype='integer';
elseif varname(1)=='v'
    vartype=sprintf('real(%s)',DOUBLE_KIND);
    varshape=':';
else
    vartype=''; % this is postponed to the writting
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

else
    % we give up for now, no conversion done
    sf=s;
end

end %function

function [ sf, end_stack ] = freplaceif( s,end_stack )

% freplaceif('if (ksdjf)',[])
% freplaceif('if i==1 ; ! why not?',[])
% freplaceif('else if i==1 ; ! why not?',[])


Ielse=strfind(s,'else');
Iif=strfind(s,'if');

% splitting the string in substrings
s_cond=strtrim(s((Iif(1)+2):end)); % the conditional string
s_comm='';
Iterm=strfind(s_cond,';'); %is there a termination..
Icomm=strfind(s_cond,'!'); %is there a comment


if ~isempty(Icomm) || ~isempty(Iterm)
    imin=min([Iterm Icomm]);
    s_comm=s_cond(imin:end);
    s_cond=strtrim(s_cond(1:(imin-1)));
end

%Adding parenthesis if needed
[bMatched po pc]=ffindmatching(s_cond,1,'(',')',1);
if ~(pc==length(s_cond) && po==1 && bMatched)
    s_cond=['(' strtrim(s_cond) ')'];
% if (s_cond(1)==' ' && s_cond(2) ~='(' ) || (s_cond(1)~=' ' && s_cond(1)~='(' ) %of course could be smarter
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

function [ s , vtype,ncol,nlines] = freplacesbracket( s);
% [ s , var1,ncol,nlines] = freplacesbracket( 'aaaa=[]!' );
% 
%
%
vtype='';
ncol=0;
nlines=0;

[b po pc]=ffindmatching(s,1,'[',']',1);
if b
    % pre and post strings without bracket
   spre=s(1:(po-1));
   spost=strtrim(s((pc+1):end));
    % inside bracket string
   sb=strtrim(s((po+1):(pc-1)));
   napostroph=length(strfind(sb,''''));
   if napostroph>0 && mod(napostroph,2)==0
       % deal with string, in an ugly way
       sb=strtrim(fremovespaces(sb));
       is=1;
       bwithin=0;
       while is<=length(sb)
           if sb(is)==''''
              if ~bwithin
                   bwithin=1;
               else
                   bwithin=0;
               end
           end
           if (sb(is)==' ' || sb(is)==',' || sb(is)==';')   && ~bwithin
               % concatenate strings
               sb=[sb(1:(is)) '// ' sb((is+1):end)];
               is=is+4;
           else
               is=is+1;
           end
       end
    vtype='character(len=*)';


   else
       % matching brackets have been found.. Assumed not including furtger brackets TODO
       % some really ugly stuff below to accept spaces, commas and semi-column as separator. A good regexp would help
       sb=fremovespaces(strtrim(sb)); % remove double spaces
       
       sb=strrep(sb,'; ', ';'); % 
       sb=strrep(sb,' ;', ';'); % 
       
       sb=strrep(sb,',', ' '); % replacing commas
       sb=fremovespaces(strtrim(sb)); % remove double spaces again
       % Now spaces have meaning, so we replace them by ,
       sb=strrep(sb,' ', ','); % replacing spaces
        
       nc=length(strfind(sb,','));
       nsc=length(strfind(sb,';'));
       nlines=nsc+1;
%      if nlines==1
           ncol=floor(nc/nlines)+1;
%        else
%            ncol= 
%        end
       if min(ncol,nlines)==1
           % no need to reshape, I think...
           sb=sprintf('[ %s ] ',strrep(sb,';', ','));
       else
           sb=sprintf('reshape([ %s ] , [ %d , %d] )',strrep(sb,';', ','),ncol,nlines);
       end

   end

   s=[spre sb spost];

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
    global DOUBLE_KIND_SUFFIX  DOUBLE_KIND

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
sf= sprintf('%sTODO = %d.0%s \n',sf,value,DOUBLE_KIND_SUFFIX);
% else
% a = 1.5 ! Initial value
% y = (/((i*a),i=1,100)/) ! m2f: kind of linspace
%     sf= [s '\n']; %we can leave the linspace, and make a function that does that.
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
    varname=strtrim(varname);
    v.type=sprintf('real(%s)',DOUBLE_KIND); 
    v.name=strtrim(varname);
    v.shape=s_shape;
    v.prop='';
    v.comment=['! m2f:check dim(' s_size ')'] ;

    decl_stack=fstack_push(decl_stack,v);

    sf= [sf 'if (allocated(' varname ')) deallocate(' varname ')\n'];
    sf= [sf 'allocate(' varname '(' s_size '))\n'];
    sf= sprintf('%s%s = %d.0%s \n',sf,varname,value,DOUBLE_KIND_SUFFIX);
    
    posttrim=strtrim(post);
    if isempty(strtrim(pre)) && (isempty(posttrim) || posttrim(1)=='!')
        % ayaya...
    else
        sf= [sf varname ' =' pre varname post '\n'];
    end
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
ifound=regexp(s,'[;]'); % the case with , requires more care and is dropped for now..
% ifound=regexp(s,'[;,]');
if ~isempty(ifound)
    notmeaningful=1;
    itry=1;
    while notmeaningful && itry<=length(ifound)
        if s(ifound(itry))==';' 
            [b1,p1,p3]=fissurroundedby(s,ifound(itry),'''','''');
            [b2,p1,p3]=fissurroundedby(s,ifound(itry),'[',']');
            b=b1+b2>0;
            % dealing with the case where what follows is a comment.. this is nasty but well
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

function [ stack ,element ] = fstack_pop( stack )
% remove the head
element=stack{end};
% shortening the stack
stack=stack(1:(length(stack)-1));
end %function

function [ stack , element] = fstack_pop_element( stack, n )
% remove an element at given location
element=stack(n);
stack=stack(setdiff(1:length(stack),n));
end %function

function [ stack ] = fstack_push( stack, addme )
if isempty(stack)
    stack=[];
end
% add an element at the tail of variable stack
stack{end+1}=addme;

end %function


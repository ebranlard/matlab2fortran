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
% Octabe will replace by & all the time
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

